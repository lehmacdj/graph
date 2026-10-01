-- | Compile surface paths to route patterns (docs/path-pattern-ir.md §2).
--
-- Compilation has three steps:
--
-- 1. push @~@ down to the steps, reversing sequences;
-- 2. build a small expression term and distribute unjoined sequencing over
--    conjunction, so that every prefix that is not joined is copied;
-- 3. glue the term into routes: every step occurrence gets a fresh variable
--    and edge id, @/@ identifies an output with an input, and a join merges
--    outputs.
module Models.PathPattern.Compile
  ( compilePath,
    compileAlternatives,
    Expr (..),
    pushBackwards,
    surfaceExprs,
    glueExpr,
  )
where

import Control.Monad.State.Strict qualified as S
import Models.Path.Simple
import Models.PathPattern
import MyPrelude hiding (fragment)

compilePath :: Path -> PathQuery
compilePath =
  PathQuery
    . setFromList
    . mapMaybe (fmap canonicalize . glueExpr)
    . toList
    . surfaceExprs
    . pushBackwards

-- * Backwards

-- | Push 'Backwards' down to the steps. @~(p/q) = ~q/~p@, and @~@
-- distributes over @&@ and @+@. Node specs are unaffected.
pushBackwards :: Path -> Path
pushBackwards = \case
  Backwards p -> invert (pushBackwards p)
  p :/ q -> pushBackwards p :/ pushBackwards q
  p :& q -> pushBackwards p :& pushBackwards q
  p :+ q -> pushBackwards p :+ pushBackwards q
  p -> p
  where
    invert = \case
      Backwards p -> p
      p@(Literal _) -> Backwards p
      p@Wild -> Backwards p
      p@(RegexMatch _) -> Backwards p
      p :/ q -> invert q :/ invert p
      p :& q -> invert p :& invert q
      p :+ q -> invert p :+ invert q
      p -> p

-- * Expressions

-- | A transient term. 'Stay' continues at its input, a 'Join' merges every
-- output of its child into one, and 'Sequence' feeds its prefix's single
-- output to the continuation.
data Expr
  = Source PVertex
  | Stay
  | Step Direction Matcher
  | Both Expr Expr [Expr]
  | Join Expr NodeTest
  | Sequence Expr Expr
  deriving stock (Eq, Ord, Show)

-- | The alternatives of a path, rooted and distributed.
surfaceExprs :: Path -> Set Expr
surfaceExprs = mapSet supplyRoot . fragment

fragment :: Path -> Set Expr
fragment = \case
  One -> one Stay
  Zero -> mempty
  Wild -> step Out MWild
  Literal t -> step Out (MLiteral t)
  RegexMatch r -> step Out (MRegex r)
  Backwards Wild -> step In MWild
  Backwards (Literal t) -> step In (MLiteral t)
  Backwards (RegexMatch r) -> step In (MRegex r)
  Backwards _ -> error "fragment: pushBackwards leaves ~ only on steps"
  Absolute nid -> one (Join Stay (Exactly nid))
  ExcludingNIDs ns -> one (Join Stay (Except ns))
  p :+ q -> fragment p <> fragment q
  p :& q -> combine (\a b -> conjoin a [b]) (fragment p) (fragment q)
  p :/ q -> combine sequenceExpr (atBoundary p) (atBoundary q)
  where
    one = singletonSet
    step d m = one (Step d m)
    -- At a sequence boundary @\@@ joins the preceding outputs, also when it
    -- is one alternative of a union. Otherwise @(\@ + a)/\@N@ would lose the
    -- @\@@ and root its @\@N@ alternative, instead of testing the current
    -- node like @\@/\@N@.
    atBoundary p = flip mapSet (fragment p) \case
      Stay -> Join Stay anyNode
      e -> e
    combine f a b = setFromList [f x y | x <- toList a, y <- toList b]

-- | Each alternative a path compiles to, with a path without @+@ (a choice
-- of one side of each, with @~@ pushed down to the steps) that compiles to
-- just that alternative. This follows 'fragment', keeping a representative
-- per expression, so it costs about as much as 'compilePath'; the list is
-- lazy, so finding some alternatives may cost less. An alternative may
-- occur more than once.
compileAlternatives :: Path -> [(PatternPath, Path)]
compileAlternatives p =
  [ (canonicalize pattern', choice)
  | (e, choice) <- mapToList (fragmentChoices (pushBackwards p)),
    Just pattern' <- [glueExpr (supplyRoot e)]
  ]

-- | 'fragment', with a representative choice for each expression.
fragmentChoices :: Path -> Map Expr Path
fragmentChoices = \case
  p@One -> one Stay p
  Zero -> mempty
  p@Wild -> step Out MWild p
  p@(Literal t) -> step Out (MLiteral t) p
  p@(RegexMatch r) -> step Out (MRegex r) p
  p@(Backwards Wild) -> step In MWild p
  p@(Backwards (Literal t)) -> step In (MLiteral t) p
  p@(Backwards (RegexMatch r)) -> step In (MRegex r) p
  Backwards _ -> error "fragmentChoices: pushBackwards leaves ~ only on steps"
  p@(Absolute nid) -> one (Join Stay (Exactly nid)) p
  p@(ExcludingNIDs ns) -> one (Join Stay (Except ns)) p
  -- left-biased, so the first choice written is kept
  p :+ q -> fragmentChoices p <> fragmentChoices q
  p :& q ->
    combine
      (\a b -> conjoin a [b])
      (:&)
      (fragmentChoices p)
      (fragmentChoices q)
  p :/ q -> combine sequenceExpr (:/) (atBoundary p) (atBoundary q)
  where
    one = singletonMap
    step d m = one (Step d m)
    atBoundary :: Path -> Map Expr Path
    atBoundary p = flip mapKeysFirst (fragmentChoices p) \case
      Stay -> Join Stay anyNode
      e -> e
    -- on a collision, the first choice is kept
    mapKeysFirst :: (Expr -> Expr) -> Map Expr Path -> Map Expr Path
    mapKeysFirst f m =
      mapFromList (reverse [(f k, v) | (k, v) <- mapToList m])
    combine ::
      (Expr -> Expr -> Expr) ->
      (Path -> Path -> Path) ->
      Map Expr Path ->
      Map Expr Path ->
      Map Expr Path
    combine f g a b =
      mapFromList . reverse $
        [ (f x y, g px py)
        | (x, px) <- mapToList a,
          (y, py) <- mapToList b
        ]

-- | Relative paths start at the current node; an @\@N@ root is that node.
supplyRoot :: Expr -> Expr
supplyRoot = \case
  Source s -> Source s
  Stay -> Source Current
  s@(Step {}) -> sequenceExpr (Source Current) s
  Both a b rest -> conjoin (supplyRoot a) (map supplyRoot (b : rest))
  Join Stay (Exactly nid) -> Source (Fixed nid)
  Join p test -> Join (supplyRoot p) test
  Sequence p q -> sequenceExpr (supplyRoot p) q

conjuncts :: Expr -> [Expr]
conjuncts (Both a b rest) = a : b : rest
conjuncts p = [p]

-- | Flatten associative conjunction, keeping every occurrence.
conjoin :: Expr -> [Expr] -> Expr
conjoin p0 ps = case concatMap conjuncts (p0 : ps) of
  [p] -> p
  a : b : more -> Both a b more
  [] -> error "conjoin: a nonempty input produced no branches"

-- | Compose without inventing a shared midpoint. Right-side joins absorb the
-- complete prefix. A left-side join stays outside a branching continuation,
-- so all its continuations share one binding of that joined prefix.
sequenceExpr :: Expr -> Expr -> Expr
sequenceExpr Stay q = q
sequenceExpr p Stay = p
sequenceExpr p (Join q test) = Join (sequenceExpr p q) test
sequenceExpr p (Sequence q r) = sequenceExpr (sequenceExpr p q) r
sequenceExpr (Sequence p q) r = prefix p (sequenceExpr q r)
sequenceExpr p@(Both {}) q = case map (`sequenceExpr` q) (conjuncts p) of
  p0 : ps -> conjoin p0 ps
  [] -> error "sequenceExpr: empty conjunction"
sequenceExpr p q = prefix p q

-- | Prefix a single segment; 'sequenceExpr' reassociates longer prefixes.
prefix :: Expr -> Expr -> Expr
prefix Stay q = q
prefix p Stay = p
prefix p (Join q test) = Join (prefix p q) test
prefix p (Sequence (Join q test) r) =
  prefix (Join (sequenceExpr p q) test) r
prefix p@(Join {}) q = Sequence p q
prefix p q@(Both {}) = case map (prefix p) (conjuncts q) of
  p0 : ps -> conjoin p0 ps
  [] -> error "prefix: empty conjunction"
prefix p q = Sequence p q

-- * Gluing

data Walk = Walk {root :: PVertex, hops :: [Hop], end :: PVertex}

data GlueState = GlueState
  { nextVar :: Int,
    nextEdge :: Int,
    parents :: Map PVertex PVertex,
    vertexTests :: Map PVertex NodeTest
  }

type Glue = S.StateT GlueState Maybe

-- | Glue one rooted alternative into a pattern. 'Nothing' if its node
-- tests contradict each other.
glueExpr :: Expr -> Maybe PatternPath
glueExpr expr = do
  (walks, st) <- S.runStateT (glue Nothing expr) (GlueState 0 0 mempty mempty)
  let rep v = findRep st.parents v
      goHop (Hop e d m t) = Hop e d m (rep t)
      routes = [Route (rep w.root) (map goHop w.hops) | w <- walks]
      tests =
        mapFromList
          [ (v, t)
          | (v, t) <- mapToList st.vertexTests,
            rep v == v,
            t /= anyNode
          ]
  case routes of
    [] -> Nothing
    r : rs -> Just (PatternPath tests (r :| rs))

glue :: Maybe PVertex -> Expr -> Glue [Walk]
glue input = \case
  Source v -> do
    guard (isNothing input)
    pure [Walk v [] v]
  Stay -> do
    i <- maybe (error "glue: Stay without an input") pure input
    pure [Walk i [] i]
  Step d m -> do
    i <- maybe (error "glue: Step without an input") pure input
    v <- freshVar
    e <- freshEdge
    pure [Walk i [Hop e d m v] v]
  Both a b rest -> concat <$> traverse (glue input) (a : b : rest)
  Join p test -> do
    walks <- glue input p
    case map (.end) walks of
      [] -> error "glue: a join over no walks"
      e : es -> do
        traverse_ (unify e) es
        constrain e test
    pure walks
  Sequence p q -> do
    walks <- glue input p
    ends <- ordNub <$> traverse (findM . (.end)) walks
    j <- case ends of
      [j] -> pure j
      _ -> error "glue: a sequence prefix with several outputs"
    continuations <- glue (Just j) q
    pure
      [ Walk w.root (w.hops ++ c.hops) c.end
      | w <- walks,
        c <- continuations
      ]

freshVar :: Glue PVertex
freshVar = do
  st <- S.get
  S.put st {nextVar = st.nextVar + 1}
  pure (Var (VarId st.nextVar))

freshEdge :: Glue EdgeId
freshEdge = do
  st <- S.get
  S.put st {nextEdge = st.nextEdge + 1}
  pure (EdgeId st.nextEdge)

findRep :: Map PVertex PVertex -> PVertex -> PVertex
findRep parents v = case lookup v parents of
  Nothing -> v
  Just p -> findRep parents p

findM :: PVertex -> Glue PVertex
findM v = S.gets (\st -> findRep st.parents v)

-- | Identify two vertices. The representative prefers 'Current', then
-- 'Fixed', then the oldest variable.
unify :: PVertex -> PVertex -> Glue ()
unify a b = do
  ra <- findM a
  rb <- findM b
  unless (ra == rb) case (ra, rb) of
    (Fixed n, Fixed m) -> guard (n == m)
    (Current, Fixed n) -> merge Current (Fixed n) *> addTest Current (Exactly n)
    (Fixed n, Current) -> merge Current (Fixed n) *> addTest Current (Exactly n)
    (Var _, _) -> merge rb ra
    (_, Var _) -> merge ra rb
    _ -> error "unify: unreachable"
  where
    -- keep @keep@, folding the test of @gone@ into it
    merge keep gone = do
      st <- S.get
      let goneTest = lookup gone st.vertexTests
      S.put
        st
          { parents = insertMap gone keep st.parents,
            vertexTests = deleteMap gone st.vertexTests
          }
      traverse_ (addTest keep) goneTest

constrain :: PVertex -> NodeTest -> Glue ()
constrain v test = do
  r <- findM v
  case test of
    Exactly n -> unify r (Fixed n)
    Except _ -> addTest r test

addTest :: PVertex -> NodeTest -> Glue ()
addTest r test = case r of
  Fixed n -> guard (testAccepts test n)
  _ -> do
    st <- S.get
    combined <- lift (meetTests test (findWithDefault anyNode r st.vertexTests))
    S.put st {vertexTests = insertMap r combined st.vertexTests}
