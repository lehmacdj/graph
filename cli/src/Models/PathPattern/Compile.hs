-- | Compile surface paths to route patterns (docs/path-pattern-ir.md §2).
--
-- Compilation has three steps:
--
-- 1. push @~@ down to the steps, reversing sequences;
-- 2. build a small expression term and distribute unjoined sequencing over
--    conjunction, so that every prefix that is not joined is copied; a
--    shared union (@++@) is chosen before, once for the whole path, and a
--    relational one (@|@) after, independently for each copy, with the
--    copies that choose a side with a join sharing one walk of it;
-- 3. glue the term into routes: every step occurrence gets a fresh variable
--    and edge id, @/@ identifies an output with an input, and a join merges
--    outputs.
module Models.PathPattern.Compile
  ( compilePath,
    compileAlternatives,
    checkAlternatives,
    TooManyAlternatives (..),
    defaultMaxAlternatives,
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
-- distributes over @&@ and both unions. Node specs are unaffected.
pushBackwards :: Path -> Path
pushBackwards = \case
  Backwards p -> invert (pushBackwards p)
  p :/ q -> pushBackwards p :/ pushBackwards q
  p :& q -> pushBackwards p :& pushBackwards q
  Union k p q -> Union k (pushBackwards p) (pushBackwards q)
  WithMaxAlternatives v l p -> WithMaxAlternatives v l (pushBackwards p)
  p -> p
  where
    invert = \case
      Backwards p -> p
      p@(Literal _) -> Backwards p
      p@Wild -> Backwards p
      p@(RegexMatch _) -> Backwards p
      p :/ q -> invert q :/ invert p
      p :& q -> invert p :& invert q
      Union k p q -> Union k (invert p) (invert q)
      WithMaxAlternatives v l p -> WithMaxAlternatives v l (invert p)
      p -> p

-- * Expressions

-- | A transient term. 'Stay' continues at its input, a 'Join' merges every
-- output of its child into one, 'Sequence' feeds its prefix's single
-- output to the continuation, and 'Alt' is a relational union whose side
-- is not chosen yet. A side of it that has a join is a 'Held' walk: the
-- union it is a side of (numbered in the path), the walk itself, and
-- the conjunct without a step (@\@@, @\@N@, @!{…}@) this copy of it
-- follows, if it follows one: the conjunct's place in its conjunction,
-- which tells two such conjuncts apart, as the places of their steps do
-- for the others.
--
-- While the unions of a term are chosen its steps are 'Written' ones: a
-- step with the place it is written at in the path. Distribution copies
-- a step with its place, so two parts of a term are equal exactly when
-- they are copies of the same part of the path, and that is how copies
-- are told apart from what was written twice. An alternative has plain
-- 'Step's ('untagged').
data Expr
  = Source PVertex
  | Stay
  | Step Direction Matcher
  | Written Int Direction Matcher
  | Both Expr Expr [Expr]
  | Join Expr NodeTest
  | Sequence Expr Expr
  | Alt [Expr]
  | Held Int Expr (Maybe Int)
  deriving stock (Eq, Ord, Show)

-- | The alternatives of a path, rooted and distributed.
surfaceExprs :: Path -> Set Expr
surfaceExprs = setFromList . map supplyRoot . concatMap resolved . fragment
  where
    resolved = toList . resolve

-- | The distributed terms of a path, one for each choice of its shared
-- unions, with its relational unions not yet chosen. Distribution copies a
-- relational union like any other segment, so that 'resolve' chooses a
-- side for each copy independently.
fragment :: Path -> [Expr]
fragment = runIdentity . fragmentWithin freely

-- | What making terms and alternatives spends, so that a limit can stop
-- it early ('checkAlternatives').
data Spend m = Spend
  { -- | the size of a set of terms or alternatives, as it grows
    sized :: Int -> m (),
    -- | a term, before anything is done with it
    termMade :: Expr -> m (),
    -- | anything else made on the way to an alternative
    made :: Expr -> m ()
  }

-- | Without a limit.
freely :: (Applicative m) => Spend m
freely = Spend (\_ -> pure ()) (\_ -> pure ()) (\_ -> pure ())

-- | 'fragment' within what may be spent.
fragmentWithin :: forall m. (Monad m) => Spend m -> Path -> m [Expr]
fragmentWithin spend path = toList . mapSet atRoot <$> go True 0 path
  where
    -- without a conjunction nothing is copied
    copies = hasConj path
    total = leaves path
    -- The terms of a part of the path: whether it starts at the root of
    -- the whole path, and the place it is written at (the leaves of the
    -- path before it), which numbers its steps and its unions.
    go :: Bool -> Int -> Path -> m (Set Expr)
    go root pos = \case
      One -> one Stay
      Zero -> pure mempty
      Wild -> step Out MWild
      Literal t -> step Out (MLiteral t)
      RegexMatch r -> step Out (MRegex r)
      Backwards Wild -> step In MWild
      Backwards (Literal t) -> step In (MLiteral t)
      Backwards (RegexMatch r) -> step In (MRegex r)
      Backwards _ -> error "fragment: pushBackwards leaves ~ only on steps"
      Absolute nid -> one (Join Stay (Exactly nid))
      ExcludingNIDs ns -> one (Join Stay (Except ns))
      p@(_ :++ _) -> whole root [(pos, p)]
      p@(_ :|| _) -> whole root [(pos, p)]
      p :& q ->
        join $
          combine (\a b -> pure (conjoin a [b]))
            <$> go root pos p
            <*> go root (pos + leaves p) q
      WithMaxAlternatives _ _ p -> go root pos p
      p@(_ :/ _) -> whole root (placed pos (sequenceSegments p))
      where
        step d m = one (Written pos d m)
    whole root segments = fromMaybe mempty <$> chain root Nothing segments []
    -- The terms of some segments of a sequence, after the terms so far and
    -- before the segments that follow. A shared union chooses first: each
    -- of its sides continues the sequence with its own segments, as it
    -- would alone. So does a relational union, which also stays a union
    -- in a term, to be copied by what surrounds it; its sides with a join
    -- become 'Held' walks, which are copied like a step too and put
    -- together again once the copies have chosen ('resolve').
    chain ::
      Bool ->
      Maybe (Set Expr) ->
      [(Int, Path)] ->
      [(Int, Path)] ->
      m (Maybe (Set Expr))
    chain _ acc [] _ = pure acc
    chain _ acc@(Just terms) _ _ | null terms = pure acc
    chain root acc ((pos, segment) : rest) after
      | shared@(_ : _ : _) <- placed pos (sharedSides segment) =
          merge (map alone (distinctOn snd shared)) >>= continue
      | written@(_ : _ : _) <- placed pos (unionSides segment) = do
          -- sides that give the same terms are one side, so that a union
          -- of a path with itself (or another way to write it) is it
          sides <-
            firstBySet . filter (not . null . snd)
              <$> for written \side ->
                (side,) . sideTerms <$> go here (fst side) (snd side)
          continue =<< case sides of
            [] -> pure (Just mempty)
            [(side, _)] -> alone side
            _ -> do
              let union' = pos * (total + 1) + leaves segment
                  walk e
                    | hasJoin e = Held union' e Nothing
                    | otherwise = e
                  -- sides that all give the same term are that term
                  lone = \case
                    Held _ e _ -> e
                    e -> e
                  together =
                    unionOf (map (mapSet walk . snd) sides)
                      >>= after' . mapSet lone
              -- The union is chosen after it is copied, by each copy, or
              -- before, as each of its sides written alone. That is the
              -- same unless something copies the union (a conjunction)
              -- and a side is more than a chain of steps.
              if copies && any (joinOrConj . snd) written
                then merge (together : map (alone . fst) sides)
                else together
      | otherwise = go here pos segment >>= after' >>= continue
      where
        following = rest ++ after
        continue acc' = chain root acc' rest after
        here = root && isNothing acc
        alone (pos', side) =
          chain root acc (placed pos' (sequenceSegments side)) following
        merge parts = Just <$> (unionAll . catMaybes =<< sequence parts)
        sequenced = isJust acc || not (null following)
        after' terms =
          Just <$> case acc of
            Nothing ->
              pure (if sequenced then mapSet atBoundary terms else terms)
            Just before ->
              combine
                (sequenceWithin spend.made)
                before
                (mapSet atBoundary terms)
        -- A side as the union has it: at a sequence boundary @ joins, and
        -- at the root of the path a leading @ joins nothing, so that @/p
        -- there is the side p ('rootKey').
        sideTerms =
          mapSet $
            (if here then rootKey else id)
              . (if sequenced then atBoundary else id)
        firstBySet = unseen (mempty :: Set (Set Expr))
        unseen seen = \case
          [] -> []
          x : xs
            | key `member` seen -> unseen seen xs
            | otherwise -> x : unseen (insertSet key seen) xs
            where
              key = mapSet renumbered (snd x)
    unionAll = foldM (\acc xs -> collect acc (map pure (toList xs))) mempty
    one = pure . singletonSet
    combine f a b = collect mempty [f x y | x <- toList a, y <- toList b]
    unionOf = \case
      [] -> pure mempty
      [a] -> pure a
      sets ->
        collect mempty (map (pure . altSorted) (traverse toList sets))
    collect :: Set Expr -> [m Expr] -> m (Set Expr)
    collect = foldM \acc making -> do
      x <- making
      spend.termMade x
      let acc' = insertSet x acc
      spend.sized (length acc')
      pure acc'

-- | Each of some parts of a path written one after the other with the
-- place it is written at: the leaves of the path before it.
placed :: Int -> [Path] -> [(Int, Path)]
placed _ [] = []
placed pos (p : ps) = (pos, p) : placed (pos + leaves p) ps

-- | How many leaves (steps, node specs, @\@@, @%never@) a path has.
leaves :: Path -> Int
leaves = \case
  Backwards p -> leaves p
  p :/ q -> leaves p + leaves q
  p :& q -> leaves p + leaves q
  Union _ p q -> leaves p + leaves q
  WithMaxAlternatives _ _ p -> leaves p
  _ -> 1

-- | The first of the elements with each key.
distinctOn :: forall k a. (Ord k) => (a -> k) -> [a] -> [a]
distinctOn key = go (mempty :: Set k)
  where
    go seen = \case
      [] -> []
      x : xs
        | k `member` seen -> go seen xs
        | otherwise -> x : go (insertSet k seen) xs
        where
          k = key x

-- | The sides of a shared union, through nested ones.
sharedSides :: Path -> [Path]
sharedSides = \case
  p :++ q -> sharedSides p ++ sharedSides q
  WithMaxAlternatives _ _ p@(_ :++ _) -> sharedSides p
  p -> [p]

-- | The sides of a relational union, through nested ones.
unionSides :: Path -> [Path]
unionSides = \case
  p :|| q -> unionSides p ++ unionSides q
  WithMaxAlternatives _ _ p@(_ :|| _) -> unionSides p
  p -> [p]

-- | The segments of a sequence, however it is nested: sequencing
-- associates, so the terms are built from the flat chain, left to right,
-- and grouping cannot matter.
sequenceSegments :: Path -> [Path]
sequenceSegments = \case
  p :/ q -> sequenceSegments p ++ sequenceSegments q
  WithMaxAlternatives _ _ p@(_ :/ _) -> sequenceSegments p
  p -> [p]

-- | A term without the leading joins of its input, which at the root of a
-- path join nothing. A join that an @\@N@ can follow is kept, since it
-- makes the @\@N@ a test of the current node instead of a root: one
-- before an @\@N@, and one before a term with an output at its own input
-- (as @c & \@@ has), which an @\@N@ written after the term would follow.
rootKey :: Expr -> Expr
rootKey e = case e of
  Sequence (Join Stay test) q
    | test == anyNode, not (hasFixed q), not (staysPut q) -> rootKey q
  Sequence p q -> sequenceExpr (rootKey p) q
  Join p test | p /= Stay -> joinOf (rootKey p) test
  Both a b rest -> conjoin (rootKey a) (map rootKey (b : rest))
  Alt es -> altSorted (map rootKey es)
  _ -> e
  where
    hasFixed = \case
      Join p test -> isExactly test || hasFixed p
      Sequence p q -> hasFixed p || hasFixed q
      Both a b rest -> any hasFixed (a : b : rest)
      Alt es -> any hasFixed es
      Held _ walk _ -> hasFixed walk
      _ -> False
    isExactly = \case
      Exactly _ -> True
      Except _ -> False
    -- whether an output of the term is its input
    staysPut = \case
      Stay -> True
      Both a b rest -> any staysPut (a : b : rest)
      Alt es -> any staysPut es
      Sequence p q -> staysPut p && staysPut q
      Held _ walk _ -> staysPut walk
      _ -> False

hasConj :: Path -> Bool
hasConj = \case
  _ :& _ -> True
  Backwards p -> hasConj p
  p :/ q -> hasConj p || hasConj q
  Union _ p q -> hasConj p || hasConj q
  WithMaxAlternatives _ _ p -> hasConj p
  _ -> False

-- | Whether a path has a join (@\@@, @\@N@ or @!{…}@) or a conjunction.
joinOrConj :: Path -> Bool
joinOrConj = \case
  One -> True
  Absolute _ -> True
  ExcludingNIDs _ -> True
  _ :& _ -> True
  Backwards p -> joinOrConj p
  p :/ q -> joinOrConj p || joinOrConj q
  Union _ p q -> joinOrConj p || joinOrConj q
  WithMaxAlternatives _ _ p -> joinOrConj p
  _ -> False

-- | At a sequence boundary @\@@ joins the preceding outputs, also when it
-- is one alternative of a union. Otherwise @(\@ + a)/\@N@ would lose the
-- @\@@ and root its @\@N@ alternative, instead of testing the current node
-- like @\@/\@N@.
atBoundary :: Expr -> Expr
atBoundary = \case
  Stay -> Join Stay anyNode
  Alt es -> alt (map atBoundary es)
  e -> e

-- | 'alt', with the sides in a fixed order, so that the order they are
-- written in does not matter, and without sides written twice.
altSorted :: [Expr] -> Expr
altSorted =
  alt . distinctOn renumbered . sortOn (\e -> (shape e, e)) . concatMap sidesOf

-- | A join; joining twice is joining once, with both tests. The joins of
-- the input itself have one form for each meaning, so that sides written
-- differently compare equal: @\@N@ followed by tests that accept @N@ is
-- @\@N@ (a root); tests without an @\@N@ are one test; and an @\@N@ after
-- another test stays a join of a join, since @\@/\@N@ tests the current
-- node where @\@N@ is a root.
joinOf :: Expr -> NodeTest -> Expr
joinOf (Join Stay test0) test = case (test0, test) of
  -- a root stays that root when the test accepts it
  (Exactly n, _) | testAccepts test n -> Join Stay test0
  (Except _, Except _) | Just both <- meetTests test0 test -> Join Stay both
  -- the current node is tested to be that node, whatever was excluded
  (Except _, Exactly n) | testAccepts test0 n -> Join (Join Stay anyNode) test
  _ -> Join (Join Stay test0) test
joinOf (Join p test') test
  | Just both <- meetTests test' test = Join p both
joinOf p test = Join p test

-- | A unit for every side of every union in a term, lazily, so that
-- counting up to a bound makes no more of the term than that.
unionSidesIn :: Expr -> [()]
unionSidesIn = \case
  Alt es -> concatMap (\e -> () : unionSidesIn e) es
  Both a b rest -> concatMap unionSidesIn (a : b : rest)
  Join p _ -> unionSidesIn p
  Sequence p q -> unionSidesIn p ++ unionSidesIn q
  _ -> []

-- | A union, flattened and without repeated sides.
alt :: [Expr] -> Expr
alt es = case ordNub (concatMap sidesOf es) of
  [e] -> e
  es' -> Alt es'

sidesOf :: Expr -> [Expr]
sidesOf = \case
  Alt xs -> xs
  e -> [e]

-- | The term of a whole path, with the joins that have nothing to share
-- written for each of their continuations. A join of the input itself
-- (@\@@, @\@N@, @!{…}@ with nothing before it) has no prefix: where the
-- input is one node (the root of the path, and what follows another
-- join) it only says where its continuations start, and each of them can
-- say so. @\@1/(a & b)@ is @\@1/a & \@1/b@, @\@/(a & b) & c@ is
-- @\@/a & \@/b & c@, and @x/\@/(\@/(a & b) & c)@ is
-- @x/\@/(\@/a & \@/b & c)@: the same routes. Written like that, the
-- copies of a union after @a@, @b@ and @c@ are conjuncts of one
-- conjunction, which is what lets them share a walk ('placeShared'), as
-- they do after @a & b & c@.
--
-- Only a whole term can be written so: in a part of one that is still to
-- be sequenced, such a join takes the outputs of what comes before it.
atRoot :: Expr -> Expr
atRoot e = conjoin' (concatMap flat (conjuncts e))
  where
    flat = \case
      Sequence (Join p test) q ->
        -- what the join merges starts where the join does, and what
        -- follows it starts at one node again
        let j = joinOf (if p == Stay then Stay else atRoot p) test
            q' = atRoot q
         in case q' of
              Both {}
                | atInput j ->
                    concatMap (flat . sequenceExpr j) (conjuncts q')
              _ -> [sequenceExpr j q']
      Join p test | p /= Stay -> [joinOf (atRoot p) test]
      c -> [c]

-- | Whether a term is joins of its input and nothing else.
atInput :: Expr -> Bool
atInput = \case
  Join Stay _ -> True
  Join p _ -> atInput p
  _ -> False

-- | Whether a term has a join, outside the shared walks in it.
hasJoin :: Expr -> Bool
hasJoin = \case
  Join _ _ -> True
  Both a b rest -> any hasJoin (a : b : rest)
  Sequence p q -> hasJoin p || hasJoin q
  Alt es -> any hasJoin es
  _ -> False

-- | Whether a term has a shared walk that is not chosen yet or not put in
-- its place yet.
hasShared :: Expr -> Bool
hasShared = \case
  Held {} -> True
  Both a b rest -> any hasShared (a : b : rest)
  Join p _ -> hasShared p
  Sequence p q -> hasShared p || hasShared q
  Alt es -> any hasShared es
  _ -> False

-- | Whether a term has a conjunction anywhere, also in the sides of its
-- unions and in its shared walks.
hasBoth :: Expr -> Bool
hasBoth = \case
  Both {} -> True
  Join p _ -> hasBoth p
  Sequence p q -> hasBoth p || hasBoth q
  Alt es -> any hasBoth es
  Held _ walk _ -> hasBoth walk
  _ -> False

-- | A term that follows a conjunct without a step: its shared walks say
-- which.
afterLone :: Int -> Expr -> Expr
afterLone = flagged . Just

-- | A term whose shared walks follow something else.
unflagged :: Expr -> Expr
unflagged = flagged Nothing

flagged :: Maybe Int -> Expr -> Expr
flagged flag = go
  where
    go e | not (hasShared e) = e
    go e = case e of
      Held n walk _ -> Held n walk flag
      Both a b rest -> Both (go a) (go b) (map go rest)
      Join p test -> Join (go p) test
      Sequence p q -> Sequence (go p) (go q)
      Alt es -> Alt (map go es)
      _ -> e

-- | A term with plain steps: an alternative.
untagged :: Expr -> Expr
untagged = \case
  Written _ d m -> Step d m
  Both a b rest -> Both (untagged a) (untagged b) (map untagged rest)
  Join p test -> Join (untagged p) test
  Sequence p q -> Sequence (untagged p) (untagged q)
  Alt es -> Alt (map untagged es)
  Held n walk flag -> Held n (untagged walk) flag
  e -> e

-- | What a term is made of, whatever places it was written at.
shape :: Expr -> Expr
shape = \case
  Written _ d m -> Step d m
  Both a b rest -> Both (shape a) (shape b) (map shape rest)
  Join p test -> Join (shape p) test
  Sequence p q -> Sequence (shape p) (shape q)
  Alt es -> Alt (map shape es)
  Held _ walk _ -> Held 0 (shape walk) Nothing
  e -> e

-- | A term with its steps and unions numbered in the order they occur in
-- it instead of by their place in the path, so that the same thing
-- written at two places compares equal.
renumbered :: Expr -> Expr
renumbered e0 = S.evalState (go e0) (mempty, mempty)
  where
    go :: Expr -> S.State (Map Int Int, Map Int Int) Expr
    go e = case e of
      Written t d m -> do
        (steps, unions') <- S.get
        case lookup t steps of
          Just t' -> pure (Written t' d m)
          Nothing -> do
            let t' = length steps
            S.put (insertMap t t' steps, unions')
            pure (Written t' d m)
      Held n walk flag -> do
        (steps, unions') <- S.get
        n' <- case lookup n unions' of
          Just n' -> pure n'
          Nothing -> do
            let n' = length unions'
            S.put (steps, insertMap n n' unions')
            pure n'
        walk' <- go walk
        pure (Held n' walk' flag)
      Both a b rest -> Both <$> go a <*> go b <*> traverse go rest
      Join p test -> (`Join` test) <$> go p
      Sequence p q -> Sequence <$> go p <*> go q
      Alt es -> Alt <$> traverse go es
      _ -> pure e

-- | Choose a side of every relational union, independently for each copy
-- distribution made of it, and put the shared walks in their place.
--
-- A union is chosen in all its copies at once: each copy becomes one of
-- its sides, the term is distributed again where a side has several
-- outputs or inputs (which copies the unions not chosen yet), and the
-- copies that chose the same shared walk are put together as one walk
-- ('placeShared'). Which union is chosen first changes what the others
-- are copied by, so every order that can make a difference is taken: the
-- alternatives are what any order of choosing ends with.
--
-- Two unions can be chosen in either order with the same result when
-- neither is ever sequenced with the other (no copy of one is before or
-- after a copy of the other), since choosing a union changes only the
-- conjuncts it is in; and when the sides of both are single chains of
-- steps, since choosing such a union only writes a chain in its place,
-- which copies nothing and puts nothing together. So the unions of a term
-- fall into groups ('dependent'), and only the orders within the first
-- group are taken; the others are chosen after it, in the same way. In a
-- term without a conjunction nothing is copied, and one order is all
-- there is. (A union of single chains cannot simply go first or last
-- when it is sequenced with a union that has other sides: chosen while it
-- is copied, its copies may differ, and still follow one shared walk once
-- the other union's copies choose that.)
resolve :: Expr -> Set Expr
resolve = runIdentity . resolveWithin freely

-- | 'resolve' within what may be spent.
resolveWithin :: forall m. (Monad m) => Spend m -> Expr -> m (Set Expr)
resolveWithin spend start = go [start] (singletonSet start) mempty
  where
    go :: [Expr] -> Set Expr -> Set Expr -> m (Set Expr)
    go [] _ done = pure done
    go (e : todo) seen done = case openUnions e of
      (_, []) -> do
        let done' = insertSet (untagged e) done
        spend.sized (length done')
        go todo seen done'
      (which, unions') -> do
        -- finding the unions goes through the whole term
        spend.made e
        let first' = case unions' of
              _ : _ : _ | hasBoth e -> dependent e which unions'
              _ -> take 1 unions'
        (todo', seen') <-
          foldM
            choose
            (todo, seen)
            [ (union', picks)
            | (union', copies, sides, _) <- first',
              picks <- traverse (const [0 .. sides - 1]) [1 .. copies]
            ]
        go todo' seen' done
        where
          choose (todo', seen') (union', picks) = do
            e' <- chosen which union' picks e
            spend.made e'
            pure
              if e' `member` seen'
                then (todo', seen')
                else (e' : todo', insertSet e' seen')
    -- the term with each copy of a union replaced by the side it chose
    chosen :: [Int] -> Int -> [Int] -> Expr -> m Expr
    chosen which union' picks e = do
      e' <- S.evalStateT (replace e) (which, picks)
      atRoot
        <$> if hasOpenShared e' then placeShared spend.made e' else pure e'
      where
        replace :: Expr -> S.StateT ([Int], [Int]) m Expr
        replace x = case x of
          Alt es ->
            S.state \case
              (this : others, i : more)
                | this == union' -> (es `indexEx` i, (others, more))
              (_ : others, more) -> (x, (others, more))
              ([], _) -> error "resolve: a union that was not found"
          Both a b rest -> conjoin' <$> traverse replace (a : b : rest)
          Join p test -> (`joinOf` test) <$> replace p
          Sequence p q -> do
            p' <- replace p
            q' <- replace q
            lift (sequenceWithin spend.made p' q')
          _ -> pure x

-- | The unions of a term that can be chosen (those not inside a side of
-- another). Copies of one union are equal, but for the conjunct without
-- a step they may follow. The unions are numbered: which one each
-- occurrence is, in the order 'resolve' goes through the term, and for
-- each how many copies of it there are, how many sides it has, and
-- whether every side is a single chain of steps.
openUnions :: Expr -> ([Int], [(Int, Int, Int, Bool)])
openUnions e0 =
  ( map number found,
    [ (n, length (filter (== union') found), length es, all isChain es)
    | (n, union'@(Alt es)) <- zip [0 ..] distinct
    ]
  )
  where
    found = map unflagged (go e0 [])
    distinct = ordNub found
    numbers = mapFromList (zip distinct [0 ..]) :: Map Expr Int
    number union' = findWithDefault 0 union' numbers
    go e acc = case e of
      Alt _ -> e : acc
      Both a b rest -> foldr go acc (a : b : rest)
      Join p _ -> go p acc
      Sequence p q -> go p (go q acc)
      _ -> acc
    isChain = \case
      Written {} -> True
      Step {} -> True
      Sequence p q -> isChain p && isChain q
      _ -> False

-- | The unions that have to be chosen in every order with the first one:
-- those it is sequenced with in the term, unless both have only single
-- chains for sides, and the ones those are sequenced with in turn.
dependent ::
  Expr -> [Int] -> [(Int, Int, Int, Bool)] -> [(Int, Int, Int, Bool)]
dependent e which unions' = filter (\(n, _, _, _) -> n `member` linked) unions'
  where
    chains = setFromList [n | (n, _, _, True) <- unions'] :: Set Int
    links :: Set (Int, Int)
    links = snd (S.execState (go e) (which, mempty))
    -- the unions in a term, having noted which are sequenced with which
    go :: Expr -> S.State ([Int], Set (Int, Int)) (Set Int)
    go = \case
      Alt _ ->
        S.state \case
          (this : others, found) -> (singletonSet this, (others, found))
          ([], _) -> error "dependent: a union that was not found"
      Both a b rest -> unions <$> traverse go (a : b : rest)
      Join p _ -> go p
      Sequence p q -> do
        before <- go p
        after <- go q
        unless (null before || null after) $ S.modify' \(left', found) ->
          ( left',
            foldl'
              (flip insertSet)
              found
              [ (x, y)
              | x <- toList before,
                y <- toList after,
                x /= y,
                not (x `member` chains && y `member` chains)
              ]
          )
        pure (before <> after)
      _ -> pure mempty
    linked :: Set Int
    linked = reach (singletonSet first') [first']
    first' = case unions' of
      (n, _, _, _) : _ -> n
      [] -> 0
    reach :: Set Int -> [Int] -> Set Int
    reach seen = \case
      [] -> seen
      x : todo ->
        let next =
              [ y
              | (a, b) <- toList links,
                (x', y) <- [(a, b), (b, a)],
                x' == x,
                y `notMember` seen
              ]
         in reach (foldr insertSet seen next) (next ++ todo)

-- | Whether a term has a shared walk that was chosen and is to be put in
-- its place.
hasOpenShared :: Expr -> Bool
hasOpenShared = \case
  Held {} -> True
  Both a b rest -> any hasOpenShared (a : b : rest)
  Join p _ -> hasOpenShared p
  Sequence p q -> hasOpenShared p || hasOpenShared q
  _ -> False

-- | A conjunct that is a prefix, a shared walk and a continuation: the
-- walk, and the conjuncts of the prefix and of the continuation. A prefix
-- that is a conjunct without a step has which one it is, so that two of
-- them are two prefixes.
data Led = Led
  { walk :: (Int, Expr),
    prefixes :: [(Maybe Int, Expr)],
    continuations :: [Expr]
  }

-- | Put the chosen shared walks of a term in their place. The copies
-- that chose a walk are conjuncts of one conjunction, each a prefix, the
-- walk and a continuation (as distributing a step in the walk's place
-- made them). They become one conjunct: the walk written once, after the
-- conjunction of their prefixes and before the conjunction of their
-- continuations, where it is sequenced as it would be written there
-- alone. A prefix or a continuation that distribution copied (once for
-- each of the others) is taken once: copies are equal, since a step is
-- copied with the place it is written at, and what was written twice is
-- not. The conjunct is put where the first of the copies was.
--
-- A copy whose prefix is a lone @\@@ conjunct follows that @\@@ as in
-- @\@/p@, where it is a join: an @\@N@ at the start of the walk tests the
-- current node there. With other prefixes the @\@@ stays a conjunct, as
-- in @(c & \@)/p@.
placeShared :: forall m. (Monad m) => (Expr -> m ()) -> Expr -> m Expr
placeShared made = fmap conjoin' . scope . conjuncts
  where
    sequenced = sequenceWithin made
    scope :: [Expr] -> m [Expr]
    scope [] = pure []
    scope (c : more) = case led c of
      Nothing -> do
        c' <- inside c
        (conjuncts c' ++) <$> scope more
      Just l -> do
        let (same, others) =
              partitionEithers
                [ case led x of
                    Just l' | l'.walk == l.walk -> Left l'
                    _ -> Right x
                | x <- more
                ]
            members = l : same
            pres = ordNub (concatMap (.prefixes) members)
            -- what follows the walk follows no conjunct without a step
            conts = ordNub (map unflagged (concatMap (.continuations) members))
        pres' <- traverse (inside . snd) pres
        conts' <- scope (concatMap conjuncts conts)
        let prefix = case pres of
              [(Just _, Stay)] -> Join Stay anyNode
              _ -> conjoin' pres'
            continuation = case conts' of
              [] -> Stay
              _ -> conjoin' conts'
        whole <- sequenced prefix (snd l.walk) >>= (`sequenced` continuation)
        made whole
        (conjuncts whole ++) <$> scope others
    -- the shared walks inside a conjunct that is not led by one
    inside :: Expr -> m Expr
    inside c | not (hasOpenShared c) = pure c
    inside c = case c of
      Sequence p q -> do
        p' <- within p
        q' <- within q
        sequenced p' q'
      Join p test -> (`joinOf` test) <$> within p
      Both {} -> within c
      _ -> pure c
    within e = conjoin' <$> scope (conjuncts e)

-- | A conjunct as a prefix, a shared walk and a continuation, if it is
-- one.
led :: Expr -> Maybe Led
led = \case
  Held n w lone -> Just (Led (n, w) [(lone, Stay)] [Stay])
  Sequence (Held n w lone) r -> Just (Led (n, w) [(lone, Stay)] [r])
  Sequence j@(Join x test) r
    -- the walk is before the join: the join is in its continuation
    | Just l <- ledAll x ->
        Just l {continuations = [joined l test (Just r)]}
    -- the walk is after the join: the join is in its prefix
    | not (hasOpenShared x),
      Just l <- ledAll r ->
        Just
          l
            { prefixes =
                [ case ordNub l.prefixes of
                    -- the join is the conjunct without a step
                    [(which, Stay)] -> (which, j)
                    several ->
                      (Nothing, Sequence j (conjoin' (map snd several)))
                ]
            }
    | otherwise -> Nothing
  Sequence h r
    | not (hasOpenShared h) ->
        (\l -> l {prefixes = map (after h . snd) l.prefixes}) <$> led r
  Join x test ->
    (\l -> l {continuations = [joined l test Nothing]}) <$> ledAll x
  _ -> Nothing
  where
    after h = \case
      Stay -> (Nothing, h)
      more -> (Nothing, Sequence h more)
    joined l test r =
      let j = joinOf (conjoin' (ordNub l.continuations)) test
       in maybe j (Sequence j) r

-- | A term all of whose conjuncts are led by the same shared walk.
ledAll :: Expr -> Maybe Led
ledAll e = case traverse led (conjuncts e) of
  Just (l : ls)
    | all ((== l.walk) . (.walk)) ls ->
        Just
          l
            { prefixes = concatMap (.prefixes) (l : ls),
              continuations = concatMap (.continuations) (l : ls)
            }
  _ -> Nothing

-- * Budget

-- | How many alternatives the relational unions of a path may make, unless
-- an @%override({max-alternatives = …}, …)@ says otherwise.
defaultMaxAlternatives :: Int
defaultMaxAlternatives = 1000

-- | Why a path is over its alternatives limit.
data TooManyAlternatives
  = -- | it compiles to more alternatives than allowed
    TooManyAlternatives {limit :: Int}
  | -- | its alternatives cannot be counted within what the limit allows
    -- to be made on the way, however many they are
    TooLargeToCount {limit :: Int}
  deriving stock (Eq, Show)

-- | The largest limit in effect at a relational union of the path: the one
-- of the innermost @max-alternatives@ override around it, or the default.
-- The alternatives multiply across unions, so they cannot be attributed to
-- one; the most generous limit asked for applies to the whole path.
pathLimit :: Path -> Maybe AlternativesLimit
pathLimit = go (AtMostAlternatives defaultMaxAlternatives)
  where
    go l = \case
      WithMaxAlternatives _ l' p -> go l' p
      p :|| q -> Just l `max` go l p `max` go l q
      p :++ q -> go l p `max` go l q
      p :/ q -> go l p `max` go l q
      p :& q -> go l p `max` go l q
      Backwards p -> go l p
      _ -> Nothing

-- | Whether compiling the path stays within its alternatives limit. The
-- alternatives are counted as 'compilePath' makes them, after
-- contradictory ones are dropped and the same ones are merged, so the
-- count is exact ('TooManyAlternatives'). Three bounds make the check
-- always finish fast, without repeating a long compilation; a path beyond
-- one of them is over the limit too, however many alternatives it would
-- come to ('TooLargeToCount'):
--
-- * the terms (one for each choice of the shared unions, and for each
--   relational union chosen before it is copied), the sides of the unions
--   in one term, and the alternatives one term resolves to before they
--   are glued, may each be four times the limit (contradictory
--   alternatives, and ones that glue to the same pattern, do not count in
--   the end);
-- * making the terms and the alternatives that are new may take
--   'checkWork' steps for each alternative allowed (a step is a term, or
--   an alternative resolved or glued);
-- * everything made may have 'checkSteps' in size for each alternative
--   allowed: every term, every copy of a term with a shared walk, every
--   term gone through to choose a union and every term a choice gives, by
--   its steps and node specs ('weight'), and every new alternative again
--   by the routes and hops it glues to ('gluedSize').
--
-- A path without a relational union has no limit, and neither has one
-- without a conjunction: nothing copies its unions, so it compiles as it
-- would with shared unions.
checkAlternatives :: Path -> Either TooManyAlternatives ()
checkAlternatives p = case pathLimit p of
  Just (AtMostAlternatives n) | hasConj p -> do
    let tooMany, tooLarge :: S.StateT CheckBudget (Either TooManyAlternatives) a
        tooMany = lift (Left (TooManyAlternatives n))
        tooLarge = lift (Left (TooLargeToCount n))
        -- anything made, new or not, costs its size
        spent k = do
          budget <- S.get
          when (budget.steps >= checkSteps * n) tooLarge
          S.put budget {steps = budget.steps + k}
        made = spent . weight
        sized allowed k = when (k > allowed) tooLarge
        -- something made that was not made before
        work = do
          budget <- S.get
          when (budget.work >= checkWork * n) tooLarge
          S.put budget {work = budget.work + 1}
        -- the unions of one term may have four times the limit in sides
        term e = do
          when (not (null (drop (4 * n) (unionSidesIn e)))) tooLarge
          made e
          work
        spend = Spend (sized (4 * n)) term made
    flip S.evalStateT (CheckBudget 0 0) do
      es <- fragmentWithin spend (pushBackwards p)
      void $
        foldM
          ( \seen e -> do
              new <- resolveWithin spend e
              foldM
                ( \(exprs, patterns) x ->
                    if x `member` exprs
                      then pure (exprs, patterns)
                      else do
                        spent (gluedSize x) *> work *> work
                        let patterns' =
                              maybe
                                patterns
                                ((`insertSet` patterns) . canonicalize)
                                (glueExpr (supplyRoot x))
                        when (length patterns' > n) tooMany
                        pure (insertSet x exprs, patterns')
                )
                seen
                (toList new)
          )
          (mempty :: Set Expr, mempty :: Set PatternPath)
          es
  _ -> pure ()

-- | What 'checkAlternatives' has spent.
data CheckBudget = CheckBudget {steps :: Int, work :: Int}

-- | Steps 'checkAlternatives' may take for each alternative allowed, to
-- make terms and alternatives it has not made before.
checkWork :: Int
checkWork = 20

-- | The size of everything 'checkAlternatives' may make, for each
-- alternative allowed. Measured in an unoptimized build, this keeps the
-- check within half a second at the default limit (0.44s at most); of
-- 1872 random unions of up to 45 targets and other sides before a
-- conjunction, it refuses six that compile to fewer alternatives than the
-- limit (in 0.2s to 2.0s).
checkSteps :: Int
checkSteps = 500

-- | Each alternative a path compiles to, with a path without unions (with
-- @~@ pushed down to the steps) that compiles to just that alternative.
-- When every copy of each union chose the same side, that is the path with
-- one side of each union kept ('uniformChoices'); otherwise the copies are
-- written out ('exprPath'), as in @a/c & b/d@ for @(a | b)/(c & d)@. The
-- alternatives are exactly those of 'compilePath'. This costs about twice
-- as much as 'compilePath'; the list is lazy, so finding some alternatives
-- may cost less. An alternative may occur more than once.
compileAlternatives :: Path -> [(PatternPath, Path)]
compileAlternatives p =
  [ (pattern', fromMaybe (exprPath e) (lookup pattern' uniform))
  | e <- concatMap (toList . resolve) (fragment p'),
    Just pattern' <- [canonicalize <$> glueExpr (supplyRoot e)]
  ]
  where
    p' = pushBackwards p
    uniform :: Map PatternPath Path
    uniform =
      mapFromList . reverse $
        [ (canonicalize pattern', choice)
        | (e, choice) <- mapToList (uniformChoices p'),
          Just pattern' <- [glueExpr (supplyRoot e)]
        ]

-- | A path without unions that compiles to an expanded expression.
exprPath :: Expr -> Path
exprPath = \case
  Stay -> One
  Written _ d m -> exprPath (Step d m)
  Step d m -> (case d of Out -> id; In -> Backwards) case m of
    MLiteral t -> Literal t
    MWild -> Wild
    MRegex r -> RegexMatch r
  Both a b rest -> foldl' (:&) (exprPath a) (map exprPath (b : rest))
  Join Stay test -> testPath test
  Join p test -> exprPath p :/ testPath test
  Sequence p q -> exprPath p :/ exprPath q
  Source _ -> error "exprPath: a rooted expression"
  Alt _ -> error "exprPath: an unexpanded union"
  Held {} -> error "exprPath: a shared walk not put in its place"
  where
    testPath = \case
      Exactly n -> Absolute n
      Except ns
        | null ns -> One
        | otherwise -> ExcludingNIDs ns

-- | The alternatives as if each union were chosen once for all its copies,
-- with a representative choice for each expression.
uniformChoices :: Path -> Map Expr Path
uniformChoices = \case
  p@One -> one Stay p
  Zero -> mempty
  p@Wild -> step Out MWild p
  p@(Literal t) -> step Out (MLiteral t) p
  p@(RegexMatch r) -> step Out (MRegex r) p
  p@(Backwards Wild) -> step In MWild p
  p@(Backwards (Literal t)) -> step In (MLiteral t) p
  p@(Backwards (RegexMatch r)) -> step In (MRegex r) p
  Backwards _ -> error "uniformChoices: pushBackwards leaves ~ only on steps"
  p@(Absolute nid) -> one (Join Stay (Exactly nid)) p
  p@(ExcludingNIDs ns) -> one (Join Stay (Except ns)) p
  -- left-biased, so the first choice written is kept
  Union _ p q -> uniformChoices p <> uniformChoices q
  WithMaxAlternatives _ _ p -> uniformChoices p
  p :& q ->
    combine
      (\a b -> conjoin a [b])
      (:&)
      (uniformChoices p)
      (uniformChoices q)
  p :/ q ->
    combine sequenceExpr (:/) (boundaryChoices p) (boundaryChoices q)
  where
    one = singletonMap
    step d m = one (Step d m)
    boundaryChoices :: Path -> Map Expr Path
    boundaryChoices p = flip mapKeysFirst (uniformChoices p) \case
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
  s@(Written {}) -> sequenceExpr (Source Current) s
  Both a b rest -> conjoin (supplyRoot a) (map supplyRoot (b : rest))
  Alt _ -> error "supplyRoot: an unexpanded union"
  Held {} -> error "supplyRoot: a shared walk not put in its place"
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

conjoin' :: [Expr] -> Expr
conjoin' = \case
  p0 : ps -> conjoin p0 ps
  [] -> error "conjoin': empty conjunction"

-- | Compose without inventing a shared midpoint. Right-side joins absorb the
-- complete prefix. A left-side join stays outside a branching continuation,
-- so all its continuations share one binding of that joined prefix. A
-- relational union is copied like a step, and so are its shared walks;
-- one that follows a conjunct without a step says which ('afterLone').
sequenceExpr :: Expr -> Expr -> Expr
sequenceExpr p q = runIdentity (sequenceWithin (\_ -> pure ()) p q)

-- | 'sequenceExpr', running the action on every copy it makes of a term
-- with a shared walk, so that a budget can stop it early.
sequenceWithin ::
  forall m. (Monad m) => (Expr -> m ()) -> Expr -> Expr -> m Expr
sequenceWithin made = sequenced
  where
    sequenced, prefixed :: Expr -> Expr -> m Expr
    sequenced Stay q = pure q
    sequenced p Stay = pure p
    sequenced p (Join q test) = (`joinOf` test) <$> sequenced p q
    sequenced p (Sequence q r) = sequenced p q >>= (`sequenced` r)
    sequenced (Sequence p q) r = sequenced q r >>= prefixed p
    sequenced p@(Both {}) q =
      conjoin' <$> for (zip [0 ..] (conjuncts p)) \(i, c) ->
        sequenced c
          =<< copy (if c == Stay || atInput c then afterLone i q else q)
    sequenced p q = prefixed p q

    -- Prefix a single segment; 'sequenced' reassociates longer prefixes.
    prefixed Stay q = pure q
    prefixed p Stay = pure p
    prefixed p (Join q test) = (`joinOf` test) <$> prefixed p q
    prefixed p (Sequence (Join q test) r) = do
      joined <- sequenced p q
      prefixed (joinOf joined test) r
    -- a join before a union stays outside it, one instance for every side
    prefixed p@(Join {}) q = pure (Sequence p q)
    prefixed p q@(Both {}) =
      conjoin' <$> for (conjuncts q) \c -> (`prefixed` c) =<< copy p
    prefixed p q = pure (Sequence p q)

    copy e
      | hasShared e = e <$ made e
      | otherwise = pure e

-- | The steps and node specs of a term, in all its conjuncts and union
-- sides: what it costs to make or compare it.
weight :: Expr -> Int
weight = \case
  Both a b rest -> sum (map weight (a : b : rest))
  Join p _ -> weight p
  Sequence p q -> weight p + weight q
  Alt es -> sum (map weight es)
  _ -> 1

-- | The routes an alternative glues to, and their hops: what it costs to
-- glue it. A sequence has a route for every pair of a route of its prefix
-- and one of its continuation.
gluedSize :: Expr -> Int
gluedSize e = let (routes, hops) = size e in routes + hops
  where
    size :: Expr -> (Int, Int)
    size = \case
      Step {} -> (1, 1)
      Written {} -> (1, 1)
      Both a b rest ->
        let sizes = map size (a : b : rest)
         in (sum (map fst sizes), sum (map snd sizes))
      Join p _ -> size p
      Sequence p q ->
        let (pr, ph) = size p
            (qr, qh) = size q
         in (pr * qr, ph * qr + pr * qh)
      _ -> (1, 0)

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
  Written _ d m -> glue input (Step d m)
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
  Alt _ -> error "glue: an unexpanded union"
  Held {} -> error "glue: a shared walk not put in its place"
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
    -- the oldest stays, so that joining many outputs (which all meet
    -- the first) follows no chain of merged variables
    (Var x, Var y) | x < y -> merge ra rb
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
