-- | Compile surface paths to route patterns (docs/path-pattern-ir.md §2).
--
-- Compilation has four steps:
--
-- 1. push @~@ down to the steps, reversing sequences;
-- 2. choose every shared union (@++@), once for the whole path, and write
--    what is left as chains of segments ('Chain'): steps, joins, groups
--    (@&@) and relational unions (@|@);
-- 3. list the routes of each chain, stretch by stretch between its own
--    joins, each route choosing a conjunct of every group it goes
--    through and a side of every relational union, and name every step
--    and join on a route by the copy of it the route goes through
--    ('annotate'): that decides what routes share;
-- 4. glue the routes into a pattern: steps with the same name are one
--    edge, and joins with the same name are one vertex.
module Models.PathPattern.Compile
  ( compilePath,
    compileAlternatives,
    checkAlternatives,
    TooManyAlternatives (..),
    defaultMaxAlternatives,
    pushBackwards,
    checkSpending,
  )
where

import Control.Monad.State.Strict qualified as S
import Data.IntMap.Strict qualified as IntMap
import Data.Map.Strict qualified as Map
import Models.Path.Simple
import Models.PathPattern
import MyPrelude

compilePath :: Path -> PathQuery
compilePath = PathQuery . setFromList . map snd . alternatives

-- | The alternatives of a path: the routes of each, and what they glue
-- to.
alternatives :: Path -> [([[Item Int]], PatternPath)]
alternatives p =
  [ (routes, pattern')
  | chain <- runIdentity (structuresWithin freely p'),
    routes <- runIdentity (resolveWithin freely chain),
    Just pattern' <- [glueRoutes routes]
  ]
  where
    p' = pushBackwards (withoutLimits p)

-- | A path without the nodes that carry its limits. A limit says how much
-- compiling or residuating the path may make, not what the path means, so
-- everything here compiles the path without them: wherever such a node
-- is, and however many are nested, the path compiles as if it were not
-- there.
withoutLimits :: Path -> Path
withoutLimits = \case
  WithLimits _ _ p -> withoutLimits p
  Backwards p -> Backwards (withoutLimits p)
  p :/ q -> withoutLimits p :/ withoutLimits q
  p :& q -> withoutLimits p :& withoutLimits q
  Union k p q -> Union k (withoutLimits p) (withoutLimits q)
  p -> p

-- * Backwards

-- | Push 'Backwards' down to the steps. @~(p/q) = ~q/~p@, and @~@
-- distributes over @&@ and both unions. Node specs are unaffected.
pushBackwards :: Path -> Path
pushBackwards = \case
  Backwards p -> invert (pushBackwards p)
  p :/ q -> pushBackwards p :/ pushBackwards q
  p :& q -> pushBackwards p :& pushBackwards q
  Union k p q -> Union k (pushBackwards p) (pushBackwards q)
  WithLimits v l p -> WithLimits v l (pushBackwards p)
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
      WithLimits v l p -> WithLimits v l (invert p)
      p -> p

-- * Chains

-- | A segment of a chain, with the place it is written at in the path
-- (which names it): a step; a join (@\@@, @\@N@, @!{…}@), which merges
-- the routes that arrive along its chain, and whether it is an @\@N@
-- that is a root when nothing precedes it on a route; a group of
-- conjuncts, each a chain of its own; a relational union of sides, each
-- a chain that is written in the union's place once it is chosen.
data Seg
  = SStep Int Direction Matcher
  | SJoin Int NodeTest Bool
  | SGroup Int [Chain]
  | SUnion Int [Chain]
  deriving stock (Eq, Ord, Show)

-- | A sequence at one nesting level. Parentheses without @&@ do not make
-- a chain of their own, and neither does a side of a union.
type Chain = [Seg]

-- | What making chains and alternatives spends, so that a limit can stop
-- it early ('checkAlternatives').
data Spend m = Spend
  { -- | something made, by its size
    made :: Int -> m (),
    -- | the routes of an alternative, once they are found
    found :: [[Item Int]] -> m ()
  }

-- | Without a limit.
freely :: (Applicative m) => Spend m
freely = Spend (\_ -> pure ()) (\_ -> pure ())

-- | The chains of a path, one for each choice of its shared unions, in
-- normal form ('norm').
structuresWithin :: forall m. (Monad m) => Spend m -> Path -> m [Chain]
structuresWithin spend path =
  ordNub . mapMaybe normal . toList <$> go 0 path
  where
    total = leaves path
    -- a name for a group or a union: no two start at the same leaf and
    -- have as many leaves
    named pos p = pos * (total + 1) + leaves p
    go :: Int -> Path -> m (Set Chain)
    go pos = \case
      One -> one (SJoin pos anyNode False)
      Zero -> pure mempty
      Wild -> one (SStep pos Out MWild)
      Literal t -> one (SStep pos Out (MLiteral t))
      RegexMatch r -> one (SStep pos Out (MRegex r))
      Backwards Wild -> one (SStep pos In MWild)
      Backwards (Literal t) -> one (SStep pos In (MLiteral t))
      Backwards (RegexMatch r) -> one (SStep pos In (MRegex r))
      Backwards _ -> error "structures: pushBackwards leaves ~ only on steps"
      Absolute nid -> one (SJoin pos (Exactly nid) True)
      ExcludingNIDs ns -> one (SJoin pos (Except ns) False)
      p :++ q -> do
        a <- go pos p
        b <- go (pos + leaves p) q
        collect a (map (pure . Just) (toList b))
      p@(_ :|| _) -> do
        -- sides that give the same chains are one side, so that a union
        -- of a path with itself is that path also when it has a shared
        -- union, which each of the two would choose
        sides <-
          distinctOn (shapes . map (shape . sideOrder))
            . filter (not . null)
            <$> for (placed pos (unionSides p)) \(pos', side) ->
              toList <$> go pos' side
        collect mempty $
          map (pure . unionOf (named pos p)) (traverse id sides)
      p :& q -> do
        a <- go pos p
        b <- go (pos + leaves p) q
        collect
          mempty
          [ pure (Just [SGroup (named pos (p :& q)) (conjuncts x <> conjuncts y)])
          | x <- toList a,
            y <- toList b
          ]
      WithLimits _ _ p -> go pos p
      p :/ q -> do
        a <- go pos p
        if null a
          then pure a
          else do
            b <- go (pos + leaves p) q
            collect mempty [pure (sequenced x y) | x <- toList a, y <- toList b]
    one = pure . singletonSet . (: [])
    shapes = setFromList :: [Chain] -> Set Chain
    collect :: Set Chain -> [m (Maybe Chain)] -> m (Set Chain)
    collect = foldM \acc making ->
      making >>= \case
        Nothing -> pure acc
        Just x -> do
          spend.made (weight x)
          pure (insertSet x acc)

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
  WithLimits _ _ p -> leaves p
  _ -> 1

-- | The sides of a relational union, through nested ones.
unionSides :: Path -> [Path]
unionSides = \case
  p :|| q -> unionSides p ++ unionSides q
  WithLimits _ _ p@(_ :|| _) -> unionSides p
  p -> [p]

-- | The conjuncts a chain is in a group: nested groups are one group.
conjuncts :: Chain -> [Chain]
conjuncts = \case
  [SGroup _ cs] -> cs
  c -> [c]

-- | A union of sides, which is its side when it has only one, and
-- nothing when it has none.
unionOf :: Int -> [Chain] -> Maybe Chain
unionOf name sides = case ordNub (concatMap sidesOf sides) of
  [] -> Nothing
  [c] -> Just c
  cs -> Just [SUnion name cs]
  where
    sidesOf = \case
      [SUnion _ cs] -> cs
      c -> [c]

-- | One chain after another. A join right after a join is one join, with
-- both tests; when they contradict each other there is no chain.
sequenced :: Chain -> Chain -> Maybe Chain
sequenced a b = case (reverse a, b) of
  (SJoin pos test root : before, SJoin _ test' _ : after) -> do
    both <- meetTests test test'
    sequenced (reverse before <> [SJoin pos both root]) after
  _ -> Just (a <> b)

-- | The normal form of a chain, in which what means the same is written
-- the same, so that the sides of a union can be compared ('shape'):
--
-- * joins right after one another are one join;
-- * a union has each of its sides once, in a fixed order, and a union of
--   one side is that side;
-- * an @\@N@ is marked as a root only where it can start a route: an
--   @\@N@ after anything, also after a leading @\@@, tests the node the
--   route is at.
--
-- A leading @\@@ is a join like any other, also where the input is one
-- node already (at the root of the path, right after another join, at
-- the start of a conjunct or a side): it is kept as written. It merges
-- nothing there, but it is a join of its chain, so it makes a conjunct it
-- starts a chain apart, which is copied whole: @(\@/(a & b) & c)/(\@/f & e)@
-- has the chain @\@/(a & b)/\@/f@, in which @a@ and @b@ meet before
-- @f@, and @(a & b & c)/(\@/f & e)@ has not.
--
-- Nothing when the chain has contradictory joins, or a union without
-- sides.
normal :: Chain -> Maybe Chain
normal c = do
  c' <- norm True c
  if c' == c then Just c else normal c'

-- | 'normal' for a chain that starts its routes, or not.
norm :: Bool -> Chain -> Maybe Chain
norm atStart chain = do
  parts <- for (zip (atStart : repeat False) chain) \case
    (start, SGroup name cs) -> do
      cs' <- traverse (norm start) cs
      pure [SGroup name (concatMap conjuncts cs')]
    (start, SUnion name cs) ->
      unionOf name
        . distinctOn shape
        . sortOn shape
        $ mapMaybe (norm start) cs
    (False, SJoin pos test _) -> pure [SJoin pos test False]
    (_, seg) -> pure [seg]
  foldM sequenced [] parts

isJoin :: Seg -> Bool
isJoin = \case
  SJoin {} -> True
  _ -> False

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

-- | What a chain is made of, whatever places it is written at.
shape :: Chain -> Chain
shape = map \case
  SStep _ d m -> SStep 0 d m
  SJoin _ test root -> SJoin 0 test root
  SGroup _ cs -> SGroup 0 (map shape cs)
  SUnion _ cs -> SUnion 0 (map shape cs)

-- | A chain with the sides of every union in it in a fixed order.
sideOrder :: Chain -> Chain
sideOrder = map \case
  SGroup name cs -> SGroup name (map sideOrder cs)
  SUnion name cs -> SUnion name (sortOn shape (map sideOrder cs))
  seg -> seg

-- | A chain with its segments named in the order they occur in it.
renamed :: Chain -> Chain
renamed c = S.evalState (chain' c) 0
  where
    chain' = traverse seg
    next = S.state \i -> (i, i + 1 :: Int)
    seg = \case
      SStep _ d m -> next <&> \i -> SStep i d m
      SJoin _ test root -> next <&> \i -> SJoin i test root
      SGroup _ cs -> SGroup <$> next <*> traverse chain' cs
      SUnion _ cs -> SUnion <$> next <*> traverse chain' cs

-- | The steps and node specs of a chain, in all its conjuncts and union
-- sides: what it costs to make or compare it.
weight :: Chain -> Int
weight =
  sum . map \case
    SGroup _ cs -> sum (map weight cs)
    SUnion _ cs -> sum (map weight cs)
    _ -> 1

-- | The sides of every union in a chain, by the union's name.
unionsIn :: Chain -> Map Int [Chain]
unionsIn = foldMap \case
  SGroup _ cs -> foldMap unionsIn cs
  SUnion name cs -> insertMap name cs (foldMap unionsIn cs)
  _ -> mempty

-- | Whether some group in a chain, or in a side of a union in it, has a
-- conjunct with a join of its own: only then are routes listed in
-- another order than the conjuncts they choose from left to right
-- ('listedBy').
hasJoinedConjunct :: Chain -> Bool
hasJoinedConjunct = any \case
  SGroup _ cs -> any (any isJoin) cs || any hasJoinedConjunct cs
  SUnion _ cs -> any hasJoinedConjunct cs
  _ -> False

-- | Whether a chain has a group, also in a side of a union in it: what
-- follows or precedes it may then be copied.
forks :: Chain -> Bool
forks = any \case
  SGroup {} -> True
  SUnion _ cs -> any forks cs
  _ -> False

-- * Routes

-- | A segment of a route: a step, a join, the conjunct of a group the
-- route goes through with its route through that conjunct, a union whose
-- side is not chosen yet, or a union with the side chosen for this route
-- and the route through that side.
data RSeg
  = RStep Int Direction Matcher
  | RJoin Int NodeTest Bool
  | RGroup Int Int [RSeg]
  | RAtom Int
  | RPicked Int Int [RSeg]
  deriving stock (Eq, Ord, Show)

-- | The routes of a chain, with its unions not chosen: a conjunct of
-- each group, the first segment's choice varying slowest.
routesOf :: Chain -> [[RSeg]]
routesOf = \case
  [] -> [[]]
  seg : rest -> [here <> after | here <- segment seg, after <- routesOf rest]
  where
    segment = \case
      SStep pos d m -> [[RStep pos d m]]
      SJoin pos test root -> [[RJoin pos test root]]
      SGroup name cs ->
        [[RGroup name j r] | (j, c) <- zip [0 ..] cs, r <- routesOf c]
      SUnion name _ -> [[RAtom name]]

-- | How many routes a chain has, and how many segments they have in all.
routesSize :: Chain -> (Int, Int)
routesSize = \case
  [] -> (1, 0)
  seg : rest ->
    let (r, s) = case seg of
          SGroup _ cs ->
            let sizes = map routesSize cs
             in (sum (map fst sizes), sum (map (\(n, k) -> n + k) sizes))
          _ -> (1, 1)
        (r', s') = routesSize rest
     in (capped (r * r'), capped (s * r' + r * s'))
  where
    capped = min 1000000000

-- | How many steps, joins and unions not chosen a route has, through
-- the conjuncts and the sides it chose: what it costs to go along it.
segmentsOf :: [RSeg] -> Int
segmentsOf =
  sum . map \case
    RGroup _ _ body -> segmentsOf body
    RPicked _ _ body -> segmentsOf body
    _ -> 1

-- | A route with the chosen sides written in their unions' places.
written :: [RSeg] -> [RSeg]
written = concatMap \case
  RPicked _ _ body -> written body
  seg -> [seg]

-- | The conjuncts a route chooses in the groups of one stretch, each
-- group by its name.
type Choices = [(Int, Int)]

-- | A step or a join on a route, with what tells its copy from the
-- others: routes share exactly the steps and joins that have the same.
data Item k
  = Along k Direction Matcher
  | Meet k NodeTest Bool
  deriving stock (Eq, Ord, Show, Functor)

-- | The copy of a step or a join, as the route that goes through it sees
-- it: where it is written, and the choices that copy it, stretch within
-- stretch.
type Name = (Int, [Choices])

-- | The steps and joins of a route whose unions are chosen, each named by
-- its copy. This is where the meaning of sequencing is.
--
-- A chain is cut into stretches by its joins. Within a stretch every
-- group copies everything else: @(a & b)/(c & d)@ is
-- @a/c & a/d & b/c & b/d@, each of the four written apart. So a stretch
-- with groups is, for a route, the chain it gets when each group is
-- replaced by the conjunct the route chooses there, written in the
-- group's place; that chain is named by those choices, and it is a chain
-- like any other: the joins of the conjuncts cut it, and the groups
-- within its stretches copy again. A step is named by the choices of all
-- the stretches it is in, from the outermost on, and so is a join, which
-- the routes that made the same choices up to its chain go through
-- together.
--
-- So a conjunct is copied whole, with the joins in it: in
-- @(x/\@/w & b)/(c & d)@ the choice between @c@ and @d@ names the chain
-- @x/\@/w/c@ that the join is in, and there are two @x@. A join of the
-- chain itself ends the stretch: in @x/\@/w/(c & d)@ only @w@ is in the
-- stretch that has the group, and the @x@ before the join is one for
-- both. A join in a conjunct of the later group, as in
-- @(p & q)/(x/\@/(c & d) & b)@, is in the chain @p/x/\@/(c & d)@, where it
-- likewise keeps @c & d@ from copying @p/x@.
--
-- A chosen side of a union is read where the union is written: its
-- segments are segments of the union's chain, and a join among them cuts
-- that chain like any other. That is what the side means for two routes
-- that both chose it; what two routes share that chose different sides
-- is not decided by their names ('sharedBy').
annotate :: [RSeg] -> [Item Name]
annotate = chain' [] . written
  where
    chain' :: [Choices] -> [RSeg] -> [Item Name]
    chain' name segments =
      let (stretch, rest) = break joinsHere segments
       in stretch' name stretch <> case rest of
            RJoin pos test root : more ->
              Meet (pos, name) test root : chain' name more
            _ -> []
    stretch' name segments =
      let (chosen', segments') = flattened segments
       in if null chosen'
            then [Along (pos, name) d m | RStep pos d m <- segments']
            else chain' (name <> [chosen']) (conjunctsWritten segments')

-- | The unions on a route that are not chosen, each with its copy as
-- 'annotate' names a step written in its place.
openCopies :: [RSeg] -> [(Int, [Choices])]
openCopies = chain' [] . written
  where
    chain' name segments =
      let (stretch, rest) = break joinsHere segments
       in stretch' name stretch <> case rest of
            _ : more -> chain' name more
            [] -> []
    stretch' name segments =
      let (chosen', segments') = flattened segments
       in if null chosen'
            then [(union'', name) | RAtom union'' <- segments']
            else chain' (name <> [chosen']) (conjunctsWritten segments')

-- | The groups of a stretch, with the conjunct the route chooses in
-- each, and the stretch without those that are no chain apart. A
-- conjunct without a join of its own is none: its segments are in the
-- stretch too, and its groups are groups of the stretch
-- (@(a/(b & c) & d)/…@ is @(a/b & a/c & d)/…@).
flattened :: [RSeg] -> (Choices, [RSeg])
flattened = foldMap \case
  RGroup name' j body
    | not (any joinsHere body') ->
        let (chosen', segments') = flattened body'
         in ((name', j) : chosen', segments')
    | otherwise -> ([(name', j)], [RGroup name' j body])
    where
      body' = written body
  seg -> ([], [seg])

-- | A stretch with each group replaced by the conjunct chosen in it.
conjunctsWritten :: [RSeg] -> [RSeg]
conjunctsWritten = concatMap \case
  RGroup _ _ body -> written body
  seg -> [seg]

joinsHere :: RSeg -> Bool
joinsHere = \case
  RJoin {} -> True
  _ -> False

-- | Where a route is listed among the routes of its alternative: by the
-- conjuncts it chooses, stretch by stretch and, within a stretch, the
-- groups of the stretch before those of the chain the choice gives. So
-- the routes are listed as the path written out lists them, conjunct by
-- conjunct, the first choice varying slowest: @a/c@, @a/d@, @b/c@, @b/d@
-- for @(a & b)/(c & d)@; and a conjunct with a join, which is copied
-- whole, has its routes together for each copy.
listedBy :: [RSeg] -> [Int]
listedBy = chain' . written
  where
    chain' [] = []
    chain' segments =
      let (stretch, rest) = break joinsHere segments
       in stretch' stretch <> chain' (drop 1 rest)
    stretch' segments =
      let (chosen', segments') = flattened segments
       in if null chosen'
            then []
            else map snd chosen' <> chain' (conjunctsWritten segments')

-- | The side a route chose of each union it goes through.
sidesOn :: [RSeg] -> Map Int Int
sidesOn = foldMap \case
  RGroup _ _ body -> sidesOn body
  RPicked name i body -> insertMap name i (sidesOn body)
  _ -> mempty

-- | The unions on a route that are not chosen.
openOn :: [RSeg] -> [Int]
openOn = concatMap \case
  RGroup _ _ body -> openOn body
  RPicked _ _ body -> openOn body
  RAtom name -> [name]
  _ -> []

-- | A route with the side chosen for it written in a union's place.
replaced :: Int -> Int -> [RSeg] -> [RSeg] -> [RSeg]
replaced name i body = map \case
  RAtom name' | name' == name -> RPicked name i body
  RGroup g j inner -> RGroup g j (replaced name i body inner)
  RPicked u k inner -> RPicked u k (replaced name i body inner)
  seg -> seg

-- * Two routes

-- | Two routes of one chain, segment by segment: a step or a join both go
-- through, a group with the conjunct each chooses and its route there, a
-- conjunct both chose that has a join of its own (once its groups are
-- found to agree), or a union they chose different sides of, and whether
-- one of those sides has a group that copies what is next to the union.
-- A union they chose the same side of is that side, read in place.
data Aligned
  = BothStep Int
  | BothJoin Int
  | BothGroup Int Int [RSeg] [RSeg]
  | Within [Aligned]
  | Differ Bool

aligned :: [RSeg] -> [RSeg] -> [Aligned]
aligned xs ys = case (xs, ys) of
  ([], []) -> []
  (RStep p _ _ : xs', RStep q _ _ : ys')
    | p == q -> BothStep p : aligned xs' ys'
  (RJoin p _ _ : xs', RJoin q _ _ : ys')
    | p == q -> BothJoin p : aligned xs' ys'
  (RGroup n j b : xs', RGroup m k c : ys')
    | n == m -> BothGroup j k b c : aligned xs' ys'
  (RPicked n i b : xs', RPicked m k c : ys')
    | n == m ->
        if i == k
          then aligned b c <> aligned xs' ys'
          else Differ (copies b || copies c) : aligned xs' ys'
  _ -> error "aligned: routes of different chains"
  where
    -- whether a chosen side has a group outside its joins: it then adds
    -- a choice to the stretch the union is in
    copies body =
      let top = written body
          (first', rest) = break isJoin' top
          last' = takeWhile (not . isJoin') (reverse top)
       in any isGroup first' || (not (null rest) && any isGroup last')
    isJoin' = \case
      RJoin {} -> True
      _ -> False
    isGroup = \case
      RGroup {} -> True
      _ -> False

-- | What two routes of a chain share: the steps and the joins that are
-- the same copy for both, by where they are written. Nothing when the
-- two cannot be routes of one alternative: when they go through the same
-- copy of a union and chose different sides of it.
--
-- This is 'annotate' for two routes at once. A union the two chose the
-- same side of is that side read in place, as it is for either. A union
-- they chose different sides of is, for the two, a segment that is not
-- chosen: it cuts nothing and chooses nothing, and what the two share
-- around it is what they would share around a step written there. So
-- two routes that chose different sides are in one copy of the union
-- exactly when nothing else tells them apart in its stretch, and the
-- joins inside the sides they chose do not put them in one copy of what
-- follows: in @(a & c)/(a/\@ | b/\@)/(c/\@/e & d)@ the route through
-- @a@ then @a/\@@ and the one through @c@ then @b/\@@ are apart from
-- the first group on.
sharedBy :: [RSeg] -> [RSeg] -> Maybe ([Int], [Int])
sharedBy a b = chain' (aligned a b)
  where
    chain' :: [Aligned] -> Maybe ([Int], [Int])
    chain' items =
      let (stretch, rest) = break isJoin' items
       in do
            here <- stretch' stretch
            case rest of
              BothJoin p : more -> do
                later <- chain' more
                pure (here <> ([], [p]) <> later)
              _ -> pure here
    stretch' items = case agreeing items of
      -- different conjuncts of a group of the stretch: written apart
      Nothing -> Just ([], [])
      Just (items', False)
        | or [True | Differ _ <- items'] -> Nothing
        | otherwise -> Just ([p | BothStep p <- items'], [])
      Just (items', True)
        -- a side with a group, chosen by one of the two, copies the
        -- chain for that one: the two share nothing of it, but still
        -- have to be routes of one alternative
        | or [fork | Differ fork <- items'] -> ([], []) <$ chain' (within items')
        | otherwise -> chain' (within items')
    within = concatMap \case
      Within body -> body
      item -> [item]
    -- the groups of a stretch, with whether there are any: nothing when
    -- the two choose different conjuncts of one
    agreeing :: [Aligned] -> Maybe ([Aligned], Bool)
    agreeing = \case
      [] -> Just ([], False)
      BothGroup j k x y : more
        | j /= k -> Nothing
        | otherwise ->
            let body = aligned x y
             in if any isJoin' body
                  then do
                    (later, _) <- agreeing more
                    Just (Within body : later, True)
                  else do
                    (here, _) <- agreeing body
                    (later, _) <- agreeing more
                    Just (here <> later, True)
      item : more -> do
        (later, grouped) <- agreeing more
        Just (item : later, grouped)
    isJoin' = \case
      BothJoin _ -> True
      _ -> False

-- | Whether two routes that chose different sides of a union can be
-- routes of one alternative: they go through no copy of a union with
-- different sides of it ('sharedBy'), and the node tests on what they
-- share do not contradict each other, as when both are at one joined
-- node and one chose @\@1@ there and the other @\@2@. Routes that cannot
-- be glued two by two cannot be glued with others either, so a way to
-- choose that has such a pair is dropped as it is made, before the ways
-- of the other routes are tried with it.
--
-- The two are routes of a chain without a join of its own, so they
-- start at one node; and when a join of the chain it is a stretch of
-- follows, they end at that join, with its test.
gluesWith :: Maybe NodeTest -> [RSeg] -> [RSeg] -> Bool
gluesWith ending a b = case sharedBy a b of
  Nothing -> False
  Just (steps, joins)
    | tests a && tests b ->
        isJust (glueRoutes [items (0 :: Int) a, items 1 b])
    | otherwise -> True
    where
      shared = (setFromList steps, setFromList joins) :: (Set Int, Set Int)
      items side route' =
        [ case item of
            Along (pos, _) d m -> Along (key (fst shared) side pos) d m
            Meet (pos, _) test root -> Meet (key (snd shared) side pos) test root
        | item <- annotate route'
        ]
          <> [Meet (-1) test False | test <- toList ending]
      key among side pos
        | pos `member` among = 3 * pos
        | otherwise = 3 * pos + 1 + side
  where
    tests route' =
      or [test /= anyNode | Meet _ test _ <- annotate route']
        || any (/= anyNode) ending

-- | Whether the node tests on a route do not contradict each other.
gluesAlone :: Maybe NodeTest -> [RSeg] -> Bool
gluesAlone ending route' =
  and [test == anyNode | Meet _ test _ <- named]
    || isJust
      ( glueRoutes
          [ concat (numbered [named])
              <> [Meet (-1) test False | test <- toList ending]
          ]
      )
  where
    named = annotate route'

-- * Choosing

-- | The alternatives of a chain: every way to choose a side of each
-- relational union for each route, as the routes of the alternative.
--
-- Every route of the chain (a conjunct of each group, its unions not
-- chosen) chooses its unions on its own: a side for each, and where a
-- side has a group, a conjunct of it for each of the routes it then is,
-- which choose the unions that are left on their own in turn
-- ('chosen'). An alternative is a way to choose for every route of the
-- chain in which any two routes can be together ('sharedBy'): no two go
-- through one copy of a union with different sides of it. What its
-- routes share is what any two of them share.
--
-- A join of the chain itself is gone through by every route, and what is
-- before it is copied by nothing after it: so the chain is its stretches
-- between those joins, each with alternatives of its own, and an
-- alternative of the chain is one of each, every route of a stretch
-- going on with every route of the next. Only the routes of one stretch
-- are compared two by two.
resolveWithin :: forall m. (Monad m) => Spend m -> Chain -> m [[[Item Int]]]
resolveWithin spend chain = case ownStretches chain of
  (single, []) -> resolveStretch spend Nothing single
  (first', more) -> do
    -- each stretch ends at the join that follows it
    let endings =
          [Just test | (SJoin _ test _, _) <- more] <> [Nothing]
    parts <-
      for (zip endings (first' : map snd more)) \(ending, stretch) ->
        resolveStretch (Spend spend.made (\_ -> pure ())) ending stretch
    let width = length parts
        -- the steps and joins of each stretch numbered apart, and the
        -- joins between them
        apart i = map (map (fmap (\k -> k * width + i)))
        joins =
          [ Meet (negate j) test root
          | (j, (SJoin _ test root, _)) <- zip [1 ..] more
          ]
        through [] _ = [[]]
        through [a] _ = a
        through (a : as) js =
          [r <> take 1 js <> r' | r <- a, r' <- through as (drop 1 js)]
    for (traverse (\(i, alts) -> map (apart i) alts) (zip [0 ..] parts)) \alts -> do
      let counts = map length alts
          routes' = product (map (min 1000000) counts)
          segments =
            sum
              [ (sum (map length a) + n) * (routes' `div` max 1 n)
              | (a, n) <- zip alts counts
              ]
      spend.made (min 1000000000 segments)
      let routes = through alts joins
      spend.found routes
      pure routes

-- | A chain at its own joins: what is before the first, and each join
-- with what follows it.
ownStretches :: Chain -> (Chain, [(Seg, Chain)])
ownStretches chain =
  let (first', rest) = break isJoin chain
   in (first', go rest)
  where
    go = \case
      [] -> []
      j : rest ->
        let (stretch, rest') = break isJoin rest
         in (j, stretch) : go rest'

-- | 'resolveWithin' for a chain without a join of its own, and the test
-- of the join that follows it, if one does.
resolveStretch ::
  forall m.
  (Monad m) =>
  Spend m ->
  Maybe NodeTest ->
  Chain ->
  m [[[Item Int]]]
resolveStretch spend ending chain = do
  let (count, size) = routesSize chain
  spend.made (count + size)
  if null sides
    then do
      let routes = numbered (map annotate (sortOn listedBy (routesOf chain)))
      spend.made size
      spend.found routes
      pure [routes]
    else do
      each <- for (routesOf chain) \r -> (openCopies r,) <$> chosen r
      ways <- narrowed each
      -- a route with no way to choose is a route of no alternative,
      -- whatever the others choose
      if any (null . snd) ways
        then pure []
        else combine ways [] mempty mempty
  where
    sides = unionsIn chain
    joinedConjunct = hasJoinedConjunct chain
    -- the joins in the own chain of a side, by the union
    sideJoins :: Map Int [Int]
    sideJoins =
      map (\sides' -> [pos | side' <- sides', SJoin pos _ _ <- side']) sides
    sideRoutes :: Map Int [([[RSeg]], Bool)]
    sideRoutes = map (map \side' -> (routesOf side', forks side')) sides
    -- The ways one route can choose its unions: each a set of routes
    -- with every union chosen. A union whose sides have no group is
    -- chosen when no other is left to choose first, since choosing it
    -- changes nothing for the others; of those with a group in a side
    -- each may be the first, since a side with a group makes the route
    -- several routes, which choose the other unions apart.
    chosen :: [RSeg] -> m [[[RSeg]]]
    chosen route' = case ordNub (openOn route') of
      []
        | gluesAlone ending route' -> pure [[route']]
        -- its node tests contradict each other: it is a route of no
        -- alternative
        | otherwise -> pure []
      open -> do
        let forking = filter (any snd . sidesOf) open
            first' = if null forking then take 1 open else forking
        ways <-
          concat <$> for first' \name ->
            concat <$> for (zip [0 ..] (sidesOf name)) \(i, (bodies, _)) -> do
              spend.made (length bodies * segmentsOf route')
              parts <- traverse (chosen . (\body -> replaced name i body route')) bodies
              -- every way of one is compared with every way of the
              -- next
              foldM
                ( \before next -> do
                    spend.made
                      ( checkPair
                          * length before
                          * length next
                          * segmentsOf route'
                      )
                    pure (together' before next)
                )
                [[]]
                parts
        pure (distinctOn sort ways)
    sidesOf name = findWithDefault [] name sideRoutes
    -- The ways of each route without those that choose, for a copy of a
    -- union that all its routes have to choose alike ('settles'), a side
    -- that some route through the copy has no way to choose: so a route
    -- that can only choose one side there is not found to be without a
    -- way after every way of the routes before it has been tried.
    narrowed ::
      [([(Int, [Choices])], [[[RSeg]]])] -> m [([(Int, [Choices])], [[[RSeg]]])]
    narrowed ways
      | all (null . fst) settling = pure ways
      | otherwise = do
          spend.made (sum [length settled * length ws | (settled@(_ : _), ws) <- settling])
          map (second (map fst)) <$> narrow [(c, s', ws) | ((c, _), (s', ws)) <- zip ways settling]
      where
        -- for each route, the copies it has to choose as the others do,
        -- and its ways with the sides they choose
        settling =
          [ ( settles copies,
              [(way, foldMap sidesOn (take 1 way)) | way <- ws]
            )
          | (copies, ws) <- ways
          ]
    narrow ::
      [([(Int, [Choices])], [(Int, [Choices])], [([[RSeg]], Map Int Int)])] ->
      m [([(Int, [Choices])], [([[RSeg]], Map Int Int)])]
    narrow ways = do
      let possible :: Map (Int, [Choices]) (Set Int)
          possible =
            Map.fromListWith
              intersection
              [ (copy', setFromList [i | (_, picks') <- ws, Just i <- [lookup name picks']])
              | (_, settled, ws) <- ways,
                copy'@(name, _) <- settled
              ]
          keeps settled (_, picks') =
            and
              [ i `member` findWithDefault mempty copy' possible
              | copy'@(name, _) <- settled,
                Just i <- [lookup name picks']
              ]
          ways' =
            [ (copies, settled, if null settled then ws else filter (keeps settled) ws)
            | (copies, settled, ws) <- ways
            ]
          sizes made = [length ws | (_, settled, ws) <- made, not (null settled)]
      if sizes ways' == sizes ways
        then pure [(copies, ws) | (copies, _, ws) <- ways]
        else do
          spend.made (sum [length settled * length ws | (_, settled@(_ : _), ws) <- ways'])
          narrow ways'
    -- every way to take one set of routes of each, the routes of which
    -- can all be together
    together' :: [[[RSeg]]] -> [[[RSeg]]] -> [[[RSeg]]]
    together' before next =
      [ a <> b
      | a <- before,
        b <- next,
        and [isJust (sharedBy x y) | x <- a, y <- b]
      ]
    -- The alternatives, depth first: the ways of the first route, each
    -- with the ways of the others that can be together with it. The
    -- routes so far are kept by the side they chose of each union, since
    -- only two routes that chose different sides of a union may be
    -- unable to be together.
    -- Two routes that go through one copy of a union before any union
    -- is chosen have to choose the same side of it, whatever the other
    -- unions on them choose, when no side of those has a group (which
    -- could tell the two apart). So the side chosen for each such copy
    -- is kept, and a later route of the copy is not tried with another.
    settles copies =
      [ copy'
      | copy'@(name, _) <- copies,
        and [not (any snd (sidesOf other)) | (other, _) <- copies, other /= name]
      ]
    combine ::
      [([(Int, [Choices])], [[[RSeg]]])] ->
      [[RSeg]] ->
      Map (Int, Int) [[RSeg]] ->
      Map (Int, [Choices]) Int ->
      m [[[Item Int]]]
    combine [] routes _ _ = do
      let (differing, size) = differingPairs routes
          segments = sum (map segmentsOf routes)
      -- what working out what the routes share costs, before it is done
      spend.made (checkPair * differing * size + (checkShare + 1) * segments)
      let listed
            -- Without a conjunct that has a join, and without a side
            -- with a group chosen, the routes are in order as they are.
            | joinedConjunct || any (any chooses) routes =
                sortOn listedBy (reverse routes)
            | otherwise = reverse routes
          -- the joins of the sides of the unions that the routes chose
          -- different sides of: where every route chose one side, the
          -- routes are listed as that side written there lists them
          mixed =
            keysSet . Map.filter ((> 1) . length) $
              ( Map.fromListWith
                  (<>)
                  [ (name, singletonSet i)
                  | r <- routes,
                    (name, i) <- mapToList (sidesOn r)
                  ] ::
                  Map Int (Set Int)
              )
          joins =
            setFromList (concat (toList (Map.restrictKeys sideJoins mixed)))
          items = uncurry together (sharing joins differing listed)
      spend.found items
      pure [items]
    combine ((copies, ways) : rest) routes bySide settled = go allowed
      where
        allowed =
          [ way
          | way <- ways,
            and
              [ all ((== Just i) . lookup name . sidesOn) way
              | copy'@(name, _) <- settles copies,
                Just i <- [lookup copy' settled]
              ]
          ]
        settledBy way =
          foldl'
            (\acc (copy', i) -> insertMap copy' i acc)
            settled
            [ (copy', i)
            | copy'@(name, _) <- settles copies,
              r <- take 1 way,
              Just i <- [lookup name (sidesOn r)]
            ]
        go [] = pure []
        go (way : more) = do
          let picked = [(r, mapToList (sidesOn r)) | r <- way]
          fits <- allM fitsIn picked
          if fits
            then do
              here <-
                combine
                  rest
                  (reverse way <> routes)
                  ( foldl'
                      (\acc (r, picks') -> foldl' (\acc' k -> Map.insertWith (<>) k [r] acc') acc picks')
                      bySide
                      picked
                  )
                  (settledBy way)
              later <- go more
              pure (here <> later)
            else go more
        fitsIn (route', picks') = do
          let others =
                [ other
                | (name, i) <- picks',
                  (i', _) <- zip [0 ..] (sidesOf name),
                  i' /= i,
                  other <- findWithDefault [] (name, i') bySide
                ]
          spend.made (1 + checkPair * length others * segmentsOf route')
          pure (all (gluesWith ending route') others)
    -- whether a segment is a side with a group that a route chose
    chooses = \case
      RPicked _ _ body -> any isGroup' body || any chooses body
      RGroup _ _ body -> any chooses body
      _ -> False
    isGroup' = \case
      RGroup {} -> True
      RPicked _ _ body -> any isGroup' body
      _ -> False
    allM f = foldM (\ok x -> if ok then f x else pure False) True

-- | How many pairs of routes chose different sides of some union, and the
-- length of the longest route: what working out what they share costs.
differingPairs :: [[RSeg]] -> (Int, Int)
differingPairs routes =
  ( sum
      [ length a * length b
      | (p, a) : rest <- tails (mapToList grouped),
        (q, b) <- rest,
        differ p q
      ],
    maximumEx (1 : map segmentsOf routes)
  )
  where
    grouped :: Map (Map Int Int) [()]
    grouped = Map.fromListWith (<>) [(sidesOn r, [()]) | r <- routes]

-- | Whether two routes chose different sides of some union.
differ :: Map Int Int -> Map Int Int -> Bool
differ p q = or (Map.intersectionWith (/=) p q)

-- | The routes of an alternative, each step and join with a number that
-- is the same for the routes that share it.
--
-- Two routes that chose the same side of every union both go through
-- share what has the same name for both ('annotate'): each union is
-- read in place for the two. Two routes that chose different sides of a
-- union share what 'sharedBy' says. What a route shares with a second
-- and the second with a third is one step or join for all three.
sharing :: Set Int -> Int -> [[RSeg]] -> (Set Int, [[Item Int]])
sharing sideJoins differing routes =
  ( if null sideJoins
      then mempty
      else
        setFromList
          [ k
          | (r, r') <- zip named items,
            (Meet (pos, _) _ _, Meet k _ _) <- zip r r',
            pos `member` sideJoins
          ],
    items
  )
  where
    named = map annotate routes
    byName = numbered named
    picks = map sidesOn routes
    items
      -- no two routes chose different sides: every union is read in
      -- place
      | differing == 0 = byName
      | otherwise =
          [ [ case item of
                Along _ d m -> Along (number node) d m
                Meet _ test root -> Meet (number node) test root
            | (node, item) <- r
            ]
          | r <- nodes
          ]
    -- every step and join of every route, numbered
    nodes :: [[(Int, Item Int)]]
    nodes =
      S.evalState
        (traverse (traverse (\item -> S.state \n -> ((n, item), n + 1))) byName)
        (0 :: Int)
    -- by name: the routes that chose alike, and those that chose
    -- compatibly
    alike :: [(Int, Int)]
    alike =
      [ (node, node')
      | byPicks <- toList inName,
        (p, node : more) : later <- tails (mapToList byPicks),
        node' <- more <> [k | (q, k : _) <- later, not (differ p q)]
      ]
    inName :: Map (Bool, Int) (Map (Map Int Int) [Int])
    inName =
      Map.fromListWith
        (Map.unionWith (flip (<>)))
        [ (key item, singletonMap p [node])
        | (p, r) <- zip picks nodes,
          (node, item) <- r
        ]
    key = \case
      Along k _ _ -> (False, k)
      Meet k _ _ -> (True, k)
    -- by two routes that chose different sides: where each step and
    -- join of a route is written, and the routes by what they chose
    written' :: [(Map Int Int, ([RSeg], Map (Bool, Int) Int))]
    written' =
      [ ( p,
          ( route',
            mapFromList
              [ (place, node)
              | ((node, _), place) <- zip r (map placeOf names')
              ]
          )
        )
      | (p, route', r, names') <- zip4 picks routes nodes named
      ]
    placeOf = \case
      Along (pos, _) _ _ -> (False, pos)
      Meet (pos, _) _ _ -> (True, pos)
    apart :: [(Int, Int)]
    apart =
      [ (node, node')
      | (p, as) : later <- tails (mapToList (Map.fromListWith (flip (<>)) [(p, [x]) | (p, x) <- written'])),
        (q, bs) <- later,
        differ p q,
        (a, placesA) <- as,
        (b, placesB) <- bs,
        Just (steps, joins) <- [sharedBy a b],
        place <- map (False,) steps <> map (True,) joins,
        Just node <- [lookup place placesA],
        Just node' <- [lookup place placesB]
      ]
    parents :: IntMap Int
    parents = foldl' union' mempty (alike <> apart)
    union' parent (x, y) =
      let rx = find' parent x
          ry = find' parent y
       in if rx == ry then parent else IntMap.insert (max rx ry) (min rx ry) parent
    find' parent x = maybe x (find' parent) (IntMap.lookup x parent)
    numbers :: IntMap Int
    numbers =
      IntMap.fromList
        (zip (ordNub [find' parents node | r <- nodes, (node, _) <- r]) [0 ..])
    number node = IntMap.findWithDefault 0 (find' parents node) numbers

-- | Routes with their names as numbers.
numbered :: [[Item Name]] -> [[Item Int]]
numbered routes = map (map (fmap number)) routes
  where
    numbers :: Map Name Int
    numbers =
      mapFromList
        (zip (ordNub [name | r <- routes, item <- r, name <- toList' item]) [0 ..])
    toList' = \case
      Along name _ _ -> [name]
      Meet name _ _ -> [name]
    number name = findWithDefault 0 name numbers

-- | The routes of an alternative in which routes chose different sides
-- of a union, listed as a path without unions lists them, if it is one.
--
-- Such a path lists the routes that share a join next to each other
-- (they are in one conjunct, or in one chain), and those that all go
-- through one join as every route up to it with every route from it on,
-- those up to it varying slowest. The routes come listed by the
-- conjuncts they choose ('listedBy'), with the unions in place: that is
-- the order of a path without unions only where the sides that were
-- chosen have no join that routes share. So the routes are put together
-- here: those that are connected by the joins they share, where the
-- first of them is; and among them, those through a join that all of
-- them go through, as that join lists them, the routes up to it and
-- those from it on put together in turn. The first route stays first.
--
-- Where the routes that chose a side with a join differ before and
-- after it, this adds the routes that cross over: the route up to the
-- join of each with the route from it on of the other. They are made of
-- steps the alternative has, so they change nothing a query finds or
-- @mk@ creates. Where one of them would end with a step no route ends
-- with (the last step of a route is what tags and file types read),
-- the routes are left as they are, and the alternative is no path
-- without unions.
--
-- Nothing but the order of the routes, and the routes that cross over,
-- is decided here: what the routes share is decided before
-- ('sharing'). An alternative in which every route chose alike is
-- listed as its path lists it already, and is left as it is.
together :: Set Int -> [[Item Int]] -> [[Item Int]]
together sideJoins
  | null sideJoins = id
  | otherwise = listed
  where
    listed :: [[Item Int]] -> [[Item Int]]
    listed rs = concatMap connected (components rs)
    -- the routes that go through a join one before them goes through; a
    -- join that every route through it starts with joins nothing
    components :: [[Item Int]] -> [[[Item Int]]]
    components rs = go rs
      where
        later = setFromList (concatMap (meetsOf . drop 1) rs) :: Set Int
        keysOf r = setFromList [k | k <- meetsOf r, k `member` later] :: Set Int
        go = \case
          [] -> []
          r : rest ->
            let (same, others) = grow (keysOf r) rest
             in (r : same) : go others
        grow ks rest
          | null ks = ([], rest)
          | otherwise =
              case partition (any (`member` ks) . meetsOf) rest of
                ([], _) -> ([], rest)
                (more, others) ->
                  let (further, others') =
                        grow (ks <> foldMap keysOf more) others
                   in (more <> further, others')
    connected :: [[Item Int]] -> [[Item Int]]
    connected rs = case rs of
      first' : rest@(_ : _)
        | k : _ <- [k | k <- meetsOf first', all (passes k) rest] ->
            let parts = map (splitOn k) rs
                befores = ordNub (map fst parts)
                afters = ordNub (map (drop 1 . snd) parts)
                meet = take 1 (dropWhile (not . isMeet k) first')
                rs' = [b <> meet <> a | b <- listed befores, a <- listed afters]
             in -- (the two parts may have routes that cross over added)
                if length befores * length afters == length (ordNub rs)
                  || (k `member` sideJoins && lastSteps rs' == lastSteps rs)
                  then rs'
                  else rs
        | otherwise -> crossed (10 :: Int) rs
      _ -> rs
    isMeet k = \case
      Meet k' _ _ -> k' == k
      _ -> False
    -- routes that no one join connects: put together at each join of a
    -- side in turn
    crossed :: Int -> [[Item Int]] -> [[Item Int]]
    crossed 0 rs = rs
    crossed n rs =
      let rs' =
            foldl'
              (flip regroup)
              rs
              ( filter
                  (\k -> k `member` sideJoins && k `member` later)
                  (ordNub (concatMap meetsOf rs))
              )
          later = setFromList (concatMap (meetsOf . drop 1) rs) :: Set Int
       in if rs' == rs then rs else crossed (n - 1) rs'
    regroup k rs =
      let through = [splitOn k r | r <- rs, passes k r]
          crossing =
            [b <> a | b <- ordNub (map fst through), a <- ordNub (map snd through)]
          place = \case
            [] -> []
            r : more
              | passes k r -> crossing <> filter (not . passes k) more
              | otherwise -> r : place more
          rs' = place rs
       in if length through < 2 || lastSteps rs' /= lastSteps rs
            then rs
            else rs'
    lastSteps :: [[Item Int]] -> Set (Maybe Int)
    lastSteps rs = setFromList [lastMay [k | Along k _ _ <- r] | r <- rs]
    passes k = any \case
      Meet k' _ _ -> k' == k
      _ -> False
    splitOn k = break \case
      Meet k' _ _ -> k' == k
      _ -> False
    meetsOf r = [k | Meet k _ _ <- r]

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
  | -- | its routes alone, listed once with no union chosen, are more than
    -- are listed to count alternatives ('checkRoutes'), whatever the
    -- limit is: only no limit at all lets it through
    TooManyRoutes {limit :: Int}
  deriving stock (Eq, Show)

-- | The largest limit in effect at a relational union of the path: the one
-- of the innermost @max-alternatives@ override around it, or the default.
-- The alternatives multiply across unions, so they cannot be attributed to
-- one; the most generous limit asked for applies to the whole path.
pathLimit :: Path -> Maybe AlternativesLimit
pathLimit = go (AtMostAlternatives defaultMaxAlternatives)
  where
    go l = \case
      WithLimits _ l' p -> go (fromMaybe l l'.maxAlternatives) p
      p :|| q -> Just l `max` go l p `max` go l q
      p :++ q -> go l p `max` go l q
      p :/ q -> go l p `max` go l q
      p :& q -> go l p `max` go l q
      Backwards p -> go l p
      _ -> Nothing

hasConj :: Path -> Bool
hasConj = \case
  _ :& _ -> True
  Backwards p -> hasConj p
  p :/ q -> hasConj p || hasConj q
  Union _ p q -> hasConj p || hasConj q
  WithLimits _ _ p -> hasConj p
  _ -> False

-- | Whether compiling the path stays within its alternatives limit. The
-- alternatives are counted as 'compilePath' makes them, after
-- contradictory ones are dropped and the same ones are merged, one after
-- the other, so the count is exact and stops at the first beyond the
-- limit ('TooManyAlternatives').
--
-- Two bounds make the check finish in a bounded time, without repeating
-- a long compilation; a path beyond either is over the limit too,
-- however many alternatives it would come to:
--
-- * the routes of the path, listed once with no union chosen, may come
--   to 'checkRoutes' ('routesSize'): a path with more is refused before
--   anything is made ('TooManyRoutes');
-- * everything made on the way is counted by its size, before it is
--   made, and may come to 'checkSteps' for each alternative allowed and
--   to 'checkOnce' times what the routes of the path come to listed
--   once ('TooLargeToCount'). What is counted: every chain by its steps and node specs
--   ('weight'); the routes of a stretch by their segments; every way a
--   route chooses a union by the routes it then is; every two routes
--   that chose different sides of a union and are compared, by the
--   length of a route, 'checkPair' times; every way of a route that is
--   tried with those of the routes before it, also when it does not go
--   with them, by one; every way to choose that is
--   found for a stretch, by the segments of its routes, once and
--   'checkShare' times more; and every alternative of a chain with
--   joins of its own by the segments of its routes.
--
-- A route is counted by all its segments, through the conjuncts and the
-- sides it chose ('segmentsOf'). Ways to choose that contradict each
-- other are not listed: a route whose choice cannot be glued with that
-- of a route before it is dropped as it is made ('gluesWith'), a route
-- with no way to choose ends the count before any other is tried, and a
-- side that some route through a copy cannot choose is not tried for
-- the others ('narrowed').
--
-- A path without a relational union has no limit, and neither has one
-- without a conjunction: nothing copies its unions, so it compiles as it
-- would with shared unions.
checkAlternatives :: Path -> Either TooManyAlternatives ()
checkAlternatives = bimap fst (const ()) . checkSpending

-- | 'checkAlternatives', with what it spent, also when it refuses the
-- path: the size of what it made, and the steps it took.
checkSpending ::
  Path -> Either (TooManyAlternatives, (Int, Int)) (Int, Int)
checkSpending p = case pathLimit p of
  Just (AtMostAlternatives n) | hasConj p -> do
    let refuse ::
          TooManyAlternatives ->
          S.StateT CheckBudget (Either (TooManyAlternatives, (Int, Int))) a
        refuse why = do
          budget <- S.get
          lift (Left (why, (budget.spent, budget.found)))
        -- refused before what is counted is made
        spent k = do
          budget <- S.get
          when (budget.spent + k > budget.allowed) $
            refuse (TooLargeToCount n)
          S.put budget {spent = budget.spent + k}
        found routes = do
          budget <- S.get
          let patterns =
                maybe id insertSet (glueRoutes routes) budget.patterns
          when (length patterns > n) (refuse (TooManyAlternatives n))
          S.put budget {found = budget.found + 1, patterns = patterns}
        spend = Spend spent found
    flip S.evalStateT (CheckBudget (checkSteps * n) 0 0 mempty) do
      chains <- structuresWithin spend (pushBackwards (withoutLimits p))
      -- what listing the routes of the path once costs is allowed some
      -- times over, whatever the limit
      let once = sum [count + size | (count, size) <- map routesSize chains]
      when (once > checkRoutes) (refuse (TooManyRoutes n))
      S.modify' \budget -> budget {allowed = budget.allowed + checkOnce * once}
      for_ chains (resolveWithin spend)
      budget <- S.get
      pure (budget.spent, budget.found)
  _ -> pure (0, 0)

-- | What 'checkAlternatives' may spend and has spent, the ways to choose
-- it has found, and what they glue to.
data CheckBudget = CheckBudget
  { allowed :: Int,
    spent :: Int,
    found :: Int,
    patterns :: Set PatternPath
  }

-- | The size of everything 'checkAlternatives' may make, for each
-- alternative allowed. With 'checkRoutes' this bounds the check at the
-- default limit to 400,000 and twice 150,000; the slowest check measured
-- in an unoptimized build (docs/path-pattern-ir.md, Alternatives limit)
-- is a measurement and not a guarantee.
checkSteps :: Int
checkSteps = 400

-- | How many times over 'checkAlternatives' may spend what listing the
-- routes of the path once costs, besides 'checkSteps': a path with few
-- alternatives is not refused for having many routes, which it has
-- without its unions too.
checkOnce :: Int
checkOnce = 2

-- | The most the routes of a path may come to, listed once with no union
-- chosen, for 'checkAlternatives' to count its alternatives at all: a
-- path with more is refused before anything is made. With 'checkSteps'
-- this bounds what the check makes whatever the path, and so the time it
-- takes.
checkRoutes :: Int
checkRoutes = 150000

-- | What working out what the routes of an alternative of a union share
-- costs 'checkAlternatives', for each segment of the routes: it takes
-- about that many times as long as listing the routes does.
checkShare :: Int
checkShare = 3

-- | What comparing two routes that chose different sides of a union
-- costs 'checkAlternatives', for each segment of a route.
checkPair :: Int
checkPair = 4

-- * Alternatives as paths

-- | Each alternative of a path for which a path without unions is found
-- that compiles to it, routes in order, with that path (with @~@ pushed
-- down to the steps). When every route chose the same side of each
-- union, that is the path with one side of each union kept
-- ('uniformChoices'); otherwise the routes are written out
-- ('writtenOut'), as in @a/c & b/d@ for @(a | b)/(c & d)@, and the
-- result is kept if it compiles to the alternative. An alternative for
-- which none is found is left out: one in which a walk is shared by a
-- route inside a joined conjunct and a route outside it has none, as
-- when in @(x/\@ | s)/((c & y)/\@/e & d)@ the routes through @c@ and
-- @d@ choose @x/\@@ and the one through @y@ chooses @s@. The list is
-- lazy, so finding some alternatives may cost less than compiling the
-- path. An alternative may occur more than once.
compileAlternatives :: Path -> [(PatternPath, Path)]
compileAlternatives p =
  [ (pattern', choice)
  | (routes, pattern') <- alternatives p,
    Just choice <- [lookup pattern' uniform <|> spelled routes pattern']
  ]
  where
    p' = pushBackwards (withoutLimits p)
    uniform :: Map PatternPath Path
    uniform =
      mapFromList . reverse $
        [ (pattern', choice)
        | (chain, choice) <- mapToList (uniformChoices p'),
          Just chain' <- [normal (renamed chain)],
          Just pattern' <-
            [glueRoutes (numbered (map annotate (sortOn listedBy (routesOf chain'))))]
        ]
    spelled routes pattern' =
      headMay
        [ choice
        | choice <- writtenOut routes,
          (compilePath choice).alternatives == singletonSet pattern'
        ]

-- | Paths without unions that have the given routes, if they are those
-- of one: a single route is its steps and joins; routes that all go
-- through one join are those up to it, the join, and those from it on;
-- and routes that fall into groups with no join in common are a
-- conjunction. Whether a path compiles to the alternative is for the
-- caller to check.
--
-- An @\@@ that every route through it starts with merges nothing, but
-- written at the start of a conjunct it keeps the conjunct together
-- ('norm'), which the routes may or may not be: so the routes are tried
-- as they are and without it.
writtenOut :: [[Item Int]] -> [Path]
writtenOut routes0 =
  ordNub (mapMaybe writtenAs [routes0, map withoutStart routes0])
  where
    withoutStart = \case
      Meet k test False : rest@(_ : _)
        | test == anyNode, k `notMember` later -> rest
      r -> r
    later :: Set Int
    later = setFromList [k | r <- routes0, Meet k _ _ <- drop 1 r]

writtenAs :: [[Item Int]] -> Maybe Path
writtenAs = \case
  [] -> Nothing
  [single] -> Just (chainPath single)
  routes
    | k : _ <- common routes -> do
        let (before, after) = unzip (map (splitOn k) routes)
            befores = ordNub before
            afters = ordNub (map (drop 1) after)
            join' = take 1 (concatMap (take 1) after)
        guard (length routes == length befores * length afters)
        l <- if befores == [[]] then Just Nothing else Just <$> writtenAs befores
        r <- if afters == [[]] then Just Nothing else Just <$> writtenAs afters
        let j = chainPath join'
        pure case (l, r) of
          (Nothing, Nothing) -> j
          (Just x, Nothing) -> x :/ testOnly join'
          (Nothing, Just y) -> j :/ y
          (Just x, Just y) -> x :/ testOnly join' :/ y
    | otherwise -> case groups routes of
        [_] -> Nothing
        parts -> foldl1Ex' (:&) <$> traverse writtenAs parts
  where
    common routes = case routes of
      [] -> []
      r : rest -> [k | Meet k _ _ <- r, all (passes k) rest]
    passes k = any \case
      Meet k' _ _ -> k' == k
      _ -> False
    splitOn k = break \case
      Meet k' _ _ -> k' == k
      _ -> False
    -- runs of routes that share a join with one before them
    groups :: [[Item Int]] -> [[[Item Int]]]
    groups = \case
      [] -> []
      r : rest ->
        let (same, others) = grow (meetsOf r) rest
         in (r : same) : groups others
    grow ks rest =
      case span (\r -> any (`elem` ks) (meetsOf r)) rest of
        ([], _) -> ([], rest)
        (more, others) ->
          let (further, others') = grow (ks <> concatMap meetsOf more) others
           in (more <> further, others')
    meetsOf r = [k | Meet k _ _ <- r]
    -- a join after something is written as its test
    testOnly = \case
      [Meet _ test _] -> testPath test
      _ -> One
    testPath = \case
      Exactly n -> Absolute n
      Except ns
        | null ns -> One
        | otherwise -> ExcludingNIDs ns
    chainPath :: [Item Int] -> Path
    chainPath items = case concatMap itemPath (zip (True : repeat False) items) of
      [] -> One
      x : xs -> foldl' (:/) x xs
    itemPath (first', item) = case item of
      Along _ d m ->
        [ (case d of Out -> id; In -> Backwards) case m of
            MLiteral t -> Literal t
            MWild -> Wild
            MRegex r -> RegexMatch r
        ]
      -- an @N that tests the current node is written after an @
      Meet _ test@(Exactly _) False | first' -> [One, testPath test]
      Meet _ test _ -> [testPath test]

-- | The alternatives as if each union were chosen once for all its copies,
-- with a representative choice for each chain (by its 'shape').
uniformChoices :: Path -> Map Chain Path
uniformChoices = \case
  p@One -> one (SJoin 0 anyNode False) p
  Zero -> mempty
  p@Wild -> one (SStep 0 Out MWild) p
  p@(Literal t) -> one (SStep 0 Out (MLiteral t)) p
  p@(RegexMatch r) -> one (SStep 0 Out (MRegex r)) p
  p@(Backwards Wild) -> one (SStep 0 In MWild) p
  p@(Backwards (Literal t)) -> one (SStep 0 In (MLiteral t)) p
  p@(Backwards (RegexMatch r)) -> one (SStep 0 In (MRegex r)) p
  Backwards _ -> error "uniformChoices: pushBackwards leaves ~ only on steps"
  p@(Absolute nid) -> one (SJoin 0 (Exactly nid) True) p
  p@(ExcludingNIDs ns) -> one (SJoin 0 (Except ns) False) p
  -- left-biased, so the first choice written is kept
  Union _ p q -> uniformChoices p <> uniformChoices q
  WithLimits _ _ p -> uniformChoices p
  p :& q ->
    combine
      (\a b -> Just [SGroup 0 (conjuncts a <> conjuncts b)])
      (:&)
      (uniformChoices p)
      (uniformChoices q)
  p :/ q -> combine sequenced (:/) (uniformChoices p) (uniformChoices q)
  where
    one seg = singletonMap [seg]
    combine ::
      (Chain -> Chain -> Maybe Chain) ->
      (Path -> Path -> Path) ->
      Map Chain Path ->
      Map Chain Path ->
      Map Chain Path
    combine f g a b =
      -- on a collision, the first choice is kept
      mapFromList . reverse $
        [ (chain, g px py)
        | (x, px) <- mapToList a,
          (y, py) <- mapToList b,
          Just chain <- [f x y]
        ]

-- * Gluing

data GlueState = GlueState
  { nextVar :: Int,
    nextEdge :: Int,
    parents :: Map PVertex PVertex,
    vertexTests :: Map PVertex NodeTest,
    hops :: Map Int (EdgeId, PVertex),
    meets :: Map Int PVertex
  }

type Glue = S.StateT GlueState Maybe

-- | Glue the routes of one alternative into a pattern: every step is an
-- edge to a variable, steps with the same name being the same edge, and
-- the routes that go through a join meet there. A relative route starts
-- at the current node, and one that starts with an @\@N@ at that node.
-- 'Nothing' if the node tests contradict each other.
glueRoutes :: [[Item Int]] -> Maybe PatternPath
glueRoutes items = do
  (walks, st) <-
    S.runStateT
      (traverse glueRoute items)
      (GlueState 0 0 mempty mempty mempty mempty)
  let rep v = findRep st.parents v
      goHop (Hop' e d m t) = Hop e d m (rep t)
      routes = [Route (rep root) (map goHop hops') | (root, hops') <- walks]
      tests =
        mapFromList
          [ (v, t)
          | (v, t) <- mapToList st.vertexTests,
            rep v == v,
            t /= anyNode
          ]
  case routes of
    [] -> Nothing
    r : rs -> Just (canonicalize (PatternPath tests (r :| rs)))

data Hop' = Hop' EdgeId Direction Matcher PVertex

glueRoute :: [Item Int] -> Glue (PVertex, [Hop'])
glueRoute items = do
  let root = case items of
        Meet _ (Exactly n) True : _ -> Fixed n
        _ -> Current
  (hops', _) <- foldM step ([], root) items
  pure (root, reverse hops')
  where
    step (hops', v) = \case
      Along name d m -> do
        st <- S.get
        (e, t) <- case lookup name st.hops of
          Just known -> pure known
          Nothing -> do
            let made = (EdgeId st.nextEdge, Var (VarId st.nextVar))
            S.put
              st
                { nextEdge = st.nextEdge + 1,
                  nextVar = st.nextVar + 1,
                  hops = insertMap name made st.hops
                }
            pure made
        pure (Hop' e d m t : hops', t)
      Meet name test _ -> do
        st <- S.get
        case lookup name st.meets of
          Just first' -> unify first' v
          Nothing -> S.put st {meets = insertMap name v st.meets}
        constrain v test
        pure (hops', v)

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
