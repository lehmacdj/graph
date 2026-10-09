-- | Compile surface paths to route patterns (docs/path-route-ir.md, Compilation).
--
-- Compilation has four steps:
--
-- 1. push @~@ down to the steps, reversing sequences;
-- 2. choose every shared union (@++@), once for the whole path, and write
--    what is left as chains of segments ('Chain'): steps, joins, groups
--    (@&@) and relational unions (@|@), every side that is written
--    being a side of its own;
-- 3. list the routes of each chain, stretch by stretch between its own
--    joins, each route choosing a conjunct of every group it goes
--    through and a side of every relational union, and name every step
--    and join on a route by the copy of it the route goes through
--    ('annotate'): that decides what routes share;
-- 4. glue the routes into a pattern: steps with the same name are one
--    edge, and joins with the same name are one vertex.
--
-- Every path compiles. Whether a command runs it is another question,
-- which 'checkCopies' answers from the path as it is written: a path in
-- which the product of step 3 would copy a meeting point is refused.
module Models.PathPattern.Compile
  ( compilePath,
    compileAlternatives,
    checkCopies,
    asRead,
    alternativeRoutes,
    Item (..),
    CopiedMeetingPoint (..),
    UnionReading (..),
    WalksAcross (..),
    IdleJoin (..),
    pushBackwards,
  )
where

import Control.Monad.State.Strict qualified as S
import Data.IntMap.Strict qualified as IntMap
import Data.List (scanl, scanr)
import Data.Map.Strict qualified as Map
import Models.Path.Simple
import Models.PathPattern
import MyPrelude

compilePath :: Path -> PathQuery
compilePath = PathQuery . setFromList . map snd . alternatives

-- | The alternatives of a path, each with its routes as lists of steps
-- and joins numbered by the copy they are ('Item'), for tests that look
-- at what the routes of an alternative share.
alternativeRoutes :: Path -> [([[Item Int]], PatternPath)]
alternativeRoutes = alternatives

-- | The alternatives of a path: the routes of each, and what they glue
-- to.
alternatives :: Path -> [([[Item Int]], PatternPath)]
alternatives p =
  [ (routes, pattern')
  | chain <- structures (prepared p),
    routes <- resolve chain,
    Just pattern' <- [glueRoutes routes]
  ]

-- | A path without the nodes that carry its limits. A limit says how often
-- @mk@ may walk the path (@max-repairs@), not what the path means,
-- so everything here compiles the path without them: wherever such a node
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

-- | The chains of a path, one for each choice of its shared unions, in
-- normal form ('norm').
structures :: Path -> [Chain]
structures path = ordNub . mapMaybe normal . toList $ go 0 path
  where
    total = leaves path
    -- a name for a group or a union: no two start at the same leaf and
    -- have as many leaves
    named pos p = pos * (total + 1) + leaves p
    go :: Int -> Path -> Set Chain
    go pos = \case
      One -> one (SJoin pos anyNode False)
      Zero -> mempty
      Wild -> one (SStep pos Out MWild)
      Literal t -> one (SStep pos Out (MLiteral t))
      RegexMatch r -> one (SStep pos Out (MRegex r))
      Backwards Wild -> one (SStep pos In MWild)
      Backwards (Literal t) -> one (SStep pos In (MLiteral t))
      Backwards (RegexMatch r) -> one (SStep pos In (MRegex r))
      Backwards _ -> error "structures: pushBackwards leaves ~ only on steps"
      Absolute nid -> one (SJoin pos (Exactly nid) True)
      ExcludingNIDs ns -> one (SJoin pos (Except ns) False)
      p :++ q -> go pos p <> go (pos + leaves p) q
      p@(_ :|| _) ->
        -- every side that is written is a side of its own, also when
        -- another is written the same; one that has no chain (@%never@,
        -- or node tests that contradict each other) is no side
        let sides =
              filter
                (not . null)
                [toList (go pos' side) | (pos', side) <- placed pos (unionSides p)]
         in setFromList (mapMaybe (unionOf (named pos p)) (sequence sides))
      p :& q ->
        let a = go pos p
            b = go (pos + leaves p) q
         in setFromList
              [ [SGroup (named pos (p :& q)) (conjuncts x <> conjuncts y)]
              | x <- toList a,
                y <- toList b
              ]
      WithLimits _ _ p -> go pos p
      p :/ q ->
        let a = go pos p
            b = go (pos + leaves p) q
         in if null a
              then a
              else setFromList (catMaybes [sequenced x y | x <- toList a, y <- toList b])
    one = singletonSet . (: [])

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
-- nothing when it has none. A side that is a union itself is that
-- union's sides (@p | (q | r)@ has the three sides @p@, @q@ and @r@).
-- No side is left out for being the same as another.
unionOf :: Int -> [Chain] -> Maybe Chain
unionOf name sides = case concatMap sidesOf sides of
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

-- | The normal form of a chain, in which the order sides are written in
-- is gone:
--
-- * joins right after one another are one join;
-- * a union has its sides in a fixed order ('shape'; sides of the same
--   shape in the order they are written), each side that is written
--   being a side of its own, and a union of one side is that side;
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

-- | A path as it is compiled: without its limits, and @~@ pushed down to
-- the steps. Nothing else is done to it: every side of a union that is
-- written is a side of its own, whatever it is.
prepared :: Path -> Path
prepared = pushBackwards . withoutLimits

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
-- found to agree), or a union they chose different sides of. A union
-- they chose the same side of is that side, read in place.
data Aligned
  = BothStep Int
  | BothJoin Int
  | BothGroup Int Int [RSeg] [RSeg]
  | Within [Aligned]
  | Differ

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
          else Differ : aligned xs' ys'
  _ -> error "aligned: routes of different chains"

-- | What two routes of a chain share: the steps and the joins that are
-- the same copy for both, by where they are written. Nothing when the
-- two cannot be routes of one alternative: when they go through the same
-- copy of a union and chose different sides of it.
--
-- This is 'annotate' for two routes at once. A union the two chose the
-- same side of is that side read in place, as it is for either. A union
-- they chose different sides of is, for the two, a segment that is not
-- chosen: it cuts nothing and chooses nothing, and what the two share
-- around it is what they would share around a step written there,
-- whatever the sides are: a side with a group and a join of its own, as
-- in @((a & b)/\@ | z)/…@, is one walk after its join as @x/\@@ is. So
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
        | or [True | Differ <- items'] -> Nothing
        | otherwise -> Just ([p | BothStep p <- items'], [])
      Just (items', True) -> chain' (within items')
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
resolve :: Chain -> [[[Item Int]]]
resolve chain = case ownStretches chain of
  (single, []) -> resolveStretch Nothing single
  (first', more) ->
    -- each stretch ends at the join that follows it
    let endings =
          [Just test | (SJoin _ test _, _) <- more] <> [Nothing]
        parts =
          [ resolveStretch ending stretch
          | (ending, stretch) <- zip endings (first' : map snd more)
          ]
        width = length parts
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
     in [ through alts joins
        | alts <- traverse (\(i, alts) -> map (apart i) alts) (zip [0 ..] parts)
        ]

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

-- | 'resolve' for a chain without a join of its own, and the test of the
-- join that follows it, if one does.
resolveStretch :: Maybe NodeTest -> Chain -> [[[Item Int]]]
resolveStretch ending chain
  | null sides = [numbered (map annotate (sortOn listedBy (routesOf chain)))]
  -- a route with no way to choose is a route of no alternative, whatever
  -- the others choose
  | any (null . snd) ways = []
  | otherwise = combine ways [] mempty mempty
  where
    ways = narrowed [(openCopies r, chosen r) | r <- routesOf chain]
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
    chosen :: [RSeg] -> [[[RSeg]]]
    chosen route' = case ordNub (openOn route') of
      []
        | gluesAlone ending route' -> [[route']]
        -- its node tests contradict each other: it is a route of no
        -- alternative
        | otherwise -> []
      open ->
        let forking = filter (any snd . sidesOf) open
            first' = if null forking then take 1 open else forking
         in distinctOn sort $
              [ way
              | name <- first',
                (i, (bodies, _)) <- zip [0 ..] (sidesOf name),
                -- every way of one is compared with every way of the
                -- next
                way <-
                  foldl'
                    together'
                    [[]]
                    [chosen (replaced name i body route') | body <- bodies]
              ]
    sidesOf name = findWithDefault [] name sideRoutes
    -- The ways of each route without those that choose, for a copy of a
    -- union that all its routes have to choose alike ('settles'), a side
    -- that some route through the copy has no way to choose: so a route
    -- that can only choose one side there is not found to be without a
    -- way after every way of the routes before it has been tried.
    narrowed ::
      [([(Int, [Choices])], [[[RSeg]]])] -> [([(Int, [Choices])], [[[RSeg]]])]
    narrowed each
      | all (null . fst) settling = each
      | otherwise =
          map (second (map fst)) $
            narrow [(c, s', ws) | ((c, _), (s', ws)) <- zip each settling]
      where
        -- for each route, the copies it has to choose as the others do,
        -- and its ways with the sides they choose
        settling =
          [ ( settles copies,
              [(way, foldMap sidesOn (take 1 way)) | way <- ws]
            )
          | (copies, ws) <- each
          ]
    narrow ::
      [([(Int, [Choices])], [(Int, [Choices])], [([[RSeg]], Map Int Int)])] ->
      [([(Int, [Choices])], [([[RSeg]], Map Int Int)])]
    narrow these =
      let possible :: Map (Int, [Choices]) (Set Int)
          possible =
            Map.fromListWith
              intersection
              [ (copy', setFromList [i | (_, picks') <- ws, Just i <- [lookup name picks']])
              | (_, settled, ws) <- these,
                copy'@(name, _) <- settled
              ]
          keeps settled (_, picks') =
            and
              [ i `member` findWithDefault mempty copy' possible
              | copy'@(name, _) <- settled,
                Just i <- [lookup name picks']
              ]
          these' =
            [ (copies, settled, if null settled then ws else filter (keeps settled) ws)
            | (copies, settled, ws) <- these
            ]
          sizes made = [length ws | (_, settled, ws) <- made, not (null settled)]
       in if sizes these' == sizes these
            then [(copies, ws) | (copies, _, ws) <- these]
            else narrow these'
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
      [[[Item Int]]]
    combine [] routes _ _ =
      let differing = differingPairs routes
          listed
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
       in [uncurry together (sharing joins differing listed)]
    combine ((copies, ways') : rest) routes bySide settled = go allowed
      where
        allowed =
          [ way
          | way <- ways',
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
        go [] = []
        go (way : more)
          | all fitsIn picked =
              combine
                rest
                (reverse way <> routes)
                ( foldl'
                    (\acc (r, picks') -> foldl' (\acc' k -> Map.insertWith (<>) k [r] acc') acc picks')
                    bySide
                    picked
                )
                (settledBy way)
                <> go more
          | otherwise = go more
          where
            picked = [(r, mapToList (sidesOn r)) | r <- way]
        fitsIn (route', picks') =
          all
            (gluesWith ending route')
            [ other
            | (name, i) <- picks',
              (i', _) <- zip [0 ..] (sidesOf name),
              i' /= i,
              other <- findWithDefault [] (name, i') bySide
            ]
    -- whether a segment is a side with a group that a route chose
    chooses = \case
      RPicked _ _ body -> any isGroup' body || any chooses body
      RGroup _ _ body -> any chooses body
      _ -> False
    isGroup' = \case
      RGroup {} -> True
      RPicked _ _ body -> any isGroup' body
      _ -> False

-- | How many pairs of routes chose different sides of some union.
differingPairs :: [[RSeg]] -> Int
differingPairs routes =
  sum
    [ length a * length b
    | (p, a) : rest <- tails (mapToList grouped),
      (q, b) <- rest,
      differ p q
    ]
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

-- * Copied meeting points

-- | A path that no command runs: a product would copy a meeting point
-- (docs/path-route-ir.md, Meeting points).
data CopiedMeetingPoint = CopiedMeetingPoint
  { -- | the conjunct in which several walks meet at a join or leave it
    meeting :: Path,
    -- | that join
    join :: Path,
    -- | the conjunct that would be copied: the one with the meeting
    -- point, or one it is written in
    copied :: Path,
    -- | the group the copied conjunct is a conjunct of
    group :: Path,
    -- | a group next to it in its stretch, which would copy it
    copier :: Path,
    -- | how the unions between the two are read for that
    readings :: [UnionReading],
    -- | the two groups, and what is between them, with a join put
    -- between them, if they are written in one chain and the path is
    -- then accepted
    joinedFirst :: Maybe Path,
    -- | the two groups, and what is between them, as their product
    -- written out, if that is short and means what the path compiles to
    writtenApart :: Maybe Path,
    -- | where the conjunct is one walk by itself: the conjunct of the
    -- copier that brings several walks to its join, or takes several
    -- from it
    across :: Maybe WalksAcross
  }
  deriving stock (Eq, Show)

-- | A conjunct that holds several walks together with a join of its own
-- (an @\@N@) and, written before another conjunct, brings them to that
-- one's join ('arriving'), or written after it, takes them from it.
data WalksAcross = WalksAcross {from :: Path, arriving :: Bool}
  deriving stock (Eq, Show)

-- | How a union is read for a group to be next to another: one of its
-- sides in its place (for a shared union, the side chosen), or, for a
-- relational union, as one step, which is what it is for two routes
-- that chose different sides of it. With the union is its place in the
-- path that was checked, which tells it from a union written the same
-- elsewhere: where unions of the same kind are written inside one
-- another, they are read as one union, and its place is the outermost
-- one's.
data UnionReading
  = SideInPlace {union :: Path, side :: Path, unionAt :: Place}
  | AsOneStep {union :: Path, unionAt :: Place}
  deriving stock (Eq, Show)

-- | An @\@@ that begins or ends a conjunct that is a plain walk (steps
-- and @\@@, nothing else) and that a product copies: it does nothing
-- there, and looks as if it pinned. Only this shape is reported, in
-- which the @\@@ can be left out whatever is written around the
-- conjunct ('idleEnds'); other joins that do nothing are not.
data IdleJoin = IdleJoin
  { -- | the conjunct
    conjunct :: Path,
    -- | whether the @\@@ begins the conjunct, and whether it ends it
    begins :: Bool,
    ends :: Bool,
    -- | a group that copies the conjunct
    copier :: Path,
    readings :: [UnionReading],
    -- | the conjunct's group and the copier with the copier written
    -- inside each conjunct, where the @\@@ then is one node for the
    -- copier's conjuncts, if they are written in one chain, the @\@@
    -- faces the copier, and the path is then accepted
    pinned :: Maybe Path
  }
  deriving stock (Eq, Show)

-- | Whether a product in the path would copy a meeting point, and if
-- none would, the joins that do nothing because a product copies them
-- ('IdleJoin').
--
-- The rule is about the product written out: a join other than an
-- @\@N@ that is written once has a copy for every term of the product
-- it comes to stand in, and a copy is a /meeting point/ when more than
-- one walk comes into it or goes out of it. A path is refused when some
-- such join has two copies or more of which one is a meeting point.
--
-- That is decided here without writing the product out. A join is
-- /copied/ when its conjunct, or a conjunct that one is written in, is
-- in a group with another group next to it in its stretch: nothing but
-- steps and unions between the two, no join of their chain (a conjunct
-- without a join of its own is no chain apart, so its groups are groups
-- of the stretch its own group is in). Several walks come into a copy,
-- or go out of it, when the conjunct has a group at its own level in
-- the stretch right before or after the join ('ownMeeting'), or when
-- the join is the first (last) of its conjunct and a conjunct of the
-- neighbouring group holds several walks together with a join of its
-- own and brings them along ('acrossList').
--
-- A union is read every way: as each of its sides written in its place
-- and, a relational union, as one step, which keeps no two groups apart
-- whatever joins its sides have (two routes that chose different sides
-- pass it as a step, 'sharedBy'). The path is refused when some way of
-- reading its unions copies a meeting point.
--
-- The path is read as it is written, with no union chosen and no route
-- listed: every group looks along its chain in both directions once
-- ('scanned'), so the work is at most the length of the path times its
-- depth for each group, whatever the path compiles to. What is not
-- noticed: that a side cannot be chosen because its node tests
-- contradict those around it (@%never@ and tests that contradict each
-- other right next to one another are noticed). Every side that is
-- written is a side of its own here as it is for the compiler, so the
-- check and the compiler read the same unions.
checkCopies :: Path -> Either CopiedMeetingPoint [IdleJoin]
checkCopies p = case scopeChain (pushBackwards (withoutLimits p)) of
  Nothing -> Right []
  Just chain -> case findings chain of
    (refusal : _, _) -> Left (refused placeOf chain refusal)
    ([], idle)
      -- no warning unless it is certain ('reordersRoutes')
      | reordersRoutes chain -> Right []
      | otherwise -> Right (map (idleJoin placeOf chain) idle)
  where
    places = unionPlaces p
    placeOf name =
      fromMaybe
        (error "checkCopies: a union that scopeChain names is a union of the path")
        (IntMap.lookup name places)

-- | The place in a path of each of its unions, by the name 'scopeChain'
-- gives it in the path without limits and with @~@ pushed down: from
-- where its leaves start there and how many it has. @~@ reverses the
-- order of what it is around.
unionPlaces :: Path -> IntMap Place
unionPlaces path = IntMap.fromList (go [] 0 False path)
  where
    total = leaves path
    go :: Place -> Int -> Bool -> Path -> [(Int, Place)]
    go place pos reversed = \case
      p@(Union kind l r) ->
        let name = pos * (total + 1) + leaves p
         in (if kind == Relational then name else negate name - 1, place)
              : inOrder l r
      Backwards q -> go (operand 0 place) pos (not reversed) q
      WithLimits _ _ q -> go (operand 0 place) pos reversed q
      l :/ r
        -- under ~ the second operand of a sequence comes first
        | reversed ->
            go (operand 0 place) (pos + leaves r) reversed l
              <> go (operand 1 place) pos reversed r
        | otherwise -> inOrder l r
      l :& r -> inOrder l r
      _ -> []
      where
        inOrder l r =
          go (operand 0 place) pos reversed l
            <> go (operand 1 place) (pos + leaves l) reversed r

-- | Whether a chain has, anywhere in it, a relational union with a join
-- in the own chain of a side (through the unions in that side, read in
-- place). Only then are the routes of an alternative put in another
-- order than the conjuncts they choose give ('together'), by the joins
-- they go through; and whether an @\@@ that joins nothing can change
-- that order is not known. So a path with such a union gets no warning
-- about an @\@@ that does nothing: a warning has to be certain.
reordersRoutes :: Chain -> Bool
reordersRoutes = any \case
  SGroup _ cs -> any reordersRoutes cs
  SUnion name sides ->
    (isRelational name && any canJoin sides) || any reordersRoutes sides
  _ -> False

-- | A path as the check reads it, and as 'CopiedMeetingPoint' and
-- 'IdleJoin' show its parts: without limits, @~@ pushed down to the
-- steps, nested groups as one group, node tests right after one another
-- as one. Nothing when the path has no chain.
asRead :: Path -> Maybe Path
asRead = fmap chainAsPath . scopeChain . pushBackwards . withoutLimits

-- | The chain of a path with no union chosen: a shared union is a union
-- too, with a negative name. Nothing when the path has no chain
-- (@%never@, or node tests right after one another that contradict).
scopeChain :: Path -> Maybe Chain
scopeChain path = go 0 path >>= settle
  where
    total = leaves path
    named pos p = pos * (total + 1) + leaves p
    go :: Int -> Path -> Maybe Chain
    go pos = \case
      One -> Just [SJoin pos anyNode False]
      Zero -> Nothing
      Wild -> Just [SStep pos Out MWild]
      Literal t -> Just [SStep pos Out (MLiteral t)]
      RegexMatch r -> Just [SStep pos Out (MRegex r)]
      Backwards Wild -> Just [SStep pos In MWild]
      Backwards (Literal t) -> Just [SStep pos In (MLiteral t)]
      Backwards (RegexMatch r) -> Just [SStep pos In (MRegex r)]
      Backwards _ -> error "scopeChain: pushBackwards leaves ~ only on steps"
      Absolute nid -> Just [SJoin pos (Exactly nid) True]
      ExcludingNIDs ns -> Just [SJoin pos (Except ns) False]
      p@(Union kind _ _) ->
        let name = case kind of
              Relational -> named pos p
              Shared -> negate (named pos p) - 1
         in oneOf name $
              mapMaybe (uncurry go) (placed pos (sidesOfKind kind p))
      p :& q -> do
        a <- go pos p
        b <- go (pos + leaves p) q
        Just [SGroup (named pos (p :& q)) (conjuncts a <> conjuncts b)]
      WithLimits _ _ p -> go pos p
      p :/ q -> do
        a <- go pos p
        b <- go (pos + leaves p) q
        sequenced a b
    sidesOfKind kind = \case
      Union kind' p q | kind' == kind -> sidesOfKind kind p <> sidesOfKind kind q
      WithLimits _ _ p@(Union kind' _ _) | kind' == kind -> sidesOfKind kind p
      p -> [p]
    -- every side that is written is a side of its own, as for the
    -- compiler ('norm'): a union of two sides written the same is also
    -- read as one step
    oneOf name sides = case sides of
      [] -> Nothing
      [c] -> Just c
      cs -> Just [SUnion name cs]
    settle c = do
      c' <- tidy c
      if c' == c then Just c else settle c'
    tidy :: Chain -> Maybe Chain
    tidy chain = do
      parts <- for chain \case
        SGroup name cs -> do
          cs' <- traverse tidy cs
          pure [SGroup name (concatMap conjuncts cs')]
        SUnion name cs -> oneOf name (mapMaybe tidy cs)
        seg -> pure [seg]
      foldM sequenced [] parts

-- | Whether a union of a 'scopeChain' is a relational one: two routes
-- may choose different sides of it, and pass it as one step.
isRelational :: Int -> Bool
isRelational name = name >= 0

-- | What is found looking along a chain from a group, in one direction,
-- up to the end of the chain: a group in the same stretch, in some
-- reading of the unions on the way; and whether the end of the chain
-- can be reached without a join of the chain, in some reading.
data Scan = Scan {found :: Maybe Copier, reach :: Bool}

-- | A group that copies, and how the unions on the way to it are read.
data Copier = Copier {group :: Seg, how :: [(Seg, Maybe Chain)]}

-- | Looking from the right end of a chain to the left, and beyond it.
scannedLeft :: Chain -> Scan -> Scan
scannedLeft chain beyond = foldl' (flip (scanned scannedLeft)) beyond chain

-- | Looking from the left end of a chain to the right, and beyond it.
scannedRight :: Chain -> Scan -> Scan
scannedRight chain beyond = foldr (scanned scannedRight) beyond chain

-- | What is found from a segment on, given what is found beyond it: a
-- step is passed, a join ends the stretch, a group is found. A union is
-- each of its sides in place: a group may be found in a side, and a
-- side without a join is passed; and a relational union is passed as a
-- step.
scanned :: (Chain -> Scan -> Scan) -> Seg -> Scan -> Scan
scanned along seg beyond = case seg of
  SStep {} -> beyond
  SJoin {} -> Scan Nothing False
  SGroup {} -> Scan (Just (Copier seg [])) beyond.reach
  SUnion name sides ->
    let inside = [(side', along side' (Scan Nothing True)) | side' <- sides]
        passing =
          take 1 $
            [Just side' | (side', scan) <- inside, scan.reach]
              <> [Nothing | isRelational name]
        via reading c = Copier c.group ((seg, reading) : c.how)
     in Scan
          ( asum [via (Just side') <$> scan.found | (side', scan) <- inside]
              <|> asum [via reading <$> beyond.found | reading <- passing]
          )
          (not (null passing) && beyond.reach)

pinsNode :: NodeTest -> Bool
pinsNode = \case
  Exactly _ -> True
  Except _ -> False

-- | A join of a conjunct's own level that is no @\@N@ and has a group of
-- that level in the stretch right before it or right after it (no
-- other join of the conjunct between the two), in some reading of the
-- conjunct's unions: several walks of the conjunct meet at it, or
-- leave it.
ownMeeting :: Chain -> Maybe Seg
ownMeeting = asum . map (\(_, _, found) -> found) . toList . foldl' along start
  where
    -- for each reading so far: whether a group is in the stretch since
    -- the last join; the last join, if it is no @N and nothing but
    -- steps came after it; and a join that is found
    start :: Set (Bool, Maybe Seg, Maybe Seg)
    start = singletonSet (False, Nothing, Nothing)
    along :: Set (Bool, Maybe Seg, Maybe Seg) -> Seg -> Set (Bool, Maybe Seg, Maybe Seg)
    along states seg = case seg of
      SStep {} -> states
      SJoin _ test _ ->
        flip mapSet states \(grouped, _, found) ->
          let open = not (pinsNode test)
           in ( False,
                seg <$ guard open,
                found <|> (seg <$ guard (open && grouped))
              )
      SGroup {} ->
        flip mapSet states \(_, waiting, found) ->
          (True, waiting, found <|> waiting)
      SUnion name sides ->
        foldMap (foldl' along states) sides
          <> (if isRelational name then states else mempty)

-- | Whether a conjunct is a plain walk that begins with an @\@@, and
-- whether it is one that ends with an @\@@: a walk of steps and @\@@
-- and nothing else, with a step in it.
--
-- This is what 'IdleJoin' warns about, and it is narrow on purpose: the
-- warning says that the @\@@ may be left out, and that has to be true
-- whatever is written around the conjunct. In a plain walk it is. The
-- path is one the check accepts, so no copy of the @\@@ has more than
-- one walk coming into it or going out of it; and the steps of the
-- conjunct are named by the choices of its stretch alike with the @\@@
-- and without it ('annotate': a conjunct without a group brings no
-- choice of its own, whether it is a chain apart or not), so the routes
-- share what they shared. What an @\@@ can do besides is left out by
-- the shape:
--
-- * an @\@@ before an @\@N@ makes it a test of the node the route is
--   at, where it would be a root ('norm'): so no @\@N@ and no union,
--   whose sides may begin with one, as in @(\@/(x | \@1/b) & a)/(c & d)@;
-- * a conjunct that is only @\@@ is a join that cannot be left out;
-- * a conjunct with a group, or with a union that has one in a side, is
--   copied whole with the join and falls apart without it.
--
-- A @!{…}@ is a test as well as a join, so a walk with one is left
-- alone too.
idleEnds :: Chain -> (Bool, Bool)
idleEnds chain
  | all plain chain && any isStep chain =
      (any isJoin (take 1 chain), any isJoin (take 1 (reverse chain)))
  | otherwise = (False, False)
  where
    plain = \case
      SStep {} -> True
      SJoin _ test _ -> test == anyNode
      _ -> False
    isStep = \case
      SStep {} -> True
      _ -> False

-- | What is found looking from a group along its chain for walks that
-- a conjunct of a group next to it holds together ('acrossList'), at one
-- depth: a group that is left in the stretch; one with a join of the
-- chain looked along beyond it; one with no such join beyond it; and
-- whether the chain can be passed without a join.
data Across = Across
  { found :: Maybe Delivery,
    foundJoined :: Maybe Delivery,
    foundPassed :: Maybe Delivery,
    pass :: Bool
  }

-- | Walks that one conjunct holds together next to another conjunct's
-- join: the group that conjunct is chosen in, the conjunct, and how the
-- unions on the way are read.
data Delivery = Delivery
  { outer :: Maybe Seg,
    deliverer :: Maybe Chain,
    sidesRead :: [(Seg, Maybe Chain)]
  }

-- | A chain as it is looked along: as written to the right, and
-- backwards to the left.
type Facing = Chain -> Chain

-- | What is found along a chain (given in the order it is looked along),
-- for every depth from -1 on.
--
-- This follows what the product makes of a stretch ('annotate'): each
-- group is replaced by the conjunct a route chooses, a conjunct without
-- a join of its own with its segments in the stretch, and one with a
-- join written whole in its place, where its joins cut the chain and
-- the groups at its own level are left as groups, to be chosen in turn.
-- A join that a chosen conjunct brings has several walks arriving (or
-- leaving) in one copy exactly when such a group is left in the
-- stretch before (after) it. So, looking from the group of the join's
-- conjunct, at depth 0: a step is passed; a join ends the stretch; of a
-- group, a conjunct without a join is looked through at the same depth,
-- and one with a join is looked into from its facing end one depth
-- lower; and a group met below depth 0 is left in the stretch: found.
-- (Leaving a conjunct with a join through its end, the depth is one
-- higher: the groups out there are chosen one round earlier.)
acrossList :: Facing -> Chain -> [Across]
acrossList facing = (\(found', _, _) -> found') . foldr before' (nothing, False, True)
  where
    nothing = repeat (Across Nothing Nothing Nothing True)
    -- what is found from a segment on, with whether what is after it
    -- has a join of the chain, and whether it can be passed without one
    before' seg (rest, joins, passes) =
      ( zipWith both (acrossSeg facing seg) rest,
        joins || canJoin [seg],
        passes && passable [seg]
      )
      where
        both a b =
          Across
            { found = a.found <|> (guard a.pass *> b.found),
              foundJoined =
                a.foundJoined
                  <|> (guard joins *> a.found)
                  <|> (guard a.pass *> b.foundJoined),
              foundPassed =
                (guard passes *> a.foundPassed)
                  <|> (guard a.pass *> b.foundPassed),
              pass = a.pass && b.pass
            }

acrossSeg :: Facing -> Seg -> [Across]
acrossSeg facing seg = case seg of
  SStep {} -> repeat (Across Nothing Nothing Nothing True)
  SJoin {} -> repeat (Across Nothing Nothing Nothing False)
  SUnion name sides ->
    foldr
      (zipWith either')
      (repeat (Across Nothing Nothing Nothing (isRelational name)))
      [ map (via side') (acrossList facing (facing side'))
      | side' <- sides
      ]
    where
      either' a b =
        Across
          (a.found <|> b.found)
          (a.foundJoined <|> b.foundJoined)
          (a.foundPassed <|> b.foundPassed)
          (a.pass || b.pass)
      via side' a =
        let read' = fmap \w -> w {sidesRead = (seg, Just side') : w.sidesRead}
         in Across (read' a.found) (read' a.foundJoined) (read' a.foundPassed) a.pass
  SGroup _ cs ->
    (Across here Nothing here True :) . map finish . foldr (zipWith either') (repeat (Nothing, False)) $
      [ zipWith
          ( \lower same ->
              ( (lower.foundJoined <&> \w -> w {deliverer = w.deliverer <|> Just c})
                  <|> same.foundPassed,
                same.pass
              )
          )
          along
          (drop 1 along)
      | c <- cs,
        let along = acrossList facing (facing c)
      ]
    where
      finish (x, p) =
        let x' = x <&> \w -> w {outer = Just seg}
         in Across x' Nothing x' p
      here = Just (Delivery (Just seg) Nothing [])
      either' (a, p) (b, q) = (a <|> b, p || q)

-- | Whether a chain has a join of its own, in some reading of its
-- unions.
canJoin :: Chain -> Bool
canJoin = any \case
  SJoin {} -> True
  SUnion _ sides -> any canJoin sides
  _ -> False

-- | Whether a chain has no join of its own, in some reading of its
-- unions.
passable :: Chain -> Bool
passable = all \case
  SJoin {} -> False
  SUnion name sides -> isRelational name || any passable sides
  _ -> True

-- | Whether the first join of its own that a conjunct has, looked along,
-- is no @\@N@, in some reading of its unions; and that join.
firstOpen :: Facing -> Chain -> Maybe Seg
firstOpen facing = fst . go . facing
  where
    go :: Chain -> (Maybe Seg, Bool)
    go [] = (Nothing, True)
    go (seg : rest) =
      let (open, pass) = case seg of
            SJoin _ test _ -> (seg <$ guard (not (pinsNode test)), False)
            SUnion name sides ->
              let each = map (go . facing) sides
               in (asum (map fst each), isRelational name || any snd each)
            _ -> (Nothing, True)
          (open', pass') = go rest
       in (open <|> (guard pass *> open'), pass && pass')

-- | A join that is a meeting point in a copy, or a conjunct that is one
-- walk with an @\@@ at an end that is copied.
data Finding = Finding
  { -- | the conjunct that is copied, and its group
    group :: Seg,
    copied :: Chain,
    copier :: Copier,
    -- | the conjunct with the join: the copied one, or one inside it
    conjunct :: Chain,
    -- | the join that is a meeting point in a copy
    at :: Maybe Seg,
    -- | where the walks that meet at it or leave it are held together
    -- by a conjunct of the copier: that conjunct, and whether they
    -- arrive
    across :: Maybe (Chain, Bool)
  }

-- | What is found beyond the ends of a chain looking for walks held
-- together ('acrossList'): at a depth, and given whether the chain has
-- a join, and whether it has none, on the side that is not looked
-- along, in some reading.
data Beyond = Beyond
  { before :: Int -> (Bool, Bool) -> Maybe Delivery,
    after :: Int -> (Bool, Bool) -> Maybe Delivery
  }

-- | The conjuncts that a product copies and must not (those with a
-- meeting point, and those with a join that a conjunct of the copier
-- brings several walks to or takes several from), and the conjuncts
-- that are a plain walk with an @\@@ at an end ('idleEnds') and that a
-- product copies.
findings :: Chain -> ([Finding], [Finding])
findings =
  walk
    (Scan Nothing True)
    (Scan Nothing True)
    Nothing
    Nothing
    (Beyond (\_ _ -> Nothing) (\_ _ -> Nothing))
  where
    -- a chain, with what is found beyond its ends, and the copier of
    -- the group it is a conjunct of
    -- (and the conjunct the chain is in that is copied, if one is: all
    -- that is in it is copied with it)
    walk ::
      Scan ->
      Scan ->
      Maybe Copier ->
      Maybe (Seg, Chain, Copier) ->
      Beyond ->
      Chain ->
      ([Finding], [Finding])
    walk left right above around' beyond chain =
      mconcat
        [ case seg of
            SGroup _ cs ->
              let copier' =
                    l.found
                      <|> r.found
                      -- a conjunct without a join is no chain apart: its
                      -- groups are groups of the stretch its group is in
                      <|> (guard (l.reach && r.reach) *> above)
                  -- inside a conjunct with a join the groups around are
                  -- chosen a round earlier; inside one without, in the
                  -- same round
                  inside =
                    Beyond
                      ( \d (joins, passes) ->
                          (guard joins *> arriving (d + 1))
                            <|> (guard passes *> arriving d)
                      )
                      ( \d (joins, passes) ->
                          (guard joins *> leaving (d + 1))
                            <|> (guard passes *> leaving d)
                      )
                  held arrives w k j =
                    Finding
                      seg
                      k
                      (Copier (fromMaybe seg w.outer) w.sidesRead)
                      k
                      (Just j)
                      (Just (fromMaybe [] w.deliverer, arrives))
                  -- the copy this conjunct is in: its own, or that of a
                  -- conjunct around it
                  copy k = (copier' <&> (seg,k,)) <|> around'
               in mconcat
                    [ ( [ Finding g k' c k (Just j) Nothing
                        | j <- toList (ownMeeting k),
                          (g, k', c) <- toList (copy k)
                        ]
                          <> [ held True w k j
                             | j <- toList (firstOpen id k),
                               w <- toList (arriving 0)
                             ]
                          <> [ held False w k j
                             | j <- toList (firstOpen reverse k),
                               w <- toList (leaving 0)
                             ],
                        [ Finding seg k c k Nothing Nothing
                        | c <- toList copier',
                          let (b, e) = idleEnds k,
                          b || e
                        ]
                      )
                        <> walk
                          (Scan Nothing True)
                          (Scan Nothing True)
                          copier'
                          (around' <|> (copier' <&> (seg,k,)))
                          inside
                          k
                    | k <- cs
                    ]
            SUnion _ sides ->
              foldMap
                ( walk
                    l
                    r
                    above
                    around'
                    ( Beyond
                        ( \d (joins, passes) ->
                            lookFrom
                              reverse
                              earlier
                              d
                              beyond.before
                              (joins || canJoin later, passes && passable later)
                        )
                        ( \d (joins, passes) ->
                            lookFrom
                              id
                              later
                              d
                              beyond.after
                              (joins || canJoin earlier, passes && passable earlier)
                        )
                    )
                )
                sides
            _ -> mempty
        | (i, (l, seg, r)) <-
            zip [0 ..] $
              zip3
                (scanl (flip (scanned scannedLeft)) left chain)
                chain
                (drop 1 (scanr (scanned scannedRight) right chain)),
          let earlier = take i chain
              later = drop (i + 1) chain
              arriving d =
                lookFrom reverse earlier d beyond.before (canJoin later, passable later)
              leaving d =
                lookFrom id later d beyond.after (canJoin earlier, passable earlier)
        ]
    -- looking along part of a chain at a depth, and beyond its end
    lookFrom ::
      Facing ->
      Chain ->
      Int ->
      (Int -> (Bool, Bool) -> Maybe Delivery) ->
      (Bool, Bool) ->
      Maybe Delivery
    lookFrom facing part d out other =
      case drop (d + 1) (acrossList facing (facing part)) of
        a : _ -> a.found <|> (guard a.pass *> out d other)
        [] -> Nothing

-- | A chain as a path.
chainAsPath :: Chain -> Path
chainAsPath = \case
  [] -> One
  seg : rest -> foldl' (:/) (segAsPath seg) (map segAsPath rest)

segAsPath :: Seg -> Path
segAsPath = \case
  SStep _ d m ->
    (case d of Out -> id; In -> Backwards) case m of
      MLiteral t -> Literal t
      MWild -> Wild
      MRegex r -> RegexMatch r
  SJoin _ test _ -> case test of
    Exactly n -> Absolute n
    Except ns
      | null ns -> One
      | otherwise -> ExcludingNIDs ns
  SGroup _ cs -> foldl1Ex' (:&) (map chainAsPath cs)
  SUnion name cs ->
    foldl1Ex'
      (Union (if isRelational name then Relational else Shared))
      (map chainAsPath cs)

-- | How the unions between a group and its copier are read, each with
-- its place in the path, given by its name.
readingsOf :: (Int -> Place) -> Copier -> [UnionReading]
readingsOf placeOf c =
  [ case reading of
      Just side' -> SideInPlace (segAsPath u) (chainAsPath side') place
      Nothing -> AsOneStep (segAsPath u) place
  | (u, reading) <- c.how,
    let place = case u of
          SUnion name _ -> placeOf name
          _ -> error "readingsOf: a copier is read through unions only"
  ]

-- | The chain in which a group and its copier stand, rewritten: the
-- two (or, where one of them is written inside a conjunct of a group of
-- that chain, that group) and what is between them are replaced by what
-- is made of the earlier of the two, what is between, and the later.
rewritten ::
  Seg ->
  Seg ->
  (Seg -> Chain -> Seg -> Maybe Chain) ->
  Chain ->
  Maybe (Chain, Chain)
rewritten g c make = go
  where
    go :: Chain -> Maybe (Chain, Chain)
    go chain = case break (\seg -> holds g seg || holds c seg) chain of
      (before, first' : rest)
        | (between, last' : after) <-
            break (\seg -> holds g seg || holds c seg) rest,
          holds g first' /= holds g last' || holds c first' /= holds c last' -> do
            made <- make first' between last'
            Just (before <> made <> after, made)
      _ -> asum (map inside (holes chain))
    -- whether a segment is a group, or has it in a conjunct of a group
    holds x seg =
      seg == x || case seg of
        SGroup _ cs -> any (any (holds x)) cs
        _ -> False
    inside (before, seg, after) = case seg of
      SGroup name cs ->
        asum
          [ go k <&> \(k', made) -> (before <> [SGroup name (ks <> [k'] <> ks')] <> after, made)
          | (ks, k, ks') <- holes cs
          ]
      SUnion name cs ->
        asum
          [ go k <&> \(k', made) -> (before <> [SUnion name (ks <> [k'] <> ks')] <> after, made)
          | (ks, k, ks') <- holes cs
          ]
      _ -> Nothing
    holes :: [a] -> [([a], a, [a])]
    holes xs = [(take i xs, x, drop (i + 1) xs) | (i, x) <- zip [0 ..] xs]

refused :: (Int -> Place) -> Chain -> Finding -> CopiedMeetingPoint
refused placeOf chain f =
  CopiedMeetingPoint
    { meeting = chainAsPath f.conjunct,
      join = maybe One segAsPath f.at,
      copied = chainAsPath f.copied,
      group = segAsPath f.group,
      copier = segAsPath f.copier.group,
      readings = readingsOf placeOf f.copier,
      across =
        f.across <&> \(k, arrives) -> WalksAcross (chainAsPath k) arrives,
      joinedFirst = do
        guard (null f.copier.how)
        (whole, made) <-
          rewritten f.group f.copier.group (\a between b -> Just ([a, SJoin (-1) anyNode False] <> between <> [b])) chain
        guard (null (fst (findings whole)))
        Just (chainAsPath made),
      writtenApart = do
        guard (null f.copier.how)
        (whole, made) <- rewritten f.group f.copier.group apart chain
        guard (null (fst (findings whole)))
        -- only where it is cheap to see that it means the same: a path
        -- without unions, with few routes
        guard (null (unionsIn chain))
        guard (snd (routesSize chain) <= 2000 && snd (routesSize whole) <= 2000)
        guard (compilePath (chainAsPath whole) == compilePath (chainAsPath chain))
        Just (chainAsPath made)
    }
  where
    apart a between b = case (a, b) of
      (SGroup _ as, SGroup _ bs)
        | length as * length bs <= 9 ->
            Just [SGroup (-2) [x <> between <> y | x <- as, y <- bs]]
      _ -> Nothing

idleJoin :: (Int -> Place) -> Chain -> Finding -> IdleJoin
idleJoin placeOf chain f =
  IdleJoin
    { conjunct = chainAsPath f.conjunct,
      begins = b,
      ends = e,
      copier = segAsPath f.copier.group,
      readings = readingsOf placeOf f.copier,
      pinned = do
        guard (null f.copier.how)
        (whole, made) <- rewritten f.group f.copier.group inside chain
        let (bad, idle) = findings whole
        guard (null bad && length idle < length (snd (findings chain)))
        Just (chainAsPath made)
    }
  where
    (b, e) = idleEnds f.conjunct
    -- the copier written inside each conjunct of the group, on the side
    -- the @ faces
    inside first' between last'
      | first' == f.group,
        e,
        SGroup name cs <- first' =
          Just [SGroup name [k <> between <> [last'] | k <- cs]]
      | last' == f.group,
        b,
        SGroup name cs <- last' =
          Just [SGroup name [[first'] <> between <> k | k <- cs]]
      | otherwise = Nothing

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
