-- | The laws of the path language (docs/path-language.md, section 4, and
-- what 2.9 and 2.10 state as laws; the table of docs/path-route-ir.md,
-- "Laws and the tests that pin them", says which test is for which),
-- each pinned three ways:
--
-- 1. a property on random paths inside the condition the document
--    states, compared at the level the law claims: what 'compilePath'
--    gives exactly, that up to the order of the routes of an alternative
--    ('alternativesUpToOrder'), or the nodes @ls@ finds from every node
--    of some graphs ('Tally');
-- 2. every counterexample the document gives outside the condition, as a
--    fixed test in which the two paths differ;
-- 3. for every clause of the condition, the same property with that
--    clause dropped: the same generator, seed and number of cases, and
--    it has to fail in the first half of them ('needsEveryClause'), by
--    a pair of paths that commands run and for which @ls@ finds
--    different nodes ('firstToldApart'). A property that would pass
--    without a clause says nothing about it, however many pairs differ
--    somewhere outside the condition. (Two clauses of 4.10 are not
--    dropped, because the property passes without them: see there.)
--
-- The generators are aimed at the clauses, so that a dropped clause
-- shows in some twenty cases of a sample, and no test here passes by
-- the luck of its seed: on 2026-10-11 all of them passed with every
-- seed shifted by 1000, 2000, 3000, 4000, 5000, 7000 and 9000.
-- Cases aimed at a clause are mostly small paths. So the properties of
-- 4.7, 4.8, 4.10 and 2.9 go through cases with nothing aimed as well
-- ('unaimedLeft', 'unaimedRight', 'unaimedTwice', 'unaimedBounded'):
-- larger and more varied paths within the condition, which only the
-- property under the whole condition is tried on.
--
-- The conditions are read off the path as it is written ('Part'), by
-- code of its own: of the compiler it uses only 'pushBackwards', which
-- applies every @~@, so that a chain is read in the order it is walked
-- (and 'runs', the check of 3.1, where a test asks whether commands run
-- a path). A property about what
-- @ls@ finds counts how many of its comparisons found a node on either
-- side, and fails when that share is small: two paths that both find
-- nothing agree whatever the law is.
--
-- Like "Models.PathPattern.CompileSpec", this is about what the compiler
-- makes of a path, also of one that no command runs ('runs'); where a
-- law is only about paths that are run, the test says so.
module Models.PathPattern.LawsSpec where

import Graph.PathPattern.Resolve
import Models.Graph (Graph)
import Models.NID
import Models.Path.Simple qualified as P
import Models.Path.TH (path)
import Models.PathPattern
import Models.PathPattern.Compile
import Models.PathPattern.CompileSpec
  ( alternativesUpToOrder,
    graphOf,
    resolveFrom,
    runs,
  )
import Models.PathPattern.Match
import Models.PathPattern.Suggest (renderSurfacePath)
import Models.PathPattern.TestCorpus (fewAlternatives)
import MyPrelude
import Test.QuickCheck.Gen (unGen)
import Test.QuickCheck.Random (mkQCGen)
import Utils.Testing

-- * Graphs, and what @ls@ finds on them

-- | A graph, as its edges and as the graph.
data World = World {edges :: [(Int, Text, Int)], graph :: Graph Text ()}

worldNodes :: [Int]
worldNodes = [0 .. 3]

worldLabels :: [Text]
worldLabels = ["a", "b", "c"]

-- | A graph on the nodes 0 to 3 with two to five tenths of the 48 edges
-- it can have with the labels @a@, @b@ and @c@, and every label on some
-- edge.
genWorld :: Gen World
genWorld = do
  tenths <- elements [2, 3, 3, 4, 5 :: Int]
  kept <-
    filterM
      (const ((<= tenths) <$> choose (1, 10)))
      [(s, l, t) | s <- worldNodes, l <- worldLabels, t <- worldNodes]
  extra <-
    for [l | l <- worldLabels, l `notElem` [l' | (_, l', _) <- kept]] \l ->
      (\s t -> (s, l, t)) <$> elements worldNodes <*> elements worldNodes
  pure (worldOf (kept <> extra))

worldOf :: [(Int, Text, Int)] -> World
worldOf es =
  World es (graphOf (ordNub (worldNodes <> concat [[s, t] | (s, _, t) <- es])) es)

-- | The random graphs of every property here, the same at every run.
worlds :: [World]
worlds = drawn 20261101 60 genWorld

-- | The random graphs the pair at a place in a list of cases is compared
-- on.
worldsFor :: Int -> [World]
worldsFor i = take 2 (drop (2 * i `mod` length worlds) (worlds <> worlds))

-- | A path as a query: @ls@ appends @/\@@ (docs/path-language.md, 2.10).
asQuery :: P.Path -> PathQuery
asQuery p = compilePath (p P.:/ P.One)

-- | The nodes @at \@cur ls p@ finds.
finds :: World -> Int -> PathQuery -> Set NID
finds w cur q = foldMap targets (resolveFrom (smallNID cur) w.graph q).resolutions

-- | The same on a graph given by its edges, as numbers.
lsOn :: [(Int, Text, Int)] -> Int -> P.Path -> [Int]
lsOn es cur p = numbers (finds (worldOf es) cur (asQuery p))

-- | What @ls@ finds from node 0 of a graph for each of some paths.
lsFinds :: [(Int, Text, Int)] -> [(P.Path, [Int])] -> Expectation
lsFinds es expected =
  [(shown p, lsOn es 0 p) | (p, _) <- expected]
    `shouldBe` [(shown p, found) | (p, found) <- expected]

shown :: P.Path -> String
shown = unpack . renderSurfacePath

-- | A graph as the documents and the golden tests write it.
shownWorld :: World -> String
shownWorld w =
  intercalate ", " [show s <> " -" <> unpack l <> "-> " <> show t | (s, l, t) <- w.edges]

-- | The graph one alternative of a query describes: a node for every
-- vertex of the pattern and an edge for every hop, and the node the
-- query is run at. The alternative finds a node there, and a second
-- query that finds the same from every node of every graph has to find
-- it too; of all graphs this is the one on which it is least likely to
-- (a query finds what another does on every graph exactly when it does
-- on the graphs the other's alternatives describe, node tests aside). So
-- two queries that differ are told apart here far more often than on a
-- random graph.
--
-- Variables are the nodes from 10 on, @\@N@ is node @N@ and the current
-- node is 0. The variants make the graphs that node tests need: in
-- variant 1 the current node is 1, and in variant @n + 2@ the variable
-- @n@ is node 1, where the tests allow it. A @*@ or a regex is an edge
-- with one of the labels of 'worldLabels' that it accepts, another one
-- in each variant.
modelOf :: Int -> PatternPath -> Maybe (World, Int)
modelOf which p = do
  cur <- find (accepts Current) (if which == 1 then [1, 0, 2] else [0, 2, 1])
  let nodeOf = \case
        Current -> cur
        Fixed n -> fromMaybe 99 (smallNIDIndex n)
        v@(Var (VarId i))
          | which == i + 2 && accepts v 1 -> 1
          | otherwise -> 10 + i
      labelOf (EdgeId e) = \case
        MLiteral t -> t
        m ->
          fromMaybe "a" . headMay $
            [ l
            | l <- drop ((e + which) `mod` 3) (worldLabels <> worldLabels),
              matcherAccepts m l
            ]
      es =
        ordNub
          [ case spec.direction of
              Out -> (nodeOf spec.source, labelOf e spec.matcher, nodeOf spec.target)
              In -> (nodeOf spec.target, labelOf e spec.matcher, nodeOf spec.source)
          | (e, spec) <- mapToList (patternEdges p)
          ]
      ns = ordNub (cur : map nodeOf (patternVertices p))
  pure (World es (graphOf ns es), cur)
  where
    accepts v n = testAccepts (vertexTest p v) (smallNID n)

-- | The graphs some alternatives of a query describe: up to four of
-- them, spread over the alternatives, and the variants of the first.
modelsOf :: PathQuery -> [(World, Int)]
modelsOf q =
  catMaybes $
    [modelOf 0 a | a <- chosen]
      <> [modelOf which a | a <- take 1 chosen, which <- [1 .. 3]]
  where
    chosen = spread 4 (toList q.alternatives)

spread :: Int -> [a] -> [a]
spread n xs
  | length xs <= n = xs
  | otherwise = mapMaybe (\i -> index xs (i * length xs `div` n)) [0 .. n - 1]

-- | A bound on the matches a query has on a graph from a node, which is
-- what resolving it costs: for each alternative, the walks its routes
-- have that end at one node, multiplied over the routes. (The resolver
-- lists every match, and a path of a few steps with one label has
-- millions on a graph in which that label is on many edges.) No node
-- test is looked at, so this is never too low.
matchesBound :: World -> Int -> PathQuery -> Integer
matchesBound w cur q = sum (map ofAlternative (toList q.alternatives))
  where
    ofAlternative a =
      let perRoute = map walks (toList a.routes)
          ends = ordNub (map routeEnd (toList a.routes))
       in case ends of
            [_] -> sum (meeting perRoute)
            _ -> product (map sum perRoute)
    meeting = \case
      [] -> mempty :: Map Int Integer
      m : more -> foldl' (intersectionWithMap (*)) m more
    walks :: Route -> Map Int Integer
    walks r = foldl' along (singletonMap (start r.root) 1) r.hops
    start = \case
      Fixed n -> fromMaybe 99 (smallNIDIndex n)
      _ -> cur
    along at' h =
      foldl' (unionWith (+)) (mempty :: Map Int Integer) $
        [ singletonMap (if h.direction == Out then t else s) n
        | (s, l, t) <- w.edges,
          matcherAccepts h.matcher l,
          Just n <- [lookup (if h.direction == Out then s else t) at']
        ]

-- | The most matches a query may have on a graph for a comparison there
-- to be made ('matchesBound').
mostMatches :: Integer
mostMatches = 8000

-- | What comparing pairs of paths by what @ls@ finds came to. A
-- comparison is a pair, on a graph, from a node: on the random graphs
-- from every node, and on the graphs the alternatives of the two paths
-- describe ('modelOf') from the node the query is run at there.
data Tally = Tally
  { -- | comparisons on random graphs, and how many of them found a node
    -- on either side
    comparisons :: Int,
    withTarget :: Int,
    -- | the same on the graphs the paths describe
    onModels :: Int,
    modelsWithTarget :: Int,
    -- | comparisons left out because they would take too long
    -- ('matchesBound')
    leftOut :: Int,
    -- | the comparisons that differed, each written so that it can be
    -- run by hand
    apart :: [String]
  }

instance Semigroup Tally where
  Tally a b c d e f <> Tally a' b' c' d' e' f' =
    Tally (a + a') (b + b') (c + c') (d + d') (e + e') (f <> f')

instance Monoid Tally where
  mempty = Tally 0 0 0 0 0 []

-- | A pair of paths compared on its graphs; the number says which random
-- graphs.
compareOn :: Int -> (P.Path, P.Path) -> Tally
compareOn i (l, r) =
  mconcat $
    [one False w cur | w <- worldsFor i, cur <- worldNodes]
      <> [one True w cur | (w, cur) <- modelsOf ql <> modelsOf qr]
  where
    ql = asQuery l
    qr = asQuery r
    one model w cur
      | matchesBound w cur ql > mostMatches || matchesBound w cur qr > mostMatches =
          mempty {leftOut = 1}
      | otherwise =
          let fl = finds w cur ql
              fr = finds w cur qr
              found = if null fl && null fr then 0 else 1
              differing =
                [ unlines
                    [ "graph: " <> shownWorld w,
                      "$ at " <> show (smallNID cur) <> " ls " <> shown l,
                      "  finds the nodes " <> show (numbers fl),
                      "$ at " <> show (smallNID cur) <> " ls " <> shown r,
                      "  finds the nodes " <> show (numbers fr),
                      "(nodes by their numbers as in the graph, which are",
                      "smallNIDs: @" <> show cur <> " is " <> show (smallNID cur) <> ")"
                    ]
                | fl /= fr
                ]
           in if model
                then mempty {onModels = 1, modelsWithTarget = found, apart = differing}
                else mempty {comparisons = 1, withTarget = found, apart = differing}

numbers :: Set NID -> [Int]
numbers = sort . mapMaybe smallNIDIndex . toList

tally :: [(P.Path, P.Path)] -> Tally
tally cases = mconcat [compareOn i c | (i, c) <- zip [0 ..] cases]

-- | No pair differs; at least so many percent of the comparisons on
-- random graphs found a node; nearly every graph that a path describes
-- has a node for it to find; and few comparisons were left out.
agrees :: Int -> Tally -> Expectation
agrees percent t = do
  case t.apart of
    [] -> pure ()
    first' : more ->
      expectationFailure
        (first' <> "(and " <> show (length more) <> " more comparisons differ)")
  ("on random graphs" :: String, t.comparisons, t.withTarget)
    `shouldSatisfy` \(_, n, found) -> 100 * found >= percent * n
  ("on the graphs the paths describe" :: String, t.onModels, t.modelsWithTarget)
    `shouldSatisfy` \(_, n, found) -> 100 * found >= 95 * n
  ("left out" :: String, t.leftOut, t.comparisons + t.onModels)
    `shouldSatisfy` \(_, out', n) -> 5 * out' <= n

-- | The place, counted from 1, of the first pair that @ls@ tells apart.
firstApart :: [(P.Path, P.Path)] -> Maybe Int
firstApart cases =
  listToMaybe [i + 1 | (i, c) <- zip [0 ..] cases, not (null (compareOn i c).apart)]

-- | The place, counted from 1, of the first pair that a command tells
-- apart: commands run both paths, and @ls@ finds different nodes for
-- them on one of the graphs. (Two paths that compile to different
-- routes need not find different nodes: a join that is one thing in the
-- one and two in the other does not show at the very end of a query,
-- where @ls@ joins the walks anyway.)
firstToldApart :: [(P.Path, P.Path)] -> Maybe Int
firstToldApart cases =
  listToMaybe
    [ i + 1
    | (i, c@(l, r)) <- zip [0 ..] cases,
      runs l,
      runs r,
      not (null (compareOn i c).apart)
    ]

-- | The place, counted from 1, of the first pair that are not the same.
firstOther :: ((P.Path, P.Path) -> Bool) -> [(P.Path, P.Path)] -> Maybe Int
firstOther same cases = listToMaybe [i | (i, c) <- zip [1 ..] cases, not (same c)]

-- | Every pair is the same, at the level named.
allSame :: String -> ((P.Path, P.Path) -> Bool) -> [(P.Path, P.Path)] -> Expectation
allSame level same cases = case filter (not . same) cases of
  [] -> pure ()
  (l, r) : more ->
    expectationFailure . unlines $
      [ "ls " <> shown l,
        "ls " <> shown r,
        "do not compile to the same thing (" <> level <> ")",
        "(and " <> show (length more) <> " more pairs)"
      ]

-- | A property that should fail finds a counterexample among its first
-- cases.
findsOneWithin :: Int -> Maybe Int -> Expectation
findsOneWithin n found = found `shouldSatisfy` maybe False (<= n)

-- * Laws with a condition

-- | A law that holds under a condition, as a property. @how@ is a way to
-- read the condition: 'reading' is the document's, and 'without' has,
-- for every clause of the condition, that reading with the clause
-- dropped, under the name the tests print. 'sample' gives the cases the
-- property is tried on for a reading: the generator, the seed and the
-- number of cases ('count') are the same for every reading, and only
-- which of the drawn cases the condition lets in differs. 'firstFailure'
-- is the place, counted from 1, of the first case that shows the law
-- not to hold: for a law about what two paths find, a pair that
-- commands tell apart ('firstToldApart').
data Law how a = Law
  { name :: String,
    reading :: how,
    without :: [(String, how)],
    count :: Int,
    sample :: how -> [a],
    firstFailure :: [a] -> Maybe Int
  }

-- | The cases of the property under the condition as the document
-- states it, all of them there.
committed :: Law how a -> IO [a]
committed law = do
  let cases = law.sample law.reading
  length cases `shouldBe` law.count
  pure cases

-- | Every clause of the condition is needed for the property to pass:
-- with the clause dropped, the property itself, on the cases the same
-- generator and seed give and as many of them, fails, and it does so in
-- the first half of its cases, so that it is not by a hair that it
-- notices. (That some pair outside the condition differs does not show
-- this: the property has to come across one. The cases after the first
-- that fails are not made.)
needsEveryClause :: Law how a -> Spec
needsEveryClause law =
  describe (law.name <> ": the same " <> show law.count <> " cases fail in their first half without the clause") $
    for_ law.without \(clause, how) ->
      it clause $
        law.firstFailure (law.sample how) `shouldSatisfy` maybe False ((<= law.count) . (* 2))

compiledAlike :: (P.Path, P.Path) -> Bool
compiledAlike (l, r) = compilePath l == compilePath r

upToRouteOrder :: (P.Path, P.Path) -> Bool
upToRouteOrder (l, r) = alternativesUpToOrder l == alternativesUpToOrder r

-- | Small enough to compile many times ('fewAlternatives').
small :: Int -> P.Path -> Bool
small n p = fewAlternatives n (p P.:/ P.One)

bothSmall :: Int -> (P.Path, P.Path) -> Bool
bothSmall n (l, r) = small n l && small n r

drawn :: Int -> Int -> Gen a -> [a]
drawn seed n g = unGen (vectorOf n g) (mkQCGen seed) 30

-- * Random paths

-- | What a random path may have.
data Allow = Allow
  { joins :: Bool,
    shares :: Bool,
    forks :: Bool,
    unions :: Bool,
    reversals :: Bool,
    wilds :: Bool
  }

anything :: Allow
anything = Allow True True True True True True

genStep :: Gen P.Path
genStep =
  elements
    [ P.Literal "a",
      P.Literal "a",
      P.Literal "b",
      P.Literal "b",
      P.Literal "c",
      P.Literal "c",
      P.Backwards (P.Literal "a"),
      P.Backwards (P.Literal "b"),
      P.Backwards (P.Literal "c")
    ]

genJoin :: Gen P.Path
genJoin =
  frequency
    [ (4, pure P.One),
      (1, pure (P.Absolute (smallNID 1))),
      (1, pure (P.ExcludingNIDs (singletonSet (smallNID 1))))
    ]

-- | One step or two.
genWalk :: Gen P.Path
genWalk = choose (1, 2) >>= \n -> sequenced <$> vectorOf n genStep

-- | A walk with a join written in it: behind it, before it, or between
-- two walks.
genJoinedWalk :: Gen P.Path
genJoinedWalk = do
  walk <- genWalk
  join' <- genJoin
  more <- genWalk
  elements [walk P.:/ join', join' P.:/ walk, walk P.:/ join' P.:/ more, walk P.:/ join' P.:/ more]

-- | A place to write a path in with something behind it, mostly: a step,
-- or two steps that fork. (At the very end of a query a join does not
-- show: @ls@ joins the walks there anyway.)
genFollowed :: Gen P.Path
genFollowed =
  frequency
    [ (1, pure hole),
      (4, (hole P.:/) <$> genStep),
      (1, (\x y -> hole P.:/ (x P.:& y)) <$> genStep <*> genStep),
      (2, choose (1, 3) >>= genContext anything)
    ]

-- | A path of so many steps and joins.
genPath :: Allow -> Int -> Gen P.Path
genPath = genChain True

-- | A path of so many steps and joins. Without the first argument it has
-- no join of its own chain: none written in its chain, and none in the
-- top chain of a side of a union there. A join in a conjunct of a group
-- is no join of the chain the group is in, so the conjuncts have them
-- either way.
genChain :: Bool -> Allow -> Int -> Gen P.Path
genChain own allow n
  | n <= 1 =
      frequency $
        [(8, genStep)]
          <> [(1, pure P.Wild) | allow.wilds]
          <> [(3, genJoin) | own && allow.joins]
  | otherwise = do
      k <- choose (1, n - 1)
      frequency $
        [(5, (P.:/) <$> genChain own allow k <*> genChain own allow (n - k))]
          <> [ (3, (P.:&) <$> genChain True allow k <*> genChain True allow (n - k))
             | allow.forks
             ]
          <> [ (2, (P.:||) <$> genChain own allow k <*> genChain own allow (n - k))
             | allow.unions
             ]
          <> [ (1, (P.:++) <$> genChain own allow k <*> genChain own allow (n - k))
             | allow.shares
             ]
          <> [(1, P.Backwards <$> genChain own allow (n - 1)) | allow.reversals]

-- | The place in a context where the path a law is about is written: a
-- label no generator makes.
hole :: P.Path
hole = P.Literal "?"

-- | A path with one 'hole' in it and so much else around it.
genContext :: Allow -> Int -> Gen P.Path
genContext allow n
  | n <= 0 = pure hole
  | otherwise = do
      k <- choose (1, min 3 n)
      other <- genPath allow k
      inner <- genContext allow (n - k)
      frequency $
        [(4, pure (inner P.:/ other)), (4, pure (other P.:/ inner))]
          <> concat [[(2, pure (inner P.:& other)), (2, pure (other P.:& inner))] | allow.forks]
          <> concat [[(1, pure (inner P.:|| other)), (1, pure (other P.:|| inner))] | allow.unions]
          <> [(1, pure (inner P.:++ other)) | allow.shares]
          <> [(1, pure (P.Backwards inner)) | allow.reversals]

-- | A context with the path written in its hole.
plug :: P.Path -> P.Path -> P.Path
plug x = go
  where
    go = \case
      p | p == hole -> x
      l P.:/ r -> go l P.:/ go r
      l P.:& r -> go l P.:& go r
      P.Union k l r -> P.Union k (go l) (go r)
      P.Backwards p -> P.Backwards (go p)
      p -> p

-- | Both paths of a pair in a context.
plugBoth :: P.Path -> (P.Path, P.Path) -> (P.Path, P.Path)
plugBoth context (l, r) = (plug l context, plug r context)

conj :: [P.Path] -> P.Path
conj = foldl1Ex' (P.:&)

sequenced :: [P.Path] -> P.Path
sequenced = foldl1Ex' (P.:/)

-- * A path as it is written

-- | A segment of a chain as docs/path-language.md, 2.5, has it, read off
-- the path with every @~@ applied: a step, a join (@\@@, @\@N@,
-- @!{…}@), @%never@, the 'hole', a group of conjuncts, or a union of
-- sides. @(p & q) & r@ is one group of three, and a union written as a
-- side of a union of its kind is its sides.
data Part
  = Step
  | Join
  | Root
  | Never
  | Hole
  | Group [[Part]]
  | Alt P.UnionKind [[Part]]
  deriving stock (Eq, Show)

-- | The chain a path is, with 'Root' for an @\@N@ and 'Join' for the
-- other two.
written :: P.Path -> [Part]
written = writtenWith True

-- | 'written', or, without the argument, the chain a path looks like
-- when a @~@ around a sequence is not applied (which is wrong: the @~@
-- turns the sequence around).
writtenWith :: Bool -> P.Path -> [Part]
writtenWith reversed = go . (if reversed then pushBackwards else id)
  where
    go = \case
      p | p == hole || p == P.Backwards hole -> [Hole]
      l P.:/ r -> go l <> go r
      p@(_ P.:& _) -> [Group (map go (conjunctsOf p))]
      p@(P.Union k _ _) -> [Alt k (map go (sidesOf k p))]
      P.One -> [Join]
      P.ExcludingNIDs _ -> [Join]
      P.Absolute _ -> [Root]
      P.Zero -> [Never]
      P.Backwards p | not reversed -> go p
      _ -> [Step]
    conjunctsOf = \case
      l P.:& r -> conjunctsOf l <> conjunctsOf r
      p -> [p]
    sidesOf k = \case
      P.Union k' l r | k' == k -> sidesOf k l <> sidesOf k r
      p -> [p]

isJoin :: Part -> Bool
isJoin = \case
  Join -> True
  Root -> True
  _ -> False

-- | Whether a chain has a join of its own (docs/path-language.md, 4): an
-- @\@@, @\@N@ or @!{…}@ written in the chain itself, or in the top chain
-- of a side of a union written there. One inside a conjunct of a group
-- does not count.
ownJoin :: [Part] -> Bool
ownJoin = any \case
  Alt _ sides -> any ownJoin sides
  part -> isJoin part

-- | Whether a join is written in the chain itself.
writtenJoin :: [Part] -> Bool
writtenJoin = any isJoin

-- | Whether a chain has an @&@ anywhere in it.
hasFork :: [Part] -> Bool
hasFork = any \case
  Group _ -> True
  Alt _ sides -> any hasFork sides
  _ -> False

-- | Whether a chain has a @++@ anywhere in it.
hasShared :: [Part] -> Bool
hasShared = any \case
  Group conjuncts -> any hasShared conjuncts
  Alt k sides -> k == P.Shared || any hasShared sides
  _ -> False

-- | Whether a path is relative (docs/path-language.md, 2.10): no @\@N@
-- starts a walk, which is the start of the path, or of a conjunct or a
-- union side that stands there, read with every @~@ applied.
relative :: P.Path -> Bool
relative = not . rooted . written

rooted :: [Part] -> Bool
rooted = \case
  Root : _ -> True
  Group conjuncts : _ -> any rooted conjuncts
  Alt _ sides : _ -> any rooted sides
  _ -> False

-- | How "nothing copies a union" is read (docs/path-language.md, 2.9):
-- the statement of the document is 'stated', and each field dropped is
-- one clause of it dropped.
data Copying = Copying
  { -- | no fork stands in the stretch of the union
    ownStretch :: Bool,
    -- | nor in the stretch of a group or union it is written inside
    insideOf :: Bool,
    -- | a union with an @&@ in a side is a fork
    forkingSides :: Bool,
    -- | a stretch ends only at a join written in the chain itself, not at
    -- one in a side of a union
    onlyWrittenJoinsCut :: Bool
  }

stated :: Copying
stated = Copying True True True True

-- | The chains the hole is written inside, the innermost first: each
-- with what it is, and the place in it of the hole or of the group or
-- union that holds the hole.
data Level = Level {chainIs :: ChainIs, parts :: [Part], place :: Int}

data ChainIs = WholePath | Conjunct | Side
  deriving stock (Eq, Show)

levelsOf :: ChainIs -> [Part] -> Maybe [Level]
levelsOf chainIs parts =
  listToMaybe
    [ deeper <> [Level chainIs parts i]
    | (i, part) <- zip [0 ..] parts,
      deeper <- case part of
        Hole -> [[]]
        Group conjuncts -> [ls | c <- conjuncts, Just ls <- [levelsOf Conjunct c]]
        Alt _ sides -> [ls | s <- sides, Just ls <- [levelsOf Side s]]
        _ -> []
    ]

-- | The clauses of "nothing copies a union", each dropped.
copyingClauses :: [(String, Copying)]
copyingClauses =
  [ ("no fork stands in the stretch of the union", stated {ownStretch = False}),
    ( "no fork stands in the stretch of a group or union it is written inside",
      stated {insideOf = False}
    ),
    ("a union with an & in a side is a fork", stated {forkingSides = False}),
    ( "a join in a side of another union does not end the stretch",
      stated {onlyWrittenJoinsCut = False}
    )
  ]

-- | Whether nothing copies what is written in the hole of a context.
--
-- A stretch is followed outward from where it starts: through the chain
-- it starts in, and, while that chain is a side of a union, on through
-- the chain the union stands in, since a side's steps continue that
-- chain (2.9). A join written in a side ends the stretch on its side of
-- the hole for the walks that take that side, which are the walks the
-- hole is on: beyond such a join nothing is looked at on that side, in
-- the side or outside its union. So in @(g & h)/(s/\@/"?" | o)@ nothing
-- copies the hole, and in @(g & h)/(s/\@/"?" | o)/(d & e)@ the group
-- @(d & e)@ does.
uncopied :: Copying -> P.Path -> Bool
uncopied = uncopiedWith True

-- | 'uncopied', or, without the first argument, a narrower reading that
-- forgets a join written in a side once the stretch has left the side
-- (and so takes the @(g & h)@ above to copy the hole). The tests use it
-- only to count the contexts in which the two differ.
uncopiedWith :: Bool -> Copying -> P.Path -> Bool
uncopiedWith carried how context = case levelsOf WholePath (written context) of
  Nothing -> False
  Just levels -> case stretches levels of
    [] -> True
    own : outer ->
      (not how.ownStretch || quiet False False own)
        && (not how.insideOf || all (quiet False False) outer)
  where
    -- the chains one stretch goes through: the sides it is written in,
    -- the innermost first, and the chain the last of their unions stands
    -- in; then the same for the group that chain is a conjunct of
    stretches levels = case span ((== Side) . (.chainIs)) levels of
      (sides, chain : rest) -> (sides <> [chain]) : stretches rest
      (_, []) -> []
    -- no fork in the stretch; the arguments say whether a join has ended
    -- it before the hole, and behind it
    quiet _ _ [] = True
    quiet endedBefore endedBehind (level : outward) =
      let (before, rest) = splitAt level.place level.parts
          behind = drop 1 rest
          open ended parts = not ended && any forks (takeWhile (not . cuts) parts)
       in not (open endedBefore (reverse before))
            && not (open endedBehind behind)
            && quiet
              (carried && (endedBefore || any cuts before))
              (carried && (endedBehind || any cuts behind))
              outward
    cuts = \case
      Alt _ sides -> not how.onlyWrittenJoinsCut && any ownJoin sides
      part -> isJoin part
    forks = \case
      Group _ -> True
      Alt _ sides -> how.forkingSides && any hasFork sides
      _ -> False

-- | What may stand next to the hole in its chain, nearest first: nothing,
-- a step, a fork, a fork behind a join, a fork behind a union with a
-- join in a side, or a union with a fork in a side.
genNear :: Gen [P.Path]
genNear =
  frequency
    [ (2, pure []),
      (1, (\x -> [x]) <$> genStep),
      (3, (\g -> [g]) <$> group'),
      (1, (\x g -> [x, g]) <$> genStep <*> group'),
      (3, (\j g -> [j, g]) <$> genJoin <*> group'),
      (2, (\u g -> [u, g]) <$> joinedSide <*> group'),
      (2, (\u -> [u]) <$> forkedSide),
      (1, (\j u -> [j, u]) <$> genJoin <*> forkedSide)
    ]
  where
    lesser = choose (1, 2) >>= genPath anything
    group' = (P.:&) <$> lesser <*> lesser
    joinedSide = do
      walk <- genStep
      join' <- genJoin
      other <- genStep
      elements [(walk P.:/ join') P.:|| other, other P.:|| (join' P.:/ walk)]
    forkedSide = do
      fork <- group'
      other <- genStep
      elements [fork P.:|| other, other P.:|| fork]

-- | Contexts in which forks stand near the hole and are often cut off
-- from it: random ones, and a hole in a chain, in a conjunct, in a side
-- of a union, or in a conjunct of a group in such a side, with something
-- next to it ('genNear'). In a side, a join often stands between the
-- hole and an end of the side, which cuts the hole off from a fork
-- outside the union on that end.
genHoled :: Gen P.Path
genHoled = genHoledAimed True

-- | 'genHoled', or, without the argument, the contexts as they were drawn
-- before the generators were aimed at the clauses: none of 'genCopying',
-- and fewer with the hole in a side of a union. They are larger and more
-- varied, and most of them are within the condition of a law.
genHoledAimed :: Bool -> Gen P.Path
genHoledAimed aimed =
  frequency $
    [ (3, choose (0, 5) >>= genContext anything),
      (4, around hole),
      (3, conjunct >>= around),
      (if aimed then 4 else 3, side hole >>= around),
      (if aimed then 2 else 1, conjunct >>= side >>= around)
    ]
      <> [(8, genCopying) | aimed]
  where
    around x = do
      before <- genNear
      after' <- genNear
      back <- frequency [(5, pure id), (1, pure P.Backwards)]
      pure (back (sequenced (reverse before <> [x] <> after')))
    conjunct = do
      next <- frequency [(1, pure []), (2, (\x -> [x]) <$> genStep)]
      other <- choose (1, 2) >>= genPath anything
      pure (sequenced (hole : next) P.:& other)
    -- what stands between the hole and an end of its side, nearest first
    inSide =
      frequency
        [ (3, pure []),
          (2, (\x -> [x]) <$> genStep),
          (3, (\j -> [j]) <$> genJoin),
          (1, (\x j -> [x, j]) <$> genStep <*> genJoin),
          (1, (\j x -> [j, x]) <$> genJoin <*> genStep)
        ]
    side x = do
      before <- inSide
      after' <- inSide
      other <- choose (1, 2) >>= genPath anything
      kind <- elements [P.Relational, P.Shared]
      let written' = sequenced (reverse before <> [x] <> after')
      elements [P.Union kind written' other, P.Union kind other written']

-- | Contexts made of steps in which just one thing copies what is
-- written in the hole, one for each clause of "nothing copies a union":
-- a fork in the hole's stretch; a fork in the stretch of the group the
-- hole is in a conjunct of; a union with a fork in a side, next to the
-- hole; and a fork that only a join in a side of another union stands
-- between. Each also the other way around.
genCopying :: Gen P.Path
genCopying = do
  fork <- (P.:&) <$> genStep <*> genStep
  x <- genStep
  y <- genStep
  z <- genStep
  join' <- genJoin
  next <- elements [hole, hole P.:/ x, x P.:/ hole]
  elements
    [ hole P.:/ fork,
      fork P.:/ hole,
      (next P.:& y) P.:/ fork,
      fork P.:/ (next P.:& y),
      (next P.:& y) P.:/ z P.:/ fork,
      (fork P.:|| y) P.:/ hole,
      hole P.:/ (y P.:|| fork),
      (fork P.:|| y) P.:/ z P.:/ hole,
      fork P.:/ ((x P.:/ join') P.:|| y) P.:/ hole,
      hole P.:/ (y P.:|| (join' P.:/ x)) P.:/ fork
    ]

-- | A path with a join of its own chain or a @++@, which is what a law
-- about a copied union is sensitive to: a random one, or, half the
-- time, a small one ('genJoinedWalk', or two steps with @++@), which
-- small graphs tell apart more often.
genSensitive :: Gen P.Path
genSensitive = oneof (unaimedSensitive <> [genJoinedWalk, (P.:++) <$> genStep <*> genStep])

-- | The random ones of 'genSensitive'.
unaimedSensitive :: [Gen P.Path]
unaimedSensitive =
  [ (choose (1, 4) >>= genPath anything) `suchThat` (ownJoin . written),
    (choose (2, 4) >>= genPath anything) `suchThat` (hasShared . written)
  ]

-- | Contexts with paths to write in them: a context, and two paths, the
-- first of which is mostly a 'genSensitive' one.
holed :: Int -> Int -> [(P.Path, P.Path, P.Path)]
holed = holedAimed True

-- | 'holed', or, without the first argument, the same with nothing aimed
-- at a clause: the contexts of @'genHoledAimed' False@, and for the first
-- path only the random ones of 'genSensitive'.
holedAimed :: Bool -> Int -> Int -> [(P.Path, P.Path, P.Path)]
holedAimed aimed seed n =
  drawn seed n $
    (,,)
      <$> genHoledAimed aimed
      <*> frequency
        [ (3, if aimed then genSensitive else oneof unaimedSensitive),
          (2, choose (1, 4) >>= genPath anything)
        ]
      <*> (choose (1, 3) >>= genPath anything)

-- * 4.2: a side written twice

-- | The contexts in which nothing copies a union, as the document has
-- it.
inside :: P.Path -> Bool
inside = uncopied stated

-- | The contexts in which nothing copies a union only because a join
-- written in a side cuts the union off from a fork outside the side's
-- union, as in @(g & h)/(s/\@/"?" | o)@.
cutOffInSide :: P.Path -> Bool
cutOffInSide context = inside context && not (uncopiedWith False stated context)

-- | A context with what is written in its hole: @C[p | p]@ against
-- @C[p]@, for the contexts chosen.
sideTwice :: (P.Path -> Bool) -> [(P.Path, (P.Path, P.Path))]
sideTwice chosen =
  [ (context, pair)
  | (context, p, _) <- holed 20261102 1500,
    chosen context,
    let pair = plugBoth context (p P.:|| p, p),
    bothSmall 40 pair
  ]

-- | 4.2: @p | p@ finds what @p@ finds where nothing copies the union.
lawSideTwice :: Law Copying (P.Path, P.Path)
lawSideTwice =
  Law
    { name = "p | p",
      reading = stated,
      without = copyingClauses,
      count = 250,
      sample = \how -> map snd (take 250 (sideTwice (uncopied how))),
      firstFailure = firstToldApart
    }

spec_lawSideTwice :: Spec
spec_lawSideTwice = describe "4.2: a side written twice" do
  it "p | p finds what p finds where nothing copies the union" do
    cases <- committed lawSideTwice
    agrees 40 (tally cases)
    -- the document's examples of the condition
    uncopied stated [path| q/"?"/c |] `shouldBe` True
    uncopied stated [path| (p & q)/@/"?"/@/(d & e) |] `shouldBe` True
    uncopied stated [path| q/("?"/k & z) |] `shouldBe` True
  it "also where a join in its side cuts the union off from a fork outside" do
    -- a join written in a side ends the stretch for the walks that take
    -- the side, on its side of the union only
    uncopied stated [path| (g & h)/(s/@/"?" | o) |] `shouldBe` True
    uncopied stated [path| (g & h)/(s/"?" | o) |] `shouldBe` False
    uncopied stated [path| (g & h)/(s/@/"?" | o)/(d & e) |] `shouldBe` False
    uncopied stated [path| (g & h)/(s/@/"?"/@ | o)/(d & e) |] `shouldBe` True
    uncopied stated [path| (g & h)/(o | (s/@/"?" ++ k)) |] `shouldBe` True
    -- so for the group a conjunct is in: it is copied whole, with the
    -- join inside it
    uncopied stated [path| (g & h)/(s/@/("?"/k & z) | o) |] `shouldBe` True
    uncopied stated [path| (g & h)/(s/("?"/@ & z) | o) |] `shouldBe` False
    cutOffInSide [path| (g & h)/(s/@/"?" | o) |] `shouldBe` True
    cutOffInSide [path| q/"?"/c |] `shouldBe` False
    -- many of the cases above are such contexts
    let cut = [() | (context, _) <- take 250 (sideTwice inside), cutOffInSide context]
    length cut `shouldSatisfy` (>= 20)
    lsFinds
      [(0, "g", 1), (0, "h", 2), (1, "s", 5), (2, "s", 5), (5, "a", 3), (5, "a", 4), (3, "d", 9), (4, "e", 9)]
      [ ([path| (g & h)/(s/@/(a/@ | a/@) | o) |], [3, 4]),
        ([path| (g & h)/(s/@/a/@ | o) |], [3, 4]),
        ([path| (g & h)/(s/@/(a/@ | a/@) | o)/(d & e) |], [9]),
        ([path| (g & h)/(s/@/a/@ | o)/(d & e) |], [])
      ]
  needsEveryClause lawSideTwice
  it "not where a fork stands in its stretch" do
    uncopied stated [path| "?"/(d & e) |] `shouldBe` False
    lsFinds
      [(0, "a", 1), (1, "d", 9), (0, "a", 2), (2, "e", 9), (0, "b", 3), (3, "d", 9), (3, "e", 9)]
      [ ([path| (a/@ | a/@)/(d & e) |], [9]),
        ([path| a/@/(d & e) |], [])
      ]
    lsFinds
      [(0, "x", 1), (1, "d", 9), (0, "y", 2), (2, "e", 9)]
      [ ([path| ((x ++ y) | (x ++ y))/(d & e) |], [9]),
        ([path| (x ++ y)/(d & e) |], []),
        ([path| ((x ++ y) ++ (x ++ y))/(d & e) |], [])
      ]
  it "nor where a join in a side of another union is taken to end the stretch" do
    -- the walks of p and of q that take n come to the second union apart
    uncopied stated [path| (p & q)/(m/@ | n)/"?"/z |] `shouldBe` False
    lsFinds
      [ (0, "p", 1),
        (0, "q", 2),
        (1, "n", 3),
        (2, "n", 4),
        (3, "a", 5),
        (4, "a", 6),
        (5, "z", 9),
        (6, "z", 9)
      ]
      [ ([path| (p & q)/(m/@ | n)/(a/@ | a/@)/z |], [9]),
        ([path| (p & q)/(m/@ | n)/a/@/z |], [])
      ]
  it "nor where a product copies the conjunct the union stands in" do
    uncopied stated [path| ("?" & z)/(d & e) |] `shouldBe` False
    uncopied stated [path| ("?"/@ & z)/(d & e) |] `shouldBe` False
    lsFinds
      [(0, "x", 1), (1, "d", 9), (0, "y", 2), (2, "e", 9), (0, "z", 3), (3, "d", 9), (3, "e", 9)]
      [ ([path| (((x ++ y) | (x ++ y)) & z)/(d & e) |], [9]),
        ([path| ((x ++ y) & z)/(d & e) |], []),
        ([path| (((x ++ y) | (x ++ y))/@ & z)/(d & e) |], [9]),
        ([path| ((x ++ y)/@ & z)/(d & e) |], [])
      ]
  it "nor where a union with an & in a side is not taken for a fork" do
    uncopied stated [path| ((d & e) | z)/"?" |] `shouldBe` False
    lsFinds
      [(0, "d", 1), (0, "e", 2), (1, "a", 3), (2, "a", 4), (3, "c", 9), (4, "c", 9)]
      [ ([path| ((d & e) | z)/(a/@ | a/@)/c |], [9]),
        ([path| ((d & e) | z)/a/@/c |], [])
      ]
  it "p ++ p compiles to what p compiles to, in every context" do
    let cases =
          [ pair
          | (context, p, _) <- holed 20261103 250,
            let pair = plugBoth context (p P.:++ p, p),
            bothSmall 60 pair
          ]
    length cases `shouldSatisfy` (> 180)
    allSame "exactly" compiledAlike cases
    -- which is not so of a | union: the same pairs with | differ
    findsOneWithin 40 . firstOther compiledAlike $
      [ pair
      | (context, p, _) <- holed 20261103 250,
        let pair = plugBoth context (p P.:|| p, p),
        bothSmall 60 pair
      ]

-- * 4.3: the empty side

spec_lawEmptySide :: Spec
spec_lawEmptySide = describe "4.3: the empty side" do
  it "p ++ %never, %never ++ p, p | %never and %never | p compile to what p compiles to" do
    let cases =
          [ pair
          | (context, p, _) <- holed 20261104 60,
            small 60 (plug p context),
            pair <-
              map
                (plugBoth context)
                [ (p P.:++ P.Zero, p),
                  (P.Zero P.:++ p, p),
                  (p P.:|| P.Zero, p),
                  (P.Zero P.:|| p, p)
                ]
          ]
    length cases `shouldSatisfy` (> 150)
    allSame "exactly" compiledAlike cases
  it "a side that is not empty is another matter: p ++ q does not compile to p" $
    -- (the law has no condition to drop; this shows that the comparison
    -- can fail)
    findsOneWithin 5 . firstOther compiledAlike $
      [plugBoth context (p P.:++ q, p) | (context, p, q) <- holed 20261104 60, small 60 (plug p context)]
  it "the example of the document: (a | b | %never)/(c & d)" do
    compiledAlike ([path| (a | b | %never)/(c & d) |], [path| (a | b)/(c & d) |]) `shouldBe` True
    compiledAlike ([path| (a | %never)/(c & d) |], [path| a/(c & d) |]) `shouldBe` True
    lsFinds
      [(0, "a", 1), (1, "c", 9), (0, "b", 2), (2, "d", 9)]
      [ ([path| (a | b)/(c & d) |], [9]),
        ([path| (a | b | %never)/(c & d) |], [9]),
        ([path| (a | %never)/(c & d) |], []),
        ([path| a/(c & d) |], [])
      ]
  it "as a step, %never makes its walk fail" $
    lsFinds
      [(0, "a", 1)]
      [ ([path| a | %never |], [1]),
        ([path| a ++ %never |], [1]),
        ([path| a & %never |], []),
        ([path| a/%never |], []),
        ([path| (a & %never) | a |], [1])
      ]

-- * 4.5: @*@ is the @|@ of all labels

-- | A path with every @*@ written as the union, of the kind given, of
-- the labels of the graphs here.
labelled :: P.UnionKind -> P.Path -> P.Path
labelled kind = \case
  P.Wild -> foldl1Ex' (P.Union kind) (map P.Literal worldLabels)
  l P.:/ r -> labelled kind l P.:/ labelled kind r
  l P.:& r -> labelled kind l P.:& labelled kind r
  P.Union k l r -> P.Union k (labelled kind l) (labelled kind r)
  P.Backwards p -> P.Backwards (labelled kind p)
  p -> p

-- | Paths with a @*@ in a random context, some with more of them.
wildPaths :: [P.Path]
wildPaths =
  [ plug wild context
  | (context, wild) <-
      drawn 20261105 600 $
        (,)
          <$> genHoled
          <*> elements [P.Wild, P.Wild, P.Backwards P.Wild],
    small 20 (plug wild context)
  ]

wildAgainst :: P.UnionKind -> [(P.Path, P.Path)]
wildAgainst kind = [(p, q) | p <- wildPaths, let q = labelled kind p, small 80 q]

-- | @a | b@ in a random context against the regex of the two labels
-- there.
regexAgainst :: P.UnionKind -> [(P.Path, P.Path)]
regexAgainst kind =
  [ pair
  | context <- drawn 20261106 400 genHoled,
    let pair =
          plugBoth
            context
            (P.Union kind (P.Literal "a") (P.Literal "b"), P.RegexMatch [re|^(a|b)$|]),
    bothSmall 40 pair
  ]

spec_lawWild :: Spec
spec_lawWild = describe "4.5: * is the | of all labels" do
  it "a path with * finds what it finds with the | of the labels of the graph" do
    let cases = take 40 (wildAgainst P.Relational)
    length cases `shouldBe` 40
    agrees 40 (tally cases)
  it "and not what it finds with their ++" do
    lsFinds
      [(0, "a", 1), (1, "c", 9), (0, "b", 2), (2, "d", 9)]
      [ ([path| */(c & d) |], [9]),
        ([path| (a | b | c | d)/(c & d) |], [9]),
        ([path| (a ++ b ++ c ++ d)/(c & d) |], [])
      ]
    findsOneWithin 20 (firstApart (wildAgainst P.Shared))
  it "4.9: a | b finds what the regex of the two finds, in every context" do
    let cases = take 40 (regexAgainst P.Relational)
    length cases `shouldBe` 40
    agrees 40 (tally cases)
    lsFinds
      [(0, "a", 1), (1, "c", 9), (0, "b", 2), (2, "d", 9)]
      [ ([path| (a | b)/(c & d) |], [9]),
        (P.RegexMatch [re|^(a|b)$|] P.:/ [path| c & d |], [9]),
        ([path| (a ++ b)/(c & d) |], [])
      ]
  it "and a ++ b does not" $
    findsOneWithin 40 (firstApart (regexAgainst P.Shared))

-- * 4.6: the product identity

-- | What the product identity is tried on: 'statedProduct' is the
-- condition, and each field set is one clause dropped.
--
-- The condition is the document's but for two things, both narrower
-- than its words, both pinned below by a pair that differs: a side of a
-- union between the groups may not have a @++@, and what the document
-- calls a conjunct that "forks without a join of its own" is read by
-- 'heldTogether', not by 'ownJoin'.
data Product = Product
  { -- | a side of a @|@ union between the groups may have a join of its
    -- own chain
    sideJoins :: Bool,
    -- | ... an @&@
    sideForks :: Bool,
    -- | ... a @++@
    sideShares :: Bool,
    -- | a conjunct may fork without any join (it is not in the normal
    -- form of 2.4)
    unnormalised :: Bool,
    -- | a conjunct that forks may have its join anywhere section 4 counts
    -- a join of its own chain ('ownJoin'): also in a side of a union when
    -- the fork is written outside that side
    joinsApart :: Bool,
    -- | a conjunct may have a @++@, and one may stand between the groups
    sharedAcross :: Bool,
    -- | a join may stand between the groups
    joinBetween :: Bool
  }

statedProduct :: Product
statedProduct = Product False False False False False False False

isGroup :: Part -> Bool
isGroup = \case
  Group _ -> True
  _ -> False

-- | Whether a conjunct, given by its chain, is copied whole by a
-- product: every fork in its chain has a join written in the chain the
-- fork is written in. That chain is the conjunct's own, or, for a fork
-- in a side of a union, the side, or a chain the side's union stands in.
-- A join in a side holds nothing together outside that side: not a fork
-- in another side (@(e & f) | \@@), whose walks do not come by it, and
-- not one before or after the union (@(e & f)/(g/\@ | h/\@)@), whose
-- walks take a side each, and not always the same one.
heldTogether :: [Part] -> Bool
heldTogether = go False
  where
    go outside chain =
      let joined = outside || writtenJoin chain
       in (joined || not (any isGroup chain))
            && and [go joined side | Alt _ sides <- chain, side <- sides]

-- | Whether a conjunct forks and has no join written in its chain
-- itself. Within 'heldTogether' such a conjunct forks only in sides of
-- unions, each fork with a join in its side: this is what that
-- condition lets in beyond a join written in the conjunct's own chain.
forksInSide :: P.Path -> Bool
forksInSide c = hasFork (written c) && not (writtenJoin (written c))

-- | Whether two groups, given by their conjuncts, and what is between
-- them are within the condition.
productWithin :: Product -> ([P.Path], [P.Path], [P.Path]) -> Bool
productWithin how (firsts, between, seconds) =
  all conjunct (firsts <> seconds) && all (all segment . written) between
  where
    conjunct c =
      let chain = written c
       in (how.unnormalised || normal chain)
            && (how.sharedAcross || not (hasShared chain))
    normal chain
      | how.joinsApart = not (hasFork chain) || ownJoin chain
      | otherwise = heldTogether chain
    segment = \case
      Step -> True
      Alt P.Relational sides -> all side sides
      Alt P.Shared sides -> how.sharedAcross && all (all (== Step)) sides
      Join -> how.joinBetween
      Root -> how.joinBetween
      _ -> False
    side chain =
      (how.sideJoins || not (ownJoin chain))
        && (how.sideForks || not (hasFork chain))
        && (how.sideShares || not (hasShared chain))
        && all inSide chain
    inSide = \case
      Alt _ sides -> all side sides
      _ -> True

-- | Two groups with something between them, and a context
-- ('genFollowed'). One draw in two is a small one: two groups of two
-- small conjuncts, and nothing, a step, a @|@ union of two sides, a
-- join or a @++@ between them. The small draws have, one at a time
-- mostly, the things the clauses of 4.6 are about; and a path with a
-- union that has a @++@ in a side is seldom within the size limit
-- otherwise.
genProduct :: Gen (([P.Path], [P.Path], [P.Path]), P.Path)
genProduct = do
  little <- arbitrary
  context <- genFollowed
  if little
    then do
      firsts <- vectorOf 2 lesser
      seconds <- vectorOf 2 lesser
      between <-
        frequency
          [ (3, pure []),
            (2, vectorOf 1 genStep),
            (8, vectorOf 1 ((P.:||) <$> lesserSide <*> lesserSide)),
            (3, vectorOf 1 genJoin),
            (1, vectorOf 1 ((P.:++) <$> genStep <*> genStep))
          ]
      pure ((firsts, between, seconds), context)
    else do
      (m, n) <- elements [(2, 2), (2, 2), (2, 2), (2, 3), (3, 2), (3, 3)]
      firsts <- vectorOf m conjunct
      seconds <- vectorOf n conjunct
      between <-
        frequency
          [ (1, pure []),
            (4, vectorOf 1 segment),
            (1, vectorOf 2 segment)
          ]
      pure ((firsts, between, seconds), context)
  where
    walks = anything {forks = False, shares = False}
    lesser =
      frequency
        [ (6, genWalk),
          (4, genJoinedWalk),
          (2, (\x y z -> x P.:/ (y P.:& z)) <$> genStep <*> genStep <*> genStep),
          (1, (P.:++) <$> genStep <*> genStep)
        ]
    lesserSide =
      frequency
        [ (5, genWalk),
          (2, genJoinedWalk),
          (3, (P.:&) <$> genStep <*> genStep),
          (5, (P.:++) <$> genStep <*> genStep),
          (1, pure P.Zero)
        ]
    conjunct =
      frequency
        [ (6, choose (1, 2) >>= genPath walks),
          (2, genJoinedFork),
          (1, choose (2, 3) >>= genPath anything)
        ]
    segment =
      frequency
        [ (3, genStep),
          (5, (P.:||) <$> side <*> side),
          (1, (\a b c -> a P.:|| b P.:|| c) <$> side <*> side <*> side),
          (1, (P.:++) <$> genStep <*> genStep)
        ]
    side =
      frequency
        [ (5, choose (1, 2) >>= genChain False walks),
          (1, pure P.Zero),
          (1, (P.:++) <$> genStep <*> genStep),
          (1, (P.:&) <$> genStep <*> genStep),
          (2, choose (1, 2) >>= genPath anything {wilds = False})
        ]

-- | A fork of two steps with a join written in its chain, before it or
-- after it.
genJoinedFork :: Gen P.Path
genJoinedFork = do
  fork <- (P.:&) <$> genPath walks 1 <*> genPath walks 1
  join' <- genJoin
  step <- genStep
  elements
    [ sequenced [step, join', fork],
      sequenced [join', fork],
      sequenced [fork, join'],
      sequenced [fork, join', step]
    ]
  where
    walks = anything {forks = False, shares = False}

-- | Two groups with a step or nothing between them, and a context
-- ('genFollowed'). Half the draws are small ones (see @lesserGroup@
-- below). In the others
-- conjuncts often have a union in whose sides, or next to which, forks
-- and joins stand: a fork with its join in the same side, a fork in one
-- side and a join in another, a fork before or after a union with a join
-- in every side. The other conjuncts mostly have a join, which is what a
-- conjunct that is not copied whole is told by.
genSidedProduct :: Gen (([P.Path], [P.Path], [P.Path]), P.Path)
genSidedProduct = do
  little <- arbitrary
  (m, n) <- if little then pure (2, 2) else elements [(2, 2), (2, 2), (2, 2), (2, 3), (3, 2)]
  firsts <- if little then lesserGroup else vectorOf m conjunct
  seconds <- if little then lesserGroup else vectorOf n conjunct
  between <- frequency [(3, pure []), (2, vectorOf 1 genStep)]
  context <- genFollowed
  pure ((firsts, between, seconds), context)
  where
    -- two conjuncts: a walk or a walk with a join, and that again or a
    -- union of two sides, one a fork of two steps, with a join in that
    -- side, in the other side, or nowhere
    lesserGroup = do
      plain <- frequency [(1, genWalk), (2, genJoinedWalk)]
      fork' <- (P.:&) <$> genStep <*> genStep
      join' <- genJoin
      step <- genStep
      forking <- elements [fork' P.:/ join', join' P.:/ fork', fork', fork']
      other <- elements [join', step P.:/ join', join' P.:/ step, step, step]
      sided' <- elements [forking P.:|| other, other P.:|| forking]
      other' <- frequency [(1, genWalk), (1, genJoinedWalk), (2, pure sided')]
      elements [[plain, other'], [other', plain]]
    walks = anything {forks = False, shares = False}
    walk = choose (1, 2) >>= genPath walks
    fork = (P.:&) <$> genPath walks 1 <*> genPath walks 1
    joined = (P.:/) <$> genStep <*> genJoin
    conjunct =
      frequency
        [ (2, sided),
          (3, walk),
          (2, joined),
          (1, (P.:/) <$> joined <*> genStep)
        ]
    side =
      frequency
        [ (4, genJoinedFork),
          (2, fork),
          (3, walk),
          (2, joined),
          (1, genJoin),
          (1, (P.:||) <$> genJoinedFork <*> walk)
        ]
    union' =
      frequency
        [ (4, (P.:||) <$> side <*> side),
          (1, (\a b c -> a P.:|| b P.:|| c) <$> side <*> side <*> side)
        ]
    sided = do
      u <- union'
      frequency
        [ (7, pure u),
          (2, (P.:/ u) <$> genStep),
          (2, (u P.:/) <$> genStep),
          (1, (P.:/ u) <$> genJoin),
          (1, (u P.:/) <$> genJoin),
          (1, (u P.:/) <$> fork),
          (1, (P.:/ u) <$> fork)
        ]

-- | The pair of the identity: the two groups with what is between them,
-- and every conjunct of the one continued by every conjunct of the
-- other.
productPair :: ([P.Path], [P.Path], [P.Path]) -> (P.Path, P.Path)
productPair (firsts, between, seconds) =
  ( through (conj firsts) (conj seconds),
    conj [through l r | l <- firsts, r <- seconds]
  )
  where
    through l r = sequenced ([l] <> between <> [r])

-- | What stands between the two groups of a product: nothing, steps
-- only, or something with a union.
data Between = Adjacent | Steps | Unions
  deriving stock (Eq, Show)

-- | The pairs of the identity for the groups chosen, in their contexts,
-- each with what stands between the two groups: so many of each kind,
-- the first that are drawn of it, the kinds taking turns (a union, steps,
-- a union, nothing; drawn in their own order the pairs with a union
-- would nearly all come last). The number is how many alternatives a
-- path of a pair may have.
products :: Int -> [(Between, Int)] -> (([P.Path], [P.Path], [P.Path]) -> Bool) -> [(Between, (P.Path, P.Path))]
products limit wanted chosen =
  weave [everyOther unions', ofKind Steps, everyOther (drop 1 unions'), ofKind Adjacent]
  where
    unions' = ofKind Unions
    ofKind kind =
      take (fromMaybe 0 (lookup kind wanted)) $
        filter (bothSmall limit . snd) (filter ((== kind) . fst) drawn')
    everyOther = \case
      x : rest -> x : everyOther (drop 1 rest)
      [] -> []
    drawn' =
      [ (kind, plugBoth context (productPair parts'))
      | (parts'@(_, between, _), context) <- drawn 20261107 8000 genProduct,
        chosen parts',
        let kind
              | null between = Adjacent
              | all ((`elem` [[Step], [Join], [Root]]) . written) between = Steps
              | otherwise = Unions
      ]

-- | Lists taking turns, the first of each, then the second of each: all
-- of them, as long as each list has one more.
weave :: [[a]] -> [a]
weave lists
  | null lists || any null lists = []
  | otherwise = map headEx lists <> weave (map (drop 1) lists)

-- | How many pairs of the property of 4.6 have a union between the two
-- groups, steps only, and nothing.
productCounts :: [(Between, Int)]
productCounts = [(Unions, 160), (Steps, 80), (Adjacent, 80)]

-- | 4.6: the product identity, across steps and across @|@ unions. The
-- size limit is 100 alternatives, since a @++@ in a side multiplies
-- them: the written-out path of the pair with @(x ++ y) | z@ between the
-- groups, below, has 81.
lawProduct :: Law Product (P.Path, P.Path)
lawProduct =
  Law
    { name = "the product",
      reading = statedProduct,
      without =
        [ ( "no side of a union between the groups has a join of its own chain",
            statedProduct {sideJoins = True}
          ),
          ("no side of a union between the groups has an &", statedProduct {sideForks = True}),
          ("no side of a union between the groups has a ++", statedProduct {sideShares = True}),
          ("a conjunct that forks has a join", statedProduct {unnormalised = True}),
          ( "no conjunct has a ++, and none stands between the groups",
            statedProduct {sharedAcross = True}
          ),
          ("no join stands between the groups", statedProduct {joinBetween = True})
        ],
      count = sum (map snd productCounts),
      sample = \how -> map snd (products 100 productCounts (productWithin how)),
      firstFailure = firstToldApart
    }

-- | The pairs of the identity for groups of 'genSidedProduct' in which
-- some conjunct forks in a side of a union and has no join written in
-- its own chain ('forksInSide').
sidedProducts :: (([P.Path], [P.Path], [P.Path]) -> Bool) -> [(P.Path, P.Path)]
sidedProducts chosen =
  [ pair
  | (parts'@(firsts, _, seconds), context) <- drawn 20261118 3000 genSidedProduct,
    any forksInSide (firsts <> seconds),
    chosen parts',
    let pair = plugBoth context (productPair parts'),
    bothSmall 60 pair
  ]

-- | 4.6 for conjuncts that fork in a side of a union: the identity holds
-- where the join is written in the chain the fork is written in
-- ('heldTogether').
lawSidedProduct :: Law Product (P.Path, P.Path)
lawSidedProduct =
  Law
    { name = "the product of conjuncts that fork in a side",
      reading = statedProduct,
      without =
        [ ( "the join of a conjunct that forks is written in the chain the fork is written in",
            statedProduct {joinsApart = True}
          ),
          ("a conjunct that forks has a join", statedProduct {unnormalised = True})
        ],
      count = 200,
      sample = \how -> take 200 (sidedProducts (productWithin how)),
      firstFailure = firstToldApart
    }

spec_lawProduct :: Spec
spec_lawProduct = describe "4.6: the product identity" do
  it "across steps and | unions whose sides have no join, & or ++" do
    cases <- committed lawProduct
    allSame "up to the order of the routes" upToRouteOrder cases
  needsEveryClause lawProduct
  it "the examples of the document" do
    productWithin statedProduct ([[path| a |], [path| b |]], [[path| x | y |]], [[path| c |], [path| d |]])
      `shouldBe` True
    lsFinds
      [(0, "a", 1), (0, "b", 2), (1, "x", 3), (2, "x", 4), (3, "c", 9), (3, "d", 9), (4, "c", 9), (4, "d", 9)]
      [ ([path| (a & b)/(x | y)/(c & d) |], [9]),
        ([path| a/(x | y)/c & a/(x | y)/d & b/(x | y)/c & b/(x | y)/d |], [9])
      ]
  it "not with a join in a side" do
    productWithin statedProduct ([[path| a |], [path| b |]], [[path| x/@ | y |]], [[path| c |], [path| d |]])
      `shouldBe` False
    lsFinds
      [(0, "a", 1), (0, "b", 2), (1, "x", 3), (2, "x", 4), (3, "c", 9), (3, "d", 9), (4, "c", 9), (4, "d", 9)]
      [ ([path| (a & b)/(x/@ | y)/(c & d) |], []),
        ([path| a/(x/@ | y)/c & a/(x/@ | y)/d & b/(x/@ | y)/c & b/(x/@ | y)/d |], [9])
      ]
  it "not with an & in a side" do
    productWithin statedProduct ([[path| x/@ |], [path| b |]], [[path| (c & d) | z |]], [[path| e |], [path| f |]])
      `shouldBe` False
    lsFinds
      [ (0, "x", 1),
        (0, "x", 2),
        (0, "b", 3),
        (1, "c", 4),
        (2, "d", 4),
        (3, "c", 4),
        (3, "d", 4),
        (4, "e", 9),
        (4, "f", 9),
        (0, "x", 5),
        (5, "c", 6),
        (5, "d", 6),
        (3, "c", 6),
        (3, "d", 6),
        (6, "e", 8),
        (6, "f", 8)
      ]
      [ ([path| (x/@ & b)/((c & d) | z)/(e & f) |], [8, 9]),
        ( [path| x/@/((c & d) | z)/e & x/@/((c & d) | z)/f & b/((c & d) | z)/e & b/((c & d) | z)/f |],
          [8]
        )
      ]
  it "not with a ++ in a side" do
    productWithin statedProduct ([[path| a |], [path| b |]], [[path| (x ++ y) | z |]], [[path| c |], [path| d |]])
      `shouldBe` False
    lsFinds
      [(0, "a", 1), (0, "b", 2), (1, "x", 3), (2, "y", 4), (3, "c", 9), (3, "d", 9), (4, "c", 9), (4, "d", 9)]
      [ ([path| (a & b)/((x ++ y) | z)/(c & d) |], []),
        ( [path| a/((x ++ y) | z)/c & a/((x ++ y) | z)/d & b/((x ++ y) | z)/c & b/((x ++ y) | z)/d |],
          [9]
        )
      ]
  it "not for a conjunct that forks without a join, which is multiplied out first" do
    productWithin statedProduct ([[path| x/@/w |], [path| b |]], [], [[path| c/(e & f) |], [path| d |]])
      `shouldBe` False
    productWithin statedProduct ([[path| x/@/w |], [path| b |]], [], [[path| c/e |], [path| c/f |], [path| d |]])
      `shouldBe` True
    lsFinds
      [ (0, "x", 1),
        (1, "w", 3),
        (3, "c", 5),
        (5, "e", 9),
        (3, "d", 9),
        (0, "x", 2),
        (2, "w", 4),
        (4, "c", 6),
        (6, "f", 9),
        (0, "b", 7),
        (7, "c", 8),
        (8, "e", 9),
        (8, "f", 9),
        (7, "d", 9)
      ]
      [ ([path| (x/@/w & b)/(c/(e & f) & d) |], [9]),
        ([path| x/@/w/c/e & x/@/w/c/f & x/@/w/d & b/c/e & b/c/f & b/d |], [9]),
        ([path| x/@/w/c/(e & f) & x/@/w/d & b/c/(e & f) & b/d |], [])
      ]
  it "not across a ++, which is chosen before" $
    lsFinds
      [(0, "a", 1), (1, "c", 9), (0, "b", 2), (2, "d", 9)]
      [ ([path| (a ++ b)/(c & d) |], []),
        ([path| (a ++ b)/c & (a ++ b)/d |], [9])
      ]
  it "not with a join between the groups, at which the walks meet" do
    productWithin statedProduct ([[path| a |], [path| b |]], [[path| @ |]], [[path| c |], [path| d |]])
      `shouldBe` False
    lsFinds
      [(0, "a", 1), (1, "c", 9), (0, "a", 2), (2, "d", 9), (0, "b", 3), (3, "c", 9), (0, "b", 4), (4, "d", 9)]
      [ ([path| (a & b)/@/(c & d) |], []),
        ([path| a/@/c & a/@/d & b/@/c & b/@/d |], [9])
      ]
  it "for a conjunct that forks in a side of a union, with its join in that side" do
    cases <- committed lawSidedProduct
    allSame "up to the order of the routes" upToRouteOrder cases
    -- (commands run few of these pairs: a conjunct with a fork and its
    -- join is mostly a meeting point, which the other group would copy,
    -- so the first path is refused (3.1) and the identity says what the
    -- compiler makes of it)
    length [() | (l, r) <- cases, runs l, runs r] `shouldSatisfy` (>= 10)
    [(shown l, shown r) | (l, r) <- cases, runs l, not (runs r)] `shouldBe` []
    heldTogether (written [path| (e & f)/@ | g |]) `shouldBe` True
    heldTogether (written [path| k/@/((e & f) | g) |]) `shouldBe` True
    heldTogether (written [path| (k/@/((e & f) | g) | h) |]) `shouldBe` True
    alternativesUpToOrder [path| (x/@ & b)/(c & ((e & f)/@ | g)) |]
      `shouldBe` alternativesUpToOrder [path| x/@/c & x/@/((e & f)/@ | g) & b/c & b/((e & f)/@ | g) |]
  needsEveryClause lawSidedProduct
  it "not where the join is in another side of the union than the fork" do
    -- (e & f | @) has a join of its own chain as section 4 counts one,
    -- and for the walks that take e & f it is multiplied out all the same
    let groups = ([[path| x/@ |], [path| b |]], [], [[path| c |], [path| e & f | @ |]])
    productWithin statedProduct groups `shouldBe` False
    productWithin statedProduct {joinsApart = True} groups `shouldBe` True
    productPair groups
      `shouldBe` ( [path| (x/@ & b)/(c & (e & f | @)) |],
                   [path| x/@/c & x/@/(e & f | @) & b/c & b/(e & f | @) |]
                 )
    lsFinds
      [ (0, "x", 1),
        (0, "x", 2),
        (1, "c", 9),
        (1, "e", 9),
        (2, "f", 9),
        (0, "b", 3),
        (3, "c", 9),
        (3, "e", 9),
        (3, "f", 9)
      ]
      [ ([path| (x/@ & b)/(c & (e & f | @)) |], [9]),
        ([path| x/@/c & x/@/(e & f | @) & b/c & b/(e & f | @) |], [])
      ]
    -- the pair that the review of these tests came across, as it was
    -- drawn, and without what was around it
    lsFinds
      [ (1, "c", 0),
        (1, "b", 2),
        (2, "a", 9),
        (3, "c", 0),
        (3, "b", 9),
        (4, "c", 0),
        (4, "c", 9),
        (0, "a", 5),
        (5, "b", 6),
        (6, "a", 9),
        (0, "a", 9)
      ]
      [ ([path| ~b | ((~c/@ & a)/(b/a & (b & c | @)) ++ ~c) |], [1, 3, 4, 9]),
        ( [path| ~b | (~c/@/(b/a) & ~c/@/(b & c | @) & a/(b/a) & a/(b & c | @) ++ ~c) |],
          [1, 3, 4]
        ),
        ([path| (~c/@ & a)/(b/a & (b & c | @)) |], [9]),
        ([path| ~c/@/(b/a) & ~c/@/(b & c | @) & a/(b/a) & a/(b & c | @) |], [])
      ]
  it "nor where the fork stands next to a union with a join in every side" do
    -- the walks of e and of f each take a side of the union, and one
    -- may take g/@ while the other takes h/@: then no join is on both.
    -- (No command runs the first path, which would copy a meeting
    -- point: the two differ in what the compiler makes of them.)
    runs [path| (x/@ & b)/(c & (e & f)/(g/@ | h/@)) |] `shouldBe` False
    runs [path| (x/@ & b)/(c & (e & f | @)) |] `shouldBe` True
    let groups = ([[path| x/@ |], [path| b |]], [], [[path| c |], [path| (e & f)/(g/@ | h/@) |]])
    productWithin statedProduct groups `shouldBe` False
    productWithin statedProduct {joinsApart = True} groups `shouldBe` True
    lsFinds
      [ (0, "x", 1),
        (0, "x", 2),
        (1, "c", 9),
        (1, "e", 4),
        (2, "f", 5),
        (4, "g", 9),
        (5, "h", 9),
        (0, "b", 3),
        (3, "c", 9),
        (3, "e", 6),
        (3, "f", 6),
        (6, "g", 9)
      ]
      [ ([path| (x/@ & b)/(c & (e & f)/(g/@ | h/@)) |], [9]),
        ( [path| x/@/c & x/@/(e & f)/(g/@ | h/@) & b/c & b/(e & f)/(g/@ | h/@) |],
          []
        )
      ]

  it "which forks need a join, and which joins hold a fork together" do
    let within second =
          productWithin statedProduct ([[path| x/@ |], [path| b |]], [], [[path| c |], second])
    -- A fork inside a conjunct of a group needs no join: it is
    -- multiplied out inside the conjunct, here k/@/(m/(e & f) & z),
    -- which the @ of its own chain holds together.
    within [path| k/@/(m/(e & f) & z) |] `shouldBe` True
    compiledAlike
      ( [path| (x/@ & b)/(c & k/@/(m/(e & f) & z)) |],
        [path| x/@/c & x/@/k/@/(m/(e & f) & z) & b/c & b/k/@/(m/(e & f) & z) |]
      )
      `shouldBe` True
    -- Every fork in the conjunct's chain or in a side needs a join: here
    -- (e & f) has one and (g & h) has none.
    within [path| (e & f)/@ | (g & h) |] `shouldBe` False
    lsFinds
      [ (0, "x", 1),
        (1, "c", 9),
        (0, "x", 2),
        (2, "g", 9),
        (0, "x", 3),
        (3, "h", 9),
        (0, "b", 4),
        (4, "c", 9),
        (0, "b", 5),
        (5, "e", 9),
        (0, "b", 6),
        (6, "f", 9)
      ]
      [ ([path| (x/@ & b)/(c & ((e & f)/@ | (g & h))) |], [9]),
        ([path| x/@/c & x/@/((e & f)/@ | (g & h)) & b/c & b/((e & f)/@ | (g & h)) |], [])
      ]
    -- A join in every side of a union holds no fork outside the union,
    -- with a step between the two or without, the union behind the fork
    -- or before it.
    within [path| (e & f)/k/(g/@ | h/@) |] `shouldBe` False
    lsFinds
      [ (0, "x", 10),
        (10, "c", 11),
        (0, "x", 12),
        (12, "e", 13),
        (13, "k", 14),
        (14, "g", 11),
        (0, "x", 15),
        (15, "f", 16),
        (16, "k", 17),
        (17, "h", 11),
        (0, "b", 18),
        (18, "c", 11),
        (0, "b", 19),
        (19, "e", 20),
        (20, "k", 21),
        (21, "g", 11),
        (0, "b", 22),
        (22, "f", 23),
        (23, "k", 24),
        (24, "g", 11)
      ]
      [ ([path| (x/@ & b)/(c & (e & f)/k/(g/@ | h/@)) |], [11]),
        ( [path| x/@/c & x/@/(e & f)/k/(g/@ | h/@) & b/c & b/(e & f)/k/(g/@ | h/@) |],
          []
        )
      ]
    within [path| (g/@ | h/@)/(e & f) |] `shouldBe` False
    lsFinds
      [ (0, "x", 10),
        (10, "c", 11),
        (0, "x", 12),
        (12, "g", 13),
        (13, "e", 11),
        (0, "x", 14),
        (14, "h", 15),
        (15, "f", 11),
        (0, "b", 16),
        (16, "c", 11),
        (0, "b", 17),
        (17, "g", 18),
        (18, "e", 11),
        (18, "f", 11)
      ]
      [ ([path| (x/@ & b)/(c & (g/@ | h/@)/(e & f)) |], [11]),
        ( [path| x/@/c & x/@/(g/@ | h/@)/(e & f) & b/c & b/(g/@ | h/@)/(e & f) |],
          []
        )
      ]
    -- (Commands refuse the first path of each of these four pairs, 3.1:
    -- it is what the compiler makes of it that is compared.)
    for_
      [ [path| (x/@ & b)/(c & k/@/(m/(e & f) & z)) |],
        [path| (x/@ & b)/(c & ((e & f)/@ | (g & h))) |],
        [path| (x/@ & b)/(c & (e & f)/k/(g/@ | h/@)) |],
        [path| (x/@ & b)/(c & (g/@ | h/@)/(e & f)) |]
      ]
      \refused -> (shown refused, runs refused) `shouldBe` (shown refused, False)

-- * 4.7 and 4.8: distribution over a fork

-- | How the conditions of 4.7 and 4.8 are read: 'statedDistribution' is
-- the document, and each field set is one clause dropped.
data Distribution = Distribution
  { -- | 4.7: @A@ and @B@ may have a join of their own chain; 4.8: so may
    -- @P@ and @Q@ though @R@ has an @&@
    conjunctJoins :: Bool,
    -- | 4.7: @C@ may have a join of its own chain; 4.8: @R@ may
    sequencedJoins :: Bool,
    -- | 4.7: @C@ may have a @++@; 4.8: @R@ may
    sequencedShares :: Bool,
    -- | a join in the top chain of a side of a union is not taken for a
    -- join of the chain the union is in
    sideJoinsForgotten :: Bool,
    -- | 4.8: an @&@ in a side of a union of @R@ is not taken for an @&@
    -- of @R@
    sideForksForgotten :: Bool
  }

statedDistribution :: Distribution
statedDistribution = Distribution False False False False False

joinOfChain :: Distribution -> P.Path -> Bool
joinOfChain how p
  | how.sideJoinsForgotten = writtenJoin (written p)
  | otherwise = ownJoin (written p)

-- | 4.7: whether @(A & B)/C = A/C & B/C@ is claimed for these conjuncts
-- and this @C@.
leftWithin :: Distribution -> ([P.Path], P.Path) -> Bool
leftWithin how (conjuncts, c) =
  (how.conjunctJoins || not (any (joinOfChain how) conjuncts))
    && (how.sequencedJoins || not (joinOfChain how c))
    && (how.sequencedShares || not (hasShared (written c)))

-- | 4.8: whether @R/(P & Q) = R/P & R/Q@ is claimed for this @R@ and
-- these conjuncts.
rightWithin :: Distribution -> (P.Path, [P.Path]) -> Bool
rightWithin how (r, conjuncts) =
  (how.sequencedJoins || not (joinOfChain how r))
    && (how.sequencedShares || not (hasShared (written r)))
    && (how.conjunctJoins || not forked || not (any (joinOfChain how) conjuncts))
  where
    forked
      | how.sideForksForgotten = or [True | Group _ <- written r]
      | otherwise = hasFork (written r)

-- | Conjuncts, what they are sequenced with, and a context
-- ('genFollowed'). Both are random paths, or small paths with one of
-- the things a clause of 4.7 or 4.8 is about: a join in a walk, a join
-- in a side of a union, a fork of walks, such a fork in a side of a
-- union, a @++@.
genDistribution :: Gen ([P.Path], P.Path, P.Path)
genDistribution = genDistributionAimed True

-- | 'genDistribution', or, without the argument, the same with nothing
-- aimed at a clause: the conjuncts and what they are sequenced with are
-- all random paths, and the context is the bare hole three times in
-- five and a random one otherwise. Most of these draws are within the
-- conditions of 4.7 and 4.8, which the small paths of 'genDistribution'
-- mostly are not.
genDistributionAimed :: Bool -> Gen ([P.Path], P.Path, P.Path)
genDistributionAimed aimed
  | aimed =
      (,,)
        <$> (choose (2, 3) >>= \n -> vectorOf n conjunct)
        <*> single
        <*> genFollowed
  | otherwise =
      (,,)
        <$> (choose (2, 3) >>= \n -> vectorOf n (part 2))
        <*> part 4
        <*> frequency [(3, pure hole), (2, choose (1, 3) >>= genContext anything)]
  where
    part n =
      frequency
        [ (3, choose (1, n) >>= genChain False anything {shares = False}),
          (2, choose (1, n) >>= genChain False anything),
          (2, choose (1, n) >>= genPath anything)
        ]
    sideJoined = do
      joined <- genJoinedWalk
      other <- genWalk
      elements [joined P.:|| other, other P.:|| joined]
    conjunct = frequency [(5, part 2), (2, genWalk), (2, genJoinedWalk), (1, sideJoined)]
    single =
      frequency
        [ (4, part 4),
          (2, genWalk),
          (3, (P.:&) <$> genWalk <*> genWalk),
          (2, (\a b c -> (a P.:& b) P.:|| c) <$> genWalk <*> genWalk <*> genWalk),
          (2, genJoinedWalk),
          (2, sideJoined),
          (2, (P.:++) <$> genWalk <*> genWalk)
        ]

leftPair :: ([P.Path], P.Path) -> (P.Path, P.Path)
leftPair (conjuncts, c) = (conj conjuncts P.:/ c, conj [a P.:/ c | a <- conjuncts])

rightPair :: (P.Path, [P.Path]) -> (P.Path, P.Path)
rightPair (r, conjuncts) = (r P.:/ conj conjuncts, conj [r P.:/ p | p <- conjuncts])

-- | The pairs of 4.7 for the draws chosen: of 'genDistribution', or,
-- without the first argument, of the generator with nothing aimed.
leftCases :: Bool -> (([P.Path], P.Path) -> Bool) -> [(P.Path, P.Path)]
leftCases aimed chosen =
  [ pair
  | (conjuncts, c, context) <- drawn 20261108 4000 (genDistributionAimed aimed),
    chosen (conjuncts, c),
    let pair = plugBoth context (leftPair (conjuncts, c)),
    bothSmall 40 pair
  ]

-- | The same for 4.8.
rightCases :: Bool -> ((P.Path, [P.Path]) -> Bool) -> [(P.Path, P.Path)]
rightCases aimed chosen =
  [ pair
  | (conjuncts, r, context) <- drawn 20261109 4000 (genDistributionAimed aimed),
    chosen (r, conjuncts),
    let pair = plugBoth context (rightPair (r, conjuncts)),
    bothSmall 40 pair
  ]

-- | 400 pairs of 4.7 within its condition in which every part is a random
-- path. The cases of 'lawLeftDistribution' are mostly small paths aimed
-- at one clause, which a dropped clause needs; these are what the
-- property itself reaches further with.
unaimedLeft :: [(P.Path, P.Path)]
unaimedLeft = take 400 (leftCases False (leftWithin statedDistribution))

-- | The same for 4.8.
unaimedRight :: [(P.Path, P.Path)]
unaimedRight = take 400 (rightCases False (rightWithin statedDistribution))

-- | 4.7: @(A & B)/C = A/C & B/C@ under its condition.
lawLeftDistribution :: Law Distribution (P.Path, P.Path)
lawLeftDistribution =
  Law
    { name = "(A & B)/C",
      reading = statedDistribution,
      without =
        [ ("A and B have no join of their own chain", statedDistribution {conjunctJoins = True}),
          ("C has no join of its own chain", statedDistribution {sequencedJoins = True}),
          ("C has no ++", statedDistribution {sequencedShares = True}),
          ( "a join in the top chain of a side of a union is a join of the chain",
            statedDistribution {sideJoinsForgotten = True}
          )
        ],
      count = 200,
      sample = \how -> take 200 (leftCases True (leftWithin how)),
      firstFailure = firstToldApart
    }

-- | 4.8: @R/(P & Q) = R/P & R/Q@ under its condition.
lawRightDistribution :: Law Distribution (P.Path, P.Path)
lawRightDistribution =
  Law
    { name = "R/(P & Q)",
      reading = statedDistribution,
      without =
        [ ("R has no join of its own chain", statedDistribution {sequencedJoins = True}),
          ("R has no ++", statedDistribution {sequencedShares = True}),
          ( "P and Q have no join of their own chain if R has an &",
            statedDistribution {conjunctJoins = True}
          ),
          ( "an & in a side of a union of R is an & of R",
            statedDistribution {sideForksForgotten = True}
          ),
          ( "a join in the top chain of a side of a union is a join of the chain",
            statedDistribution {sideJoinsForgotten = True}
          )
        ],
      count = 200,
      sample = \how -> take 200 (rightCases True (rightWithin how)),
      firstFailure = firstToldApart
    }

spec_lawLeftDistribution :: Spec
spec_lawLeftDistribution = describe "4.7: (A & B)/C = A/C & B/C" do
  it "where A and B have no join of their chain, and C none and no ++" do
    cases <- committed lawLeftDistribution
    length unaimedLeft `shouldBe` 400
    allSame "up to the order of the routes" upToRouteOrder (cases <> unaimedLeft)
  needsEveryClause lawLeftDistribution
  it "not where A has a join and C forks" do
    leftWithin statedDistribution ([[path| x/@ |], [path| b |]], [path| c & d |]) `shouldBe` False
    leftWithin statedDistribution ([[path| x/@ | zz |], [path| b |]], [path| c & d |]) `shouldBe` False
    lsFinds
      [(0, "x", 1), (0, "x", 2), (1, "c", 9), (2, "d", 9), (0, "b", 3), (3, "c", 9), (3, "d", 9)]
      [ ([path| (x/@ & b)/(c & d) |], [9]),
        ([path| x/@/(c & d) & b/(c & d) |], []),
        ([path| ((x/@ | zz) & b)/(c & d) |], [9]),
        ([path| (x/@ | zz)/(c & d) & b/(c & d) |], [])
      ]
  it "not where C has a join of its chain" do
    leftWithin statedDistribution ([[path| a |], [path| b |]], [path| @/c |]) `shouldBe` False
    leftWithin statedDistribution ([[path| a |], [path| b |]], [path| c/@/d |]) `shouldBe` False
    leftWithin statedDistribution ([[path| a |], [path| b |]], [path| c/@/e | d |]) `shouldBe` False
    lsFinds
      [(0, "a", 1), (0, "b", 2), (1, "c", 9), (2, "c", 9)]
      [ ([path| (a & b)/@/c |], []),
        ([path| a/@/c & b/@/c |], [9])
      ]
    -- (the document's graph for the next pair has no d edge, so both
    -- paths find nothing on it; on this one they differ)
    lsFinds
      [(0, "a", 1), (0, "b", 2), (1, "c", 3), (2, "c", 4), (3, "d", 9), (4, "d", 9)]
      [ ([path| (a & b)/c/@/d |], []),
        ([path| a/c/@/d & b/c/@/d |], [9])
      ]
    lsFinds
      [(0, "a", 1), (0, "b", 2), (1, "c", 8), (8, "e", 9), (2, "c", 7), (7, "e", 9)]
      [ ([path| (a & b)/(c/@/e | d) |], []),
        ([path| a/(c/@/e | d) & b/(c/@/e | d) |], [9])
      ]
  it "not where C has a ++" do
    leftWithin statedDistribution ([[path| a |], [path| b |]], [path| c ++ d |]) `shouldBe` False
    lsFinds
      [(0, "a", 1), (0, "b", 2), (1, "c", 9), (2, "d", 9)]
      [ ([path| (a & b)/(c ++ d) |], []),
        ([path| a/(c ++ d) & b/(c ++ d) |], [9])
      ]

spec_lawRightDistribution :: Spec
spec_lawRightDistribution = describe "4.8: R/(P & Q) = R/P & R/Q" do
  it "where R has no join of its chain and no ++, and P and Q no join if R has an &" do
    cases <- committed lawRightDistribution
    length unaimedRight `shouldBe` 400
    allSame "up to the order of the routes" upToRouteOrder (cases <> unaimedRight)
    lsFinds
      [(0, "a", 1), (1, "b", 9), (0, "a", 2), (2, "c", 9)]
      [([path| a/(b & c) |], [9]), ([path| a/b & a/c |], [9])]
  needsEveryClause lawRightDistribution
  it "not where R has a join of its chain, at its end or earlier" do
    rightWithin statedDistribution ([path| a/@ |], [[path| b |], [path| c |]]) `shouldBe` False
    rightWithin statedDistribution ([path| x/@/w |], [[path| c |], [path| d |]]) `shouldBe` False
    rightWithin statedDistribution ([path| x/@ | zz |], [[path| c |], [path| d |]]) `shouldBe` False
    lsFinds
      [(0, "a", 1), (1, "b", 9), (0, "a", 2), (2, "c", 9)]
      [([path| a/@/(b & c) |], []), ([path| a/@/b & a/@/c |], [9])]
    lsFinds
      [(0, "x", 1), (1, "w", 2), (2, "c", 9), (0, "x", 3), (3, "w", 4), (4, "d", 9)]
      [ ([path| x/@/w/(c & d) |], []),
        ([path| x/@/w/c & x/@/w/d |], [9]),
        ([path| x/w/(c & d) |], [9]),
        ([path| x/w/c & x/w/d |], [9])
      ]
    lsFinds
      [(0, "x", 1), (1, "c", 9), (0, "x", 2), (2, "d", 9)]
      [ ([path| (x/@ | zz)/(c & d) |], []),
        ([path| (x/@ | zz)/c & (x/@ | zz)/d |], [9])
      ]
  it "not where R has a ++" do
    rightWithin statedDistribution ([path| a ++ b |], [[path| c |], [path| d |]]) `shouldBe` False
    lsFinds
      [(0, "a", 1), (1, "c", 9), (0, "b", 2), (2, "d", 9)]
      [ ([path| (a ++ b)/(c & d) |], []),
        ([path| (a ++ b)/c & (a ++ b)/d |], [9])
      ]
  it "not where R has an & and P a join of its chain" do
    rightWithin statedDistribution ([path| p & q |], [[path| x/@/w |], [path| y |]]) `shouldBe` False
    rightWithin statedDistribution ([path| a & b |], [[path| @/c |], [path| d |]]) `shouldBe` False
    rightWithin statedDistribution ([path| (a & b) | z |], [[path| x/@/w |], [path| y |]]) `shouldBe` False
    rightWithin statedDistribution ([path| p & q |], [[path| x/w |], [path| y |]]) `shouldBe` True
    lsFinds
      [ (0, "p", 1),
        (0, "q", 2),
        (1, "x", 3),
        (3, "w", 9),
        (2, "x", 4),
        (4, "w", 9),
        (1, "y", 9),
        (2, "y", 9)
      ]
      [ ([path| (p & q)/(x/@/w & y) |], [9]),
        ([path| (p & q)/x/@/w & (p & q)/y |], []),
        ([path| (p & q)/(x/w & y) |], [9]),
        ([path| (p & q)/x/w & (p & q)/y |], [9])
      ]
    lsFinds
      [(0, "a", 1), (1, "c", 9), (0, "b", 2), (2, "c", 9), (1, "d", 9), (2, "d", 9)]
      [ ([path| (a & b)/(@/c & d) |], [9]),
        ([path| (a & b)/@/c & (a & b)/d |], [])
      ]
    lsFinds
      [ (0, "a", 1),
        (0, "b", 2),
        (1, "x", 3),
        (2, "x", 4),
        (3, "w", 9),
        (4, "w", 9),
        (1, "y", 9),
        (2, "y", 9)
      ]
      [ ([path| ((a & b) | z)/(x/@/w & y) |], [9]),
        ([path| ((a & b) | z)/x/@/w & ((a & b) | z)/y |], [])
      ]

-- * 4.9: unions and sequencing

-- | @p/(q ++ r)@ against @p/q ++ p/r@ and @(p ++ q)/r@ against
-- @p/r ++ q/r@, for the kind of union given, in a context.
unionOverSequence :: P.UnionKind -> [(P.Path, P.Path)]
unionOverSequence kind =
  [ pair
  | (context, p, q, r) <-
      drawn 20261110 150 $
        (,,,)
          <$> frequency [(2, pure hole), (3, genHoled)]
          <*> part
          <*> part
          <*> part,
    pair <-
      map
        (plugBoth context)
        [ (p P.:/ P.Union kind q r, P.Union kind (p P.:/ q) (p P.:/ r)),
          (P.Union kind p q P.:/ r, P.Union kind (p P.:/ r) (q P.:/ r))
        ],
    bothSmall 60 pair
  ]
  where
    part = choose (1, 3) >>= genPath anything

-- | A context with what is written in its hole: @C[p | q]@ against
-- @C[p ++ q]@, for the contexts chosen.
eitherUnion :: (P.Path -> Bool) -> [(P.Path, (P.Path, P.Path))]
eitherUnion chosen =
  [ (context, pair)
  | (context, p, q) <- holed 20261111 1500,
    chosen context,
    let pair = plugBoth context (p P.:|| q, p P.:++ q),
    bothSmall 40 pair
  ]

-- | 4.9: a union that nothing copies finds the same written @|@ or
-- @++@.
lawEitherUnion :: Law Copying (P.Path, P.Path)
lawEitherUnion =
  Law
    { name = "| or ++",
      reading = stated,
      without = copyingClauses,
      count = 150,
      sample = \how -> map snd (take 150 (eitherUnion (uncopied how))),
      firstFailure = firstToldApart
    }

spec_lawUnionsAndSequencing :: Spec
spec_lawUnionsAndSequencing = describe "4.9: unions and sequencing" do
  it "a ++ union distributes over / both ways, in every context" do
    let cases = unionOverSequence P.Shared
    length cases `shouldSatisfy` (> 200)
    allSame "exactly" compiledAlike cases
    lsFinds
      [(0, "a", 1), (0, "b", 2), (1, "c", 9), (2, "c", 9)]
      [([path| (a & b)/(c ++ d) |], [9]), ([path| (a & b)/c ++ (a & b)/d |], [9])]
    lsFinds
      [(0, "a", 1), (1, "c", 9), (1, "d", 9)]
      [([path| (a ++ b)/(c & d) |], [9]), ([path| a/(c & d) ++ b/(c & d) |], [9])]
  it "a | union does not" do
    lsFinds
      [(0, "a", 1), (0, "b", 2), (1, "c", 9), (2, "d", 9)]
      [ ([path| (a & b)/(c | d) |], [9]),
        ([path| (a & b)/c | (a & b)/d |], []),
        ([path| (a | b)/(c & d) |], [9]),
        ([path| a/(c & d) | b/(c & d) |], [])
      ]
    findsOneWithin 40 (firstApart (unionOverSequence P.Relational))
  it "a union that nothing copies finds the same written | or ++" do
    cases <- committed lawEitherUnion
    agrees 40 (tally cases)
    lsFinds
      [(0, "a", 1), (0, "b", 2), (1, "c", 9), (2, "d", 9)]
      [([path| (a | b)/c |], [9]), ([path| (a ++ b)/c |], [9])]
    -- many of the contexts are ones in which only a join in the union's
    -- side cuts it off from a fork outside ('cutOffInSide')
    let cut = [() | (context, _) <- take 150 (eitherUnion inside), cutOffInSide context]
    length cut `shouldSatisfy` (>= 12)
    lsFinds
      [(0, "g", 1), (0, "h", 2), (1, "s", 5), (2, "s", 5), (5, "a", 9), (5, "b", 8)]
      [ ([path| (g & h)/(s/@/(a | b) | o) |], [8, 9]),
        ([path| (g & h)/(s/@/(a ++ b) | o) |], [8, 9])
      ]
    -- without the join the walks of g and of h come to the union apart
    lsFinds
      [(0, "g", 1), (0, "h", 2), (1, "s", 5), (2, "s", 6), (5, "a", 9), (6, "b", 9)]
      [ ([path| (g & h)/(s/(a | b) | o) |], [9]),
        ([path| (g & h)/(s/(a ++ b) | o) |], [])
      ]
  needsEveryClause lawEitherUnion
  it "not where a fork stands in its stretch" do
    lsFinds
      [(0, "a", 1), (0, "b", 2), (1, "c", 9), (2, "d", 9)]
      [([path| (a | b)/(c & d) |], [9]), ([path| (a ++ b)/(c & d) |], [])]
  it "nor where a product copies the conjunct the union stands in" do
    lsFinds
      [(0, "x", 1), (0, "y", 2), (1, "c", 9), (2, "d", 9), (0, "z", 3), (3, "c", 9), (3, "d", 9)]
      [ ([path| ((x | y) & z)/(c & d) |], [9]),
        ([path| ((x ++ y) & z)/(c & d) |], [])
      ]
    -- nor with a join of its own after the union in that conjunct
    lsFinds
      [(0, "x", 1), (1, "c", 9), (0, "y", 2), (2, "d", 9), (0, "w", 3), (3, "c", 9), (3, "d", 9)]
      [ ([path| ((x | y)/@ & w)/(c & d) |], [9]),
        ([path| ((x ++ y)/@ & w)/(c & d) |], [])
      ]
  it "nor where a join in a side of another union is taken to end the stretch" $
    lsFinds
      [(0, "p", 1), (0, "q", 2), (1, "n", 3), (2, "n", 4), (3, "a", 9), (4, "b", 9)]
      [ ([path| (p & q)/(m/@ | n)/(a | b) |], [9]),
        ([path| (p & q)/(m/@ | n)/(a ++ b) |], [])
      ]
  it "nor where a union with an & in a side is not taken for a fork" $
    lsFinds
      [(0, "d", 1), (0, "e", 2), (1, "a", 9), (2, "b", 9)]
      [ ([path| ((d & e) | z)/(a | b) |], [9]),
        ([path| ((d & e) | z)/(a ++ b) |], [])
      ]

-- * 4.10: a conjunct written twice

-- | A path with @\@/@ in front and @/\@@ behind.
sealed :: P.Path -> P.Path
sealed x = P.One P.:/ x P.:/ P.One

-- | How the condition of 4.10 is read: 'statedTwice' is the document,
-- and each field set is one clause dropped.
data Twice = Twice
  { -- | the two paths are written without @\@/@ before and @/\@@ behind
    unsealed :: Bool,
    -- | @q@ need not be relative
    anyStart :: Bool,
    -- | one of the two paths may be one that no command runs
    refusedToo :: Bool
  }

statedTwice :: Twice
statedTwice = Twice False False False

-- | @C[\@/(q & q)/\@]@ against @C[\@/q/\@]@ for the contexts and paths
-- given, as the condition is read.
twice :: Twice -> [(P.Path, P.Path)] -> [(P.Path, P.Path)]
twice how drawn' =
  [ pair
  | (context, q) <- drawn',
    how.anyStart || relative q,
    let wrap = if how.unsealed then id else sealed
        pair = plugBoth context (wrap (q P.:& q), wrap q),
    how.refusedToo || (runs (fst pair) && runs (snd pair)),
    bothSmall 40 pair
  ]

-- | The contexts are 'genHoled' ones, and @q@ is, in half the draws, a
-- walk with a join ('genJoinedWalk', which is three draws in seven here
-- and one in four of 'genSensitive'): next to a group that is what the
-- seal is for. One draw in fourteen is two steps with @++@, one in seven
-- a random path that has a join of its own chain or a @++@ (the other
-- half of 'genSensitive'), and two in seven are any random path.
conjunctTwice :: Twice -> [(P.Path, P.Path)]
conjunctTwice how = twice how (drawn 20261112 1500 ((,) <$> genHoled <*> q))
  where
    q =
      frequency
        [ (3, genJoinedWalk),
          (2, genSensitive),
          (2, choose (1, 4) >>= genPath anything)
        ]

-- | 4.10: sealed, @q & q@ finds what @q@ finds. Of the three clauses of
-- its condition only the seal is here: the property does not fail
-- without either of the other two (see the test that says so).
lawConjunctTwice :: Law Twice (P.Path, P.Path)
lawConjunctTwice =
  Law
    { name = "q & q",
      reading = statedTwice,
      without = [("@/ stands before both paths and /@ behind", statedTwice {unsealed = True})],
      count = 150,
      sample = take 150 . conjunctTwice,
      firstFailure = firstToldApart
    }

-- | 80 pairs of 4.10 within its condition with nothing aimed at the seal
-- ('holedAimed'): the cases of 'lawConjunctTwice' have mostly a small
-- @q@ and often a small context, and in these more of the @q@ fork or
-- have a @*@.
unaimedTwice :: [(P.Path, P.Path)]
unaimedTwice =
  take 80 $
    twice statedTwice [(context, q) | (context, q, _) <- holedAimed False 20261112 1500]

-- | The same for paths @q@ that are not relative: those of 'genEnded'
-- in which an @\@1@ starts a walk. Where commands run both paths.
unrelativeTwice :: [(P.Path, P.Path)]
unrelativeTwice =
  twice statedTwice {anyStart = True} $
    [ (context, q)
    | (context, q) <- drawn 20261119 1500 ((,) <$> genHoled <*> genEnded),
      not (relative q)
    ]

-- | The same for the pairs of which commands run one path and not the
-- other.
oneRefusedTwice :: [(P.Path, P.Path)]
oneRefusedTwice =
  [ pair
  | pair@(l, r) <- conjunctTwice statedTwice {refusedToo = True},
    runs l /= runs r
  ]

spec_lawConjunctTwice :: Spec
spec_lawConjunctTwice = describe "4.10: a conjunct written twice" do
  it "sealed, q & q finds what q finds in every context, where both are run" do
    cases <- committed lawConjunctTwice
    length unaimedTwice `shouldBe` 80
    -- (35 and not 40 percent: sealed, all the walks of a pair have to end
    -- at one node, and fewer comparisons find one)
    agrees 35 (tally (cases <> unaimedTwice))
  needsEveryClause lawConjunctTwice
  it "no pair is known to differ for a q that is not relative, nor where one path is refused" do
    -- The document states the law for a relative q, and where commands
    -- run both paths. Neither clause is dropped above, because the
    -- property does not fail without it. On 2026-10-11 no pair found
    -- different nodes among 8,529 sealed pairs with a q that is not
    -- relative, nor among 1,502 with a relative q of which commands
    -- refuse one path (always the one with q & q). Whether the law needs
    -- the two clauses is a question for the designer; both stay in the
    -- condition. This pins what was seen: should a pair here come to
    -- differ, the clause is needed after all.
    let unrelative = take 80 unrelativeTwice
        refused = take 40 oneRefusedTwice
    (length unrelative, length refused) `shouldBe` (80, 40)
    (tally unrelative).apart `shouldBe` []
    -- (what the compiler makes of a refused path is compared: a command
    -- does not run it, and so finds nothing with it)
    (tally refused).apart `shouldBe` []
  it "a whole query is sealed: ls (q & q) finds what ls q finds, for every q" do
    let cases = take 60 [(q P.:& q, q) | (_, q, _) <- holed 20261113 400, small 40 (q P.:& q)]
    length cases `shouldBe` 60
    agrees 40 (tally cases)
  it "not without the seal, next to a group" do
    lsFinds
      [(0, "a", 1), (1, "c", 9), (0, "b", 2), (2, "c", 9)]
      [ ([path| (a & b)/@/c |], []),
        ([path| (a & b)/(@/c & @/c) |], [9]),
        ([path| (a & b)/@/@/c/@ |], []),
        ([path| (a & b)/@/(@/c & @/c)/@ |], [])
      ]
    lsFinds
      [(0, "a", 1), (0, "b", 2), (0, "a", 3), (0, "b", 3), (1, "c", 9), (2, "c", 9), (3, "c", 8)]
      [ ([path| (a & b)/(@/c & @/c) |], [8, 9]),
        ([path| (a & b)/@/c |], [8])
      ]
    lsFinds
      [(0, "x", 5), (5, "a", 1), (1, "b", 9), (5, "a", 2), (2, "c", 9)]
      [ ([path| x/@/(a/@ & a/@) |], [1, 2]),
        ([path| x/@/a/@ |], [1, 2]),
        ([path| x/@/(a/@ & a/@)/(b & c) |], [9]),
        ([path| x/@/a/@/(b & c) |], []),
        ([path| @/x/@/(a/@ & a/@)/@/(b & c) |], []),
        ([path| @/x/@/a/@/@/(b & c) |], [])
      ]
    lsFinds
      [(0, "a", 1), (1, "b", 9), (0, "a", 2), (2, "c", 9)]
      [ ([path| (a/@ & a/@)/(b & c) |], [9]),
        ([path| a/@/(b & c) |], []),
        ([path| @/(a/@ & a/@)/@/(b & c) |], []),
        ([path| @/a/@/@/(b & c) |], []),
        ([path| a/@/(b & c) & a/@/(b & c) |], [])
      ]

-- * 2.10: relative paths

-- | Paths in which an @\@N@ often stands at an end of a walk, with a @~@
-- around it or not.
genEnded :: Gen P.Path
genEnded =
  frequency
    [ (2, choose (1, 5) >>= genPath anything),
      ( 5,
        do
          core <- choose (1, 3) >>= genPath anything
          other <- choose (1, 2) >>= genPath anything
          let root = P.Absolute (smallNID 1)
          elements
            [ core P.:/ root,
              root P.:/ core,
              P.Backwards (core P.:/ root),
              P.Backwards (root P.:/ core),
              other P.:& P.Backwards (core P.:/ root),
              other P.:/ P.Backwards (root P.:/ core),
              P.Backwards (core P.:/ root) P.:|| other,
              P.Backwards (core P.:/ (root P.:|| other)),
              P.Backwards ((root P.:/ core) P.:& other)
            ]
      )
    ]

-- | @P@ against @\@/P@, for the paths chosen.
relativeCases :: (P.Path -> Bool) -> [(P.Path, P.Path)]
relativeCases chosen =
  [ (p, P.One P.:/ p)
  | p <- drawn 20261114 1200 genEnded,
    chosen p,
    small 40 p
  ]

-- | 2.10: @ls P@ finds what @ls \@/P@ finds for relative @P@. A reading
-- of the condition is what it takes for relative.
lawRelative :: Law (P.Path -> Bool) (P.Path, P.Path)
lawRelative =
  Law
    { name = "@/P",
      reading = relative,
      without =
        [ ("P is relative", const True),
          ( "where a walk starts is read with every ~ applied",
            not . rooted . writtenWith False
          )
        ],
      count = 200,
      sample = \chosen -> take 200 (relativeCases chosen),
      firstFailure = firstToldApart
    }

spec_lawRelative :: Spec
spec_lawRelative = describe "2.10: a relative path starts as if @/ were written before it" do
  it "ls P finds what ls @/P finds, for relative P with ~ and @N" do
    cases <- committed lawRelative
    let chain = pushBackwards
        hasRoot = \case
          P.Absolute _ -> True
          l P.:/ r -> hasRoot l || hasRoot r
          l P.:& r -> hasRoot l || hasRoot r
          P.Union _ l r -> hasRoot l || hasRoot r
          P.Backwards p -> hasRoot p
          _ -> False
    -- many of them have an @N, and a ~ around more than a step
    length [() | (p, _) <- cases, hasRoot p, chain p /= p] `shouldSatisfy` (> 60)
    agrees 30 (tally cases)
  needsEveryClause lawRelative
  it "what is relative" do
    relative [path| a/@000000000001 |] `shouldBe` True
    relative [path| a/(@000000000001 | b) |] `shouldBe` True
    relative [path| @000000000001/a |] `shouldBe` False
    relative [path| a & @000000000001/b |] `shouldBe` False
    relative [path| ~(a/@000000000001) |] `shouldBe` False
    relative [path| ~(@000000000001/a) |] `shouldBe` True
  it "not for a path that is not relative" do
    lsFinds
      [(0, "a", 2), (1, "b", 2)]
      [ ([path| a & @000000000001/b |], [2]),
        ([path| (a & @000000000001/b)/@ |], [2]),
        ([path| @/(a & @000000000001/b) |], [])
      ]
  it "nor for one that is relative only as written, before its ~ is applied" do
    lsFinds
      [(0, "a", 1), (1, "b", 2), (0, "b", 3)]
      [ ([path| ~(a/@000000000001) |], [0]),
        ([path| @/~(a/@000000000001) |], []),
        ([path| ~(b/@000000000002) |], [1]),
        ([path| @/~(b/@000000000002) |], [])
      ]

-- * 2.9: n sides, at most n nodes

-- | The step written after the join of every side, to find the join by
-- in what the path compiles to.
marker :: P.Path
marker = P.Literal "m"

-- | How the bound of 2.9 is read: 'statedBound' is the document, and
-- each field set is one clause dropped.
data Bound = Bound
  { -- | the join of a side may be inside a conjunct of a group in the side
    joinInConjunct :: Bool,
    -- | a product may copy the conjunct the union stands in
    conjunctCopied :: Bool
  }

statedBound :: Bound
statedBound = Bound False False

-- | A union of two or three sides, each with one join and the 'marker'
-- after it, in a context; with how many sides it has. The join is
-- written in the side's own chain, or, in two unions of five, in a
-- conjunct of a group that each side is, which the condition leaves out;
-- as it leaves out the contexts in which a product copies a conjunct
-- that the union stands in (forks in the union's own stretch may copy
-- it: that is what the bound is about).
bounded :: Bound -> [(Int, P.Path)]
bounded = map snd . boundedFrom True 20261115 1500

-- | 200 unions of 2.9 within the condition with nothing aimed at a clause
-- ('boundedFrom'): their contexts are larger than those of 'bounded', a
-- third of which are made of steps.
unaimedBounded :: [(Int, P.Path)]
unaimedBounded = take 200 (map snd (boundedFrom False 20261115 1500 statedBound))

-- | 'bounded' for a seed and a number of draws, each path with the
-- context its union is written in. Without the first argument nothing is
-- aimed at a clause: the contexts are those of @'genHoledAimed' False@,
-- and the join is in a conjunct in one side of four, each side by
-- itself.
boundedFrom :: Bool -> Int -> Int -> Bound -> [(P.Path, (Int, P.Path))]
boundedFrom aimed seed draws how =
  [ (context, (length sides, p))
  | (context, sides) <- drawn seed draws ((,) <$> contexts <*> genSides),
    how.conjunctCopied || uncopied stated {ownStretch = False} context,
    how.joinInConjunct || all (writtenJoin . written) sides,
    let p = plug (foldl1Ex' (P.:||) sides) context,
    fewAlternatives 60 p
  ]
  where
    walks = Allow {joins = False, shares = False, forks = False, unions = True, reversals = True, wilds = True}
    contexts
      | aimed = frequency [(2, genHoled), (1, genThrice)]
      | otherwise = genHoledAimed False
    genSides
      | aimed = do
          n <- elements [2, 2, 3]
          alike <- elements [True, False]
          inConjunct <- frequency [(3, pure False), (2, pure True)]
          sides <- vectorOf n (side (pure inConjunct))
          pure (if alike then replicate n (headEx sides) else sides)
      | otherwise = do
          n <- elements [2, 2, 3]
          alike <- elements [True, False]
          sides <- vectorOf n (side (frequency [(3, pure False), (1, pure True)]))
          pure (if alike then replicate n (headEx sides) else sides)
    side genInConjunct = do
      walk <- choose (1, 2) >>= genPath walks
      join' <- elements [P.One, P.One, P.ExcludingNIDs (singletonSet (smallNID 1))]
      after' <- frequency [(2, pure []), (1, (\x -> [x]) <$> genStep)]
      other <- genStep
      inConjunct <- genInConjunct
      let joined = sequenced ([walk, join', marker] <> after')
      pure (if inConjunct then joined P.:& other else joined)
    -- three walks through the hole, which is one more than two sides
    -- have: a fork of three next to it, or next to the group it is in a
    -- conjunct of
    genThrice = do
      fork <- (\a b c -> a P.:& b P.:& c) <$> genStep <*> genStep <*> genStep
      x <- genStep
      y <- genStep
      elements
        [ hole P.:/ fork,
          fork P.:/ hole,
          (hole P.:& y) P.:/ fork,
          fork P.:/ ((hole P.:/ x) P.:& y)
        ]

-- | 2.9: a union of @n@ sides, each with a join in its own chain, is at
-- most @n@ nodes.
lawBound :: Law Bound (Int, P.Path)
lawBound =
  Law
    { name = "n sides, n nodes",
      reading = statedBound,
      without =
        [ ( "the join of each side is written in the side's own chain",
            statedBound {joinInConjunct = True}
          ),
          ( "no product copies a conjunct that the union stands in",
            statedBound {conjunctCopied = True}
          )
        ],
      count = 200,
      sample = take 200 . bounded,
      firstFailure = \paths ->
        listToMaybe [i | (i, (n, p)) <- zip [1 ..] paths, mostJoinNodes p > n]
    }

-- | In what a path compiles to, the most nodes the joins of the sides
-- are in one alternative (the vertices the 'marker' steps leave from),
-- and the most routes that go through the union in one alternative.
joinNodes :: P.Path -> (Int, Int)
joinNodes p =
  ( most
      [ length . ordNub $
          [ if h.direction == Out then origin else h.target
          | r <- toList a.routes,
            (origin, h) <- zip (r.root : map (.target) r.hops) r.hops,
            h.matcher == MLiteral "m"
          ]
      | a <- compiled
      ],
    most
      [ length [() | r <- toList a.routes, any ((== MLiteral "m") . (.matcher)) r.hops]
      | a <- compiled
      ]
  )
  where
    compiled = toList (compilePath p).alternatives
    most = maximumEx . (0 :)

mostJoinNodes :: P.Path -> Int
mostJoinNodes = fst . joinNodes

-- | @X/(S1/\@ | … | Sn/\@)/(c1 & … & ck)@ against the @|@ of every way to
-- give each continuation a side, with the continuations that have the
-- same side leaving from its one join: @X/S1/\@/(c1 & c3) & X/S2/\@/c2@.
-- For three continuations the other path has also the path in which each
-- has a side of the first kind to itself, which is a node more than the
-- union has sides when it has two.
assigned :: Bool -> [(P.Path, P.Path)]
assigned oneEach =
  [ pair
  | (prefix, sides, continuations) <-
      drawn 20261116 400 $
        (,,)
          <$> frequency [(2, pure []), (3, (\x -> [x]) <$> walk)]
          <*> (elements [2, 2, 3] >>= \n -> elements [True, False] >>= sidesOf n)
          <*> (elements [2, 3, 3] >>= \k -> vectorOf k walk),
    let through side rest = sequenced (prefix <> [side, rest])
        ways =
          [ conj
              [ through side (conj [c | (c, j) <- zip continuations way, j == i])
              | (i, side) <- zip [0 :: Int ..] sides,
                i `elem` way
              ]
          | way <- replicateM (length continuations) [0 .. length sides - 1]
          ]
        apart' = conj [through (headEx sides) c | c <- continuations]
        pair =
          ( through (foldl1Ex' (P.:||) sides) (conj continuations),
            foldl1Ex' (P.:||) (ways <> [apart' | oneEach])
          ),
    bothSmall 60 pair
  ]
  where
    walk = choose (1, 2) >>= \n -> sequenced <$> vectorOf n genStep
    sidesOf n alike = do
      sides <- vectorOf n ((\w -> w P.:/ P.One) <$> walk)
      pure (if alike then replicate n (headEx sides) else sides)

spec_lawSidesAndNodes :: Spec
spec_lawSidesAndNodes = describe "2.9: a union of n sides, each with a join in its own chain, is at most n nodes" do
  it "in every alternative, however many routes take the sides" do
    cases <- map (\(n, p) -> (n, p, joinNodes p)) <$> committed lawBound
    -- in many of them more routes go through the union than it has sides
    length [() | (n, _, (_, routes)) <- cases, routes > n] `shouldSatisfy` (> 50)
    [(n, shown p) | (n, p, (nodes, _)) <- cases, nodes > n] `shouldBe` []
    -- nor is it passed by the unions in larger contexts
    length unaimedBounded `shouldBe` 200
    [(n, shown p) | (n, p) <- unaimedBounded, mostJoinNodes p > n] `shouldBe` []
    -- and the bound is reached
    length [() | (n, _, (nodes, _)) <- cases, nodes == n] `shouldSatisfy` (> 70)
  needsEveryClause lawBound
  it "the union is every way to give each continuation a side" do
    let cases = take 40 (assigned False)
    length cases `shouldBe` 40
    allSame "up to the order of the routes" upToRouteOrder cases
    findsOneWithin 40 (firstApart (assigned True))
  it "the examples of the document" do
    lsFinds
      [(0, "a", 1), (1, "d", 9), (0, "a", 2), (2, "e", 9)]
      [ ([path| a/(d & e) |], [9]),
        ([path| a/@/(d & e) |], []),
        ([path| (a/@ | a/@)/(d & e) |], [9]),
        ([path| (a/@ | @/a/@)/(d & e) |], [9]),
        ([path| (a/@ ++ a/@)/(d & e) |], []),
        ([path| (a | a/@)/(d & e) |], [9])
      ]
    lsFinds
      [(0, "a", 1), (0, "a", 2), (0, "a", 3), (1, "d", 9), (2, "e", 9), (3, "f", 9)]
      [ ([path| (a/@ | a/@)/(d & e & f) |], []),
        ([path| (a/@ | a/@ | a/@)/(d & e & f) |], [9]),
        ([path| (a/@ | (a/@ | a/@))/(d & e & f) |], [9]),
        ([path| (a/@ | (a/@ ++ a/@))/(d & e & f) |], [])
      ]
    lsFinds
      [(0, "a", 1), (0, "a", 2), (1, "d", 9), (1, "e", 9), (2, "f", 9)]
      [ ([path| a/@/(d & e & f) |], []),
        ([path| (a/@ | a/@)/(d & e & f) |], [9])
      ]
  it "not for a join inside a conjunct of a group in the side" do
    lsFinds
      [ (0, "a", 1),
        (0, "a", 2),
        (0, "a", 3),
        (0, "a", 4),
        (1, "d", 9),
        (2, "e", 9),
        (3, "f", 9),
        (4, "d", 8),
        (4, "e", 8),
        (4, "f", 8)
      ]
      [ ([path| ((a/@ & a) | (a/@ & a))/(d & e & f) |], [8, 9]),
        ([path| (a/@ | a/@)/(d & e & f) & a/(d & e & f) |], [8])
      ]
    mostJoinNodes [path| ((a/@/m & z) | (a/@/m & z))/(d & e & f) |] `shouldBe` 3
    mostJoinNodes [path| (a/@/m | a/@/m)/(d & e & f) |] `shouldBe` 2
  it "nor over the copies of a conjunct that the union stands in" do
    lsFinds
      [(0, "p", 1), (0, "q", 2), (1, "a", 3), (2, "a", 4), (3, "d", 9), (4, "d", 9), (1, "z", 9), (2, "z", 9)]
      [ ([path| (p & q)/(a/@ | k)/d |], []),
        ([path| (p & q)/((a/@ | k)/d & z) |], [9]),
        ([path| p/(a/@ | k)/d & p/z & q/(a/@ | k)/d & q/z |], [9])
      ]
    lsFinds
      [ (0, "a", 1),
        (0, "a", 2),
        (0, "a", 3),
        (1, "d", 9),
        (2, "e", 9),
        (3, "f", 9),
        (0, "z", 4),
        (4, "d", 9),
        (4, "e", 9),
        (4, "f", 9)
      ]
      [ ([path| (a/@ | a/@)/(d & e & f) |], []),
        ([path| ((a/@ | a/@) & z)/(d & e & f) |], [9]),
        ([path| ((a/@ & z) | (a/@ & z))/(d & e & f) |], [9])
      ]
    mostJoinNodes [path| ((a/@/m | a/@/m) & z)/(d & e & f) |] `shouldBe` 3

-- * The order of the alternatives that match

spec_lawOrderOfAlternatives :: Spec
spec_lawOrderOfAlternatives = describe "the order of the alternatives that match" do
  let smallestWitness r = minimumEx (map (concreteRoutes r) (witnesses r))
      resolutionsOf w cur q = (resolveFrom (smallNID cur) w.graph q).resolutions
  it "is that of their smallest witnesses, whatever the order of the compiled patterns" do
    let cases =
          [ (p, w, cur, resolved)
          | (i, (_, p, _)) <- zip [0 ..] (holed 20261117 400),
            small 40 p,
            let q = asQuery p,
            w <- worldsFor i,
            cur <- worldNodes,
            matchesBound w cur q <= 2000,
            let resolved = resolutionsOf w cur q
          ]
        several = [c | c@(_, _, _, resolved) <- cases, length resolved > 1]
    length several `shouldSatisfy` (> 600)
    -- in many of them that is not the order of the patterns
    length
      [ ()
      | (_, _, _, resolved) <- several,
        map (.patternPath) resolved /= sort (map (.patternPath) resolved)
      ]
      `shouldSatisfy` (> 70)
    forM_ several \(p, w, cur, resolved) ->
      (shown p, shownWorld w, cur, map smallestWitness resolved)
        `shouldBe` (shown p, shownWorld w, cur, sort (map smallestWitness resolved))
  it "the alternative through a is first, though the pattern with b is" do
    let w = worldOf [(0, "a", 1), (0, "b", 2)]
        union' = [path| b |] P.:|| P.RegexMatch [re|^a$|]
        flipped = P.RegexMatch [re|^a$|] P.:|| [path| b |]
        firstLabel a = [h.matcher | r <- take 1 (toList a.routes), h <- r.hops]
    for_ [union', flipped] \p -> do
      map firstLabel (toList (asQuery p).alternatives)
        `shouldBe` [[MLiteral "b"], [MRegex [re|^a$|]]]
      map (map smallNIDIndex . toList . targets) (resolutionsOf w 0 (asQuery p))
        `shouldBe` [[Just 1], [Just 2]]
