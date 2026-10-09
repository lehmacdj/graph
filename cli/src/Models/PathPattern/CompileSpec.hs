-- | Tests for "Models.PathPattern.Compile": the examples and the laws in
-- docs/path-pattern-ir.md, and the compiler as it was before relational
-- unions, with an interpreter of its terms, as a reference that shares no
-- code with it.
--
-- 'compilePath' compiles every path, also one that no command runs
-- because a product in it would copy a meeting point ('checkCopies').
-- The random law tests here are about the compiler and keep those
-- paths: none of them leaves a path out for being refused. Where a test
-- is about what a command finds, it says how many of its paths are run
-- ('runs'), and a test of a refused path written out says so
-- ('refuses').
module Models.PathPattern.CompileSpec where

import Control.Monad.State.Strict qualified as S
import Effectful.State.Dynamic
import Graph.GraphMetadataEditing (runInMemoryGraphMetadataEditing)
import Graph.PathPattern.Resolve
import Models.Connect
import Models.Edge (Edge' (..))
import Models.Graph
  ( Graph,
    emptyGraph,
    insertEdges,
    insertNode,
    maybeNodeLookup,
  )
import Models.NID
import Models.Node
import Models.Path.Simple qualified as P
import Models.Path.TH (path)
import Models.PathPattern
import Models.PathPattern.Compile
import Models.PathPattern.Match
import Models.PathPattern.ResiduateSpec (genRepairCase)
import Models.PathPattern.Suggest (parseSurfacePath)
import Models.PathPattern.TestCorpus
import MyPrelude
import Test.QuickCheck.Gen (unGen)
import Test.QuickCheck.Random (mkQCGen)
import Utils.Testing

matchedRoutes :: Graph Text () -> P.Path -> Set [ConcreteRoute]
matchedRoutes g surface = matchedBy g (compilePath surface)

matchedBy :: Graph Text () -> PathQuery -> Set [ConcreteRoute]
matchedBy g query' =
  setFromList
    [ concreteRoutes r w
    | r <- resolved.resolutions,
      w <- witnesses r
    ]
  where
    resolved =
      resolveQuery corpusCurrent query'
        & runInMemoryGraphMetadataEditing
        & evalStateLocal g
        & runPureEff

spec_compileAlternatives :: Spec
spec_compileAlternatives =
  describe "each alternative with a choice of + sides" do
    forM_ corpusCompileQueries \surface -> it (show surface) do
      let chosen = compileAlternatives surface
      setFromList (map fst chosen) `shouldBe` (compilePath surface).alternatives
      forM_ chosen \(alternative, choice) -> do
        (show choice, hasUnion choice) `shouldBe` (show choice, False)
        toList (compilePath choice).alternatives `shouldBe` [alternative]
  where
    hasUnion = \case
      P.Union {} -> True
      l P.:/ r -> hasUnion l || hasUnion r
      l P.:& r -> hasUnion l || hasUnion r
      P.Backwards p -> hasUnion p
      _ -> False

spec_compileEquivalences :: Spec
spec_compileEquivalences = describe "equivalent paths" do
  forM_ (corpusEquivalences ++ corpusLargeEquivalences) \(a, b) ->
    it (show a <> " == " <> show b) $ compilePath a `shouldBe` compilePath b

-- | On nodes 1 (current) and 2 with one edge 1 -a-> 2, an @ alternative
-- before @N tests the current node instead of jumping to N.
spec_compileAtAlternatives :: Spec
spec_compileAtAlternatives = describe "@ alternatives before @N" do
  let g =
        insertEdges
          [Edge (smallNID 1) "a" (smallNID 2)]
          ( insertNode (emptyNode (smallNID 2)) $
              insertNode (emptyNode (smallNID 1)) emptyGraph
          ) ::
          Graph Text ()
      reached surface =
        foldMap targets (resolveIn g (compilePath surface)).resolutions
  it "(@ + b)/@2 does not reach 2 without a b edge" $
    reached [path| (@ ++ b)/@000000000002 |] `shouldBe` mempty
  it "(@ + a)/@2 reaches 2 along a" $
    reached [path| (@ ++ a)/@000000000002 |]
      `shouldBe` singletonSet (smallNID 2)
  it "(@ + b)/@1 reaches the current node" $
    reached [path| (@ ++ b)/@000000000001 |]
      `shouldBe` singletonSet (smallNID 1)
  where
    resolveIn g q =
      resolveQuery corpusCurrent q
        & runInMemoryGraphMetadataEditing
        & evalStateLocal g
        & runPureEff

-- | On 0 -a-> 1 -c-> 3 and 0 -b-> 2 -d-> 3, each copy of a prefix union
-- chooses its own label, like a wildcard does.
spec_compilePerCopyChoice :: Spec
spec_compilePerCopyChoice = describe "each copy chooses its | side" do
  let n = smallNID
      g =
        graphOf
          [0, 1, 2, 3]
          [(0, "a", 1), (1, "c", 3), (0, "b", 2), (2, "d", 3)]
      reached surface =
        foldMap targets (resolveFrom (n 0) g (compilePath surface)).resolutions
  it "*/(c & d)/@ reaches 3" $
    reached [path| */(c & d)/@ |] `shouldBe` singletonSet (n 3)
  it "(a | b | c | d)/(c & d)/@ reaches 3" $
    reached [path| (a | b | c | d)/(c & d)/@ |] `shouldBe` singletonSet (n 3)
  it "(a | b)/@/(c & d)/@ chooses once, so reaches nothing" $
    reached [path| (a | b)/@/(c & d)/@ |] `shouldBe` mempty
  it "(a ++ b ++ c ++ d)/(c & d)/@ chooses once, so reaches nothing" $
    reached [path| (a ++ b ++ c ++ d)/(c & d)/@ |] `shouldBe` mempty
  it "(~c & ~d)/(~a | ~b) from 3 reaches 0 along both" $
    foldMap
      targets
      ( resolveFrom
          (n 3)
          g
          (compilePath [path| (~c & ~d)/(~a | ~b)/@ |])
      ).resolutions
      `shouldBe` singletonSet (n 0)

-- | Graphs on which the reviewer of per-copy choice found a union side
-- removing results.
spec_compileUnionRegressions :: Spec
spec_compileUnionRegressions = describe "adding a | side keeps results" do
  let reached cur g surface =
        foldMap targets (resolveFrom (smallNID cur) g (compilePath surface)).resolutions
      g1 =
        graphOf
          [0, 1, 2, 3, 4, 5, 9]
          [ (0, "c", 1),
            (1, "b", 3),
            (0, "d", 2),
            (2, "b", 4),
            (1, "x", 9),
            (2, "x", 9),
            (3, "y", 9),
            (4, "y", 9)
          ]
      g2 =
        graphOf
          [0, 1, 2, 3, 9]
          [(0, "a", 1), (1, "c", 9), (0, "b", 2), (2, "d", 9), (0, "x", 3), (3, "f", 9)]
      g3 =
        graphOf [0, 1, 2, 9] [(0, "a", 1), (0, "b", 2), (1, "c", 9), (2, "d", 9)]
  it "a side with several outputs before a joined conjunct" do
    reached 0 g1 [path| (c & d)/(b/!{@000000000005}/y & x)/@ |]
      `shouldBe` singletonSet (smallNID 9)
    reached 0 g1 [path| (c & d | e)/(b/!{@000000000005}/y & x)/@ |]
      `shouldBe` singletonSet (smallNID 9)
  it "a nested union in a suffix chosen for each copy" do
    reached 0 g2 [path| (x | a & b)/((c | d) & f)/@ |]
      `shouldBe` singletonSet (smallNID 9)
    reached 0 g2 [path| (x | a & b)/(e | (c | d) & f)/@ |]
      `shouldBe` singletonSet (smallNID 9)
  it "a nested union in a joining side chosen for each copy" do
    reached 0 g3 [path| (a & b)/((c | d)/@) |]
      `shouldBe` singletonSet (smallNID 9)
    -- each output's copy also chooses @ on its own, ending at a or b
    reached 0 g3 [path| (a & b)/(@ | (c | d)/@) |]
      `shouldBe` setFromList (map smallNID [1, 2, 9])
  it "three levels of unions with several outputs" $
    length (compilePath [path| (a & b | x)/(c & d | e)/(f & g | h) |]).alternatives
      `shouldSatisfy` (> 0)

spec_compileJoinedSides :: Spec
spec_compileJoinedSides = describe "copies share a joined side" do
  let count p = length (compilePath p).alternatives
  it "((a & b)/@ | c)/(x & y) has 4 alternatives" $
    count [path| ((a & b)/@ | c)/(x & y) |] `shouldBe` 4
  it "a/(@1 | @2)/(x & y) lets each copy choose its node" do
    count [path| a/(@000000000001 | @000000000002)/(x & y) |] `shouldBe` 4
    compilePath [path| a/(@000000000001 | @000000000002)/(x & y) |]
      `shouldBe` compilePath
        [path| a/@000000000001/(x & y) | a/@000000000002/(x & y)
               | a/@000000000001/x & a/@000000000002/y
               | a/@000000000002/x & a/@000000000001/y |]
  it "a/(@1 ++ @2)/(x & y) chooses one node" $
    count [path| a/(@000000000001 ++ @000000000002)/(x & y) |] `shouldBe` 2

-- | A join written before a union joins for every side, however the
-- sequence is nested.
spec_compileJoinBeforeUnion :: Spec
spec_compileJoinBeforeUnion = describe "a join before a union stays outside" do
  let same a b = compilePath a `shouldBe` compilePath b
  it "(b & a)/(@/(@ | c))" $
    same [path| (b & a)/(@/(@ | c)) |] [path| (b & a)/@/(@ | c) |]
  it "~((@ | ~c)/@/(~b & ~a))" $
    same [path| ~((@ | ~c)/@/(~b & ~a)) |] [path| (b & a)/@/(@ | c) |]
  it "(b & a)/(x/@/(d | c/@))" $
    same [path| (b & a)/(x/@/(d | c/@)) |] [path| (b & a)/x/@/(d | c/@) |]
  it "the union's sides are still chosen by each copy after the join" $
    length (compilePath [path| (b & a)/@/(@ | c)/(x & y) |]).alternatives
      `shouldBe` 4

-- | Every side of a union that is written is a side of its own, also
-- when another side is written the same or compiles to the same thing
-- ('spec_compileEverySide' has the results this is for).
spec_compileSameSides :: Spec
spec_compileSameSides = describe "a side written twice is two sides" do
  let same a b = compilePath a `shouldBe` compilePath b
      count q = length (compilePath q).alternatives
      n1 = smallNID 1
      p = (P.Literal "a" P.:|| P.ExcludingNIDs (singletonSet n1)) P.:/ [path| b/@ |]
      p' = (P.ExcludingNIDs (singletonSet n1) P.:|| P.Literal "a") P.:/ [path| b/@ |]
      before x = x P.:/ [path| d & e |]
  it "(a/@ | a/@/@)/(d & e)" do
    -- a/@/@ is a/@ (joins right after one another are one join), and the
    -- union is not a/@: the copies before d and e may choose one each
    same [path| (a/@ | a/@/@)/(d & e) |] [path| (a/@ | a/@)/(d & e) |]
    same [path| (a/@ | a/@)/(d & e) |] [path| a/@/(d & e) | a/@/d & a/@/e |]
    count [path| (a/@ | a/@)/(d & e) |] `shouldBe` 2
    count [path| a/@/(d & e) |] `shouldBe` 1
  it "(p | q) | p and p | (q | p) are the three sides p, q and p" do
    same (before ((p P.:|| [path| x |]) P.:|| p)) (before (p P.:|| ([path| x |] P.:|| p)))
    same (before ((p P.:|| [path| x |]) P.:|| p)) (before ((p P.:|| p) P.:|| [path| x |]))
    -- and not the two sides p and q
    count (before ((p P.:|| [path| x |]) P.:|| p))
      `shouldSatisfy` (> count (before (p P.:|| [path| x |])))
  it "p with its inner union written the other way around is p again" do
    same (before (p P.:|| p')) (before (p P.:|| p))
    same (before p') (before p)
    compilePath (before (p P.:|| p')) `shouldNotBe` compilePath (before p)
  it "p | p/@ and p | @/p are two sides" do
    -- p ends with a join, so p/@ is p written again
    same (before (p P.:|| (p P.:/ P.One))) (before (p P.:|| p))
    compilePath (before (p P.:|| (p P.:/ P.One))) `shouldNotBe` compilePath (before p)
    compilePath (before (p P.:|| (P.One P.:/ p))) `shouldNotBe` compilePath (before p)
    -- a leading @ is kept as written; where the path starts it joins
    -- nothing, and the side it starts is a side of its own all the same
    count [path| (a/@ | @/a/@)/(d & e) |] `shouldBe` 2
    same [path| (a/@ | @/a/@)/(d & e) |] [path| (a/@ | a/@)/(d & e) |]
  it "@/a is a where nothing forks after the @" do
    same [path| @/a |] [path| a |]
    same [path| (@/a & b)/(c & d) |] [path| (a & b)/(c & d) |]
    -- two sides, but neither has a join that the copies could share
    -- after it: whichever each copy chooses, d and e have an a each
    same [path| (a | @/a)/(d & e) |] [path| a/(d & e) |]
    same [path| (a | a)/(d & e) |] [path| a/(d & e) |]
    same [path| @/(a & b)/(c & d) |] [path| (a & b)/(c & d) |]
  it "p | @/p is two sides when p forks, and then no command runs it" do
    -- the copy before @/f would choose between a and b that meet before
    -- f and a and b apart: @/(a & b) is a meeting point that the group
    -- after it copies
    refuses [path| (((a & b) | @/(a & b)) & c)/(@/f & e) |]
    count [path| ((a & b) & c)/(@/f & e) |] `shouldBe` 1
    -- where nothing copies it the two sides stay two
    runsQuietly [path| q/(((a & b) | @/(a & b)) & c) |]
    count [path| q/(((a & b) | @/(a & b)) & c) |] `shouldBe` 2
  it "an unjoined side and a joined one differ" $
    compilePath [path| (a | a/@)/(d & e) |]
      `shouldBe` compilePath [path| a/(d & e) | a/@/(d & e) |]
  it "a test repeated after an @ side" do
    -- the second side is the first written again (a test passed twice
    -- is passed once): two sides
    same
      [path| ((c | @)/!{@000000000001} | (c | @)/!{@000000000001}/!{@000000000001})/(e & d) |]
      [path| ((c | @)/!{@000000000001} | (c | @)/!{@000000000001})/(e & d) |]
    same
      [path| (@/!{@000000000001} | !{@000000000001}/@ | !{@000000000001})/(e & d) |]
      [path| (!{@000000000001} | !{@000000000001} | !{@000000000001})/(e & d) |]
  it "a test that an @N side already passes" do
    same
      [path| ((~a | * | @000000000001)/@000000000001 | (~a | * | @000000000001)/@000000000001/!{@000000000002})/(d & b) |]
      [path| ((~a | * | @000000000001)/@000000000001 | (~a | * | @000000000001)/@000000000001)/(d & b) |]
    -- every side is the node 1, which is one node whichever side a copy
    -- chooses: here the union is what one side is
    same
      [path| (@000000000001 | @000000000001/@ | @000000000001/@000000000001 | @000000000001/!{@000000000002})/(d & b) |]
      [path| @000000000001/(d & b) |]
  it "but @/@N, which tests the current node, is not the root @N" do
    length (compilePath [path| (@/@000000000001 | @000000000001)/(e & d) |]).alternatives
      `shouldBe` 2
    length (compilePath [path| @000000000001/(e & d) |]).alternatives
      `shouldBe` 1
  it "and conjuncts in another order are another side, as their routes are" do
    -- each of the two copies chooses one of two sides
    length (compilePath [path| (c & * | * & c)/(!{@000000000001} & d) |]).alternatives
      `shouldBe` 4
    length (compilePath [path| (c & *)/(!{@000000000001} & d) |]).alternatives
      `shouldBe` 1
    -- with joins the two sides are two walks: one before each of c and d
    length (alternativesUpToOrder [path| ((a & b)/@ | (b & a)/@)/(c & d) |])
      `shouldBe` 2
    length (alternativesUpToOrder [path| (a & b)/@/(c & d) |]) `shouldBe` 1

-- | A leading @ is a join, kept as it is written, so that an @N after it
-- tests the current node.
spec_compileRootJoin :: Spec
spec_compileRootJoin = describe "a leading @ before an @N conjunct" do
  let (v0, v1, v2) = (var 0, var 1, var 2)
      isOne = [(current, exactly 1)]
      neverJumps p =
        [ r.root
        | a <- toList (compilePath p).alternatives,
          r <- toList a.routes,
          r.root /= current
        ]
          `shouldBe` []
  it "@/(c & @)/(@1 & d)" $
    [path| @/(c & @)/(@000000000001 & d) |]
      `compiles` [ patWith
                     isOne
                     [ route current [out 0 "c" current],
                       route current [out 1 "c" v0, out 2 "d" v1],
                       route current [],
                       route current [out 3 "d" v2]
                     ]
                 ]
  it "@/(c & @)/(d & @1)/@" $
    [path| @/(c & @)/(d & @000000000001)/@ |]
      `compiles` [ patWith
                     isOne
                     [ route current [out 0 "c" v0, out 1 "d" current],
                       route current [out 2 "c" current],
                       route current [out 3 "d" current],
                       route current []
                     ]
                 ]
  it "@/(@ & c)/(d & @1)" $
    [path| @/(@ & c)/(d & @000000000001) |]
      `compiles` [ patWith
                     isOne
                     [ route current [out 0 "d" v0],
                       route current [],
                       route current [out 1 "c" v1, out 2 "d" v2],
                       route current [out 3 "c" current]
                     ]
                 ]
  it "@/((@ ++ c) & e)/(@1 & d)" do
    let p = [path| @/((@ ++ c) & e)/(@000000000001 & d) |]
    compilePath p
      `shouldBe` compilePath
        [path| @/(@ & e)/(@000000000001 & d) ++ @/(c & e)/(@000000000001 & d) |]
    neverJumps p
  it "@/((@ | c)/(@1 & d))" do
    let p = [path| @/((@ | c)/(@000000000001 & d)) |]
    compilePath p `shouldBe` compilePath [path| (@ | c)/(@000000000001 & d) |]
    length (compilePath p).alternatives `shouldBe` 4
    neverJumps p
  it "sides that differ in a leading @ before a fork are two sides, and neither jumps" do
    -- (c & @)/(@1 & d) tests the current node as @/(c & @)/(@1 & d) does
    -- (before chain-scoped joins it jumped to node 1)
    let both = [path| ((@/(c & @) | (c & @)) & g)/(@000000000001 & d) |]
    -- no command runs it: the group after it would copy @/(c & @), in
    -- which two walks leave the @ ('spec_compileCopiedMeetingPoints')
    refuses both
    refuses [path| (@/(c & @) & g)/(@000000000001 & d) |]
    neverJumps both
    -- where nothing copies it, it runs, and does not jump either
    let alone = [path| q/((@/(c & @) | (c & @)) & g)/@000000000001 |]
    isRight (checkCopies alone) `shouldBe` True
    neverJumps alone
  it "a leading @ before a side that does not fork changes nothing" $
    alternativesUpToOrder [path| ((@/c | c) & g)/(@000000000001 & d) |]
      `shouldBe` alternativesUpToOrder [path| (c & g)/(@000000000001 & d) |]

-- | A union of many joined sides costs what its alternatives cost, not a
-- reading for each subset of its sides (docs/path-pattern-ir.md, Unions).
spec_compileManyJoinedSides :: Spec
spec_compileManyJoinedSides = describe "a union of many joined sides" do
  let nodes n = foldl1Ex' (P.:||) [P.Absolute (smallNID i) | i <- [1 .. n]]
      walks n =
        foldl1Ex'
          (P.:||)
          [P.Literal ("x" <> tshow i) P.:/ P.One | i <- [0 .. n - 1 :: Int]]
      -- the allowance is that of 'spec_compileContradictions': a search
      -- that does not end fails instead of hanging; it measures nothing
      counts name p n = it name do
        compiled <-
          timeout 60000000 (evaluate (length (compilePath p).alternatives))
        compiled `shouldBe` Just n
  counts "(@1 | … | @12)/(a & b)" (nodes 12 P.:/ [path| a & b |]) 144
  counts "(@1 | … | @20)/(a & b)" (nodes 20 P.:/ [path| a & b |]) 400
  counts "c & (@1 | … | @12)/a" ([path| c |] P.:& (nodes 12 P.:/ [path| a |])) 12
  counts "(x0/@ | … | x15/@)/(a & b)" (walks 16 P.:/ [path| a & b |]) 256
  counts "c & (@1 | … | @40)/a" ([path| c |] P.:& (nodes 40 P.:/ [path| a |])) 40
  counts "(a & b)/(@1 | … | @30)" ([path| a & b |] P.:/ nodes 30) 900
  counts
    "sides of several kinds"
    ((nodes 12 P.:|| walks 6 P.:|| [path| a | b | c & d | e & f |]) P.:/ [path| a & b |])
    -- 22 sides for each of two copies, and nothing besides
    484

-- | Unions cost what their alternatives cost: unions that are never
-- sequenced with each other, and unions of single chains, are compiled
-- in milliseconds (docs/path-pattern-ir.md, Unions).
spec_compileIndependentUnions :: Spec
spec_compileIndependentUnions = describe "unions whose order cannot matter" do
  let union' i = P.Literal ("a" <> tshow i) P.:|| P.Literal ("b" <> tshow i)
      conjoined n f = foldl1Ex' (P.:&) [f (union' i) | i <- [1 .. n :: Int]]
      chained n = foldl1Ex' (P.:/) [union' i | i <- [1 .. n :: Int]]
      -- the allowance is that of 'spec_compileContradictions': a search
      -- that does not end fails instead of hanging; it measures nothing
      counts name p n = it name do
        compiled <-
          timeout 60000000 (evaluate (length (compilePath p).alternatives))
        compiled `shouldBe` Just n
  counts "six unions joined by &" (conjoined 6 id) 64
  counts "seven unions joined by &" (conjoined 7 id) 128
  counts "eight unions joined by &" (conjoined 8 id) 256
  counts "nine unions joined by &" (conjoined 9 id) 512
  counts "eight (a | b)/x joined by &" (conjoined 8 (P.:/ [path| x |])) 256
  counts "a chain of nine unions, & z" (chained 9 P.:& [path| z |]) 512
  counts
    "(a | b)/(c | d)/(e | f)/(g | h)/(m & n)"
    [path| (a | b)/(c | d)/(e | f)/(g | h)/(m & n) |]
    256
  it "a union chooses once for each copy it has in the end" do
    -- two unions with conjunctions copy each other: choosing either one
    -- first alone gives 6 of these 8
    length (alternativesUpToOrder [path| ((a & b) | x)/((c & d) | e) |])
      `shouldBe` 8
    -- Each of the four routes (through c or d, then a or b) chooses a
    -- side of both unions. A route that chose w/@ is cut there, so the
    -- routes that differ only after it are one copy of the first union,
    -- and choose the same side of it; likewise for the second union
    -- after x/@. (Choosing one union in the copies it has while the
    -- other is not chosen, in either order, gave 221 up to the order of
    -- routes.)
    let staged = [path| (c & d)/(x/@ | y)/(w/@ | z)/(a & b) |]
        routes = [(l, r) | l <- "cd", r <- "ab"] :: [(Char, Char)]
        sides = [(f, s) | f <- [True, False], s <- [True, False]]
        allowed =
          [ picks
          | picks <- traverse (const sides) routes,
            let chose = zip routes picks,
            and
              [ f == f'
              | ((l, _), (f, s)) <- chose,
                ((l', _), (f', s')) <- chose,
                l == l',
                s && s'
              ],
            and
              [ s == s'
              | ((_, r), (f, s)) <- chose,
                ((_, r'), (f', s')) <- chose,
                r == r',
                f && f'
              ]
          ]
    length allowed `shouldBe` 152
    length (compilePath staged).alternatives `shouldBe` 152
    ( alternativesUpToOrder [path| c/y/z/a & (c/x & d/x)/@/(w/@/(b & a)) |]
        `difference` alternativesUpToOrder staged
      )
      `shouldBe` mempty
    -- no two edges of an alternative are the same step between the same
    -- two nodes
    let edges a =
          ordNub
            [ (h.edge, (source, h.direction, h.matcher, h.target))
            | r <- toList a.routes,
              (source, h) <- zip (r.root : map (.target) r.hops) r.hops
            ]
        doubled a = length (ordNub (map snd (edges a))) /= length (edges a)
    filter doubled (toList (compilePath staged).alternatives) `shouldBe` []

-- | Leaving a side out of a union never adds an alternative, and the order
-- its sides are written in does not matter at all.
spec_compileSubUnions :: Spec
spec_compileSubUnions = describe "leaving out a side of a union" do
  let isWithin small big =
        (alternativesUpToOrder small `difference` alternativesUpToOrder big)
          `shouldBe` mempty
  it "a side that absorbs a prefix left out" $
    [path| (x/@ | y/@)/(c & a | d)/(e & f) |]
      `isWithin` [path| (x/@ | y/@)/(c & a | d | b/@)/(e & f) |]
  it "a side with several outputs left out" $
    [path| (a & b | c & d/@)/((e & f)/(g | h/@)) |]
      `isWithin` [path| (a & b | c & d/@ | c & a)/((e & f)/(g | h/@)) |]
  it "random unions of three to six sides" do
    let cases =
          unGen
            ( vectorOf 150 $
                (,,,)
                  <$> (choose (3, 6) >>= \n -> vectorOf n (elements pool))
                  <*> elements prefixes
                  <*> elements suffixes
                  <*> choose (0 :: Int, 6)
            )
            (mkQCGen 20261005)
            30
        plug ctx before suf x = case ctx of
          0 -> x P.:/ suf
          1 -> before P.:/ x P.:/ suf
          2 -> [path| q |] P.:& (before P.:/ x)
          3 -> (x P.:& before) P.:/ suf
          4 -> before P.:/ x P.:/ P.One P.:/ suf
          -- a union with a walk after a conjunction that holds the union
          -- at the root of the path
          5 -> (x P.:& [path| c |]) P.:/ [path| y | d/@ |] P.:/ suf
          _ -> (x P.:& before) P.:/ [path| w/@ | z |] P.:/ suf
        union' = foldl1Ex' (P.:||)
        withinLimit x =
          fewAlternatives 300 x
        checked =
          [ (plug ctx before suf (union' fewer), full)
          | (sides', before, suf, ctx) <- cases,
            let sides = ordNub sides'
                full = plug ctx before suf (union' sides),
            length sides > 2,
            withinLimit full,
            i <- [0 .. length sides - 1],
            let fewer = [side | (j, side) <- zip [0 :: Int ..] sides, j /= i]
          ]
    length checked `shouldSatisfy` (> 200)
    forM_ checked \(fewer, full) ->
      ( show (fewer, full),
        alternativesUpToOrder fewer `difference` alternativesUpToOrder full
      )
        `shouldBe` (show (fewer, full), mempty)
    -- the sides written the other way around are the same union
    forM_ cases \(sides', before, suf, ctx) -> do
      let sides = ordNub sides'
          full = plug ctx before suf (union' sides)
      when (length sides > 1 && withinLimit full) $
        (show full, compilePath (plug ctx before suf (union' (reverse sides))))
          `shouldBe` (show full, compilePath full)
  where
    pool =
      [ [path| a |],
        [path| b/c |],
        [path| d |],
        [path| x/@ |],
        [path| y/@ |],
        [path| b/@ |],
        [path| (a & b)/@ |],
        [path| @000000000001 |],
        [path| @000000000002 |],
        [path| !{@000000000001} |],
        [path| z/!{@000000000001} |],
        [path| a & b |],
        [path| c & a |],
        [path| c & d/@ |],
        [path| x/@ & d |],
        [path| @ |],
        [path| @/c |],
        [path| @/(c & d) |],
        [path| @/(a & b) |],
        [path| a/@/(c & d) |],
        [path| (a | x/@)/c |],
        [path| c/(d | @) |],
        [path| x ++ y/@ |]
      ]
    prefixes =
      [ [path| w |],
        [path| x/@ | y/@ |],
        [path| c & a | d |],
        [path| @000000000001 | @000000000002 |],
        [path| g & h |],
        [path| (g & h)/@ |],
        [path| x | y/@ |]
      ]
    suffixes =
      [ [path| e & f |],
        [path| (e | f) & g |],
        [path| @/(e & f) |],
        [path| (e & f)/@ |],
        [path| e & @ |],
        [path| e & f | g |],
        [path| (e | @) & f |],
        [path| @000000000001 & e |]
      ]

-- | A side with a join is one walk for all the copies that choose it,
-- however many conjunctions made the copies: writing the distribution of
-- the conjunctions out changes nothing
-- (@(c & d)/(a & b) = c/a & c/b & d/a & d/b@).
spec_compileSharedWalks :: Spec
spec_compileSharedWalks = describe "a joined side is one walk" do
  let reached g surface =
        foldMap
          targets
          (resolveFrom (smallNID 0) g (compilePath (surface P.:/ P.One))).resolutions
      same a b = alternativesUpToOrder a `shouldBe` alternativesUpToOrder b
  it "for copies made by two conjunctions" do
    let staged = [path| (x/*/@ | y)/(c & d)/(a & b) |]
        written = [path| (x/*/@ | y)/(c/a & c/b & d/a & d/b) |]
        -- x/k/c/a, x/l/d/b, y/d/a and y/c/b all end at 9, but no one walk
        -- of x/* is before both c/a and d/b
        g =
          graphOf
            [0, 1, 2, 3, 4, 5, 6, 7, 8, 9]
            [ (0, "x", 1),
              (1, "k", 2),
              (1, "l", 3),
              (2, "c", 4),
              (4, "a", 9),
              (3, "d", 5),
              (5, "b", 9),
              (0, "y", 6),
              (6, "d", 7),
              (7, "a", 9),
              (6, "c", 8),
              (8, "b", 9)
            ]
    same staged written
    ( alternativesUpToOrder [path| x/*/@/(c/a & d/b) & y/c/b & y/d/a |]
        `difference` alternativesUpToOrder staged
      )
      `shouldBe` mempty
    reached g staged `shouldBe` mempty
    reached g written `shouldBe` mempty
  it "for copies a union with a conjunction makes" do
    let staged = [path| (x/@ | y)/(a & b | c)/(c & d) |]
        -- y/a/c, x/b/c and x/c/d all end at 9
        g =
          graphOf
            [0, 1, 2, 3, 4, 5, 9]
            [ (0, "y", 1),
              (1, "a", 2),
              (2, "c", 9),
              (0, "x", 3),
              (3, "b", 4),
              (4, "c", 9),
              (3, "c", 5),
              (5, "d", 9)
            ]
    ( alternativesUpToOrder [path| y/a/c & x/@/(b/c & c/d) |]
        `difference` alternativesUpToOrder staged
      )
      `shouldBe` mempty
    reached g staged `shouldBe` singletonSet (smallNID 9)
    reached g [path| (x/@ | y)/(a/c & b/c & c/d) |]
      `shouldBe` singletonSet (smallNID 9)
  it "writing out the conjunctions of random paths" do
    let cases =
          unGen
            ( vectorOf 300 $
                (,)
                  <$> (choose (3, 4) >>= \n -> vectorOf n segment)
                  <*> elements [id, (P.:/ P.One), ([path| q |] P.:&)]
            )
            (mkQCGen 20261006)
            30
        withinLimit x =
          fewAlternatives 300 x
        checked =
          [ (whole, written)
          | (segments, wrap) <- cases,
            let whole = wrap (foldl1Ex' (P.:/) segments)
                written = wrap (foldl1Ex' (P.:/) (writtenOut segments)),
            writtenOut segments /= segments,
            withinLimit whole && withinLimit written
          ]
    length checked `shouldSatisfy` (> 100)
    forM_ checked \(whole, written) ->
      (show whole, alternativesUpToOrder whole)
        `shouldBe` (show whole, alternativesUpToOrder written)
  where
    segment :: Gen P.Path
    segment =
      frequency
        [ (6, elements plain),
          (3, foldl1Ex' (P.:||) <$> (choose (2, 3) >>= \n -> vectorOf n (elements sides)))
        ]
    plain =
      [ [path| c & d |],
        [path| a & b |],
        [path| c |],
        [path| a/b |],
        [path| (a & b)/c |],
        [path| c & d & a |],
        [path| ~a & b |]
      ]
    sides =
      [ [path| a |],
        [path| b/c |],
        [path| x/@ |],
        [path| y/@ |],
        [path| (a & b)/@ |],
        [path| @000000000001 |],
        [path| !{@000000000001} |],
        [path| @ |],
        [path| @/c |],
        [path| a & b |],
        [path| c & d/@ |],
        [path| a/@/(c & d) |],
        [path| (a | x/@)/c |]
      ]
    -- every run of adjacent segments without a union or a join, replaced
    -- by its routes
    writtenOut :: [P.Path] -> [P.Path]
    writtenOut = \case
      [] -> []
      x : xs -> case span (isJust . routesOf) (x : xs) of
        (run@(_ : _ : _), rest)
          | Just routes <- routesOf (foldl1Ex' (P.:/) run) ->
              foldl1Ex' (P.:&) (map (foldl1Ex' (P.:/)) routes) : writtenOut rest
        _ -> x : writtenOut xs
    routesOf :: P.Path -> Maybe [[P.Path]]
    routesOf = \case
      p@(P.Literal _) -> Just [[p]]
      p@(P.Backwards (P.Literal _)) -> Just [[p]]
      p P.:/ q -> (\a b -> [x <> y | x <- a, y <- b]) <$> routesOf p <*> routesOf q
      p P.:& q -> (<>) <$> routesOf p <*> routesOf q
      _ -> Nothing

-- | A union is what its sides written for each copy are: a side a copy
-- chooses is sequenced after that copy's conjunct as it would be written
-- there.
spec_compileChoicesWrittenOut :: Spec
spec_compileChoicesWrittenOut = describe "a union is its choices written out" do
  let same a b = alternativesUpToOrder a `shouldBe` alternativesUpToOrder b
      reached cur g surface =
        foldMap
          targets
          (resolveFrom (smallNID cur) g (compilePath (surface P.:/ P.One))).resolutions
  it "an @ conjunct that chooses an @N side tests the current node" do
    same
      [path| (a & @)/(@000000000001 | b) |]
      [path| (a & @)/@000000000001 | (a & @)/b | a/@000000000001 & @/b | a/b & @/@000000000001 |]
    same
      [path| (@000000000001 & @)/(@000000000001/a | @000000000001/b) |]
      [path| (@000000000001 & @)/(@000000000001/a) | (@000000000001 & @)/(@000000000001/b)
             | @000000000001/@000000000001/a & @/@000000000001/b
             | @000000000001/@000000000001/b & @/@000000000001/a |]
    let g = graphOf [0, 1, 2] [(1, "a", 2), (1, "b", 2)]
    reached 0 g [path| (@000000000001 & @)/(@000000000001/a | @000000000001/b) |]
      `shouldBe` mempty
  it "where every copy chose it, a side is as it is written alone" $
    -- the @1 after the @ conjunct tests the current node either way
    ( (compilePath [path| (c & @)/(@000000000001 & d) |]).alternatives
        `difference` (compilePath [path| (c & @)/((@000000000001 & d) | z) |]).alternatives
    )
      `shouldBe` mempty
  it "a leading @ in a side at the root is no side of its own" do
    let g = graphOf [0, 5] [(0, "b", 0), (0, "c", 5), (5, "d", 5), (5, "a", 0)]
        before x = x P.:/ [path| (a & b | c)/(c & d) |]
    forM_
      [ [path| b | ~a |],
        [path| @/b | ~a |],
        [path| b | ~a | @/b |],
        [path| @/b | ~a | b |],
        [path| @/b | ~a | !{@000000000001} | b |]
      ]
      \u -> (show u, reached 0 g (before u)) `shouldBe` (show u, singletonSet (smallNID 5))
    compilePath [path| (@/b | e | b)/(a & b)/(c & d) |]
      `shouldBe` compilePath [path| (e | b)/(a & b)/(c & d) |]
    compilePath [path| (b | e | @/b)/(a & b)/(c & d) |]
      `shouldBe` compilePath [path| (e | b)/(a & b)/(c & d) |]

-- | A join of the input itself (@\@@, @\@N@, @!{…}@ with nothing before
-- it) has nothing to share where the input is one node: at the root of a
-- path and after another join. There @J/(p & q)@ is @J/p & J/q@, also for
-- the copies of a union that follows, so that a leading @\@@ at the root
-- is the same wherever it is written (docs/path-pattern-ir.md, Unions).
spec_compileRootJoins :: Spec
spec_compileRootJoins = describe "a join of the root has nothing to share" do
  let reached g surface =
        foldMap
          targets
          (resolveFrom (smallNID 0) g (compilePath (surface P.:/ P.One))).resolutions
      same a b =
        (show (a, b), alternativesUpToOrder a)
          `shouldBe` (show (a, b), alternativesUpToOrder b)
      -- a/y/e, b/d/e and c/d/e all end at 9: by two different d, or by
      -- one
      edges =
        [ (0, "a", 1),
          (1, "y", 2),
          (2, "e", 9),
          (0, "b", 3),
          (4, "e", 9),
          (0, "c", 5),
          (5, "d", 6),
          (6, "e", 9)
        ]
      apart = graphOf [0, 1, 2, 3, 4, 5, 6, 9] ((3, "d", 4) : edges)
      together = graphOf [0, 1, 2, 3, 4, 5, 6, 9] ((3, "d", 6) : edges)
      pairs =
        [ ( [path| ((@/(a & b) | a & b) & c)/(y | d/@)/e |],
            [path| ((a & b | @/(a & b)) & c)/(y | d/@)/e |]
          ),
          ( [path| (@/(a & b) & c)/(y | d/@)/e |],
            [path| (a & b & c)/(y | d/@)/e |]
          ),
          ( [path| ((@/(a & b) | z) & c)/(y | d/@)/e |],
            [path| ((a & b | z) & c)/(y | d/@)/e |]
          )
        ]
  it "a leading @ in a root conjunct, alone or as a side of a union" do
    forM_ pairs \(p, q) -> do
      same p q
      -- the copies after b and c share the walk d/@
      forM_ [p, q] \x -> do
        (show x, reached apart x) `shouldBe` (show x, mempty)
        (show x, reached together x)
          `shouldBe` (show x, singletonSet (smallNID 9))
    same
      [path| ((@/(a & b) | a & b) & c)/(y | d/@)/e |]
      [path| (a & b & c)/(y | d/@)/e |]
  it "@N and !{…} at the root, and a conjunction nested in another" do
    same
      [path| (@000000000001/(a & b) & c)/(y | d/@)/e |]
      [path| (@000000000001/a & @000000000001/b & c)/(y | d/@)/e |]
    same
      [path| (!{@000000000001}/(a & b) & c)/(y | d/@)/e |]
      [path| (!{@000000000001}/a & !{@000000000001}/b & c)/(y | d/@)/e |]
    same
      [path| ((a & b) & c)/(y | d/@)/e |]
      [path| (a & (b & c))/(y | d/@)/e |]
    same
      [path| (@/(a & b) & c)/(y | d/@)/e/@ |]
      [path| (a & b & c)/(y | d/@)/e/@ |]
  it "a join right after another join" $
    same
      [path| x/@/(@/(a & b) & c)/(y | d/@)/e |]
      [path| x/@/(a & b & c)/(y | d/@)/e |]

-- | Two prefixes without unions that compile to the same pattern, alone
-- and before each side of the unions that follow, are the same prefix:
-- the unions after them give the same alternatives. The prefixes are a
-- random path and that path rewritten at one place by a rule that keeps
-- what it means (regrouping a conjunction or a sequence, distributing a
-- conjunction, a join of the root over its continuations, dropping a
-- leading @\@@). The prefixes are generated without a conjunct that has no
-- step (@\@@, @\@N@); the exception this law had for a conjunct that is
-- only @\@@ is gone with chain-scoped joins (see the last test).
spec_compileInterchangeablePrefixes :: Spec
spec_compileInterchangeablePrefixes =
  describe "prefixes that compile the same are the same before a union" do
    let cases =
          unGen
            ( vectorOf 500 $
                (,,)
                  <$> (choose (1 :: Int, 3) >>= prefix)
                  <*> choose (0 :: Int, 1000)
                  <*> elements continuations
            )
            (mkQCGen 20261006)
            30
        withinLimit x =
          fewAlternatives 300 x
        checked =
          [ (p, p', c)
          | (p, k, c) <- cases,
            let rewritten = filter (/= p) (ordNub (rewrites p)),
            not (null rewritten),
            let p' = rewritten `indexEx` (k `mod` length rewritten),
            compilePath p == compilePath p',
            and
              [ compilePath (p P.:/ r) == compilePath (p' P.:/ r)
              | r <- readings c <> probes
              ],
            withinLimit (p P.:/ c),
            withinLimit (p' P.:/ c)
          ]
    it "random prefixes rewritten at one place" do
      -- Up to the order of the routes, and with the same first routes: a
      -- conjunct that starts with @ has its routes next to each other in
      -- every alternative, as its conjuncts written apart need not when
      -- they chose different sides.
      let firstRoutes q =
            setFromList
              [ canonicalize a {routes = first' :| []}
              | a <- toList q.alternatives,
                let first' :| _ = a.routes
              ] ::
              Set PatternPath
          compared q = (alternativesUpToOrder q, firstRoutes (compilePath q))
      length checked `shouldSatisfy` (> 150)
      forM_ checked \(p, p', c) ->
        (show (p, p', c), compared (p P.:/ c))
          `shouldBe` (show (p, p', c), compared (p' P.:/ c))
    it "a lone @ conjunct is no exception any more" do
      -- before chain-scoped joins the second was a root, the first a test
      compilePath [path| @/(c & @) |] `shouldBe` compilePath [path| c & @ |]
      compilePath [path| @/(c & @)/(@000000000001 & d) |]
        `shouldBe` compilePath [path| (c & @)/(@000000000001 & d) |]
  where
    atom = elements [[path| a |], [path| b |], [path| c |], [path| ~a |], [path| * |]]
    joinOfInput =
      elements [P.One, [path| @000000000001 |], [path| !{@000000000001} |]]
    prefix :: Int -> Gen P.Path
    prefix 0 = atom
    prefix d =
      oneof
        [ atom,
          (P.:&) <$> sub <*> sub,
          (P.:/) <$> sub <*> sub,
          (P.:/) <$> joinOfInput <*> sub,
          (P.:/ P.One) <$> sub,
          (\j x y -> j P.:/ (x P.:& y)) <$> joinOfInput <*> sub <*> sub,
          (\j x y z -> (j P.:/ (x P.:& y)) P.:& z)
            <$> joinOfInput
            <*> sub
            <*> sub
            <*> sub
        ]
      where
        sub = prefix (d - 1)
    -- every way to rewrite one place of a path
    rewrites :: P.Path -> [P.Path]
    rewrites p = top p <> inner
      where
        top = \case
          P.One P.:/ x -> x : rule (P.One P.:/ x)
          q -> rule q
        rule = \case
          j P.:/ (x P.:& y) | bare j -> [(j P.:/ x) P.:& (j P.:/ y)]
          x P.:/ (y P.:& z) -> [(x P.:/ y) P.:& (x P.:/ z)]
          (x P.:& y) P.:/ z -> [(x P.:/ z) P.:& (y P.:/ z)]
          (x P.:& y) P.:& z -> [x P.:& (y P.:& z)]
          (j P.:/ x) P.:& (j' P.:/ y) | bare j, j == j' -> [j P.:/ (x P.:& y)]
          (x P.:/ y) P.:/ z -> [x P.:/ (y P.:/ z)]
          _ -> []
        inner = case p of
          x P.:/ y ->
            [x' P.:/ y | x' <- rewrites x] <> [x P.:/ y' | y' <- rewrites y]
          x P.:& y ->
            [x' P.:& y | x' <- rewrites x] <> [x P.:& y' | y' <- rewrites y]
          _ -> []
    bare = \case
      P.One -> True
      P.Absolute _ -> True
      P.ExcludingNIDs _ -> True
      _ -> False
    -- continuations that fork and join: before them a conjunct that a
    -- leading @ keeps together differs from its conjuncts
    probes = [[path| (m & n)/@ |], [path| @/m & n |], [path| m/@ & n |]]
    -- every way to keep one side of each union
    readings :: P.Path -> [P.Path]
    readings = \case
      x P.:|| y -> readings x <> readings y
      x P.:/ y -> (P.:/) <$> readings x <*> readings y
      x P.:& y -> (P.:&) <$> readings x <*> readings y
      q -> [q]
    continuations =
      [ [path| (y | d/@)/e |],
        [path| x/@ | y |],
        [path| @000000000001 | x |],
        [path| (x | @/y)/(a & b) |],
        [path| (a/@ | b)/(c | d/@) |],
        [path| (a & b)/@ | c |],
        [path| @ | x |],
        [path| !{@000000000001} | x/@ |],
        [path| @000000000001 & d | x |],
        [path| (x/@ | y)/(a & b) |],
        [path| (a | b/@)/@ |],
        [path| (@000000000001/a | b)/c |],
        [path| a/@/b | c |],
        [path| (a & b | x)/(c & d | e/@) |],
        [path| (@/(a & b) | c)/d |],
        [path| (@000000000001 | @000000000002)/a |],
        [path| (x | y)/(w/@ | z) |]
      ]

-- | The alternatives a path compiles to, each with its routes in an order
-- that depends on nothing but what the pattern is ('upToOrder'): equal
-- for two paths exactly when their alternatives are the same up to the
-- order of the routes.
alternativesUpToOrder :: P.Path -> Set PatternPath
alternativesUpToOrder surface =
  mapSet upToOrder (compilePath surface).alternatives

-- | A pattern with its routes in an order that does not depend on the
-- order they were in, and its variables and edges renamed for it.
--
-- The result is the pattern itself with its routes reordered, so two
-- patterns have the same only if they differ in nothing but the order
-- of their routes and their names: this never takes different patterns
-- for the same. The routes are ordered by what they look like
-- ('routeColours'). Routes that look alike are ordered by how they are
-- attached to those before them, and while no more than three are left
-- alike, each is tried first and the least result is taken. Only with
-- more than three routes that look alike and are attached alike so far,
-- but differently to what follows, can two patterns that are the same
-- up to order get different results: a test would then fail, not pass.
upToOrder :: PatternPath -> PatternPath
upToOrder p =
  case take 64 (orders [] classes) of
    [] -> canonicalize p
    found -> minimumEx [canonicalize p {routes = r :| rs} | r : rs <- found]
  where
    coloured = sortOn fst (zip (routeColours p) (toList p.routes))
    classes = map (map snd) (groupBy ((==) `on` fst) coloured)
    orders :: [Route] -> [[Route]] -> [[Route]]
    orders prefix = \case
      [] -> [prefix]
      [] : more -> orders prefix more
      [r] : more -> orders (prefix <> [r]) more
      tied : more ->
        let keyed = [(attached (prefix <> [r]), (r, rest)) | (r, rest) <- picks tied]
            best = minimumEx (map fst keyed)
            nexts = [x | (k, x) <- keyed, k == best]
         in concat
              [ orders (prefix <> [r]) (rest : more)
              | (r, rest) <- if length nexts <= 3 then nexts else take 1 nexts
              ]
    attached :: [Route] -> Maybe PatternPath
    attached = \case
      [] -> Nothing
      r : more ->
        let vs = setFromList (concat [x.root : map (.target) x.hops | x <- r : more]) :: Set PVertex
         in Just . canonicalize $
              PatternPath
                (mapFromList [(v, t) | (v, t) <- mapToList p.tests, v `member` vs])
                (r :| more)
    picks :: [a] -> [(a, [a])]
    picks xs = [(x, before <> after) | (before, x : after) <- zip (inits xs) (tails xs)]

-- | What each route of a pattern looks like, whatever order the routes
-- are in and whatever the variables and edges are called: every node is
-- coloured by its kind, its test and whether a route ends there, and
-- then, a few times over, by the colours of what its edges lead to; a
-- route is its root and its hops by those colours. Routes that differ
-- only in order and names look the same; this does not tell all others
-- apart (it is an invariant), which is why 'upToOrder' uses it only to
-- order the routes.
routeColours :: PatternPath -> [Int]
routeColours p =
  [ mix (colour r.root, [(show h.direction, show h.matcher, colour h.target, edgeColour h.edge) | h <- r.hops])
  | r <- toList p.routes
  ]
  where
    colour v = findWithDefault 0 v colours
    edges =
      ordNub
        [ (h.edge, source, h.direction, h.matcher, h.target)
        | r <- toList p.routes,
          (source, h) <- zip (r.root : map (.target) r.hops) r.hops
        ]
    -- an edge by how many routes go along it, so that routes that share
    -- a step differ from routes that have one each
    edgeColour e =
      length [() | r <- toList p.routes, h <- r.hops, h.edge == e]
    vertices = ordNub (concat [r.root : map (.target) r.hops | r <- toList p.routes])
    ends = setFromList [lastDef r.root (map (.target) r.hops) | r <- toList p.routes] :: Set PVertex
    lastDef d xs = fromMaybe d (lastMay xs)
    start :: Map PVertex Int
    start =
      mapFromList
        [ ( v,
            mix
              ( case v of
                  Var _ -> "var"
                  _ -> show v,
                show (lookup v p.tests),
                v `member` ends
              )
          )
        | v <- vertices
        ]
    refine :: Map PVertex Int -> Map PVertex Int
    refine c =
      mapFromList
        [ ( v,
            mix
              ( findWithDefault 0 v c,
                sort
                  ( [ (True, show d, show m, edgeColour e, findWithDefault 0 t c)
                    | (e, a, d, m, t) <- edges,
                      a == v
                    ]
                      <> [ (False, show d, show m, edgeColour e, findWithDefault 0 a c)
                         | (e, a, d, m, t) <- edges,
                           t == v
                         ]
                  )
              )
          )
        | v <- vertices
        ]
    colours = foldl' (\c _ -> refine c) start [1 .. 6 :: Int]
    mix :: (Show a) => a -> Int
    mix = foldl' (\acc ch -> (acc * 1000003 + fromEnum ch) `rem` 2305843009213693951) 7 . show

-- | The compiler does not list the ways to choose that contradict each
-- other: a route whose choice cannot be glued with that of a route before
-- it is dropped as it is made, a route with no way to choose ends the
-- search, and a side that some route through a copy cannot choose is not
-- tried for the others. Nothing limits how large a path may be
-- (docs/path-cost-limits.md), so only this keeps such paths from taking
-- as long as their choices are many.
--
-- Each compile here is over in a fraction of a second, and takes hours
-- or does not end when the search lists what it should drop. The
-- allowance is there to make that a failure instead of a suite that
-- hangs; it measures nothing.
spec_compileContradictions :: Spec
spec_compileContradictions = describe "choices that contradict each other are not listed" do
  let alternativesOf p = do
        compiled <- timeout 60000000 (evaluate (length (compilePath p).alternatives))
        maybe (expectationFailure ("did not compile within a minute: " <> show p) $> 0) pure compiled
      unions n =
        foldl1Ex'
          (P.:&)
          [ P.Literal ("a" <> tshow i) P.:|| P.Literal ("b" <> tshow i)
          | i <- [0 .. n - 1 :: Int]
          ]
  it "one alternative of many choices that contradict a join" do
    -- 64 * 64 ways to choose, of which all but one contradict the @1
    -- that follows
    let nodes = foldl1Ex' (P.:||) [P.Absolute (smallNID i) | i <- [1 .. 64]]
    alternativesOf ([path| a & b |] P.:/ nodes P.:/ [path| @000000000001 |])
      `shouldReturn` 1
  it "a conjunct that cannot be satisfied leaves nothing, wherever it is written" do
    -- every way to choose the last conjunct contradicts itself, and the
    -- 2^22 ways to choose the others are not gone through
    for_
      [ unions 22 P.:& [path| (@000000000001 | @000000000002)/@000000000003 |],
        [path| (@000000000001 | @000000000002)/@000000000003 |] P.:& unions 22,
        unions 22 P.:& [path| @/(@000000000001 & c)/@000000000002 |]
      ]
      \p -> alternativesOf p `shouldReturn` 0
  it "a route that can only choose one side settles it for the others" do
    -- the last route can only choose @2 for a union the others choose
    -- too: they are not tried with @1 first
    let settled n =
          [path| q |]
            P.:& ( [path| @/(@000000000001 | @000000000002)/@ |]
                     P.:/ ([path| c |] P.:& unions n P.:& [path| @000000000002 |])
                 )
    alternativesOf (settled 8) `shouldReturn` 256
  it "a union of targets after a join has its targets for alternatives" do
    -- two routes at one joined node cannot choose different nodes there:
    -- 5 alternatives of 5^5 ways to choose, 46 of 46^4
    let targets n = foldl1Ex' (P.:||) [P.Absolute (smallNID i) | i <- [1 .. n]]
    alternativesOf ([path| x/@ |] P.:/ targets 5 P.:/ [path| a & b & c & d & e |])
      `shouldReturn` 5
    alternativesOf ([path| (c & d)/@ |] P.:/ targets 46 P.:/ [path| a & b |])
      `shouldReturn` 46
  it "joins that contradict each other where written leave no chain" do
    -- no chain is made for them, whatever is before
    let labels' = foldl1Ex' (P.:++) [P.Literal (tshow i) | i <- [1 .. 31 :: Int]]
        none =
          labels'
            P.:/ labels'
            P.:/ [path| (a | b | c)/(e & f & g & h & i & j & k)/@000000000001/@000000000002 |]
    alternativesOf none `shouldReturn` 0
    compilePath none `shouldBe` query []

spec_compileOverride :: Spec
spec_compileOverride = describe "%override in the quasiquoter" do
  it "lowers + to |" $
    compilePath [path| %override({plus = relational}, (a + b)/(c & d)) |]
      `shouldBe` compilePath [path| (a | b)/(c & d) |]
  it "lowers + to ++" $
    compilePath [path| %override({plus = shared}, (a + b)/(c & d)) |]
      `shouldBe` compilePath [path| (a ++ b)/(c & d) |]
  it "the innermost override wins, and | and ++ are unaffected" $
    compilePath
      [path| %override({plus = shared}, (a + %override({plus = relational}, b + c))/(d & e) ++ f) |]
      `shouldBe` compilePath [path| (a ++ (b | c))/(d & e) ++ f |]

-- | Adding a side to a union never removes targets, in a few contexts and
-- for both kinds of union, over the corpus.
spec_compileUnionMonotone :: Spec
spec_compileUnionMonotone = describe "a union side never removes targets" do
  forM_ corpusQueries \p -> it (show p) $
    forM_ graphs \g -> forM_ contexts \c -> forM_ sides \q ->
      forM_ [P.Relational, P.Shared] \k -> do
        let before = reached g (c p)
            after' = reached g (c (P.Union k p q))
        (show (g, c p, k, q), null (before `difference` after'))
          `shouldBe` (show (g, c p, k, q), True)
  where
    graphs = [g | (i, g) <- zip [0 :: Int ..] corpusGraphs, i `mod` 8 == 0]
    sides = [[path| a |], [path| a & b |]]
    contexts =
      [ (P.:/ P.One),
        \x -> x P.:/ [path| (a & b)/@ |],
        \x -> [path| a & b |] P.:/ x P.:/ P.One,
        \x -> (x P.:& [path| a |]) P.:/ [path| b/@ |]
      ]
    reached g surface =
      foldMap
        targets
        (resolveFrom corpusCurrent g (compilePath surface)).resolutions

-- | Adding a side to a union never removes targets, for random paths with
-- joins in random contexts, on a sample of the corpus graphs.
spec_compileUnionMonotoneRandom :: Spec
spec_compileUnionMonotoneRandom =
  it "union laws on random paths with joins" do
    -- a fixed sample, so that the time it takes does not vary
    let cases =
          unGen
            ( vectorOf 400 $
                (,,,,)
                  <$> genPath 4
                  <*> genPath 3
                  <*> genPath 3
                  <*> genPath 2
                  <*> choose (0 :: Int, 6)
            )
            (mkQCGen 20261003)
            30
        -- keep resolving cheap
        withinLimit x =
          fewAlternatives 100 (x P.:/ P.One)
        checked =
          [ (base, bigger)
          | (p, q, c, d, ctx) <- cases,
            k <- [P.Relational, P.Shared],
            let base = plug ctx c d p
                bigger = plug ctx c d (P.Union k p q),
            withinLimit base && withinLimit bigger
          ]
    length checked `shouldSatisfy` (> 400)
    -- a union of a path with %never is that path
    forM_ cases \(p, _, c, d, ctx) -> when (withinLimit (plug ctx c d p)) do
      let same x =
            (show (p, ctx, c, d), compilePath (plug ctx c d x))
              `shouldBe` (show (p, ctx, c, d), compilePath (plug ctx c d p))
      same (p P.:|| P.Zero)
      same (P.Zero P.:|| p)
    -- A union of a path with itself is two sides, and on this sample it
    -- is that path where the path has no join of the chain it is
    -- written in and no ++ union: then the routes through it share
    -- nothing of it, whichever side each takes, and the two sides are
    -- the same path. (With such a join it may be more, and so it is,
    -- by one alternative that finds nothing more, for a path that forks
    -- next to a conjunct in which walks meet at an @N and go on as one:
    -- 'spec_compileEverySide'; no path of this sample shows that. A ++
    -- written twice is two unions, which are chosen apart.)
    let twice = [x | x@(p, _, c, d, ctx) <- cases, withinLimit (plug ctx c d (p P.:|| p))]
        joinless = [x | x@(p, _, _, _, _) <- twice, not (ownJoin p), not (hasShared p)]
    length joinless `shouldSatisfy` (> 60)
    length twice - length joinless `shouldSatisfy` (> 60)
    forM_ joinless \(p, _, c, d, ctx) ->
      (show (p, ctx, c, d), compilePath (plug ctx c d (p P.:|| p)))
        `shouldBe` (show (p, ctx, c, d), compilePath (plug ctx c d p))
    forM_ twice \(p, _, c, d, ctx) ->
      ( show (p, ctx, c, d),
        alternativesOf (plug ctx c d p) `difference` alternativesOf (plug ctx c d (p P.:|| p))
      )
        `shouldBe` (show (p, ctx, c, d), mempty)
    -- The sides of a union in another order, or grouped another way,
    -- are the same union; and so is one with the sides of every union
    -- in a side written the other way around.
    forM_ cases \(p, _, c, d, ctx) -> do
      let other = P.Union P.Relational c d
          sameAs y x =
            (show (x, ctx), compilePath (plug ctx c d x))
              `shouldBe` (show (x, ctx), compilePath (plug ctx c d y))
      when (withinLimit (plug ctx c d ((p P.:|| other) P.:|| p))) do
        (p P.:|| (other P.:|| p)) `sameAs` ((p P.:|| other) P.:|| p)
        ((p P.:|| p) P.:|| other) `sameAs` ((p P.:|| other) P.:|| p)
        ((other P.:|| p) P.:|| p) `sameAs` ((p P.:|| other) P.:|| p)
        (other P.:|| p) `sameAs` (p P.:|| other)
        (p P.:|| swapUnions p) `sameAs` (p P.:|| p)
    -- sequencing associates: the same alternatives however it is grouped
    forM_ cases \(_, p, q, c, _) ->
      when (withinLimit (p P.:/ q P.:/ c)) $
        (show (p, q, c), compilePath (p P.:/ (q P.:/ c)))
          `shouldBe` (show (p, q, c), compilePath (p P.:/ q P.:/ c))
    forM_ checked \(base, bigger) -> do
      -- no alternative is lost (up to the order of its routes)...
      ( show (base, bigger),
        alternativesOf base `difference` alternativesOf bigger
        )
        `shouldBe` (show (base, bigger), mempty)
      -- ...so no target is
      forM_ graphs \g ->
        ( show (base, bigger, g),
          null (reached g base `difference` reached g bigger)
        )
          `shouldBe` (show (base, bigger, g), True)
  where
    graphs = [g | (i, g) <- zip [0 :: Int ..] corpusGraphs, i `mod` 29 == 0]
    reached g surface =
      foldMap
        targets
        (resolveFrom corpusCurrent g (compilePath (surface P.:/ P.One))).resolutions
    swapUnions = \case
      P.Union k l r -> P.Union k (swapUnions r) (swapUnions l)
      l P.:/ r -> swapUnions l P.:/ swapUnions r
      l P.:& r -> swapUnions l P.:& swapUnions r
      P.Backwards x -> P.Backwards (swapUnions x)
      x -> x
    -- whether a path has a join at its own level: one that is a join of
    -- the chain the path is written in (one in a conjunct is not)
    ownJoin = \case
      P.One -> True
      P.Absolute _ -> True
      P.ExcludingNIDs _ -> True
      l P.:/ r -> ownJoin l || ownJoin r
      P.Union _ l r -> ownJoin l || ownJoin r
      P.Backwards x -> ownJoin x
      P.WithLimits _ _ x -> ownJoin x
      _ -> False
    hasShared = \case
      P.Union P.Shared _ _ -> True
      P.Union _ l r -> hasShared l || hasShared r
      l P.:/ r -> hasShared l || hasShared r
      l P.:& r -> hasShared l || hasShared r
      P.Backwards x -> hasShared x
      P.WithLimits _ _ x -> hasShared x
      _ -> False
    alternativesOf = alternativesUpToOrder
    plug :: Int -> P.Path -> P.Path -> P.Path -> P.Path
    plug ctx c d x = case ctx of
      0 -> x
      1 -> x P.:/ c
      2 -> c P.:/ x
      3 -> x P.:& c
      4 -> c P.:/ x P.:/ d
      5 -> (x P.:& c) P.:/ d
      _ -> c P.:/ (x P.:& d)
    genPath :: Int -> Gen P.Path
    genPath n
      | n <= 1 =
          elements
            [ P.Literal "a",
              P.Literal "b",
              P.Wild,
              P.Backwards (P.Literal "a"),
              P.One,
              P.Absolute (smallNID 1),
              P.ExcludingNIDs (singletonSet (smallNID 1))
            ]
      | otherwise = do
          k <- choose (1, n - 1)
          frequency
            [ (4, (P.:/) <$> genPath k <*> genPath (n - k)),
              (2, (P.:&) <$> genPath k <*> genPath (n - k)),
              (3, (P.:||) <$> genPath k <*> genPath (n - k)),
              (1, (P.:++) <$> genPath k <*> genPath (n - k)),
              (1, P.Backwards <$> genPath (n - 1))
            ]

-- | A wildcard is the relational union of every label: on graphs labelled a to d, the
-- same routes match.
spec_compileWildIsUnion :: Spec
spec_compileWildIsUnion = describe "* is the union of the labels" do
  forM_ laws \(wild, union') ->
    it (show wild <> " ~ " <> show union') $
      forM_ graphs \g ->
        (show g, routesFrom g wild) `shouldBe` (show g, routesFrom g union')
  where
    laws =
      [ ([path| */(c & d) |], [path| (a | b | c | d)/(c & d) |]),
        ([path| (a & b)/* |], [path| (a & b)/(a | b | c | d) |]),
        ([path| */(c & d)/@ |], [path| (a | b | c | d)/(c & d)/@ |]),
        ( [path| (* & a)/(* & d) |],
          [path| ((a | b | c | d) & a)/((a | b | c | d) & d) |]
        ),
        ( [path| */*/(c & d) |],
          [path| (a | b | c | d)/(a | b | c | d)/(c & d) |]
        )
      ]
    pool =
      [ (1, "a", 2),
        (1, "b", 3),
        (2, "c", 4),
        (3, "d", 4),
        (2, "d", 2),
        (1, "c", 1),
        (3, "a", 1)
      ]
    graphs = map (graphOf [1, 2, 3, 4]) (subsequences pool)
    routesFrom :: Graph Text () -> P.Path -> Set [ConcreteRoute]
    routesFrom g surface =
      setFromList
        [ concreteRoutes r w
        | r <- (resolveFrom (smallNID 1) g (compilePath surface)).resolutions,
          w <- witnesses r
        ]

graphOf :: [Int] -> [(Int, Text, Int)] -> Graph Text ()
graphOf ns es =
  insertEdges
    [Edge (smallNID s) t (smallNID d) | (s, t, d) <- es]
    (foldl' (\g' i -> insertNode (emptyNode (smallNID i)) g') emptyGraph ns)

resolveFrom :: NID -> Graph Text () -> PathQuery -> ResolvedPath
resolveFrom cur g q =
  resolveQuery cur q
    & runInMemoryGraphMetadataEditing
    & evalStateLocal g
    & runPureEff

compiles :: P.Path -> [PatternPath] -> Expectation
compiles surface expected = compilePath surface `shouldBe` query expected

spec_compileExamples :: Spec
spec_compileExamples = describe "compile examples" do
  let (v0, v1, v2, v3) = (var 0, var 1, var 2, var 3)
  it "a/(b & c) copies the unjoined prefix" $
    [path| a/(b & c) |]
      `compiles` [ pat
                     [ route current [out 0 "a" v0, out 1 "b" v1],
                       route current [out 2 "a" v2, out 3 "c" v3]
                     ]
                 ]
  it "(a | b)/(c & d) chooses a side for each copy" $
    map
      (map (map hopLabels . toList . (.routes)) . toList . (.alternatives))
      [compilePath [path| (a | b)/(c & d) |]]
      `shouldBe` [ [ [["a", "c"], ["a", "d"]],
                     [["a", "c"], ["b", "d"]],
                     [["b", "c"], ["a", "d"]],
                     [["b", "c"], ["b", "d"]]
                   ]
                 ]
  it "a/@/(b & c) shares the joined prefix and its edge" $
    [path| a/@/(b & c) |]
      `compiles` [ pat
                     [ route current [out 0 "a" v0, out 1 "b" v1],
                       route current [out 0 "a" v0, out 2 "c" v2]
                     ]
                 ]
  it "(a & b)/(c & d) distributes prefix-major" $
    map (map hopLabels . toList . (.routes)) (toList (compilePath [path| (a & b)/(c & d) |]).alternatives)
      `shouldBe` [[["a", "c"], ["a", "d"], ["b", "c"], ["b", "d"]]]
  it "(a & b)/@ & c" $
    [path| (a & b)/@ & c |]
      `compiles` [ pat
                     [ route current [out 0 "a" v0],
                       route current [out 1 "b" v0],
                       route current [out 2 "c" v1]
                     ]
                 ]
  it "appending /@ to (a & b)/@ & c joins every output" $
    [path| ((a & b)/@ & c)/@ |]
      `compiles` [ pat
                     [ route current [out 0 "a" v0],
                       route current [out 1 "b" v0],
                       route current [out 2 "c" v0]
                     ]
                 ]
  it "x/(@ & a)/@ copies x and keeps the zero-hop route first" $
    [path| x/(@ & a)/@ |]
      `compiles` [ pat
                     [ route current [out 0 "x" v0],
                       route current [out 1 "x" v1, out 2 "a" v0]
                     ]
                 ]
  it "x/(a & @)/@ puts the a route first" $
    [path| x/(a & @)/@ |]
      `compiles` [ pat
                     [ route current [out 0 "x" v0, out 1 "a" v1],
                       route current [out 2 "x" v1]
                     ]
                 ]
  it "x/@/(@ & a)/@ shares x, so a is a loop" $
    [path| x/@/(@ & a)/@ |]
      `compiles` [ pat
                     [ route current [out 0 "x" v0],
                       route current [out 0 "x" v0, out 1 "a" v0]
                     ]
                 ]
  it "(@ & a)/@ is a loop on current" $
    [path| (@ & a)/@ |]
      `compiles` [pat [route current [], route current [out 0 "a" current]]]
  it "(* & *)/@ has two independent edges" $
    [path| (* & *)/@ |]
      `compiles` [pat [route current [outW 0 v0], route current [outW 1 v0]]]
  it "*/@/(b & c) shares the wildcard edge" $
    [path| */@/(b & c) |]
      `compiles` [ pat
                     [ route current [outW 0 v0, out 1 "b" v1],
                       route current [outW 0 v0, out 2 "c" v2]
                     ]
                 ]
  it "a & a has independent occurrences" $
    [path| a & a |]
      `compiles` [pat [route current [out 0 "a" v0], route current [out 1 "a" v1]]]
  it "@N at the root is a fixed vertex and every @N is one vertex" $
    [path| @000000000005/a/@000000000005 |]
      `compiles` [pat [route (fixedN 5) [out 0 "a" (fixedN 5)]]]
  it "@N & a are independent roots" $
    [path| @000000000005 & a |]
      `compiles` [pat [route (fixedN 5) [], route current [out 0 "a" v0]]]
  it "joining current with @N tests current" $
    [path| (@ & @000000000005)/@ |]
      `compiles` [ patWith
                     [(current, exactly 5)]
                     [route current [], route current []]
                 ]
  it "an @ alternative joins, so (@ + a)/@N tests current" $
    [path| (@ ++ a)/@000000000005 |]
      `compiles` [ patWith [(current, exactly 5)] [route current []],
                   pat [route current [out 0 "a" (fixedN 5)]]
                 ]
  it "an @ alternative on the right joins the prefix" $
    [path| (a & b)/(@ ++ c) |]
      `compiles` [ pat
                     [ route current [out 0 "a" v0],
                       route current [out 1 "b" v0]
                     ],
                   pat
                     [ route current [out 0 "a" v0, out 1 "c" v1],
                       route current [out 2 "b" v2, out 3 "c" v3]
                     ]
                 ]
  it "!{...} tests a variable" $
    [path| a/!{@000000000002} |]
      `compiles` [patWith [(v0, except [2])] [route current [out 0 "a" v0]]]
  it "exclusions meet" $
    [path| a/!{@000000000002}/!{@000000000003} |]
      `compiles` [ patWith
                     [(v0, except [2, 3])]
                     [route current [out 0 "a" v0]]
                 ]
  it "contradictory fixed nodes drop the alternative" $
    compilePath [path| a/@000000000001/@000000000002 ++ b |]
      `shouldBe` query [pat [route current [out 0 "b" v0]]]
  it "a fixed node that is excluded drops the alternative" $
    compilePath [path| a/!{@000000000002}/@000000000002 |]
      `shouldBe` query []
  it "~(a/b) reverses the sequence and flips each step" $
    [path| ~(a/b) |]
      `compiles` [pat [route current [inc 0 "b" v0, inc 1 "a" v1]]]
  it "~ keeps node specs in place" $
    [path| ~(a/@000000000002) |]
      `compiles` [pat [route (fixedN 2) [inc 0 "a" v0]]]
  it "%never compiles to no alternatives" $
    compilePath P.Zero `shouldBe` query []
  it "Show prints the helper syntax (a node spec joins, sharing a)" $
    show (compilePath [path| a/!{@000000000002}/(b & c) |])
      `shouldBe` "query [patWith [(var 0,except [2])] [route current \
                 \[out 0 \"a\" (var 0),out 1 \"b\" (var 1)],route current \
                 \[out 0 \"a\" (var 0),out 2 \"c\" (var 2)]]]"
  where
    hopLabels r = [l | Hop _ _ (MLiteral l) _ <- r.hops]

-- | The path with limit nodes put around random parts of it, up to three
-- deep at one place.
genWithLimits :: P.Path -> Gen P.Path
genWithLimits = \case
  P.Backwards p -> wrapped (P.Backwards <$> genWithLimits p)
  p P.:/ q -> wrapped ((P.:/) <$> genWithLimits p <*> genWithLimits q)
  p P.:& q -> wrapped ((P.:&) <$> genWithLimits p <*> genWithLimits q)
  P.Union k p q -> wrapped (P.Union k <$> genWithLimits p <*> genWithLimits q)
  p -> wrapped (pure p)
  where
    wrapped inner = do
      depth <- frequency [(3, pure 0), (2, pure 1), (1, choose (2, 3))]
      limits <- vectorOf depth genLimits
      flip (foldr (P.WithLimits ())) limits <$> inner
    genLimits =
      P.PathLimits
        <$> elements
          [Nothing, Just (P.AtMostRepairs 5), Just P.UnlimitedRepairs]

spec_compileIgnoresLimits :: Spec
spec_compileIgnoresLimits = describe "limit nodes do not change compilation" do
  let override l = P.WithLimits () l
      repairs = P.PathLimits (Just (P.AtMostRepairs 5000))
      fewer = P.PathLimits (Just (P.AtMostRepairs 50))
      plain = [path| (a & b)/(@ | d) |]
  it "two nested around a union after a conjunction" $
    compilePath
      ([path| a & b |] P.:/ override repairs (override fewer [path| @ | d |]))
      `shouldBe` compilePath plain
  it "one around a join in a sequence" $
    compilePath
      ([path| a & b |] P.:/ override repairs P.One P.:/ [path| c |])
      `shouldBe` compilePath [path| (a & b)/@/c |]
  it "nested around shared sides and segments" do
    compilePath
      ( override repairs (override repairs [path| a ++ b |])
          P.:++ [path| c |]
          P.:/ [path| d & e |]
      )
      `shouldBe` compilePath [path| (a ++ b) ++ c/(d & e) |]
    compilePath
      ( [path| x & y |]
          P.:/ override fewer (override repairs [path| a/@ |])
          P.:/ [path| d & e |]
      )
      `shouldBe` compilePath [path| (x & y)/a/@/(d & e) |]
  it "any number anywhere in random paths" . withMaxSuccess 2000 $
    forAll (fst <$> genRepairCase) \p -> forAll (genWithLimits p) \limited ->
      fewAlternatives 1000 p
        ==> conjoin
          [ compilePath limited === compilePath p,
            compileAlternatives limited === compileAlternatives p
          ]

-- * The compiler before relational unions, as a reference

-- | A term of the compiler as it was before relational unions (the tree
-- of commits up to 095f99a0): 'Stay' continues at its input, a 'Join'
-- merges every output of its child into one, and 'Sequence' feeds its
-- prefix's single output to the continuation. It compiles paths whose
-- unions are all chosen once for the whole path, by rewriting terms, and
-- shares no code with "Models.PathPattern.Compile".
data Expr
  = Source PVertex
  | Stay
  | Step Direction Matcher
  | Both Expr Expr [Expr]
  | Join Expr NodeTest
  | Sequence Expr Expr
  deriving stock (Eq, Ord, Show)

-- | The compiler as it was, which a join inside a conjunct of a left
-- group stops from copying that group; or with the two changes of
-- chain-scoped joins made to it: a conjunction is sequenced with a
-- conjunction conjunct by conjunct, whatever joins they have, and a
-- conjunct that is only @\@@ is sequenced as the join it is in @\@/…@
-- and @…/\@@.
data Reference = AsBefore | WrittenOut
  deriving stock (Eq, Show)

-- | The alternatives of a path as terms, each with whether making it met
-- class 1 (a conjunct with a join in a conjunction that is sequenced
-- with a conjunction; or a conjunct that is only @\@@ in a conjunction
-- that is sequenced with anything, since that @\@@ is a join too) and
-- class 2 (a conjunct that is only @\@@ sequenced with a conjunction
-- that has a conjunct starting with an @\@N@).
referenceExprs :: Reference -> P.Path -> [(Expr, (Bool, Bool))]
referenceExprs mode surface =
  S.runStateT (fragment (pushBackwards surface)) (False, False)
  where
    fragment :: P.Path -> S.StateT (Bool, Bool) [] Expr
    fragment = \case
      P.One -> pure Stay
      P.Zero -> lift []
      P.Wild -> pure (Step Out MWild)
      P.Literal t -> pure (Step Out (MLiteral t))
      P.RegexMatch r -> pure (Step Out (MRegex r))
      P.Backwards P.Wild -> pure (Step In MWild)
      P.Backwards (P.Literal t) -> pure (Step In (MLiteral t))
      P.Backwards (P.RegexMatch r) -> pure (Step In (MRegex r))
      P.Backwards _ -> error "reference: pushBackwards leaves ~ only on steps"
      P.Absolute nid -> pure (Join Stay (Exactly nid))
      P.ExcludingNIDs ns -> pure (Join Stay (Except ns))
      P.Union _ p q -> join (lift [fragment p, fragment q])
      P.WithLimits _ _ p -> fragment p
      p P.:& q -> (\a b -> conjoinExprs (a : [b])) <$> fragment p <*> fragment q
      p P.:/ q -> do
        a <- atBoundary <$> fragment p
        b <- atBoundary <$> fragment q
        sequenced a b
    atBoundary = \case
      Stay -> Join Stay anyNode
      e -> e
    sequenced, prefixed :: Expr -> Expr -> S.StateT (Bool, Bool) [] Expr
    sequenced Stay q = pure q
    sequenced p Stay = pure p
    sequenced p (Join q test) = (`Join` test) <$> sequenced p q
    sequenced p (Sequence q r) = sequenced p q >>= (`sequenced` r)
    sequenced (Sequence p q) r = sequenced q r >>= prefixed p
    sequenced p@(Both {}) q = do
      let cs = exprConjuncts p
      S.modify' \(one, two) ->
        ( one
            || (isBoth q && any hasJoin cs)
            || Stay `elem` cs
            || Stay `elem` exprConjuncts q,
          two || (Stay `elem` cs && startsWithNode q)
        )
      conjoinExprs <$> case mode of
        AsBefore -> traverse (`sequenced` q) cs
        WrittenOut ->
          sequence
            [ sequenced (atBoundary c) (atBoundary d)
            | c <- cs,
              d <- exprConjuncts q
            ]
    sequenced p q = prefixed p q
    prefixed Stay q = pure q
    prefixed p Stay = pure p
    prefixed p (Join q test) = (`Join` test) <$> prefixed p q
    prefixed p (Sequence (Join q test) r) = do
      joined <- sequenced p q
      prefixed (Join joined test) r
    prefixed p@(Join {}) q = pure (Sequence p q)
    prefixed p q@(Both {}) = do
      S.modify' (first (|| Stay `elem` exprConjuncts q))
      conjoinExprs <$> case mode of
        AsBefore -> traverse (prefixed p) (exprConjuncts q)
        -- a conjunct that is only @ joins what is before it
        WrittenOut -> traverse (sequenced p . atBoundary) (exprConjuncts q)
    prefixed p q = pure (Sequence p q)
    isBoth = \case
      Both {} -> True
      _ -> False
    hasJoin = \case
      Join _ _ -> True
      Sequence (Join _ _) _ -> True
      _ -> False
    startsWithNode = \case
      Join Stay (Exactly _) -> True
      Join p _ -> startsWithNode p
      Sequence p _ -> startsWithNode p
      Both a b rest -> any startsWithNode (a : b : rest)
      _ -> False

exprConjuncts :: Expr -> [Expr]
exprConjuncts (Both a b rest) = a : b : rest
exprConjuncts p = [p]

conjoinExprs :: [Expr] -> Expr
conjoinExprs ps = case concatMap exprConjuncts ps of
  [p] -> p
  a : b : more -> Both a b more
  [] -> error "conjoinExprs: no conjuncts"

-- | Relative terms start at the current node; an @\@N@ root is that node.
rooted :: Expr -> Expr
rooted = \case
  Source s -> Source s
  Stay -> Source Current
  s@(Step {}) -> Sequence (Source Current) s
  Both a b rest -> conjoinExprs (map rooted (a : b : rest))
  Join Stay (Exactly nid) -> Source (Fixed nid)
  Join p test -> Join (rooted p) test
  Sequence p q -> case rooted p of
    Sequence p' q' -> Sequence p' (Sequence q' q)
    p' -> Sequence p' q

-- | Whether a path without relational unions is in the class that
-- chain-scoped joins changed, as it is written: somewhere in it a group
-- has a conjunct with a join (a conjunct that is only a join counts, and
-- so does one that has the join in a conjunct of a group of its own,
-- when it has no join itself and is those conjuncts written out), and
-- is followed in its chain by another group with no join of that chain
-- in between. Outside this class a path compiles exactly as before.
inChangedClass :: P.Path -> Bool
inChangedClass = any chain' . choices . pushBackwards
  where
    -- every shared union chosen
    choices :: P.Path -> [P.Path]
    choices = \case
      P.Union _ l r -> choices l <> choices r
      l P.:/ r -> (P.:/) <$> choices l <*> choices r
      l P.:& r -> (P.:&) <$> choices l <*> choices r
      P.WithLimits _ _ p -> choices p
      p -> [p]
    segments = \case
      l P.:/ r -> segments l <> segments r
      p -> [p]
    conjuncts' = \case
      l P.:& r -> conjuncts' l <> conjuncts' r
      p -> [p]
    isJoin' = \case
      P.One -> True
      P.Absolute _ -> True
      P.ExcludingNIDs _ -> True
      _ -> False
    isGroup = \case
      _ P.:& _ -> True
      _ -> False
    -- a conjunct that has a join, once conjuncts that fork without one
    -- are written out
    joined c =
      let segs = segments c
       in any isJoin' segs
            || or [any joined (conjuncts' g) | g <- segs, isGroup g]
    chain' c =
      let segs = segments c
       in or
            [ any joined (conjuncts' g)
                && any isGroup (take 1 (dropWhile (\x -> not (isJoin' x || isGroup x)) rest))
            | g : rest <- tails segs,
              isGroup g
            ]
            || or [chain' x | g <- segs, isGroup g, x <- conjuncts' g]

-- | The classes of chain-scoped joins a path is in, as the compiler
-- before it meets them (more paths than change: see 'inChangedClass').
referenceClasses :: P.Path -> (Bool, Bool)
referenceClasses surface =
  (any (fst . snd) made, any (snd . snd) made)
  where
    made = referenceExprs AsBefore surface

referenceCompile :: Reference -> P.Path -> PathQuery
referenceCompile mode surface =
  PathQuery . setFromList $
    [ canonicalize pattern'
    | (e, _) <- referenceExprs mode surface,
      Just pattern' <- [glueExpr (rooted e)]
    ]

data ExprWalk = ExprWalk {root :: PVertex, hops :: [Hop], end :: PVertex}

data ExprGlue = ExprGlue
  { nextVar :: Int,
    nextEdge :: Int,
    parents :: Map PVertex PVertex,
    vertexTests :: Map PVertex NodeTest
  }

-- | Glue one rooted term into a pattern, as the compiler before unions
-- did. 'Nothing' if its node tests contradict each other.
glueExpr :: Expr -> Maybe PatternPath
glueExpr expr = do
  (walks, st) <- S.runStateT (glue Nothing expr) (ExprGlue 0 0 mempty mempty)
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
  where
    glue :: Maybe PVertex -> Expr -> S.StateT ExprGlue Maybe [ExprWalk]
    glue input = \case
      Source v -> do
        guard (isNothing input)
        pure [ExprWalk v [] v]
      Stay -> pure [ExprWalk i [] i | i <- toList input]
      Step d m -> do
        i <- lift input
        st <- S.get
        S.put st {nextVar = st.nextVar + 1, nextEdge = st.nextEdge + 1}
        let v = Var (VarId st.nextVar)
        pure [ExprWalk i [Hop (EdgeId st.nextEdge) d m v] v]
      Both a b rest -> concat <$> traverse (glue input) (a : b : rest)
      Join p test -> do
        walks <- glue input p
        case map (.end) walks of
          [] -> error "glue: a join over no walks"
          e : es -> do
            traverse_ (unify e) es
            r <- find' e
            case test of
              Exactly n -> unify r (Fixed n)
              Except _ -> addTest r test
        pure walks
      Sequence p q -> do
        walks <- glue input p
        ends <- ordNub <$> traverse (find' . (.end)) walks
        j <- case ends of
          [j] -> pure j
          _ -> error "glue: a sequence prefix with several outputs"
        continuations <- glue (Just j) q
        pure
          [ ExprWalk w.root (w.hops ++ c.hops) c.end
          | w <- walks,
            c <- continuations
          ]
    findRep parents v = maybe v (findRep parents) (lookup v parents)
    find' v = S.gets (\st -> findRep st.parents v)
    unify a b = do
      ra <- find' a
      rb <- find' b
      unless (ra == rb) case (ra, rb) of
        (Fixed n, Fixed m) -> guard (n == m)
        (Current, Fixed n) -> merge Current (Fixed n) *> addTest Current (Exactly n)
        (Fixed n, Current) -> merge Current (Fixed n) *> addTest Current (Exactly n)
        (Var x, Var y) | x < y -> merge ra rb
        (Var _, _) -> merge rb ra
        (_, Var _) -> merge ra rb
        _ -> error "unify: unreachable"
    merge keep gone = do
      st <- S.get
      let goneTest = lookup gone st.vertexTests
      S.put
        st
          { parents = insertMap gone keep st.parents,
            vertexTests = deleteMap gone st.vertexTests
          }
      traverse_ (addTest keep) goneTest
    addTest r test = case r of
      Fixed n -> guard (testAccepts test n)
      _ -> do
        st <- S.get
        combined <- lift (meetTests test (findWithDefault anyNode r st.vertexTests))
        S.put st {vertexTests = insertMap r combined st.vertexTests}

-- | Evaluate a rooted term directly, producing each witness as its
-- ordered concrete routes. It shares no code with gluing or matching.
interpretExpr :: Graph Text () -> NID -> Expr -> [[ConcreteRoute]]
interpretExpr g cur = go Nothing
  where
    exists n = isJust (maybeNodeLookup n g)
    go input = \case
      Source Current -> [[ConcreteRoute cur []] | exists cur]
      Source (Fixed n) -> [[ConcreteRoute n []] | exists n]
      Source (Var _) -> error "interpret: variable root"
      Stay -> [[ConcreteRoute (fromJustEx input) []]]
      Step d m ->
        let i = fromJustEx input
            connects = case maybeNodeLookup i g of
              Nothing -> []
              Just n -> toList case d of
                Out -> n.outgoing
                In -> n.incoming
         in [ [ConcreteRoute i [ConcreteHop d c.transition c.node]]
            | c <- connects,
              matcherAccepts m c.transition
            ]
      Both a b rest -> map concat (traverse (go input) (a : b : rest))
      Join p test ->
        [ ws
        | ws <- go input p,
          e : es <- [map endOf ws],
          all (== e) es,
          testAccepts test e
        ]
      Sequence p q ->
        [ [ConcreteRoute w.root (w.hops ++ c.hops) | w <- ws, c <- cs]
        | ws <- go input p,
          j <- ordNub (map endOf ws),
          cs <- go (Just j) q
        ]
    endOf r = maybe r.root (.target) (lastMay r.hops)

-- | Every alternative that has a path without unions matches what the
-- reference's terms for that path match, evaluated directly.
spec_compileOracle :: Spec
spec_compileOracle = describe "compiled patterns agree with the interpreter" do
  forM_ corpusCompileQueries \surface -> it (show surface) do
    let choices = ordNub (map snd (compileAlternatives surface))
    forM_ choices \choice -> do
      let exprs = map (rooted . fst) (referenceExprs WrittenOut choice)
      forM_ sampled \g ->
        (show (choice, g), matchedRoutes g choice)
          `shouldBe` ( show (choice, g),
                       setFromList (concatMap (interpretExpr g corpusCurrent) exprs)
                     )
  it "an alternative in which the routes chose different sides" do
    -- one of the alternatives interpreted above
    map snd (compileAlternatives [path| (a | b)/(a & b) |])
      `shouldSatisfy` elem [path| a/a & b/b |]
  it "an alternative that is no path without unions" do
    -- In (a/@ | b)/((a & b)/@/a & b) the routes through the conjuncts a
    -- and b of the joined conjunct may choose a/@ and b, and the route
    -- through the last b choose a/@ too: one walk a for the first and
    -- the last, which no path without unions has. Found here by hand:
    -- cur -a-> n, n -a-> m, cur -b-> k, k -b-> m, m -a-> t, n -b-> u.
    let p = [path| (a/@ | b)/((a & b)/@/a & b) |]
        spelled = setFromList (map fst (compileAlternatives p)) :: Set PatternPath
        labels a = [[t | h <- r.hops, MLiteral t <- [h.matcher]] | r <- toList a.routes]
        unspelled =
          [ a
          | a <- toList (compilePath p).alternatives,
            a `notMember` spelled,
            labels a == [["a", "a", "a"], ["a", "b"], ["b", "b", "a"]]
          ]
        out t n g =
          [ c.node
          | Just node <- [maybeNodeLookup n g],
            c <- toList node.outgoing,
            c.transition == t
          ]
        hop = ConcreteHop Out
        byHand g =
          setFromList
            [ [ ConcreteRoute cur [hop "a" n, hop "a" m, hop "a" t],
                ConcreteRoute cur [hop "a" n, hop "b" u],
                ConcreteRoute cur [hop "b" k, hop "b" m, hop "a" t]
              ]
            | let cur = corpusCurrent,
              isJust (maybeNodeLookup cur g),
              n <- out "a" cur g,
              m <- out "a" n g,
              k <- out "b" cur g,
              m' <- out "b" k g,
              m' == m,
              t <- out "a" m g,
              u <- out "b" n g
            ] ::
            Set [ConcreteRoute]
    length unspelled `shouldBe` 1
    forM_ unspelled \a -> do
      forM_ sampled \g ->
        (show g, matchedBy g (PathQuery (singletonSet a)))
          `shouldBe` (show g, byHand g)
      -- some graph has it
      any (not . null . byHand) sampled `shouldBe` True
  where
    sampled = corpusGraphs

-- * Chain-scoped joins

-- | A path without relational unions compiles exactly as the compiler
-- before unions compiled it, outside two classes (law K); inside them it
-- is the product written out, which the reference with the two changes
-- gives. Route 0 has the same steps everywhere.
spec_compileUnionFree :: Spec
spec_compileUnionFree = describe "paths without | compile as before" do
  let cases =
        ordNub $
          unGen
            ( (<>)
                <$> vectorOf 2500 (choose (2, 7) >>= genUnionFree)
                <*> vectorOf 1500 genInClasses
            )
            (mkQCGen 20261007)
            30
      inClass = inChangedClass
      changed p = compilePath p /= referenceCompile AsBefore p
      firstSteps q =
        setFromList
          [ [(h.direction, h.matcher) | h <- first'.hops]
          | a <- toList q.alternatives,
            first' : _ <- [toList a.routes]
          ] ::
          Set [(Direction, Matcher)]
  it "outside the two classes exactly as before" do
    let outside = filter (not . inClass) cases
    length outside `shouldSatisfy` (> 1500)
    forM_ outside \p ->
      (show p, compilePath p) `shouldBe` (show p, referenceCompile AsBefore p)
  it "everywhere as the product written out" $
    forM_ cases \p ->
      (show p, compilePath p) `shouldBe` (show p, referenceCompile WrittenOut p)
  it "route 0 has the steps it had" $
    -- (an alternative may be gone, where a root became a test, or be new,
    -- where a join pinned to two nodes is now two joins)
    forM_ cases \p -> do
      let now = firstSteps (compilePath p)
          before = firstSteps (referenceCompile AsBefore p)
      unless (null now || null before) $
        (show p, now) `shouldBe` (show p, before)
  it "the class is met, and changes what a path compiles to" do
    length (filter changed (filter inClass cases)) `shouldSatisfy` (> 200)
    length (filter (snd . referenceClasses) (filter changed cases))
      `shouldSatisfy` (> 50)
    -- the reference meets the class wherever a path is in it as written
    filter (\p -> inClass p && not (fst (referenceClasses p))) cases
      `shouldBe` []
  it "the examples of the two classes" do
    -- class 1: the conjunct x/@/w is copied whole, so there are two x
    compilePath [path| (x/@/w & b)/(c & d) |]
      `shouldBe` compilePath [path| x/@/w/c & x/@/w/d & b/c & b/d |]
    compilePath [path| (x/@/w & b)/k/(c & d) |]
      `shouldBe` compilePath [path| x/@/w/k/c & x/@/w/k/d & b/k/c & b/k/d |]
    compilePath [path| (x/@ & b)/(c & d) |]
      `shouldBe` compilePath [path| x/@/c & x/@/d & b/c & b/d |]
    -- a join of the chain between the group and the fork still stops it
    compilePath [path| (x/@/w & b)/k/@/(c & d) |]
      `shouldBe` referenceCompile AsBefore [path| (x/@/w & b)/k/@/(c & d) |]
    -- and so does a join the fork follows in its own chain
    compilePath [path| x/@/w/(c & d) |]
      `shouldBe` referenceCompile AsBefore [path| x/@/w/(c & d) |]
    referenceClasses [path| (x/@/w & b)/(c & d) |] `shouldBe` (True, False)
    referenceClasses [path| (x/@/w & b)/k/@/(c & d) |] `shouldBe` (False, False)
    inChangedClass [path| (x/@/w & b)/(c & d) |] `shouldBe` True
    inChangedClass [path| (x/@/w & b)/k/@/(c & d) |] `shouldBe` False
    -- no group follows the group here: the routes are in the order they
    -- were
    inChangedClass [path| (@ & @)/@/(a & b/@) & c |] `shouldBe` False
    compilePath [path| (@ & @)/@/(a & b/@) & c |]
      `shouldBe` referenceCompile AsBefore [path| (@ & @)/@/(a & b/@) & c |]
    -- to keep one x for both branches without merging b
    compilePath [path| x/@/w/(c & d) & b/(c & d) |]
      `shouldBe` referenceCompile AsBefore [path| (x/@/w & b)/(c & d) |]
    -- class 2: the @N after the @ conjunct tests the current node
    compilePath [path| (a & @)/(@000000000001 & d) |]
      `shouldBe` compilePath
        [path| a/@000000000001 & a/d & @/@000000000001 & @/d |]
    referenceClasses [path| (a & @)/(@000000000001 & d) |]
      `shouldBe` (True, True)
    -- a conjunct that is only @ is the join it is in @/…, so it is in
    -- class 1 too: here @/(c & d)/@ has one q where there were two.
    -- No command runs this path now (the group before would copy
    -- (c & d)/@, where c and d meet); what it compiles to is still its
    -- product written out, which is what the error says to write
    refuses [path| q/(@ & b)/((c & d)/@ & e) |]
    isRight (checkCopies [path| q/@/(c & d)/@ & q/e & q/b/(c & d)/@ & q/b/e |])
      `shouldBe` True
    compilePath [path| q/(@ & b)/((c & d)/@ & e) |]
      `shouldBe` compilePath
        [path| q/@/(c & d)/@ & q/e & q/b/(c & d)/@ & q/b/e |]
    compilePath [path| q/(@ & b)/((c & d)/@ & e) |]
      `shouldNotBe` referenceCompile AsBefore [path| q/(@ & b)/((c & d)/@ & e) |]
    compilePath [path| q/(@ & b)/((c & d)/@ & e) |]
      `shouldBe` referenceCompile WrittenOut [path| q/(@ & b)/((c & d)/@ & e) |]
  it "a leading @ keeps its conjunct together, as the product written out" do
    -- The conjunct @/(c & d) has a join of its own, so it is copied whole
    -- and is no group of the stretch: @/(c & d)/@/e merges c and d,
    -- which (c & d & a)/(@/e & y) does not. Before, the @ was left out.
    let p = [path| (@/(c & d) & a)/(@/e & y) |]
    -- no command runs it, nor the one below: @/(c & d) is a meeting
    -- point (c and d leave one node) that the next group would copy.
    -- The compiler still gives the product written out for both.
    refuses p
    refuses [path| (@/(c & d) & a)/(e & y) |]
    runsQuietly [path| @/(c & d)/e & @/(c & d)/y & a/e & a/y |]
    compilePath p `shouldBe` referenceCompile WrittenOut p
    compilePath p `shouldNotBe` compilePath [path| (c & d & a)/(@/e & y) |]
    compilePath p `shouldNotBe` referenceCompile AsBefore p
    inChangedClass p `shouldBe` True
    -- when the group after it has no join, nothing merges and the two
    -- are the same six walks
    -- (listed copy by copy: c/e, d/e, c/y, d/y, where the conjuncts
    -- c and d list c/e, c/y, d/e, d/y)
    alternativesUpToOrder [path| (@/(c & d) & a)/(e & y) |]
      `shouldBe` alternativesUpToOrder [path| (c & d & a)/(e & y) |]
    compilePath [path| (@/(c & d) & a)/(e & y) |]
      `shouldBe` compilePath [path| @/(c & d)/e & @/(c & d)/y & a/e & a/y |]
    -- an @N after a leading @ is still a test of the current node
    [ r.root
      | a <- toList (compilePath [path| (@/@000000000001/c & a)/(e & y) |]).alternatives,
        r <- toList a.routes
      ]
      `shouldSatisfy` all (== current)
  where
    atom = elements [[path| a |], [path| b |], [path| c |], [path| ~a |], [path| * |]]
    node =
      elements [P.One, [path| @000000000001 |], [path| !{@000000000001} |]]
    genUnionFree :: Int -> Gen P.Path
    genUnionFree n
      | n <= 1 = frequency [(3, atom), (2, node)]
      | otherwise = do
          k <- choose (1, n - 1)
          frequency
            [ (5, (P.:/) <$> genUnionFree k <*> genUnionFree (n - k)),
              (4, (P.:&) <$> genUnionFree k <*> genUnionFree (n - k)),
              (1, (P.:++) <$> genUnionFree k <*> genUnionFree (n - k)),
              (1, P.Backwards <$> genUnionFree (n - 1))
            ]
    -- a conjunction with a join or a lone @ in a conjunct, a conjunction
    -- after it, and sometimes something in between or around
    genInClasses :: Gen P.Path
    genInClasses = do
      left <- conj <$> (choose (2, 3) >>= \n -> vectorOf n (elements lefts))
      right <- conj <$> (choose (2, 3) >>= \n -> vectorOf n (elements rights))
      between <-
        elements
          [ id,
            id,
            (P.:/ [path| k |]),
            (P.:/ [path| k/@ |]),
            (P.:/ [path| k & l |]),
            (P.:/ P.One)
          ]
      around <-
        elements
          [ id,
            id,
            ([path| q |] P.:/),
            ([path| q/@ |] P.:/),
            (P.:/ [path| t |]),
            (P.:/ P.One),
            ([path| q |] P.:&),
            ([path| q & r |] P.:/)
          ]
      pure (around (between left P.:/ right))
    conj = foldl1Ex' (P.:&)
    lefts =
      [ [path| x/@ |],
        [path| x/@/w |],
        [path| (a & b)/@ |],
        [path| x/@/(b & c) |],
        [path| x/!{@000000000001} |],
        [path| x/@000000000001/w |],
        [path| @ |],
        [path| @ |],
        [path| @000000000001 |],
        [path| !{@000000000001} |],
        [path| @/a |],
        [path| a |],
        [path| b/c |],
        [path| a/(b & c) |]
      ]
    rights =
      [ [path| c |],
        [path| d |],
        [path| c/@ |],
        [path| @/c |],
        [path| @000000000001 |],
        [path| @000000000001/c |],
        [path| @000000000002 |],
        [path| !{@000000000001} |],
        [path| @ |],
        [path| c/(e & f) |],
        [path| (c & d)/@ |]
      ]

-- | Two adjacent conjunctions are their product, every conjunct copied
-- whole with its joins and its unions (law H): the alternatives are those
-- of the product written out, also inside a longer path.
spec_compileProduct :: Spec
spec_compileProduct = describe "adjacent conjunctions are their product" do
  let cases =
        unGen
          ( vectorOf 600 $
              (,,)
                <$> (choose (2, 3) >>= \n -> vectorOf n (elements lefts))
                <*> (choose (2, 3) >>= \n -> vectorOf n (elements rights))
                <*> elements contexts
          )
          (mkQCGen 20261008)
          30
      conj = foldl1Ex' (P.:&)
      withinLimit x = fewAlternatives 300 x
      checked =
        [ (plain, whole, product')
        | (ls, rs, context) <- cases,
          let whole = context (conj (map snd ls) P.:/ conj (map snd rs))
              product' =
                context (conj [l P.:/ r | (_, l) <- ls, (_, r) <- rs])
              plain = all fst ls && all fst rs,
          withinLimit whole && withinLimit product'
        ]
  it "random conjunctions with joins, unions and conjuncts without a step" do
    length checked `shouldSatisfy` (> 400)
    length (filter (\(plain, _, _) -> not plain) checked)
      `shouldSatisfy` (> 200)
    forM_ checked \(_, whole, product') ->
      (show whole, alternativesUpToOrder whole)
        `shouldBe` (show whole, alternativesUpToOrder product')
  it "with the routes in the same order when no conjunct forks or chooses" $
    forM_ checked \(plain, whole, product') ->
      when plain $
        (show whole, compilePath whole)
          `shouldBe` (show whole, compilePath product')
  it "the unions in the copies of a conjunct are different unions" do
    -- each copy of (x/@ | y) chooses on its own, and two that choose x/@
    -- are two walks
    alternativesUpToOrder [path| (p & q)/((x/@ | y) & z) |]
      `shouldBe` alternativesUpToOrder
        [path| p/(x/@ | y) & p/z & q/(x/@ | y) & q/z |]
    length (compilePath [path| (p & q)/((x/@ | y) & z) |]).alternatives
      `shouldBe` 4
    alternativesUpToOrder [path| ((a/@ | b)/c & d)/(a & b) |]
      `shouldBe` alternativesUpToOrder
        [path| (a/@ | b)/c/a & (a/@ | b)/c/b & d/a & d/b |]
  it "nested conjunctions are one conjunction first" do
    -- the leading @ joins only its own copy's input, after p, q and r
    compilePath [path| ((p & q) & r)/(@/x & y) |]
      `shouldBe` compilePath
        [path| p/@/x & p/y & q/@/x & q/y & r/@/x & r/y |]
    -- and so is a conjunct that forks without a join: it is the
    -- conjuncts distribution makes of it, as it always was
    compilePath [path| (x/@ & d)/(c/(e & f) & y) |]
      `shouldBe` compilePath [path| (x/@ & d)/(c/e & c/f & y) |]
    compilePath [path| (k/(m & n) & d)/(c/@ & y) |]
      `shouldBe` compilePath [path| (k/m & k/n & d)/(c/@ & y) |]
    -- with a join before the fork it is one conjunct, copied whole:
    -- which no command runs, since e and f leave one node in it. The
    -- copies written out, which the error says to write, are run, and
    -- are what the compiler makes of it
    refuses [path| (x/@ & d)/(c/@/(e & f) & y) |]
    isRight (checkCopies [path| x/@/c/@/(e & f) & x/@/y & d/c/@/(e & f) & d/y |])
      `shouldBe` True
    compilePath [path| (x/@ & d)/(c/@/(e & f) & y) |]
      `shouldBe` compilePath
        [path| x/@/c/@/(e & f) & x/@/y & d/c/@/(e & f) & d/y |]
  it "a shared union is chosen before" $
    -- not the product: both copies have the same side
    compilePath [path| (a & b)/((x ++ y) & z) |]
      `shouldBe` compilePath
        [path| (a & b)/(x & z) ++ (a & b)/(y & z) |]
  where
    -- whether the conjunct is one route and has no union
    lefts =
      [ (True, [path| a |]),
        (True, [path| b/c |]),
        (True, [path| x/@ |]),
        (True, [path| x/@/w |]),
        (True, [path| x/!{@000000000001}/w |]),
        (True, P.One),
        (True, [path| @000000000001 |]),
        (True, [path| !{@000000000001} |]),
        (True, [path| @000000000001/a |]),
        (False, [path| (a & b)/@ |]),
        (False, [path| x/@/(b & c) |]),
        (False, [path| a | b |]),
        (False, [path| x/@ | y |]),
        (False, [path| (a | b)/@ |]),
        (False, [path| a/(b | c/@) |]),
        (False, [path| (a & b | c)/@ |]),
        -- a leading @ keeps the conjunct it starts together
        (True, [path| @/a |]),
        (False, [path| @/(a & b) |]),
        (False, [path| @/(a & b)/c |]),
        (False, [path| @/a/(b & c) |]),
        (False, [path| @/(a & b)/@ |])
      ]
    rights =
      [ (True, [path| c |]),
        (True, [path| d |]),
        (True, [path| c/@ |]),
        (True, [path| @/c |]),
        (True, [path| @000000000001 |]),
        (True, [path| @000000000001/c |]),
        (True, [path| !{@000000000001} |]),
        (True, P.One),
        (False, [path| (c & d)/@ |]),
        (False, [path| c | d |]),
        (False, [path| c/@ | d |]),
        (False, [path| @000000000001 | zz |]),
        (False, [path| @/(c & d) |]),
        (False, [path| @/(c & d)/@ |]),
        (False, [path| @/c/(d & e) |])
      ]
    contexts :: [P.Path -> P.Path]
    contexts =
      [ id,
        id,
        ([path| k/@ |] P.:/),
        ([path| k |] P.:/),
        (P.:/ P.One),
        (P.:/ [path| t |]),
        ([path| q |] P.:&),
        (P.:/ [path| t/@/(u & v) |])
      ]

-- | Reversal (law I): a relative path without an @\@N@ reaches @t@ from
-- @c@ exactly when its reverse reaches @c@ from @t@, as queries.
spec_compileReversal :: Spec
spec_compileReversal = describe "the reverse of a path is its converse" do
  let cases =
        unGen
          ( (<>)
              <$> vectorOf 400 ((,) <$> (choose (1, 6) >>= genRelative) <*> vectorOf 3 genGraph)
              <*> vectorOf 300 ((,) <$> genJoined <*> vectorOf 4 genGraph)
          )
          (mkQCGen 20261009)
          30
      withinLimit x = fewAlternatives 100 (x P.:/ P.One)
      checked = [(p, gs) | (p, gs) <- cases, withinLimit p]
      reached cur g surface =
        foldMap
          targets
          (resolveFrom (smallNID cur) g (compilePath (surface P.:/ P.One))).resolutions
  it "on random paths and graphs" do
    length checked `shouldSatisfy` (> 550)
    -- the paths that commands run; the others are refused in both
    -- directions, and the compiler makes a converse of them all the same
    length (filter (runs . fst) checked) `shouldSatisfy` (> 300)
    forM_ checked \(p, _) ->
      (show p, runs (P.Backwards p)) `shouldBe` (show p, runs p)
    forM_ checked \(p, gs) -> forM_ gs \(edges, g) ->
      forM_ nodes \c -> forM_ nodes \t ->
        (show (p, edges, c, t), smallNID t `member` reached c g p)
          `shouldBe` ( show (p, edges, c, t),
                       smallNID c `member` reached t g (P.Backwards p)
                     )
  it "the example that failed before chain-scoped joins" do
    let g =
          graphOf
            [0, 1, 2, 3, 4, 9]
            [ (0, "p", 1),
              (0, "q", 2),
              (1, "x", 3),
              (3, "w", 9),
              (2, "x", 4),
              (4, "w", 9),
              (1, "y", 9),
              (2, "y", 9)
            ]
    reached 0 g [path| (p & q)/(x/@/w & y) |] `shouldBe` singletonSet (smallNID 9)
    reached 9 g [path| (~w/@/~x & ~y)/(~p & ~q) |]
      `shouldBe` singletonSet (smallNID 0)
    reached 9 g [path| ~((p & q)/(x/@/w & y)) |]
      `shouldBe` singletonSet (smallNID 0)
    reached 0 g [path| (p & q)/x/@/w |] `shouldBe` mempty
    reached 9 g [path| ~w/@/~x/(~p & ~q) |] `shouldBe` mempty
  it "a conjunct that forks and joins, and its reverse, which starts with @" do
    -- a and b end apart, so ((a & b)/@ & c)/q finds nothing; its
    -- reverse from 9 finds nothing either, because the leading @ of
    -- @/(~a & ~b) is kept: without it 0 is found
    let apart =
          graphOf
            [0, 1, 2, 3, 9]
            [ (0, "a", 1),
              (0, "b", 3),
              (1, "q", 9),
              (3, "q", 9),
              (0, "c", 2),
              (2, "q", 9)
            ]
    runsQuietly [path| ((a & b)/@ & c)/q |]
    runsQuietly [path| ~q/(@/(~a & ~b) & ~c) |]
    reached 0 apart [path| ((a & b)/@ & c)/q |] `shouldBe` mempty
    reached 9 apart [path| ~q/(@/(~a & ~b) & ~c) |] `shouldBe` mempty
    reached 9 apart [path| ~(((a & b)/@ & c)/q) |] `shouldBe` mempty
    reached 9 apart [path| ~q/(~a & ~b & ~c) |]
      `shouldBe` singletonSet (smallNID 0)
  it "between two groups such a conjunct is refused, as is its reverse" do
    -- The compiler still makes a converse of the reverse: c/@/(c & d)/@
    -- finds no node with both c and d to 9, and @/(~c & ~d)/@/~c has to
    -- merge ~c and ~d likewise. But no command runs either path: the
    -- group next to (c & d)/@ would copy the node c and d meet at.
    refuses [path| (x & c/@)/((c & d)/@ & a) |]
    refuses [path| (@/(~c & ~d) & ~a)/(~x & @/~c) |]
    refuses [path| ~((x & c/@)/((c & d)/@ & a)) |]
    let g =
          graphOf
            [0, 1, 2, 5, 9]
            [ (0, "x", 1),
              (1, "c", 9),
              (1, "d", 9),
              (1, "a", 9),
              (0, "c", 2),
              (0, "c", 5),
              (2, "c", 9),
              (5, "d", 9),
              (2, "a", 9),
              (5, "a", 9)
            ]
    reached 0 g [path| (x & c/@)/((c & d)/@ & a) |] `shouldBe` mempty
    reached 9 g [path| (@/(~c & ~d) & ~a)/(~x & @/~c) |] `shouldBe` mempty
    reached 9 g [path| ~((x & c/@)/((c & d)/@ & a)) |] `shouldBe` mempty
    -- without the leading @ the three are conjuncts of the group, which
    -- the next group copies apart
    reached 9 g [path| (~c & ~d & ~a)/(~x & @/~c) |]
      `shouldBe` singletonSet (smallNID 0)
  where
    nodes = [0, 1, 2 :: Int]
    genGraph = do
      edges <-
        choose (2, 8) >>= \n ->
          vectorOf n $
            (,,) <$> elements nodes <*> elements ["a", "b", "c"] <*> elements nodes
      pure (edges, graphOf nodes edges)
    -- two groups, with conjuncts that start with @ and fork, or fork and
    -- join, and sometimes something around them
    genJoined :: Gen P.Path
    genJoined = do
      let conj = foldl1Ex' (P.:&)
          part =
            elements
              [ [path| a |],
                [path| b/c |],
                [path| c/@ |],
                [path| @/a |],
                [path| @/(a & b) |],
                [path| @/(c & b)/a |],
                [path| @/a/(b & c) |],
                [path| (a & b)/@ |],
                [path| (c & a)/@/b |],
                [path| @/(a & c)/@ |],
                [path| ~a/@ |],
                [path| @/~b |],
                P.One
              ]
      left <- conj <$> (choose (2, 3) >>= \n -> vectorOf n part)
      right <- conj <$> (choose (2, 3) >>= \n -> vectorOf n part)
      around <-
        elements
          [ id,
            id,
            id,
            ([path| a |] P.:/),
            (P.:/ [path| b |]),
            ([path| c/@ |] P.:/),
            (P.:/ [path| @/c |]),
            P.Backwards
          ]
      pure (around (left P.:/ right))
    genRelative :: Int -> Gen P.Path
    genRelative n
      | n <= 1 =
          frequency
            [ (6, elements [[path| a |], [path| b |], [path| c |], [path| ~a |], [path| * |]]),
              (2, pure P.One),
              (1, pure [path| !{@000000000001} |])
            ]
      | otherwise = do
          k <- choose (1, n - 1)
          frequency
            [ (5, (P.:/) <$> genRelative k <*> genRelative (n - k)),
              (4, (P.:&) <$> genRelative k <*> genRelative (n - k)),
              (2, (P.:||) <$> genRelative k <*> genRelative (n - k)),
              (1, (P.:++) <$> genRelative k <*> genRelative (n - k)),
              (1, P.Backwards <$> genRelative (n - 1))
            ]

-- | A union is the choices its routes make and nothing else (law J): the
-- alternatives of @C[p | q]@ in which no route chose @q@ are exactly
-- those of @C[p]@, routes in the same order. The side @q@ has a step no
-- other part of the path has on each of its routes, which tells the
-- routes that chose it. So adding a side never removes an alternative.
spec_compileUnionIsItsChoices :: Spec
spec_compileUnionIsItsChoices =
  describe "a union is the choices of its routes" do
    let cases =
          unGen
            ( vectorOf 500 $
                (,,,,)
                  <$> (choose (1, 4) >>= genPath)
                  <*> elements marked
                  <*> (choose (1, 3) >>= genPath)
                  <*> (choose (1, 3) >>= genPath)
                  <*> choose (0 :: Int, 7)
            )
            (mkQCGen 20261010)
            30
        withinLimit x = fewAlternatives 150 x
        checked =
          [ (plug ctx c d p, plug ctx c d (p P.:|| q), plug ctx c d (q P.:|| p))
          | (p, q, c, d, ctx) <- cases,
            withinLimit (plug ctx c d (p P.:|| q))
          ]
        choseMarked a =
          or [m `elem` marks | r <- toList a.routes, Hop _ _ m _ <- r.hops]
        withoutMarked x =
          setFromList (filter (not . choseMarked) (toList (compilePath x).alternatives))
    it "the routes that all chose p are C[p]" do
      length checked `shouldSatisfy` (> 300)
      length (filter (\(base, _, _) -> length (compilePath base).alternatives > 1) checked)
        `shouldSatisfy` (> 100)
      forM_ checked \(base, bigger, swapped) -> do
        (show bigger, withoutMarked bigger)
          `shouldBe` (show bigger, (compilePath base).alternatives)
        (show swapped, compilePath swapped) `shouldBe` (show swapped, compilePath bigger)
    it "the routes are in the order of C[p] also where a conjunct has a join" do
      -- the conjunct (!{@1} & *)/!{@1} is copied whole for each conjunct
      -- of the group that follows, and its routes are listed together.
      -- (No command runs these two paths: two walks meet at the !{@1}
      -- that ends the conjunct, and the other group would copy it. The
      -- order of the routes is still the compiler's to keep.)
      refuses [path| (x & (!{@000000000001} & *)/!{@000000000001})/((!{@000000000001} & *)/!{@000000000001}) |]
      (compilePath [path| ((x | zq) & (!{@000000000001} & *)/!{@000000000001})/((!{@000000000001} & *)/!{@000000000001}) |]).alternatives
        `shouldSatisfy` ( null
                            . ( (compilePath [path| (x & (!{@000000000001} & *)/!{@000000000001})/((!{@000000000001} & *)/!{@000000000001}) |]).alternatives
                                  `difference`
                              )
                        )
    it "a side written alone is no further alternative" do
      -- both copies of (x/@ | y) are different unions: no alternative
      -- has one walk of x/@ after both p and q
      alternativesUpToOrder [path| (p & q)/((x/@ | y) & z) |]
        `shouldNotContain'` alternativesUpToOrder [path| (p & q)/x/@ & p/z & q/z |]
      -- 22 sides in two copies, and nothing for a side chosen before it
      -- is copied
      length
        ( compilePath
            [path| (a | b | c & d | e & f)/(a & b) |]
        ).alternatives
        `shouldBe` 16
  where
    shouldNotContain' big small = (small `difference` big) `shouldBe` small
    marks = [MLiteral "zq", MLiteral "zr"]
    marked =
      [ [path| zq |],
        [path| zq/@ |],
        [path| zq & zr |],
        [path| (zq & zr)/@ |],
        [path| @/zq |],
        [path| zq/!{@000000000001} |],
        [path| zq/@/zr |],
        [path| zq/(zr & zq) |]
      ]
    plug :: Int -> P.Path -> P.Path -> P.Path -> P.Path
    plug ctx c d x = case ctx of
      0 -> x
      1 -> x P.:/ c
      2 -> c P.:/ x
      3 -> x P.:& c
      4 -> c P.:/ x P.:/ d
      5 -> (x P.:& c) P.:/ d
      6 -> c P.:/ (x P.:& d)
      _ -> (c P.:& d) P.:/ x P.:/ [path| e & f |]
    genPath :: Int -> Gen P.Path
    genPath n
      | n <= 1 =
          elements
            [ P.Literal "a",
              P.Literal "b",
              P.Wild,
              P.Backwards (P.Literal "a"),
              P.One,
              P.Absolute (smallNID 1),
              P.ExcludingNIDs (singletonSet (smallNID 1))
            ]
      | otherwise = do
          k <- choose (1, n - 1)
          frequency
            [ (4, (P.:/) <$> genPath k <*> genPath (n - k)),
              (3, (P.:&) <$> genPath k <*> genPath (n - k)),
              (3, (P.:||) <$> genPath k <*> genPath (n - k)),
              (1, (P.:++) <$> genPath k <*> genPath (n - k))
            ]

-- | Writing a conjunct twice changes nothing a query finds (law L):
-- @P/(q & q)@ reaches what @P/q@ reaches, when @q@ has no @\@@ or
-- @!{…}@ in its own chain or @P@ has one output. (A join of @q@'s chain
-- is, in @P/q@, a join of the chain @P@ is in, where all the outputs of
-- @P@ meet; in @P/(q & q)@ it is in a conjunct, where each meets it
-- alone.)
spec_compileRepeatedConjunct :: Spec
spec_compileRepeatedConjunct = describe "a conjunct written twice" do
  let cases =
        unGen
          ( vectorOf 300 $
              (,) <$> (choose (1, 4) >>= genPath) <*> (choose (1, 3) >>= genPath)
          )
          (mkQCGen 20261011)
          30
      withinLimit x = fewAlternatives 40 (x P.:/ P.One)
      reached cur g surface =
        foldMap
          targets
          (resolveFrom cur g (compilePath (surface P.:/ P.One))).resolutions
  it "P/(q & q) reaches what P/q reaches" do
    let checked =
          [ (p P.:/ q, p P.:/ (q P.:& q))
          | (p, q) <- cases,
            oneOutput p || not (joinsItsChain q),
            withinLimit (p P.:/ (q P.:& q))
          ]
    length checked `shouldSatisfy` (> 120)
    forM_ checked \(once', twice) -> forM_ graphs \g ->
      (show (twice, g), reached corpusCurrent g twice)
        `shouldBe` (show (twice, g), reached corpusCurrent g once')
  it "sealed, q & q finds what q finds, in every context" do
    -- To seal a path is to write @/ before it and /@ after it: then
    -- nothing around it copies into its joins, and its joins hold
    -- nothing around it together. (The compiler's law: some of these
    -- contexts copy the sealed q & q, which is then a meeting point, so
    -- no command runs it; 'runs' says how many of the pairs are run.)
    let sealed x = P.One P.:/ x P.:/ P.One
        contexts =
          [ id,
            (P.:/ [path| b & c |]),
            ([path| a & b |] P.:/),
            (\x -> [path| a & b |] P.:/ x P.:/ [path| b & c |]),
            (\x -> (x P.:& [path| c |]) P.:/ [path| a & b |]),
            (\x -> [path| a & c |] P.:/ (x P.:& [path| b |])),
            (\x -> [path| a | c/@ |] P.:/ x P.:/ [path| b & c |])
          ] ::
            [P.Path -> P.Path]
        checked =
          [ (ctx (sealed q), ctx (sealed (q P.:& q)))
          | q <- concat [[p, q] | (p, q) <- take 120 cases],
            relative q,
            ctx <- contexts,
            withinLimit (ctx (sealed (q P.:& q)))
          ]
    length checked `shouldSatisfy` (> 600)
    length [() | (once', twice) <- checked, runs once', runs twice]
      `shouldSatisfy` (> 300)
    forM_ checked \(once', twice) -> forM_ (take 6 graphs) \g ->
      (show (twice, g), reached corpusCurrent g twice)
        `shouldBe` (show (twice, g), reached corpusCurrent g once')
  it "but not when q joins the outputs of P" do
    let g = graphOf [0, 1, 2, 9] [(0, "a", 1), (0, "b", 2), (1, "c", 9), (2, "c", 9)]
    reached (smallNID 0) g [path| (a & b)/@/c |] `shouldBe` mempty
    reached (smallNID 0) g [path| (a & b)/(@/c & @/c) |]
      `shouldBe` singletonSet (smallNID 9)
  it "(b & @)/@1 and (b & @)/(@1 & @1)" do
    -- before chain-scoped joins the second jumped to node 1
    let g = graphOf [0, 1] [(0, "b", 1)]
    reached (smallNID 0) g [path| (b & @)/@000000000001 |] `shouldBe` mempty
    reached (smallNID 0) g [path| (b & @)/(@000000000001 & @000000000001) |]
      `shouldBe` mempty
    reached
      (smallNID 1)
      (graphOf [1] [(1, "b", 1)])
      [path| (b & @)/(@000000000001 & @000000000001) |]
      `shouldBe` singletonSet (smallNID 1)
  where
    graphs = [g | (i, g) <- zip [0 :: Int ..] corpusGraphs, i `mod` 37 == 0]
    relative = \case
      P.Absolute _ -> False
      l P.:/ r -> relative l && relative r
      l P.:& r -> relative l && relative r
      P.Union _ l r -> relative l && relative r
      _ -> True
    oneOutput = \case
      _ P.:& _ -> False
      l P.:/ r -> oneOutput l && oneOutput r
      P.Union _ l r -> oneOutput l && oneOutput r
      _ -> True
    joinsItsChain = \case
      P.One -> True
      P.ExcludingNIDs _ -> True
      l P.:/ r -> joinsItsChain l || joinsItsChain r
      P.Union _ l r -> joinsItsChain l || joinsItsChain r
      _ -> False
    genPath :: Int -> Gen P.Path
    genPath n
      | n <= 1 =
          elements
            [ P.Literal "a",
              P.Literal "b",
              P.Wild,
              P.Backwards (P.Literal "a"),
              P.One,
              P.Absolute (smallNID 1),
              P.ExcludingNIDs (singletonSet (smallNID 1))
            ]
      | otherwise = do
          k <- choose (1, n - 1)
          frequency
            [ (4, (P.:/) <$> genPath k <*> genPath (n - k)),
              (3, (P.:&) <$> genPath k <*> genPath (n - k)),
              (2, (P.:||) <$> genPath k <*> genPath (n - k)),
              (1, (P.:++) <$> genPath k <*> genPath (n - k))
            ]

-- | The expected results of the definition of chain-scoped joins
-- (docs/path-pattern-ir.md, Unions), each on its graph, as @ls@ finds
-- them from node 0.
spec_compileChainScoped :: Spec
spec_compileChainScoped = describe "chain-scoped joins" do
  let isSubsetOf' x y = null (x `difference` y)
      ls cur edges surface =
        map smallNIDIndex . toList $
          foldMap
            targets
            ( resolveFrom
                (smallNID cur)
                (graphOf (ordNub (0 : concat [[s, t] | (s, _, t) <- edges])) edges)
                (compilePath (surface P.:/ P.One))
            ).resolutions
      twoX = [(0, "x", 1), (0, "x", 2), (1, "c", 9), (2, "d", 9)]
  it "a join stops the copying of what is before it in its chain" $
    ls 0 twoX [path| x/@/(c & d) |] `shouldBe` []
  it "a leading @ is a join: the forks of its conjunct meet at the next" do
    let g =
          [ (0, "a", 1),
            (0, "b", 2),
            (1, "f", 9),
            (2, "f", 9),
            (0, "c", 3),
            (3, "f", 9),
            (1, "e", 9),
            (2, "e", 9),
            (3, "e", 9)
          ]
    -- the first has the chain @/(a & b)/@/f, where a and b have to meet:
    -- as a group that the next one copies it is refused, and written
    -- out it finds nothing
    refuses [path| (@/(a & b) & c)/(@/f & e) |]
    refuses [path| (@/(a & b) & c)/(f & e) |]
    runsQuietly [path| @/(a & b)/@/f & @/(a & b)/e & c/@/f & c/e |]
    ls 0 g [path| @/(a & b)/@/f & @/(a & b)/e & c/@/f & c/e |] `shouldBe` []
    compilePath [path| (@/(a & b) & c)/(@/f & e) |]
      `shouldBe` compilePath [path| @/(a & b)/@/f & @/(a & b)/e & c/@/f & c/e |]
    ls 0 g [path| (a & b & c)/(@/f & e) |] `shouldBe` [Just 9]
    ls 0 g [path| @/(a & b)/f & @/(a & b)/e & c/f & c/e |] `shouldBe` [Just 9]
  it "the copies of a union that choose a joined side share its walk" do
    ls 0 twoX [path| (x/@ | zz)/(c & d) |] `shouldBe` []
    -- a side written twice is two sides: the copies may choose one each
    ls 0 twoX [path| (x/@ | x/@)/(c & d) |] `shouldBe` [Just 9]
    ls 0 twoX [path| (x/@ ++ x/@)/(c & d) |] `shouldBe` []
    ls 0 twoX [path| (x | x/@)/(c & d) |] `shouldBe` [Just 9]
  it "conjuncts in another order are another side" do
    let g =
          [ (0, "a", 1),
            (0, "b", 1),
            (0, "a", 2),
            (0, "b", 2),
            (1, "c", 9),
            (2, "d", 9)
          ]
    ls 0 g [path| ((a & b)/@ | (b & a)/@)/(c & d) |] `shouldBe` [Just 9]
    ls 0 g [path| ((a & b) | (b & a))/@/(c & d) |] `shouldBe` []
  it "one walk for a route inside a joined conjunct and one outside it" do
    let g d =
          [ (0, "x", 1),
            (1, "c", 5),
            (0, "s", 3),
            (3, "y", 5),
            (5, "e", 9),
            (0, "x", 2),
            (d, "d", 9)
          ]
        p = [path| (x/@ | s)/((c & y)/@/e & d) |]
    ls 0 (g 2) p `shouldBe` []
    ls 0 (g 1) p `shouldBe` [Just 9]
    -- that alternative is no path without unions
    length (compilePath p).alternatives `shouldBe` 8
    length (ordNub (map fst (compileAlternatives p))) `shouldBe` 6
  it "the routes of an alternative are listed as its spelling lists them" do
    let spelled q = ordNub (map fst (compileAlternatives q))
        mixed = [path| (c & d)/@/(w/@ | z)/(a & b) |]
    -- the copy before a chose w/@ and the one before b chose z
    lookup
      (compilePath [path| (c & d)/@/(w/@/a & z/b) |])
      (mapFromList [(PathQuery (singletonSet a), c) | (a, c) <- compileAlternatives mixed] :: Map PathQuery P.Path)
      `shouldSatisfy` isJust
    length (spelled mixed) `shouldBe` 4
    length (spelled [path| (c & d)/(x/@ | y)/(w/@ | z)/(a & b) |])
      `shouldBe` 132
  it "an alternative that starts with a side that is only @ is spelled without it" do
    let p = [path| (@ | x)/((c & y)/@ & d) |]
    lookup
      (compilePath [path| (c & x/y)/@ & d |])
      (mapFromList [(PathQuery (singletonSet a), c) | (a, c) <- compileAlternatives p] :: Map PathQuery P.Path)
      `shouldBe` Just [path| (c & x/y)/@ & d |]
    length (ordNub (map fst (compileAlternatives p))) `shouldBe` 8
  it "routes that cross over at a shared join end where a route ends" do
    -- When the copies before @ after p and before b after q choose x/@,
    -- the route that crosses over from q to the @ would end with q's x,
    -- where no route ends. Each alternative has four routes that were
    -- chosen, so no more than four last steps.
    let lastSteps a =
          setFromList [(.edge) <$> lastMay r.hops | r <- toList a.routes] ::
            Set (Maybe EdgeId)
    forM_
      [ [path| (p & q)/(x/@ | y)/(@ & b) |],
        [path| (p & q)/(x/@ | y)/(w/@ | z)/(@ & b) |],
        [path| (c & d)/(x/@ | y)/(@ | z)/(a & @) |]
      ]
      \p -> forM_ (compilePath p).alternatives \a ->
        (show (p, a), length (lastSteps a)) `shouldSatisfy` ((<= 4) . snd)
    -- where they end with a step some route ends with, they are there
    compilePath [path| (p & q)/x/@/(a & b) & p/y/b & q/y/a |]
      `shouldSatisfy` (\q -> q.alternatives `isSubsetOf'` (compilePath [path| (p & q)/(x/@ | y)/(a & b) |]).alternatives)
  it "nesting a joined conjunction after a union changes nothing" do
    let g =
          [ (0, "x", 1),
            (1, "b", 9),
            (0, "x", 2),
            (2, "c", 9),
            (0, "s", 3),
            (3, "a", 9)
          ]
        together' =
          [ (0, "x", 1),
            (1, "b", 9),
            (1, "c", 9),
            (0, "x", 2),
            (0, "s", 3),
            (3, "a", 9)
          ]
    ls 0 g [path| (x/@ | s)/((a & b)/@ & c)/@ |] `shouldBe` []
    ls 0 g [path| (x/@ | s)/(a & b & c)/@ |] `shouldBe` []
    ls 0 together' [path| (x/@ | s)/((a & b)/@ & c)/@ |] `shouldBe` [Just 9]
    ls 0 together' [path| (x/@ | s)/(a & b & c)/@ |] `shouldBe` [Just 9]
    alternativesUpToOrder [path| (x/@ | s)/((a & b)/@ & c)/@ |]
      `shouldBe` alternativesUpToOrder [path| (x/@ | s)/(a & b & c)/@ |]
  it "a join in a conjunct merges only within that copy" do
    let g =
          [ (0, "p", 1),
            (0, "q", 2),
            (1, "x", 3),
            (3, "w", 9),
            (2, "x", 4),
            (4, "w", 9),
            (1, "y", 9),
            (2, "y", 9)
          ]
        third = [(0, "r", 5), (5, "s", 6), (6, "w", 9), (5, "y", 9)]
    ls 0 g [path| (p & q)/(x/@/w & y) |] `shouldBe` [Just 9]
    ls 0 g [path| (p & q)/x/@/w |] `shouldBe` []
    ls 0 g [path| (p & q)/((x/@ | s)/w & y) |] `shouldBe` [Just 9]
    ls 0 (g <> third) [path| (p & q & r)/((x/@ | s)/w & y) |]
      `shouldBe` [Just 9]
  it "a join in a conjunct of a left group does not stop its copying" do
    let g =
          [ (0, "x", 1),
            (1, "w", 2),
            (2, "c", 9),
            (0, "x", 3),
            (3, "w", 4),
            (4, "d", 9),
            (0, "b", 5),
            (5, "c", 9),
            (5, "d", 9)
          ]
    ls 0 g [path| (x/@/w & b)/(c & d) |] `shouldBe` [Just 9]
    ls 0 g [path| x/@/w/(c & d) & b/(c & d) |] `shouldBe` []
  it "a side's conjunction keeps its own joins, copy by copy" $
    -- the alternative in which both copies choose (x/@ & z) is that side
    -- written there, with two walks of x
    ( (compilePath [path| (x/@ & z)/(c & d) |]).alternatives
        `difference` (compilePath [path| ((x/@ & z) | y)/(c & d) |]).alternatives
    )
      `shouldBe` mempty
  it "what follows the join of a shared side is still copied" $
    ls
      0
      [(0, "x", 1), (1, "w", 2), (2, "c", 9), (1, "w", 3), (3, "d", 9)]
      [path| (x/@/w | zz)/(c & d) |]
      `shouldBe` [Just 9]
  it "an @N after a conjunct that is only @ tests the current node" do
    let g = [(0, "a", 1), (0, "d", 1), (1, "d", 1)]
    ls 0 g [path| (a & @)/(@000000000001 & d) |] `shouldBe` []
    ls 0 g [path| (a & @)/@000000000001 |] `shouldBe` []
    ls 0 g [path| (a & @)/(@000000000001 | zz) |] `shouldBe` []
    ls 0 g [path| @/(a & @)/(@000000000001 & d) |] `shouldBe` []
    ls 0 g [path| (a & @)/((@000000000001 | zz) & d) |] `shouldBe` []
    -- from node 1 itself, with a loop for the a step, it is found
    ls 1 ((1, "a", 1) : g) [path| (a & @)/(@000000000001 & d) |]
      `shouldBe` [Just 1]
  it "a union before a join in a left conjunct chooses in every copy" do
    let count p = length (compilePath p).alternatives
    count [path| ((x | y)/@ & z)/(c & d) |] `shouldBe` 4
    count [path| ((x | y)/@ & z)/(c & d & e) |] `shouldBe` 8
    count [path| ((x | y)/@ & z)/(c & d & e & f) |] `shouldBe` 16
    -- in the chain itself the join keeps it to one choice
    count [path| (x | y)/@/(c & d & e) |] `shouldBe` 2
    count [path| ((x | y)/@ & z)/k/@/(c & d & e) |] `shouldBe` 2

-- | The copies of a union choose on their own, whatever the others
-- choose and in whatever order: sequencing distributes over a
-- conjunction of paths without joins, also when what is distributed has
-- unions with conjunctions in their sides.
spec_compileCopies :: Spec
spec_compileCopies = describe "the copies of a union choose on their own" do
  let cases =
        ordNub $
          unGen
            (vectorOf 300 (choose (3, 6) >>= genPath))
            (mkQCGen 20261012)
            30
      unions = \case
        l P.:|| r -> 1 + unions l + unions r :: Int
        l P.:/ r -> unions l + unions r
        l P.:& r -> unions l + unions r
        _ -> 0
      withinLimit x = fewAlternatives 200 x
      ls edges surface =
        map smallNIDIndex . toList $
          foldMap
            targets
            ( resolveFrom
                (smallNID 0)
                (graphOf (ordNub (0 : concat [[s, t] | (s, _, t) <- edges])) edges)
                (compilePath (surface P.:/ P.One))
            ).resolutions
  it "(p & q)/r is p/r & q/r, and r/(p & q) is r/p & r/q" do
    let checked =
          [ (whole, apart)
          | r <- cases,
            unions r >= 1,
            (whole, apart) <-
              [ ([path| p & q |] P.:/ r, ([path| p |] P.:/ r) P.:& ([path| q |] P.:/ r)),
                (r P.:/ [path| p & q |], (r P.:/ [path| p |]) P.:& (r P.:/ [path| q |]))
              ],
            withinLimit whole && withinLimit apart
          ]
    length checked `shouldSatisfy` (> 100)
    forM_ checked \(whole, apart) ->
      (show whole, alternativesUpToOrder whole)
        `shouldBe` (show whole, alternativesUpToOrder apart)
  it "a copy is chosen once only when no other union can tell its routes apart" do
    -- After the join the union (u | v) has one copy for c and d, but a
    -- route that chose m & n is two routes, which choose u or v apart:
    -- z with u or v, and m & n with each of u/u, u/v, v/u, v/v.
    length (compilePath [path| q & (c & d)/@/((m & n) | z)/(u | v) |]).alternatives
      `shouldBe` 6
    length (compilePath [path| q & (c & d)/@/(m | z)/(u | v) |]).alternatives
      `shouldBe` 4
  it "the routes of a joined conjunct in a joined conjunct are listed as written out" do
    -- @/(a & b) is copied whole by m & n inside the conjunct that the
    -- leading @ keeps together: a/m, b/m, a/n, b/n
    let p = [path| @/(@/(a & b) & c)/(m & n) & z |]
    -- (which no command runs: m & n would copy the node a and b leave;
    -- the order of the routes is that of the copies written out)
    refuses p
    runsQuietly [path| @/(@/(a & b)/m & @/(a & b)/n & c/m & c/n) & z |]
    compilePath p
      `shouldBe` compilePath
        [path| @/(@/(a & b)/m & @/(a & b)/n & c/m & c/n) & z |]
    compilePath p `shouldBe` referenceCompile WrittenOut p
    [ [t | h <- r.hops, MLiteral t <- [h.matcher]]
      | a <- toList (compilePath p).alternatives,
        r <- toList a.routes
      ]
      `shouldBe` [["a", "m"], ["b", "m"], ["a", "n"], ["b", "n"], ["c", "m"], ["c", "n"], ["z"]]
  it "two unions with conjunctions copy each other" $
    -- each union chooses once, or one chooses its conjunction and the
    -- other chooses for each conjunct
    length (compilePath [path| ((a & b) | x)/((c & d) | e) |]).alternatives
      `shouldBe` 8
  it "three unions with conjunctions, each route choosing in any order" do
    -- Without a join every route is a walk of its own, so an alternative
    -- is the set of its routes, each by its labels. Those are worked
    -- out here apart from the compiler: a route with unions left chooses
    -- any of them next, either the side without a conjunction or the
    -- conjunction, which makes it two routes that go on choosing apart.
    let labelled q =
          setFromList
            [ setFromList
                [ [t | h <- r.hops, MLiteral t <- [h.matcher]]
                | r <- toList a.routes
                ]
            | a <- toList q.alternatives
            ] ::
            Set (Set [Text])
        byHand :: [Either (Text, Text, Text) Text] -> Set (Set [Text])
        byHand route' = case [k | (k, Left _) <- zip [0 :: Int ..] route'] of
          [] -> singletonSet (singletonSet [t | Right t <- route'])
          open ->
            setFromList . concat $
              [ toList (byHand (put z))
                  <> [ a <> b
                     | a <- toList (byHand (put x)),
                       b <- toList (byHand (put y))
                     ]
              | k <- open,
                Left (x, y, z) <- [route' `indexEx` k],
                let put t = [if j == k then Right t else seg | (j, seg) <- zip [0 ..] route']
              ]
        three = [path| (a & b | x)/(c & d | e)/(f & g | h) |]
        expected = byHand [Left ("a", "b", "x"), Left ("c", "d", "e"), Left ("f", "g", "h")]
    length expected `shouldBe` 146
    labelled (compilePath three) `shouldBe` expected
    length (compilePath three).alternatives `shouldBe` 146
    labelled (compilePath [path| (a & b | x)/(c & d | e) |])
      `shouldBe` byHand [Left ("a", "b", "x"), Left ("c", "d", "e")]
  it "each copy in its own order" do
    -- eight ways for each of the two copies; in one of them the copy
    -- after p takes k & y and chooses the second union for each conjunct,
    -- while the copy after q takes b & c and chooses the first for each
    let p = [path| (p & q)/(k & y | m)/(d | (b & c)) |]
    length (compilePath p).alternatives `shouldBe` 64
    alternativesUpToOrder p
      `shouldBe` alternativesUpToOrder
        [path| p/(k & y | m)/(d | (b & c)) & q/(k & y | m)/(d | (b & c)) |]
    ls
      [ (0, "p", 1),
        (1, "k", 2),
        (2, "b", 9),
        (2, "c", 9),
        (1, "y", 3),
        (3, "d", 9),
        (0, "q", 4),
        (4, "k", 5),
        (5, "b", 9),
        (4, "m", 6),
        (6, "c", 9),
        (4, "y", 7),
        (7, "b", 9)
      ]
      p
      `shouldBe` [Just 9]
  it "a copy of a conjunct is apart from the other whatever follows" do
    -- the route through c takes x and m/@, the one through d takes y
    -- and n: two copies of the conjunct (x | y)/@
    let p = [path| ((x | y)/@ & z)/(m/@ | n)/(c & d) |]
        g =
          [ (0, "x", 1),
            (1, "m", 2),
            (2, "c", 9),
            (0, "y", 3),
            (3, "n", 4),
            (4, "d", 9),
            (0, "z", 5),
            (5, "m", 2),
            (5, "n", 6),
            (6, "d", 9)
          ]
    length (compilePath p).alternatives `shouldBe` 56
    ls g p `shouldBe` [Just 9]
    ls g [path| x/@/m/@/c & y/@/n/d & z/m/@/c & z/n/d |] `shouldBe` [Just 9]
  it "routes that chose different sides do not meet at a later conjunct's join" do
    let g =
          [ (0, "a", 1),
            (1, "b", 2),
            (2, "c", 5),
            (5, "e", 9),
            (2, "d", 9),
            (0, "c", 3),
            (3, "a", 4),
            (4, "c", 6),
            (6, "e", 9),
            (4, "d", 9)
          ]
    ls g [path| (a & c)/(a/@ | b/@)/(c/@/e & d) |] `shouldBe` [Just 9]
    ls g [path| (a & c)/(a/@ | b/@)/(c/e & d) |] `shouldBe` [Just 9]
  it "two unions are not each other's only reason for two copies" do
    -- After k/@ the routes through x and c are one, so both unions that
    -- follow choose once: 4 alternatives. Not among those of the path
    -- with the union: c/@ then a for one route and d then b/@ for the
    -- other after the one k/@.
    let p = [path| (x & c)/(k/@ | y)/(c/@ | d)/(b/@ | a) |]
    ( alternativesUpToOrder [path| (x & c)/k/@/(c/@/a & d/b/@) |]
        `intersection` alternativesUpToOrder p
      )
      `shouldBe` mempty
    length (compilePath [path| (x & c)/k/@/(c/@ | d)/(b/@ | a) |]).alternatives
      `shouldBe` 4
    -- by hand: the copies after x and after c choose k/@ or y; when both
    -- choose k/@ they are one from there on (4 ways), and otherwise two
    -- copies choose the two unions that follow apart, with what they
    -- share when both choose c/@ or both b/@
    length (compilePath p).alternatives `shouldBe` 46
  where
    genPath :: Int -> Gen P.Path
    genPath n
      | n <= 1 = elements [P.Literal "a", P.Literal "b", P.Literal "c"]
      | otherwise = do
          k <- choose (1, n - 1)
          frequency
            [ (4, (P.:/) <$> genPath k <*> genPath (n - k)),
              (3, (P.:&) <$> genPath k <*> genPath (n - k)),
              (4, (P.:||) <$> genPath k <*> genPath (n - k))
            ]

-- | What a union finds is what the ways its routes can choose find,
-- each written out by hand as a path without unions and run on graphs:
-- for @(p1 & p2)/(side | …)/(c1 & c2)@ the four routes choose a side
-- each. The routes that chose a side with a join are that side written
-- in place for them, their prefixes into the one join and their
-- continuations out of it; a route that chose a side without a join is
-- written apart. A shared union in a side is chosen before, once for the
-- path.
spec_compileGridOracle :: Spec
spec_compileGridOracle = describe "a union finds what its choices written out find" do
  let cases =
        unGen (vectorOf 50 genCase) (mkQCGen 20261014) 30
      reached g surface =
        foldMap targets (resolveFrom (smallNID 0) g (compilePath surface)).resolutions
      results =
        [ (show (whole, edges, joined), reached g (end whole), foldMap (reached g . end) spellings)
        | (whole, spellings, graphs') <- cases,
          (edges, g) <- graphs',
          (joined, end) <- [(False, id), (True, (P.:/ P.One))]
        ]
  it "on random sides with joins, node tests and shared unions" do
    forM_ results \(shown, whole, spelled) ->
      (shown, whole) `shouldBe` (shown, spelled)
    -- not vacuous
    length (filter (\(_, whole, _) -> not (null whole)) results)
      `shouldSatisfy` (> 60)
    -- nor always everything: the join of a side keeps some out
    length
      ( filter
          (\(_, whole, _) -> not (null whole) && length whole < length nodes)
          results
      )
      `shouldSatisfy` (> 15)
  it "the routes that chose a joined side share its walk" do
    let pres = [([path| p |], [[path| p |]]), ([path| q |], [[path| q |]])]
        sides = [Joined [[path| x |]] P.One [], Plain [[path| y |]]]
    setFromList (gridSpellings (map snd pres) sides [[path| a |], [path| b |]])
      `shouldSatisfy` ( \(written :: Set P.Path) ->
                          [path| (p/x & q/x)/@/(a & b) |] `member` written
                            && [path| (p/x & q/x)/@/a & p/y/b & q/y/b |] `member` written
                            && [path| p/y/a & p/y/b & q/y/a & q/y/b |] `member` written
                            && length written == 16
                      )
  where
    nodes = [0, 1, 2 :: Int]
    labels' = ["a", "b", "c"] :: [Text]
    step = P.Literal <$> elements labels'
    chain' = do
      n <- choose (1, 2 :: Int)
      foldl1Ex' (P.:/) <$> vectorOf n step
    genGraph = do
      edges <-
        choose (9, 14) >>= \n ->
          vectorOf n ((,,) <$> elements nodes <*> elements labels' <*> elements nodes)
      pure (edges, graphOf nodes edges)
    -- a prefix conjunct as it is written, and the ways to write a copy
    -- of it without its union
    genPre :: Gen (P.Path, [P.Path])
    genPre =
      frequency
        [ (5, (\c -> (c, [c])) <$> chain'),
          (2, (\c -> (c P.:/ P.One, [c P.:/ P.One])) <$> chain'),
          ( 3,
            do
              x <- step
              y <- step `suchThat'` (/= x)
              joined <- elements [True, False]
              let after c = if joined then c P.:/ P.One else c
              pure (after (x P.:|| y), [after x, after y])
          )
        ]
    genCont :: Gen P.Path
    genCont = do
      a <- step
      b <- step
      elements [a, a, a P.:/ P.One P.:/ b, a P.:/ b, a P.:/ P.One]
    -- a side as it is written, and what it is for each choice of a
    -- shared union in it
    genSide :: Gen (P.Path, [GridSide])
    genSide = do
      a <- chain'
      b <- step
      c <- step
      d <- step
      let one' side = (sidePath side, [side])
          node' = [path| @000000000001 |]
          other = [path| !{@000000000001} |]
      elements
        [ one' (Plain [a]),
          one' (Plain [a]),
          one' (Plain [a, b]),
          one' (Joined [a] P.One []),
          one' (Joined [a] P.One []),
          one' (Joined [a] P.One [b]),
          one' (Joined [a, b] P.One []),
          one' (Joined [a] P.One [b, c]),
          one' (Joined [a, b] P.One [c]),
          one' (Joined [] node' []),
          one' (Joined [a] node' []),
          one' (Joined [] node' [b]),
          one' (Joined [a] other [b]),
          (b P.:++ c, [Plain [b], Plain [c]]),
          ((b P.:++ c) P.:/ P.One, [Joined [b] P.One [], Joined [c] P.One []]),
          (a P.:/ P.One P.:/ (c P.:++ d), [Joined [a] P.One [c], Joined [a] P.One [d]])
        ]
    genCase :: Gen (P.Path, [P.Path], [([(Int, Text, Int)], Graph Text ())])
    genCase = do
      pres <- vectorOf 2 genPre
      conts <- vectorOf 2 genCont
      n <- frequency [(3, pure 2), (1, pure (if all ((== 1) . length . snd) pres then 3 else 2))]
      -- (sides that are the same for the same choices of their shared
      -- unions are one side, 'spec_compileSameSides', so that p | p is
      -- p: no two such are generated)
      sides <-
        vectorOf n genSide `suchThat'` \made ->
          length (ordNub (map (sort . map sidePath . snd) made)) == length made
      around <- elements [id, id, ([path| c/@ |] P.:/)]
      graphs' <- vectorOf 3 genGraph
      let conj = foldl1Ex' (P.:&)
          whole =
            around $
              conj (map fst pres)
                P.:/ foldl1Ex' (P.:||) (map fst sides)
                P.:/ conj conts
          spellings =
            [ around spelling
            | world <- traverse snd sides,
              spelling <- gridSpellings (map snd pres) world conts
            ]
      pure (whole, spellings, graphs')
    suchThat' gen ok = gen >>= \x -> if ok x then pure x else suchThat' gen ok

-- | A side of a union for 'gridSpellings': conjuncts without a join, or
-- conjuncts into a join (none: the join alone), the join, and conjuncts
-- out of it (none: nothing after it).
data GridSide
  = Plain [P.Path]
  | Joined [P.Path] P.Path [P.Path]

sidePath :: GridSide -> P.Path
sidePath = \case
  Plain us -> foldl1Ex' (P.:&) us
  Joined pre node post ->
    let into = if null pre then node else foldl1Ex' (P.:&) pre P.:/ node
     in if null post then into else into P.:/ foldl1Ex' (P.:&) post

-- | Every way the routes of @(p1 & …)/(side | …)/(c1 & …)@ can choose a
-- side each, written as a path without unions; each prefix conjunct is
-- given as the ways to write a copy of it.
gridSpellings :: [[P.Path]] -> [GridSide] -> [P.Path] -> [P.Path]
gridSpellings pres sides conts =
  [ conj parts
  | sigma <- traverse (const [0 .. length sides - 1]) routes',
    let chose = zip routes' sigma,
    parts <- traverse id (concatMap (blocks chose) (zip [0 ..] sides)),
    not (null parts)
  ]
  where
    conj = foldl1Ex' (P.:&)
    routes' =
      [(i, j) | i <- [0 .. length pres - 1], j <- [0 .. length conts - 1]]
    -- for each thing to write, the ways to write it
    blocks :: [((Int, Int), Int)] -> (Int, GridSide) -> [[P.Path]]
    blocks chose (k, side) =
      let mine = [r | (r, k') <- chose, k' == k]
       in case side of
            Plain us ->
              [ [p P.:/ u P.:/ (conts `indexEx` j) | p <- pres `indexEx` i]
              | (i, j) <- mine,
                u <- us
              ]
            Joined pre node post
              | null mine -> []
              | otherwise ->
                  let is = ordNub (map fst mine)
                      js = ordNub (map snd mine)
                      into =
                        traverse
                          id
                          [ [maybe p (p P.:/) u | p <- pres `indexEx` i]
                          | i <- is,
                            u <- if null pre then [Nothing] else map Just pre
                          ]
                      out =
                        [ maybe c (P.:/ c) w
                        | j <- js,
                          let c = conts `indexEx` j,
                          w <- if null post then [Nothing] else map Just post
                        ]
                   in [[conj terms P.:/ node P.:/ conj out | terms <- into]]

-- * Copied meeting points

-- | Whether commands run the path: no product in it copies a meeting
-- point.
runs :: P.Path -> Bool
runs = isRight . checkCopies

-- | No command runs the path: a product would copy a meeting point.
refuses :: P.Path -> Expectation
refuses p = (show p, isLeft (checkCopies p)) `shouldBe` (show p, True)

-- | Commands run the path, and nothing is warned about.
runsQuietly :: P.Path -> Expectation
runsQuietly p = (show p, checkCopies p) `shouldBe` (show p, Right [])

-- | Commands run the path, with a warning about an @ that does nothing.
runsWarning :: P.Path -> Expectation
runsWarning p =
  (show p, fmap (not . null) (checkCopies p)) `shouldBe` (show p, Right True)

-- | The path that keeps what the test is about: @K@ is @(c & y)/\@/e@, a
-- conjunct in which @c@ and @y@ meet.
spec_compileCopiedMeetingPoints :: Spec
spec_compileCopiedMeetingPoints = describe "a product may not copy a meeting point" do
  it "refuses a conjunct with a meeting point that a group copies" do
    refuses [path| (a & b)/((c & y)/@/e & d) |]
    refuses [path| (x/@/(w & v) & b)/(c & d) |]
    refuses [path| (@/(a & b) & c)/(@/f & e) |]
    refuses [path| (x & y)/(@/(a & b) & c) |]
    refuses [path| (a/@/(b & c) & d)/(e & f) |]
    refuses [path| (p & q)/(a/@/(b & c) & d) |]
    -- steps between the groups do not keep them apart
    refuses [path| (a & b)/k/((c & y)/@/e & d) |]
    -- !{…} is a meeting point where walks meet at it
    refuses [path| (a & b)/((c & y)/!{@000000000001}/e & d) |]
    -- the meeting point may be deeper in the copied conjunct
    refuses [path| (p & q)/(@000000000001/((c & y)/@/e & z) & d) |]
    refuses [path| (p & q)/(m/((c & y)/@/e & z) & d) |]
    -- a conjunct that forks without a join is no chain apart: its groups
    -- are groups of the stretch around it
    refuses [path| ((p & q)/x & z)/((c & y)/@/e & d) |]
    refuses [path| (m/((c & y)/@/e & z)/(e & f) & d) |]
  it "names the conjunct, the join, the copier and what to write" do
    let refusal = either Just (const Nothing) . checkCopies
    refusal [path| (a & b)/((c & y)/@/e & d) |]
      `shouldBe` Just
        CopiedMeetingPoint
          { meeting = [path| (c & y)/@/e |],
            join = P.One,
            copied = [path| (c & y)/@/e |],
            group = [path| (c & y)/@/e & d |],
            copier = [path| a & b |],
            readings = [],
            joinedFirst = Just [path| (a & b)/@/((c & y)/@/e & d) |],
            writtenApart =
              Just [path| a/(c & y)/@/e & a/d & b/(c & y)/@/e & b/d |],
            across = Nothing
          }
    -- what it says to write is run, and the copies written out are what
    -- the path compiles to
    for_
      [ [path| (a & b)/((c & y)/@/e & d) |],
        [path| (x/@/(w & v) & b)/(c & d) |],
        [path| (@/(a & b) & c)/(@/f & e) |],
        [path| (x & y)/(@/(a & b) & c) |],
        [path| (a/@/(b & c) & d)/(e & f) |],
        [path| (p & q)/(a/@/(b & c) & d) |],
        [path| q/(a & b)/k/((c & y)/@/e & d)/w |]
      ]
      \p -> case refusal p of
        Just CopiedMeetingPoint {joinedFirst = Just joined, writtenApart = Just apart} -> do
          runsQuietly joined
          isRight (checkCopies apart) `shouldBe` True
        other -> expectationFailure (show (p, other))
    (.writtenApart)
      <$> refusal [path| (a & b)/((c & y)/@/e & d) |]
        `shouldSatisfy` maybe False (maybe False (\apart -> compilePath apart == compilePath [path| (a & b)/((c & y)/@/e & d) |]))
    -- the conjunct that is copied is not always the one with the meeting point
    ((\r -> (r.meeting, r.copied)) <$> refusal [path| (p & q)/(@000000000001/((c & y)/@/e & z) & d) |])
      `shouldBe` Just ([path| (c & y)/@/e |], [path| @000000000001/((c & y)/@/e & z) |])
  it "refuses when some way to take the sides of a | copies one" do
    refuses [path| ((a & b) | z)/((c & y)/@/e & d) |]
    refuses [path| (p & q)/(x/@ | s)/((c & y)/@/e & d) |]
    refuses [path| (x/@ | (p & q)/s)/((c & y)/@/e & d) |]
    refuses [path| ((a & b) | (u & v))/((c & y)/@/e & d) |]
    refuses [path| ((a & b) | @000000000003)/((c & y)/@/e & d) |]
    refuses [path| (x/@/(m & n) & q)/((a & b) | z) |]
    -- no merging of equal conjuncts
    refuses [path| ((a & a) | z)/((c & y)/@/e & d) |]
    ((.readings) <$> either Just (const Nothing) (checkCopies [path| ((a & b) | z)/((c & y)/@/e & d) |]))
      `shouldBe` Just [SideInPlace [path| (a & b) | z |] [path| a & b |]]
  it "a | keeps no two groups apart, whatever joins its sides have" do
    refuses [path| (p & q)/(x/@ | w/@)/((c & y)/@/e & d) |]
    refuses [path| (p & q)/(@000000000001 | @000000000002)/((c & y)/@/e & d) |]
    ((.readings) <$> either Just (const Nothing) (checkCopies [path| (p & q)/(x/@ | w/@)/((c & y)/@/e & d) |]))
      `shouldBe` Just [AsOneStep [path| x/@ | w/@ |]]
    -- a side written twice is two sides, for the check as for the
    -- compiler: p and q may take one each, and then pass the union as a
    -- step
    refuses [path| (p & q)/(x/@ | x/@)/((c & y)/@/e & d) |]
    ((.readings) <$> either Just (const Nothing) (checkCopies [path| (p & q)/(x/@ | x/@)/((c & y)/@/e & d) |]))
      `shouldBe` Just [AsOneStep [path| x/@ | x/@ |]]
    copiesMeetingPoint [path| (p & q)/(x/@ | x/@)/((c & y)/@/e & d) |] `shouldBe` True
    -- so are two sides that compile to the same thing in place (the
    -- check refused this while the compiler had one side: no more)
    refuses [path| (p & q)/(x/@/y/z/@ | x/@/y/@/z/@)/((c & y)/@/e & d) |]
    copiesMeetingPoint [path| (p & q)/(x/@/y/z/@ | x/@/y/@/z/@)/((c & y)/@/e & d) |]
      `shouldBe` True
    -- %never is no side, and a union of one side is that side
    runsQuietly [path| (p & q)/(x/@ | %never)/((c & y)/@/e & d) |]
    runsQuietly [path| (p & q)/x/@/((c & y)/@/e & d) |]
  it "a ++ is its choices: refused when one of them is" do
    refuses [path| ((a & b) ++ z)/((c & y)/@/e & d) |]
    refuses [path| (((a & b) | z) ++ w)/((c & y)/@/e & d) |]
    -- every choice has a join of the chain between the groups
    runsQuietly [path| (p & q)/(x/@ ++ w/@)/((c & y)/@/e & d) |]
    runsQuietly [path| (p & q)/(@000000000001 ++ @000000000002)/((c & y)/@/e & d) |]
  it "runs a path with a join of the chain between the groups" do
    runsQuietly [path| (a/@/(b & c) & d)/@ |]
    runsQuietly [path| (a/@/(b & c) & d)/@/(e & f) |]
    runsQuietly [path| (p & q)/@/(a/@/(b & c) & d) |]
    runsQuietly [path| (p & q)/@000000000001/(a/@/(b & c) & d) |]
  it "runs a meeting point that nothing copies" do
    runsQuietly [path| z/((c & y)/@/e & d) |]
    runsQuietly [path| (a | z)/((c & y)/@/e & d) |]
    runsQuietly [path| (x/@ | s)/((c & y)/@/e & d) |]
    runsQuietly [path| q/(@/(a & b) & c) |]
    runsQuietly [path| a/@/(a & b)/@/c |]
    -- the product written out has it in a group that stands alone
    runsQuietly [path| a/(c & y)/@/e & a/d & b/(c & y)/@/e & b/d |]
  it "runs a join that is no meeting point, though copied" do
    -- @N is one node for all copies
    runsQuietly [path| (p & q)/(x/@000000000001/w & y) |]
    runsQuietly [path| (p & q)/(x/@000000000001/(w & v) & y) |]
    runsQuietly [path| a/(@000000000001 | @000000000002)/(x & y) |]
    -- a join on one walk joins nothing
    runsQuietly [path| (x/@/w & b)/(c & d) |]
    runsQuietly [path| (p & q)/(x/@ | s)/(m/@/e & d) |]
    runsQuietly [path| (x/@ | zz)/(c & d) |]
    -- !{…} on one walk is a filter
    runsQuietly [path| (x/!{@000000000001} & b)/(c & d) |]
    runsQuietly [path| (!{@000000000001}/x & b)/(c & d) |]
  it "the leading @ of a conjunct that forks is refused where it is copied" do
    refuses [path| (@/(a & b) & c)/(d & e) |]
    -- also inside a side of a union
    refuses [path| (((a & b) | @/(a & b)) & c)/(@/f & e) |]
    refuses [path| ((@/(c & @) | (c & @)) & g)/(@000000000001 & d) |]
  it "warns of an @ at an end of a copied conjunct that is a plain walk" do
    runsWarning [path| (x/@ & b)/(c & d) |]
    runsWarning [path| (a & b)/(@/c & d) |]
    checkCopies [path| (x/@ & b)/(c & d) |]
      `shouldBe` Right
        [ IdleJoin
            { conjunct = [path| x/@ |],
              begins = False,
              ends = True,
              copier = [path| c & d |],
              readings = [],
              pinned = Just [path| x/@/(c & d) & b/(c & d) |]
            }
        ]
    ((\j -> (j.begins, j.ends, j.pinned)) <$$> checkCopies [path| (a & b)/(@/c & d) |])
      `shouldBe` Right [(True, False, Just [path| (a & b)/@/c & (a & b)/d |])]
    -- one for each such conjunct
    (length <$> checkCopies [path| (x/@ & y/@)/(c & d) |]) `shouldBe` Right 2
    -- at both ends, and at an end of a walk that has an @ inside too
    ((\j -> (j.begins, j.ends)) <$$> checkCopies [path| (@/x/@ & b)/(c & d) |])
      `shouldBe` Right [(True, True)]
    ((\j -> (j.begins, j.ends)) <$$> checkCopies [path| (x/@/w/@ & b)/(c & d) |])
      `shouldBe` Right [(False, True)]
    -- the copier may stand past steps and past unions without a join
    runsWarning [path| (x/@ & b)/k/(m | n)/(c & d) |]
    runsWarning [path| (x/@ & b)/(m/@ ++ n)/(c & d) |]
    -- a group in a conjunct without a join is a group of the stretch
    -- its conjunct's group is in, and is copied as that one is
    runsWarning [path| (b & c)/(a/(@/x & b) & w) |]
    runsQuietly [path| q/(a/(@/x & b) & w) |]
  it "does not warn where the @ is inside, is not copied, or is a test" do
    runsQuietly [path| (x/@/w & b)/(c & d) |]
    runsQuietly [path| q/(x/@ & b) |]
    runsQuietly [path| (x/@ & b)/@/(c & d) |]
    runsQuietly [path| (x/!{@000000000001} & b)/(c & d) |]
    runsQuietly [path| (x/@000000000001 & b)/(c & d) |]
    runsQuietly [path| (x/@/@000000000001 & b)/(c & d) |]
    -- nor before an @N in a side of a union, which the @ makes a test
    runsQuietly [path| (@/(@000000000002 | a/a) & b)/(c & d) |]
    -- nor where the conjunct is not copied itself but lies in a copied
    -- one, past a join of that
    runsQuietly [path| (p & q)/(m/@000000000001/(x/@ & z) & y) |]
  it "does not warn unless the @ can be left out whatever is around it" do
    -- a conjunct that is only @ cannot be written without it
    runsQuietly [path| (a & @)/(@000000000001 & d) |]
    runsQuietly [path| (a & b)/(@ & c) |]
    -- a union in the conjunct: a side may begin with an @N, which the @
    -- makes a test, or have a group
    runsQuietly [path| ((x | y)/@ & z)/(c1 & c2 & c3) |]
    runsQuietly [path| (p & q)/((x/@ | zz) & z) |]
    runsQuietly [path| (@/(x | @000000000001/b) & a)/(c & d) |]
    runsQuietly [path| ((@/(x | @000000000001/b) | y) & a)/(c & d) |]
    runsQuietly [path| ((@/(x | @000000000001/b) ++ y) & a)/(c & d) |]
    -- an @N or a !{…} in the walk
    runsQuietly [path| (x/@000000000001/w/@ & b)/(c & d) |]
    runsQuietly [path| (@/x/!{@000000000001}/w & b)/(c & d) |]
    -- a group in the conjunct (held together by an @N, so that the
    -- path is run)
    runsQuietly [path| ((a & b)/@000000000001/w/@ & c)/(d & e) |]
    -- a | union with a join in a side, anywhere in the path: routes
    -- that took different sides of it are listed by the joins they pass
    runsQuietly [path| (x/@ & b)/(m/@ | n)/(c & d) |]
    runsQuietly [path| (m/@ | n)/q/@/(x/@ & b)/(c & d) |]
    runsQuietly [path| a/(@000000000001 | @000000000002)/(x/@ & b)/(c & d) |]
    -- How far that looks: the join may be in a ++ that is a side of
    -- the |, the | may be a side of a ++, and the | may be in a conjunct
    -- of a group. Without the join in the side, each is warned about.
    runsQuietly [path| (x/@ & b)/((m/@ ++ k) | n)/(c & d) |]
    runsWarning [path| (x/@ & b)/((m ++ k) | n)/(c & d) |]
    runsQuietly [path| (x/@ & b)/((m/@ | j) ++ n)/(c & d) |]
    runsWarning [path| (x/@ & b)/((m | j) ++ n)/(c & d) |]
    runsQuietly [path| (x/@ & b)/(c & (m/@ | n)/d) |]
    runsWarning [path| (x/@ & b)/(c & (m | n)/d) |]
    -- The false warning the review found. With the @ the @1 tests the
    -- node the path starts at, and without it the @1 is a root.
    let g = [(1, "b", 5), (5, "c", 9), (5, "d", 9), (0, "a", 6), (6, "c", 9), (6, "d", 9)]
    reaches g [path| ((@/(x | @000000000001/b) | y) & a)/(c & d) |] `shouldBe` []
    reaches g [path| (((x | @000000000001/b) | y) & a)/(c & d) |] `shouldBe` [Just 9]
  it "the side with a join of its own is one walk, whatever is before the join" do
    -- (a & b)/@ is one joined walk as x/@ is (the build had a special
    -- case for a side with a group)
    runsQuietly [path| ((a & b)/@ | z)/((c & y)/@/e & d) |]
    let g =
          graphOf
            [0, 1, 3, 4, 6, 9]
            [ (0, "a", 1),
              (0, "b", 1),
              (0, "x", 1),
              (1, "c", 4),
              (4, "e", 9),
              (0, "z", 3),
              (3, "y", 6),
              (6, "e", 9),
              (3, "d", 9)
            ]
        found surface =
          foldMap
            targets
            (resolveFrom (smallNID 0) g (compilePath (surface P.:/ P.One))).resolutions
    found [path| (x/@ | z)/((c & y)/@/e & d) |] `shouldBe` mempty
    found [path| ((a & b)/@ | z)/((c & y)/@/e & d) |] `shouldBe` mempty
  it "is the same for a path and its reverse" do
    let verdict p = bimap (const ()) (not . null) (checkCopies p)
        cases = unGen (vectorOf 3000 (choose (2, 9) >>= genScoped False)) (mkQCGen 20261101) 30
    length (filter (isLeft . verdict) cases) `shouldSatisfy` (> 200)
    length (filter ((== Right True) . verdict) cases) `shouldSatisfy` (> 150)
    forM_ cases \p ->
      (show p, verdict (P.Backwards p)) `shouldBe` (show p, verdict p)
  it "is the hand rule, with every way to read the unions written out" do
    let cases =
          unGen
            ( (<>)
                <$> vectorOf 2500 (choose (2, 10) >>= genScoped True)
                <*> vectorOf 2000 genPlanted
            )
            (mkQCGen 20261102)
            30
        -- each path with what the rule says, and what the quick test
        -- of the docs says
        worked = [(p, handRefuses p, shortcutRefuses p) | p <- cases]
        refused = [p | (p, True, _) <- worked]
    length refused `shouldSatisfy` (> 800)
    length [() | (_, False, _) <- drop 2500 worked] `shouldSatisfy` (> 300)
    length (filter (any (\case P.Union {} -> True; _ -> False) . universe') refused)
      `shouldSatisfy` (> 200)
    -- the quick test is no definition: it misses some and refuses others
    length [() | (_, True, False) <- worked] `shouldSatisfy` (> 80)
    length [() | (_, False, True) <- worked] `shouldSatisfy` (> 20)
    forM_ worked \(p, refuse, _) ->
      (show p, isLeft (checkCopies p)) `shouldBe` (show p, refuse)
  it "refuses a meeting point in a side of a union, and one deep in a copied conjunct" do
    -- (each tells a wrong check from the right one where the random
    -- paths did not)
    refuses [path| (p & q)/(((c & y)/@/e & d) | z) |]
    refuses [path| (p & q)/(x/@000000000001/(u/((c & y)/@/e & d) & z) & w) |]
    runsQuietly [path| q/(((c & y)/@/e & d) | z) |]
    runsQuietly [path| q/(x/@000000000001/(u/((c & y)/@/e & d) & z) & w) |]
  it "refuses a join that a neighbouring conjunct brings several walks to" do
    -- @1/(c & e) holds c and e together; in its copy of x/@/w they meet
    -- at the @, which the conjunct b has a copy of too
    refuses [path| (@000000000001/(c & e) & b)/(x/@/w & d) |]
    refuses [path| (q/@000000000001/(c & e) & b)/(x/@/w & d) |]
    -- the mirror image: c and e leave the @ of a/@ (and of a/!{…}, which
    -- is no filter there)
    refuses [path| (a/@ & b)/((c & e)/@000000000001/f & d) |]
    refuses [path| (a/!{@000000000007} & b)/((c & e)/@000000000001/f & d) |]
    -- through a union, whose side forks or not
    refuses [path| (@000000000001/(c & e) & m)/((a & b) | z)/(x/@/w & d) |]
    -- the conjunct that holds the walks may lie deeper
    refuses [path| (m/(@000000000001/(c & e) & z) & b)/(x/@/w & d) |]
    refuses [path| (@000000000001/(u/@000000000002/(c & e) & v) & b)/(m/(x/@/w & z)/@000000000003 & d) |]
    let refusal = either Just (const Nothing) . checkCopies
    ((\r -> (r.meeting, r.join, r.across)) <$> refusal [path| (@000000000001/(c & e) & b)/(x/@/w & d) |])
      `shouldBe` Just
        ( [path| x/@/w |],
          P.One,
          Just (WalksAcross [path| @000000000001/(c & e) |] True)
        )
    ((.across) <$> refusal [path| (a/@ & b)/((c & e)/@000000000001/f & d) |])
      `shouldBe` Just (Just (WalksAcross [path| (c & e)/@000000000001/f |] False))
    -- what the error says to write is run
    for_
      [ [path| (@000000000001/(c & e) & b)/(x/@/w & d) |],
        [path| (a/@ & b)/((c & e)/@000000000001/f & d) |]
      ]
      \p -> case refusal p of
        Just CopiedMeetingPoint {joinedFirst = Just joined, writtenApart = Just apart} -> do
          isRight (checkCopies joined) `shouldBe` True
          isRight (checkCopies apart) `shouldBe` True
          compilePath apart `shouldBe` compilePath p
        other -> expectationFailure (show (p, other))
    -- run: the fork is not held together (its walks are copies), a join
    -- of the conjunct stands between, or no open join is copied
    runsQuietly [path| (q/(c & e) & b)/(x/@/w & d) |]
    runsQuietly [path| (@000000000001/(c & e)/@000000000002 & b)/(x/@/w & d) |]
    runsQuietly [path| (@000000000001/(c & e) & b)/(x/@000000000002/@/w & d) |]
    runsQuietly [path| (@000000000001/(c & e) & b)/(x/w & d) |]
    runsQuietly [path| ((a & b) | z)/((c & y)/@000000000001/e & d) |]
    runsQuietly [path| (@000000000001/(c & e) & b)/(x/@000000000002/w & d) |]
    -- the @ is not at the end of its conjunct that faces the fork
    runsQuietly [path| (@000000000001/(c & e) & b)/(x/@000000000002/w/@/v & d) |]
    -- the @ is in a conjunct of a group inside the copied conjunct: c & e
    -- and that group are chosen together, so each of c and e has its own
    -- copy of the @ (in both directions)
    runsQuietly [path| (@000000000001/(c & e) & b)/(m/(x/@/w & z)/@000000000003 & d) |]
    runsQuietly [path| (d & @000000000003/(z & w/@/x)/m)/(b & (c & e)/@000000000001) |]
    refuses [path| (d & @000000000003/(z & w/@/x)/m)/(b & (v & (c & e)/@000000000002/u)/@000000000001) |]
  it "looks at where the walks are, not at how the conjunct is written" do
    -- c and y are made one walk by the @4 before the @: one walk comes
    -- into the @ and one goes out, in every copy
    runsQuietly [path| (a & b)/((c & y)/@000000000004/w/@/e & d) |]
    runsQuietly [path| (a & b)/(e/@/w/@000000000004/(c & y) & d) |]
    -- two walks leave this @, through w
    refuses [path| (a & b)/(c/@/w/(e & d) & d) |]
    refuses [path| (a & b)/((e & d)/w/@/c & d) |]
    -- a conjunct inside a copied conjunct is copied with it
    refuses [path| (p & q)/(x/@000000000001/((c & y)/@/e & z) & w) |]
    runsQuietly [path| (p & q)/(x/@000000000001/((c & y)/@000000000004/w/@/e & z) & w) |]
  it "reads a | union as one step inside a conjunct too" do
    -- The routes through m and n may take a side each. Each side ends at
    -- a node of its own, so with either side in place the @N stands
    -- between the fork and the @; but the walks w and v of the two
    -- routes meet at the @.
    let meets = [path| (p & q)/((m & n)/(x/@000000000001/w | y/@000000000002/v)/@ & d) |]
        -- x and y, of two routes, both leave this @
        leaves' = [path| (p & q)/(@/(x/@000000000001 | y/@000000000002)/(m & n) & d) |]
        -- walks that a neighbouring conjunct holds together come to a
        -- join that is the first of its conjunct only past the union
        arrives = [path| (u/@000000000007/(m & n) & z)/((x/@000000000001/w | y/@000000000002/v)/@/s & b) |]
        departs = [path| (s/@/(w/@000000000001/x | v/@000000000002/y) & b)/((m & n)/@000000000007/u & z) |]
    for_ [meets, leaves', arrives, departs] \p -> do
      refuses p
      -- and it is no over-refusal: what the compiler makes of it has a
      -- join in two copies with a fork at one
      (show p, copiesMeetingPoint p) `shouldBe` (show p, True)
    -- a ++ is chosen once: every route has the same @N before the @
    runsQuietly [path| (p & q)/((m & n)/(x/@000000000001/w ++ y/@000000000002/v)/@ & d) |]
    runsQuietly [path| (u/@000000000007/(m & n) & z)/((x/@000000000001/w ++ y/@000000000002/v)/@/s & b) |]
  it "refuses a | of two joined sides before walks that an @N holds, though nothing is copied" do
    -- An over-refusal that the spec lists (docs/path-pattern-ir.md,
    -- Meeting points), pinned here so that relaxing it is a change that
    -- shows. The check reads the | as one step, past which the group
    -- after it copies p/@. In what the compiler makes, no alternative
    -- has p/@ in two copies with a fork at one: the walks through c and
    -- y are held together by the @1, and the union has only two sides.
    let two = [path| (p/@ & q)/(x/@ | w/@)/((c & y)/@000000000001/e & f) |]
    refuses two
    length (alternativeRoutes two) `shouldBe` 64
    copiesMeetingPoint two `shouldBe` False
    -- also with one side written twice
    let twice = [path| (p/@ & q)/(x/@ | x/@)/((c & y)/@000000000001/e & f) |]
    refuses twice
    copiesMeetingPoint twice `shouldBe` False
    -- with a third joined side the copy is there, and the refusal is
    -- none too many
    let three = [path| (p/@ & q)/(x/@ | w/@ | v/@)/((c & y)/@000000000001/e & f) |]
    refuses three
    length (alternativeRoutes three) `shouldBe` 729
    copiesMeetingPoint three `shouldBe` True
    -- a ++ there is chosen once, and one joined side is a join of the
    -- chain: both are run
    runsQuietly [path| (p/@ & q)/(x/@ ++ w/@)/((c & y)/@000000000001/e & f) |]
    runsQuietly [path| (p/@ & q)/x/@/((c & y)/@000000000001/e & f) |]
  it "the results that made the rule: an @ on one walk that is no no-op" do
    let found edges surface =
          map smallNIDIndex . toList $
            foldMap
              targets
              ( resolveFrom
                  (smallNID 0)
                  (graphOf (ordNub (0 : concat [[a, b] | (a, _, b) <- edges])) edges)
                  (compilePath (surface P.:/ P.One))
              ).resolutions
        g =
          [ (1, "c", 2),
            (1, "e", 3),
            (2, "x", 4),
            (3, "x", 5),
            (4, "w", 9),
            (5, "w", 9),
            (0, "b", 6),
            (6, "x", 7),
            (7, "w", 9),
            (6, "d", 9),
            (2, "d", 9),
            (3, "d", 9)
          ]
    -- the compiler makes the product, in which c and e meet at the @
    found g [path| (@000000000001/(c & e) & b)/(x/@/w & d) |] `shouldBe` []
    found g [path| (@000000000001/(c & e) & b)/(x/w & d) |] `shouldBe` [Just 9]
  it "no alternative of a path that is run has a join in two copies with a fork at one" do
    -- every /@ and !{…} is marked, and the routes of every alternative
    -- looked at: a path is flagged when one written join is two vertices
    -- or more, and at one of them routes arrive by different steps or
    -- leave by different steps
    let cases =
          unGen
            ( (<>)
                <$> vectorOf 1500 (choose (2, 9) >>= genScoped True)
                <*> vectorOf 1500 genPlanted
            )
            (mkQCGen 20261104)
            30
        -- explicitly: only paths that are run, and that are within a
        -- limit that keeps the test short
        run' = [p | p <- cases, runs p, fewAlternatives 200 (marked p)]
        flagged p = any (not . null . copiedJoins True) (alternativeRoutes (marked p))
        written' p = any (not . null . copiedJoins False) (alternativeRoutes (marked p))
    length run' `shouldSatisfy` (> 1500)
    -- the oracle does flag what the rule refuses
    length (filter flagged [p | p <- drop 1500 cases, not (runs p), fewAlternatives 200 (marked p)])
      `shouldSatisfy` (> 200)
    flagged [path| (@000000000001/(c & e) & b)/(x/@/w & d) |] `shouldBe` True
    flagged [path| (a & b)/((c & y)/@/e & d) |] `shouldBe` True
    forM_ run' \p -> (show p, flagged p) `shouldBe` (show p, False)
    -- and the check is that definition, with the copies counted as they
    -- are written: on every path within the limit, run or not
    -- (paths in which no node tests contradict each other: where they
    -- do, an alternative that the check reads is not compiled, and the
    -- check refuses more than is compiled)
    let plain =
          unGen (vectorOf 1500 (choose (2, 9) >>= genScoped False)) (mkQCGen 20261107) 30
            <> drop 1500 cases
        within = [p | p <- plain, fewAlternatives 200 (marked p)]
    length within `shouldSatisfy` (> 2400)
    length (filter (not . runs) within) `shouldSatisfy` (> 500)
    forM_ within \p ->
      (show p, written' p) `shouldBe` (show p, not (runs p))
    -- with contradicting tests: what is compiled as a copied meeting
    -- point is refused
    forM_ [p | p <- take 1500 cases, fewAlternatives 200 (marked p)] \p ->
      when (written' p) ((show p, runs p) `shouldBe` (show p, False))
    -- where the two ways to count differ, the copies are one node: the
    -- leading @ of a forking conjunct whose group starts its stretch
    -- (refused all the same)
    let p8 = [path| (@/(a & b) & c)/(d & e) |]
    (flagged p8, written' p8, runs p8) `shouldBe` (False, True, False)
  it "an @ that is warned about does nothing: without it the path compiles the same" do
    -- Paths with unions, @N and !{…} anywhere, as the reviewer of the
    -- warning made them, and the shapes the warning is for in contexts.
    -- For EVERY path that is warned about, and every @ in it whose
    -- removal takes a warning away, the path without that @ compiles to
    -- the same alternatives, routes in the same order.
    let cases =
          -- (an @ written twice in a row is written once, so that
          -- taking one @ out takes the join out)
          map onceEach $
            unGen
              ( concat
                  <$> sequence
                    [ vectorOf 5000 genStretchy,
                      vectorOf 1500 (choose (2, 9) >>= genScoped True),
                      vectorOf 1000 genPlanted,
                      map fst <$> vectorOf 1500 genIdle
                    ]
              )
              (mkQCGen 20261105)
              30
        warnings p =
          either
            (const Nothing)
            (Just . sum . map (\j -> fromEnum j.begins + fromEnum j.ends))
            (checkCopies p)
        within p = fewAlternatives 200 p
        warned = [(p, n) | p <- cases, Just n <- [warnings p], n > 0, within p]
    length warned `shouldSatisfy` (> 400)
    -- among them paths with unions, with @N, and from each generator
    length [() | (p, _) <- warned, any isUnion (universe' p)] `shouldSatisfy` (> 80)
    length [() | (p, _) <- warned, any isFixed (universe' p)] `shouldSatisfy` (> 80)
    length [() | p <- take 5000 cases, maybe False (> 0) (warnings p), within p]
      `shouldSatisfy` (> 100)
    forM_ warned \(p, n) -> do
      let without = [p' | p' <- removals p, Just n' <- [warnings p'], n' < n]
      -- each @ that is warned about can be taken out
      (show p, length without >= n) `shouldBe` (show p, True)
      forM_ without \p' ->
        (show (p, p'), compilePath p') `shouldBe` (show (p, p'), compilePath p)
    -- the pairs of a walk with an @ at an end and without it
    let pairs = unGen (vectorOf 1500 genIdle) (mkQCGen 20261105) 30
    forM_ pairs \(with', without) ->
      when (maybe False (> 0) (warnings with')) $
        (show with', compilePath with') `shouldBe` (show with', compilePath without)
  it "does bounded work, however much the path would compile to" do
    -- 2^40 alternatives and 2^40 choices: the path is read as written.
    -- The allowance is that of 'spec_compileContradictions': work that
    -- grows with what the path would compile to fails instead of hanging;
    -- it measures nothing
    let wide = foldl1Ex' (P.:/) (replicate 40 [path| (a ++ b)/(c | d) |] :: [P.Path])
        long = [path| (p & q) |] P.:/ wide P.:/ [path| ((c & y)/@/e & d) |]
    result <- timeout 60000000 (evaluate (isLeft (checkCopies long)))
    result `shouldBe` Just True
    -- many routes: what to write instead is only worked out for few
    let many = foldl1Ex' (P.:/) (replicate 30 [path| (a & b) |] :: [P.Path])
    result' <-
      timeout 60000000 . evaluate . either (isNothing . (.writtenApart)) (const False) $
        checkCopies (many P.:/ [path| ((c & y)/@/e & d) |])
    result' `shouldBe` Just True
    -- a union with a side for each of many copies: refused like any
    -- other, without a way to choose being listed
    refuses [path| (a & b)/(x | y | z)/(c1 & c2 & c3 & c4 & c5 & c6 & c7)/((c & y)/@/e & d) |]
  where
    (<$$>) = fmap . fmap
    isUnion = \case
      P.Union {} -> True
      _ -> False
    isFixed = \case
      P.Absolute _ -> True
      _ -> False
    -- what `ls` finds from node 0 on a graph given by its edges
    reaches edges surface =
      map smallNIDIndex . toList $
        foldMap
          targets
          ( resolveFrom
              (smallNID 0)
              (graphOf (ordNub (0 : concat [[a, b] | (a, _, b) <- edges])) edges)
              (compilePath (surface P.:/ P.One))
          ).resolutions
    -- a path with every run of @ in a sequence written as one @
    onceEach :: P.Path -> P.Path
    onceEach = \case
      p@(_ P.:/ _) ->
        foldl1Ex' (P.:/) . map headEx . groupBy (\a b -> a == P.One && b == P.One) $
          map onceEach (sequenceOf p)
      l P.:& r -> onceEach l P.:& onceEach r
      P.Union k l r -> P.Union k (onceEach l) (onceEach r)
      P.Backwards x -> P.Backwards (onceEach x)
      P.WithLimits v lim x -> P.WithLimits v lim (onceEach x)
      p -> p
    sequenceOf = \case
      l P.:/ r -> sequenceOf l <> sequenceOf r
      p -> [p]
    -- the path with one @ taken out, for every @ that stands in a
    -- sequence
    removals :: P.Path -> [P.Path]
    removals = \case
      l P.:/ r ->
        [r | l == P.One]
          <> [l | r == P.One]
          <> [l' P.:/ r | l' <- removals l]
          <> [l P.:/ r' | r' <- removals r]
      l P.:& r -> [l' P.:& r | l' <- removals l] <> [l P.:& r' | r' <- removals r]
      P.Union k l r -> [P.Union k l' r | l' <- removals l] <> [P.Union k l r' | r' <- removals r]
      P.Backwards x -> P.Backwards <$> removals x
      P.WithLimits v lim x -> P.WithLimits v lim <$> removals x
      _ -> []
    universe' :: P.Path -> [P.Path]
    universe' p =
      p : case p of
        P.Backwards q -> universe' q
        a P.:/ b -> universe' a <> universe' b
        a P.:& b -> universe' a <> universe' b
        P.Union _ a b -> universe' a <> universe' b
        P.WithLimits _ _ q -> universe' q
        _ -> []

-- | Random paths for the meeting-point rule: every step has a label of
-- its own, so that no two sides of a union are the same, and a side has
-- a step. With the argument, also @N and !{…}; without, the path is
-- relative and has no @N.
genScoped :: Bool -> Int -> Gen P.Path
genScoped tests size = S.evalStateT (chain' (2 :: Int) (max 2 (size `div` 2))) (0 :: Int)
  where
    step = do
      i <- S.state \n -> (n, n + 1)
      back <- lift (frequency [(5, pure id), (1, pure P.Backwards)])
      pure (back (P.Literal ("s" <> tshow i)))
    joinOf =
      lift . frequency $
        [(6, pure P.One), (1, pure (P.ExcludingNIDs (singletonSet (smallNID 1))))]
          <> [(2, pure (P.Absolute (smallNID 1))) | tests]
    -- a chain of segments: steps, joins, groups and unions
    chain' depth len = do
      n <- lift (choose (1, len))
      foldl1Ex' (P.:/) <$> for [1 .. n :: Int] (const (segment depth))
    segment depth = do
      k <- lift (choose (0, if depth > 0 then 11 else 4 :: Int))
      case k of
        _ | k < 3 -> step
        _ | k < 5 -> joinOf
        _ | k < 9 -> do
          n <- lift (elements [2, 2, 2, 3])
          foldl1Ex' (P.:&) <$> for [1 .. n :: Int] (const (chain' (depth - 1) 3))
        9 -> P.Backwards <$> chain' (depth - 1) 3
        _ -> do
          kind <- lift (frequency [(4, pure (P.:||)), (1, pure (P.:++))])
          n <- lift (elements [2, 2, 3])
          foldl1Ex' kind <$> for [1 .. n :: Int] (const (side depth))
    -- a side has a step somewhere
    side depth = do
      p <- chain' (depth - 1) 3
      if hasStep p then pure p else (p P.:/) <$> step
    hasStep = \case
      P.Literal _ -> True
      P.Backwards p -> hasStep p
      a P.:/ b -> hasStep a || hasStep b
      a P.:& b -> hasStep a || hasStep b
      P.Union _ a b -> hasStep a || hasStep b
      _ -> False

-- | Random paths with a meeting point planted in them: a conjunct with
-- one (or two conjuncts of neighbouring groups, of which one holds
-- several walks together with an @N and the other has an open join
-- facing it), wrapped in conjuncts, sides and sequences, with a group
-- next to it or not.
genPlanted :: Gen P.Path
genPlanted = S.evalStateT go (0 :: Int)
  where
    step = S.state \n -> (P.Literal ("s" <> tshow n), n + 1)
    fork = (P.:&) <$> step <*> step
    fixed = P.Absolute (smallNID 1)
    open = lift (elements [P.One, P.One, P.ExcludingNIDs (singletonSet (smallNID 2))])
    pick xs = join (lift (elements xs))
    kernel =
      pick
        [ (\f j x -> f P.:/ j P.:/ x) <$> fork <*> open <*> step,
          (\f j -> j P.:/ f) <$> fork <*> open,
          (\f j x -> x P.:/ j P.:/ f) <$> fork <*> open <*> step,
          (\f j -> f P.:/ j) <$> fork <*> open,
          -- a fork and an open join with a join of the conjunct between
          (\f j x y -> f P.:/ fixed P.:/ x P.:/ j P.:/ y) <$> fork <*> open <*> step <*> step,
          (\f j x y -> y P.:/ j P.:/ x P.:/ fixed P.:/ f) <$> fork <*> open <*> step <*> step,
          -- walks held together before an open join, and after one
          (\f x j y z w -> (fixed P.:/ f P.:& z) P.:/ (x P.:/ j P.:/ y P.:& w))
            <$> fork
            <*> step
            <*> open
            <*> step
            <*> step
            <*> step,
          (\f x j y z w -> (x P.:/ j P.:& z) P.:/ (f P.:/ fixed P.:/ y P.:& w))
            <$> fork
            <*> step
            <*> open
            <*> step
            <*> step
            <*> step,
          (\f x j z w -> (x P.:/ fixed P.:/ f P.:& z) P.:/ (j P.:/ x P.:& w))
            <$> fork
            <*> step
            <*> open
            <*> step
            <*> step,
          -- an open join in a group of a conjunct with a join, next to
          -- walks held together, or to walks held together a level deeper
          (\f x j y z w v u -> (fixed P.:/ f P.:& z) P.:/ (u P.:/ (x P.:/ j P.:/ y P.:& w) P.:/ fixed P.:& v))
            <$> fork
            <*> step
            <*> open
            <*> step
            <*> step
            <*> step
            <*> step
            <*> step,
          (\f x j y z w v u t -> (fixed P.:/ (t P.:/ fixed P.:/ f P.:& t) P.:& z) P.:/ (u P.:/ (x P.:/ j P.:/ y P.:& w) P.:/ fixed P.:& v))
            <$> fork
            <*> step
            <*> open
            <*> step
            <*> step
            <*> step
            <*> step
            <*> step
            <*> step,
          (\f x j y z w v u -> (v P.:& fixed P.:/ (w P.:& y P.:/ j P.:/ x) P.:/ u) P.:/ (z P.:& f P.:/ fixed))
            <$> fork
            <*> step
            <*> open
            <*> step
            <*> step
            <*> step
            <*> step
            <*> step,
          -- an open join on one walk, with nothing held together
          (\x j y z w v -> (x P.:& z) P.:/ (y P.:/ j P.:/ w P.:& v))
            <$> step
            <*> open
            <*> step
            <*> step
            <*> step
            <*> step
        ]
    wrap x =
      pick
        [ (x P.:&) <$> step,
          (P.:& x) <$> step,
          (\a b -> a P.:/ (x P.:& b)) <$> step <*> step,
          (\a b -> a P.:/ fixed P.:/ (x P.:& b)) <$> step <*> step,
          (\a b -> (x P.:& b) P.:/ fixed P.:/ a) <$> step <*> step,
          (x P.:||) <$> step,
          (\a -> (a P.:/ P.One) P.:|| x) <$> step,
          (P.:/ x) <$> step,
          (x P.:/) <$> step,
          (\a b -> (x P.:& a) P.:/ b) <$> step <*> step,
          (x P.:++) <$> step
        ]
    around x =
      pick
        [ pure x,
          (P.:/ x) <$> fork,
          (x P.:/) <$> fork,
          (\f a -> f P.:/ a P.:/ x) <$> fork <*> step,
          (\f -> f P.:/ P.One P.:/ x) <$> fork,
          (\f -> x P.:/ fixed P.:/ f) <$> fork,
          (\f a b -> f P.:/ ((a P.:/ P.One) P.:|| (b P.:/ P.One)) P.:/ x)
            <$> fork
            <*> step
            <*> step,
          (\f a b -> f P.:/ ((a P.:/ P.One) P.:++ (b P.:/ P.One)) P.:/ x)
            <$> fork
            <*> step
            <*> step,
          (\f a -> (f P.:|| a) P.:/ x) <$> fork <*> step,
          (\f a -> x P.:/ ((a P.:/ P.One) P.:|| f)) <$> fork <*> step
        ]
    go = do
      k <- kernel
      n <- lift (choose (0, 3 :: Int))
      wrapped <- foldM (\x _ -> wrap x) k [1 .. n]
      around wrapped

-- | Random paths as the second reviewer of the warning made them
-- (@h/gen.py@ of that review, profile "mixed"): mostly two or three
-- groups in one stretch with steps, unions and joins between them, so
-- that conjuncts are often copied; and unions (@|@ and @++@), @\@N@,
-- @!{…}@ and @~@ anywhere, with few labels, so that sides are often
-- written alike.
genStretchy :: Gen P.Path
genStretchy = frequency [(4, stretchy), (1, sequence' 0)] `suchThat` ((<= 16) . leaves)
  where
    -- (that review kept paths of up to 70 characters)
    leaves :: P.Path -> Int
    leaves = \case
      l P.:/ r -> leaves l + leaves r
      l P.:& r -> leaves l + leaves r
      P.Union _ l r -> leaves l + leaves r
      P.Backwards p -> leaves p
      P.WithLimits _ _ p -> leaves p
      _ -> 1
    chance :: Double -> Gen Bool
    chance p = (< p) <$> choose (0, 1)
    step = do
      label <- elements ["a", "b", "c", "d", "e", "x", "y", "w"]
      wild <- chance 0.04
      back <- chance 0.05
      pure ((if back then P.Backwards else id) (if wild then P.Wild else P.Literal label))
    join' =
      frequency
        [ (25, P.Absolute . smallNID <$> elements [1, 2, 3]),
          (10, pure (P.ExcludingNIDs (singletonSet (smallNID 7)))),
          (65, pure P.One)
        ]
    union' = chance 0.3 <&> \shared -> if shared then (P.:++) else (P.:||)
    sequence' :: Int -> Gen P.Path
    sequence' depth = do
      n <- choose (1, 3)
      foldl1Ex' (P.:/) <$> vectorOf n (item depth)
    item depth = do
      joins <- chance 0.3
      x <- choose (0, 1 :: Double)
      segment depth joins x
    segment :: Int -> Bool -> Double -> Gen P.Path
    segment depth joins x
      | joins = join'
      | depth < 3 && x < 0.3 = do
          n <- frequency [(4, pure 2), (1, pure 3)]
          group' <- foldl1Ex' (P.:&) <$> vectorOf n (sequence' (depth + 1))
          back <- chance 0.05
          pure (if back then P.Backwards group' else group')
      | depth < 3 && x < 0.55 = do
          n <- frequency [(4, pure 2), (1, pure 3)]
          kind <- union'
          foldl1Ex' kind <$> vectorOf n (sequence' (depth + 1))
      | otherwise = step
    optional p gen = chance p >>= \yes -> if yes then (: []) <$> gen else pure []
    stretchy = do
      n <- choose (2, 3 :: Int)
      parts <- for [1 .. n] \_ -> do
        size <- frequency [(17, pure 2), (3, pure 3)]
        group' <- foldl1Ex' (P.:&) <$> vectorOf size (sequence' 1)
        inUnion <- chance 0.25
        grouped <-
          if inUnion
            then union' <*> pure group' <*> sequence' 1
            else pure group'
        between <- optional 0.4 step
        unions <- optional 0.2 (union' <*> sequence' 2 <*> sequence' 2)
        joins <- optional 0.08 join'
        pure ([grouped] <> between <> unions <> joins)
      first' <- optional 0.3 step
      root <- optional 0.15 join'
      last' <- optional 0.2 step
      pure (foldl1Ex' (P.:/) (root <> first' <> concat parts <> last'))

-- | A conjunct that is one walk with an @ at an end, in a context, and
-- the same without the @.
genIdle :: Gen (P.Path, P.Path)
genIdle = do
  steps <-
    choose (1, 2) >>= \n ->
      vectorOf n . elements $
        [ [path| a |],
          [path| b |],
          [path| ~a |],
          [path| * |],
          [path| (a | b) |],
          [path| (a | b/c) |],
          [path| a/@000000000002/b |],
          [path| !{@000000000003}/c |]
        ]
  leading <- arbitrary
  let walk = foldl1Ex' (P.:/) steps
      with' = if leading then P.One P.:/ walk else walk P.:/ P.One
  ctx <-
    elements
      [ \x -> (x P.:& [path| b |]) P.:/ [path| c & a |],
        \x -> [path| a & b |] P.:/ (x P.:& [path| c |]),
        \x -> [path| a & b |] P.:/ (x P.:& [path| c |]) P.:/ [path| a & c |],
        \x -> [path| a |] P.:/ (x P.:& [path| b |]),
        \x -> (x P.:& [path| b |]) P.:/ [path| @/(c & a) |],
        \x -> ((x P.:& [path| b |]) P.:/ [path| c & a |] P.:& [path| a |]) P.:/ [path| b & c |],
        \x -> [path| a | c |] P.:/ (x P.:& [path| b |]) P.:/ [path| c & a |],
        \x -> [path| a & b |] P.:/ ((x P.:|| [path| c |]) P.:& [path| b |]),
        \x -> [path| a/@ | c |] P.:/ (x P.:& [path| b |]) P.:/ [path| c & a |],
        \x -> (x P.:& x) P.:/ [path| a & b |],
        \x -> [path| a & b |] P.:/ (x P.:& x),
        \x -> (x P.:& [path| b |]) P.:/ [path| (c & a)/@000000000001/b & c |],
        \x -> [path| @000000000001/(c & a) & b |] P.:/ (x P.:& [path| c |]),
        \x -> [path| b/@000000000001/(c & a) & b |] P.:/ [path| c |] P.:/ (x P.:& [path| c |]),
        \x -> (x P.:& [path| b |]) P.:/ [path| c/@000000000001/b & c |],
        \x -> [path| p & q |] P.:/ ([path| b/@000000000001 |] P.:/ (x P.:& [path| c |]) P.:& [path| a |]),
        \x -> [path| (a & b) | c |] P.:/ (x P.:& [path| c |]),
        \x -> (x P.:& [path| c |]) P.:/ [path| (a & b)/@000000000001/c | c |]
      ]
  pure (ctx with', ctx walk)

-- | A path with every @\@@ and @!{…}@ marked by a node of its own that
-- it excludes, so that the copies of each written join can be told in
-- what the path compiles to.
marked :: P.Path -> P.Path
marked p0 = S.evalState (go p0) (100 :: Int)
  where
    next = S.state \i -> (smallNID i, i + 1)
    go = \case
      P.One -> next <&> \n -> P.ExcludingNIDs (singletonSet n)
      P.ExcludingNIDs ns -> next <&> \n -> P.ExcludingNIDs (insertSet n ns)
      P.Backwards p -> P.Backwards <$> go p
      a P.:/ b -> (P.:/) <$> go a <*> go b
      a P.:& b -> (P.:&) <$> go a <*> go b
      P.Union k a b -> P.Union k <$> go a <*> go b
      P.WithLimits v l p -> P.WithLimits v l <$> go p
      p -> pure p

-- | The marked joins ('marked') of an alternative that have two copies
-- or more, at one copy of which several routes arrive by different
-- steps or leave by different steps: a meeting point that was copied.
-- This looks only at what the compiler made, and shares nothing with
-- 'checkCopies'.
--
-- Copies are counted as the product written out has them (the routes
-- go through the join under two names or more), or, with the first
-- argument, only when they are also two vertices or more in the
-- pattern: the copies of an @ that starts a stretch, or stands right
-- next to an @N, are written apart and are one node.
copiedJoins :: Bool -> ([[Item Int]], PatternPath) -> [NID]
copiedJoins asVertices (routes, p) =
  [ m
  | m <- ordNub [m | (m, _, _, _) <- passes],
    not asVertices || length (findWithDefault [] m byMarker) >= 2,
    let copies =
          foldr
            (\(k, pr, nx) -> insertWith (<>) k (singletonSet pr, singletonSet nx))
            (mempty :: Map Int (Set (Maybe Int), Set (Maybe Int)))
            [(k, pr, nx) | (m', k, pr, nx) <- passes, m' == m],
    length copies >= 2,
    any (\(a, b) -> length a >= 2 || length b >= 2) (toList copies)
  ]
  where
    isMarker n = maybe False (>= 100) (smallNIDIndex n)
    byMarker :: Map NID [PVertex]
    byMarker =
      foldr
        (\(m, v) -> insertWith (<>) m [v])
        mempty
        [(m, v) | (v, Except ns) <- mapToList p.tests, m <- toList ns, isMarker m]
    passes :: [(NID, Int, Maybe Int, Maybe Int)]
    passes = concatMap (go Nothing) routes
    -- each marked join on a route, with the step or join before it and
    -- the one after it
    go :: Maybe Int -> [Item Int] -> [(NID, Int, Maybe Int, Maybe Int)]
    go prev = \case
      [] -> []
      Along k _ _ : rest -> go (Just (2 * k)) rest
      Meet k test _ : rest ->
        let next = case rest of
              Along k' _ _ : _ -> Just (2 * k')
              Meet k' _ _ : _ -> Just (2 * k' + 1)
              [] -> Nothing
         in [(m, k, prev, next) | Except ns <- [test], m <- toList ns, isMarker m]
              <> go (Just (2 * k + 1)) rest

-- | Whether some alternative of a path has a join that was copied with a
-- fork at one of its copies ('copiedJoins', copies counted as vertices).
copiesMeetingPoint :: P.Path -> Bool
copiesMeetingPoint p =
  any (not . null . copiedJoins True) (alternativeRoutes (marked p))

-- | What is written at one level of a path: a step, a join with its
-- test, a group of conjuncts, or a union of sides (and whether it is a |
-- union).
data Written
  = WStep
  | WJoin NodeTest
  | WGroup [[Written]]
  | WUnion Bool [[Written]]

-- | The rule of docs/path-pattern-ir.md (Meeting points), worked the
-- long way: the path is written once for every way to read its unions
-- (each side in its place, and a | union as one step), and for each the
-- product is written out: the path is refused when a @/\@@ or @!{…}@
-- written once has two copies or more and, in one of them, more than
-- one walk comes into it or goes out of it.
--
-- Node tests written right after one another are one join, and where
-- they contradict each other there is nothing to read: a side with such
-- a join is no side.
--
-- The rule is worked on the product written out, round by round
-- ('product'').
handRefuses :: P.Path -> Bool
handRefuses = handRule False

-- | The quick test of the docs, which is no definition: two groups next
-- to each other, and in one of them a conjunct with a join of its own
-- that has, at any depth, a conjunct with a @/\@@ or @!{…}@ and a fork.
-- It misses walks that a conjunct of the other group holds together,
-- and refuses where a join of the conjunct stands between the fork and
-- the @/\@@.
shortcutRefuses :: P.Path -> Bool
shortcutRefuses = handRule True

handRule :: Bool -> P.Path -> Bool
handRule shortcut =
  maybe False (any (if shortcut then chain' else product' . map (False,)) . readings)
    . written
    . pushBackwards
  where
    -- The product written out, round by round. A chain, each segment
    -- with whether it comes from a conjunct that is copied. Refused
    -- when a /@ or !{…} that comes from a copied conjunct has a group
    -- left in the stretch before it or after it: several walks meet at
    -- that copy of it, or leave it. Otherwise each stretch with groups
    -- is written out every way: a conjunct of each group is chosen (a
    -- conjunct without a join is written into the stretch, and chooses
    -- in its groups too; a chosen conjunct with a join is written whole
    -- in its group's place). A conjunct is copied when it is chosen in
    -- two ways or more, or its group comes from a copied conjunct. What
    -- is written is a chain again: the next round.
    product' :: [(Bool, Written)] -> Bool
    product' c = held c || any round' (stretchesOf c)
    held c =
      or
        [ any (isGroup . snd) (takeWhile (not . isJoin . snd) (reverse before))
            || any (isGroup . snd) (takeWhile (not . isJoin . snd) after)
        | (before, (True, WJoin (Except _)) : after) <- zip (inits c) (tails c)
        ]
    stretchesOf c = case break (isJoin . snd) c of
      (s, []) -> [s]
      (s, _ : rest) -> s : stretchesOf rest
    round' s =
      let ways = choices [] s
          copies key = length [() | way <- ways, key `elem` mapMaybe (fst . fst) way]
       in any (isGroup . snd) s
            && or
              [ product'
                  [ (inherited || maybe False ((>= 2) . copies) key, x)
                  | ((key, inherited), x) <- way
                  ]
              | way <- ways
              ]
    -- each segment with the conjunct it is written from in this round,
    -- if it is, and whether what it comes from was copied before
    choices :: [Int] -> [(Bool, Written)] -> [[((Maybe [Int], Bool), Written)]]
    choices at xs =
      map concat . traverse id $
        [ case x of
            WGroup cs ->
              [ picked
              | (j, k) <- zip [0 ..] cs,
                let key = at <> [i, j],
                picked <-
                  if any isJoin k
                    then [map ((Just key, copied),) k]
                    else choices key (map (copied,) k)
              ]
            _ -> [[((Nothing, copied), x)]]
        | (i, (copied, x)) <- zip [0 ..] xs
        ]
    isGroup = \case
      WGroup _ -> True
      _ -> False
    written :: P.Path -> Maybe [Written]
    written p = traverse part (sequenceOf p) >>= merged . concat
    sequenceOf = \case
      a P.:/ b -> sequenceOf a <> sequenceOf b
      P.WithLimits _ _ p -> sequenceOf p
      p -> [p]
    part :: P.Path -> Maybe [Written]
    part = \case
      P.One -> Just [WJoin anyNode]
      P.Zero -> Nothing
      P.ExcludingNIDs ns -> Just [WJoin (Except ns)]
      P.Absolute n -> Just [WJoin (Exactly n)]
      p@(_ P.:& _) -> do
        cs <- traverse written (conjunctionOf p)
        Just [WGroup (concatMap flat cs)]
      p@(P.Union kind _ _) -> case mapMaybe written (sidesOf kind p) of
        [] -> Nothing
        [side] -> Just side
        sides -> Just [WUnion (kind == P.Relational) sides]
      _ -> Just [WStep]
    conjunctionOf = \case
      a P.:& b -> conjunctionOf a <> conjunctionOf b
      p -> [p]
    sidesOf kind = \case
      P.Union kind' a b | kind' == kind -> sidesOf kind a <> sidesOf kind b
      p -> [p]
    -- joins written right after one another are one join
    merged :: [Written] -> Maybe [Written]
    merged = \case
      WJoin a : WJoin b : rest -> do
        both <- meetTests a b
        merged (WJoin both : rest)
      x : rest -> (x :) <$> merged rest
      [] -> Just []
    flat = \case
      [WGroup cs] -> cs
      c -> [c]
    readings :: [Written] -> [[Written]]
    readings = \case
      [] -> [[]]
      x : rest -> [here <> after | here <- reading x, after <- readings rest]
    reading = \case
      -- nested groups are one group
      WGroup cs -> [[WGroup (concatMap flat ks)] | ks <- traverse readings cs]
      WUnion relational sides -> concatMap readings sides <> [[WStep] | relational]
      x -> [[x]]
    -- a chain apart: each of its stretches, with no group around it
    chain' :: [Written] -> Bool
    chain' = any (stretch False) . stretches
    stretches c = case break isJoin c of
      (s, []) -> [s]
      (s, _ : rest) -> s : stretches rest
    isJoin = \case
      WJoin _ -> True
      _ -> False
    -- a stretch, and whether a group is around it
    stretch around s =
      let groups = [cs | WGroup cs <- s]
          copied = around || length groups >= 2
       in or
            [ if any isJoin k
                then (copied && meeting k) || chain' k
                else stretch copied k
            | cs <- groups,
              k <- cs
            ]
    meeting k =
      (or [True | WJoin (Except _) <- k] && or [True | WGroup _ <- k])
        || or [meeting k' | WGroup cs <- k, k' <- cs]

-- | Every side of a union that is written is a side of its own, for @|@
-- and for @++@: no two are one side, whether they are written the same,
-- compile to the same thing, or differ in an @\@@ that does nothing. A
-- route picks a side, routes that pick the same side of one copy of the
-- union share that side's joins, and routes that pick different sides
-- share nothing of them. So a union of n sides has at most n nodes for a
-- join written once in each side.
spec_compileEverySide :: Spec
spec_compileEverySide = describe "every written side of a union is its own side" do
  let same a b = compilePath a `shouldBe` compilePath b
      count p = length (compilePath p).alternatives
      found edges surface =
        map smallNIDIndex . toList $
          foldMap
            targets
            ( resolveFrom
                (smallNID 0)
                (graphOf (ordNub (0 : concat [[s, t] | (s, _, t) <- edges])) edges)
                (compilePath (surface P.:/ P.One))
            ).resolutions
      -- two a, one before d and one before e
      g = [(0, "a", 1), (0, "a", 2), (1, "d", 9), (2, "e", 9)]
  it "routes that pick different sides share nothing of them" do
    found g [path| a/@/(d & e) |] `shouldBe` []
    found g [path| (a/@ | a/@)/(d & e) |] `shouldBe` [Just 9]
    found g [path| (a/@ | @/a/@)/(d & e) |] `shouldBe` [Just 9]
    found g [path| (@/a/@ | a/@)/(d & e) |] `shouldBe` [Just 9]
    count [path| (a/@ | a/@)/(d & e) |] `shouldBe` 2
    count [path| (a/@ | @/a/@)/(d & e) |] `shouldBe` 2
    -- the two sides picked alike are a/@/(d & e), and picked apart they
    -- are two walks
    same [path| (a/@ | a/@)/(d & e) |] [path| a/@/(d & e) | a/@/d & a/@/e |]
    -- a ++ is chosen once for the whole path: both sides are a/@/(d & e)
    found g [path| (a/@ ++ a/@)/(d & e) |] `shouldBe` []
    same [path| (a/@ ++ a/@)/(d & e) |] [path| a/@/(d & e) |]
  it "routes that pick the same side share its joins: n sides, at most n nodes" do
    let three = [(0, "a", 1), (0, "a", 2), (0, "a", 3), (1, "d", 9), (2, "e", 9), (3, "f", 9)]
        two = [(0, "a", 1), (0, "a", 2), (1, "d", 9), (1, "e", 9), (2, "f", 9)]
    -- d, e and f each need an a node of their own
    found three [path| (a/@ | a/@)/(d & e & f) |] `shouldBe` []
    found three [path| (a/@ | a/@ | a/@)/(d & e & f) |] `shouldBe` [Just 9]
    -- two a nodes are enough
    found two [path| (a/@ | a/@)/(d & e & f) |] `shouldBe` [Just 9]
    found two [path| a/@/(d & e & f) |] `shouldBe` []
    -- as with sides that are written differently
    found
      [(0, "a", 1), (0, "a", 2), (0, "b", 3), (1, "d", 9), (2, "e", 9), (3, "f", 9)]
      [path| (a/@ | b/@)/(d & e & f) |]
      `shouldBe` []
    found
      [(0, "a", 1), (0, "b", 3), (1, "d", 9), (1, "e", 9), (3, "f", 9)]
      [path| (a/@ | b/@)/(d & e & f) |]
      `shouldBe` [Just 9]
    -- 2 sides for each of 3 routes, up to which side is which
    count [path| (a/@ | a/@)/(d & e & f) |] `shouldBe` 4
  it "a union written as a side is its sides, and %never is no side" do
    same [path| (a/@ | (a/@ | b/@))/(d & e) |] [path| (a/@ | a/@ | b/@)/(d & e) |]
    same [path| ((a/@ | a/@) | b/@)/(d & e) |] [path| (a/@ | a/@ | b/@)/(d & e) |]
    found g [path| (b/@ | (a/@ | a/@))/(d & e) |] `shouldBe` [Just 9]
    same [path| (a/@ | %never)/(d & e) |] [path| a/@/(d & e) |]
    same [path| (%never | a/@ | %never)/(d & e) |] [path| a/@/(d & e) |]
    same [path| (a/@ | %never | a/@)/(d & e) |] [path| (a/@ | a/@)/(d & e) |]
    found g [path| (a/@ | %never)/(d & e) |] `shouldBe` []
    -- so is a side whose node tests contradict each other
    same
      [path| (a/@ | x/@000000000001/@000000000002)/(d & e) |]
      [path| a/@/(d & e) |]
  it "p | p is p where p has no join of its chain, or one route goes through" do
    -- no join of the chain the union is written in: the routes share
    -- nothing of p, whichever side each takes
    for_
      [ [path| a |],
        [path| a/b |],
        [path| a & b |],
        [path| a/(b & c) |],
        [path| x/@/w & y |],
        [path| a | b |]
      ]
      \p -> for_
        [ (P.:/ [path| d & e |]),
          ([path| p & q |] P.:/),
          (\x -> [path| p & q |] P.:/ x P.:/ [path| d & e |]),
          (\x -> [path| q/@ |] P.:/ x P.:/ [path| d & e |])
        ]
        \ctx -> (show (ctx p), compilePath (ctx (p P.:|| p))) `shouldBe` (show (ctx p), compilePath (ctx p))
    -- Before a group with a conjunct whose walks an @N holds together,
    -- that is so for a p that does not fork.
    let held x = x P.:/ [path| (c & y)/@000000000001/e & d |]
    for_ [[path| a |], [path| a/b |], [path| a | b |]] \p ->
      (show (held p), compilePath (held (p P.:|| p)))
        `shouldBe` (show (held p), compilePath (held p))
    -- For a p that forks, p | p there has one alternative more. Where
    -- the routes of p did not all take the same side, the e after the
    -- @1 is one step for the copies of the conjunct made for them; where
    -- they did, it is a step for each copy, as in p alone. The two paths
    -- still find the same nodes.
    let heldGraph extra =
          [ (0, "a", 2),
            (0, "b", 3),
            (2, "c", 1),
            (2, "y", 1),
            (3, "c", 1),
            (3, "y", 1),
            (2, "d", 5),
            (3, "d", 5)
          ]
            <> extra
    for_ [[path| a & b |], [path| a/a & b |]] \p -> do
      let twice = held (p P.:|| p)
      (show p, count twice, count (held p)) `shouldBe` (show p, 2, 1)
      (show p, (compilePath (held p)).alternatives `difference` (compilePath twice).alternatives)
        `shouldBe` (show p, mempty)
      for_ [[], [(1, "e", 5)], [(1, "e", 5), (1, "e", 6)], [(2, "a", 2)]] \extra ->
        (show p, extra, found (heldGraph extra) twice)
          `shouldBe` (show p, extra, found (heldGraph extra) (held p))
    found (heldGraph [(1, "e", 5)]) (held [path| (a & b) | (a & b) |]) `shouldBe` [Just 5]
    -- (likewise after such a conjunct, turned around; not where the
    -- walks fork on the far side of the @1, or do not go on from it)
    count [path| (e/@000000000001/(c & y) & d)/((a & b) | (a & b)) |] `shouldBe` 2
    count [path| (e/@000000000001/(c & y) & d)/(a & b) |] `shouldBe` 1
    same
      [path| ((a & b) | (a & b))/(e/@000000000001/(c & y) & d) |]
      [path| (a & b)/(e/@000000000001/(c & y) & d) |]
    same
      [path| ((a & b) | (a & b))/((c & y)/@000000000001 & d) |]
      [path| (a & b)/((c & y)/@000000000001 & d) |]
    -- no group in the stretch the union is in: one route goes through
    -- each copy of it, and takes one side
    for_
      [ [path| a/@ |],
        [path| @/a/@ |],
        [path| a/@/b |],
        [path| (a & b)/@ |]
      ]
      \p -> for_
        [ id,
          (P.:/ [path| c |]),
          (\x -> [path| q |] P.:/ x P.:/ [path| c |]),
          (\x -> [path| (p & q)/@ |] P.:/ x P.:/ [path| @/(d & e) |])
        ]
        \ctx -> (show (ctx p), compilePath (ctx (p P.:|| p))) `shouldBe` (show (ctx p), compilePath (ctx p))
    -- and it is not p where routes that fork, after the union or before
    -- it, can take one side each
    compilePath [path| (a/@ | a/@)/(d & e) |] `shouldNotBe` compilePath [path| a/@/(d & e) |]
    compilePath [path| (p & q)/(a/@ | a/@) |] `shouldNotBe` compilePath [path| (p & q)/a/@ |]
    -- nor where p has a ++: written twice it is two unions, which are
    -- chosen apart, so the two sides may be x and y
    same
      [path| ((x ++ y) | (x ++ y))/(d & e) |]
      [path| (x ++ y)/(d & e) | x/d & y/e | y/d & x/e |]
    count [path| ((x ++ y) | (x ++ y))/(d & e) |] `shouldBe` 4
    count [path| (x ++ y)/(d & e) |] `shouldBe` 2
    -- (p ++ p is p whatever p is: either choice is p)
    same [path| ((x | y)/@ ++ (x | y)/@)/(d & e) |] [path| (x | y)/@/(d & e) |]
  it "the review's examples give one result in every order and grouping of their sides" do
    -- (they gave different results as the sides were ordered, grouped,
    -- or followed by more unions, while sides that compile to the same
    -- thing in place were merged within bounds)
    let everyWay name edges expected p = do
          let ways = orderings p
          (name, length ways > 1) `shouldBe` (name, True)
          forM_ ways \way -> do
            (name, show way, compilePath way) `shouldBe` (name, show way, compilePath p)
            (name, show way, found edges way) `shouldBe` (name, show way, expected)
        g2a =
          [(0, "n", 1), (1, "c", 2), (1, "c", 3), (2, "e", 4), (3, "e", 5), (4, "f", 9), (5, "g", 9)]
    everyWay "D2a" g2a [Just 9] [path| (m/@ | n)/(c/@ | c/@/@ | @/c/@)/(d/@ | e)/(f & g) |]
    found g2a [path| (m/@ | n)/c/@/(d/@ | e)/(f & g) |] `shouldBe` []
    everyWay "D2b flat" g [Just 9] [path| (a/@ | @/a/@ | @/c)/(d & e) |]
    everyWay "D2b nested" g [Just 9] [path| (a/@ | @/(a/@ | c))/(d & e) |]
    everyWay "D2b with c" g [Just 9] [path| (a/@ | c | @/(a/@ | c))/(d & e) |]
    -- what follows the union does not change what its sides are
    let g3 = g <> [(9, "x", 9)]
        following k = foldl' (P.:/) [path| @ |] (replicate k [path| x | y |] :: [P.Path])
    for_ [0 .. 8 :: Int] \k -> do
      (k, found g3 ([path| (a/@ | @/a/@)/(d & e) |] P.:/ following k)) `shouldBe` (k, [Just 9])
      (k, found g3 ([path| a/@/(d & e) |] P.:/ following k)) `shouldBe` (k, [])
    everyWay "D3" g3 [Just 9] ([path| (a/@ | @/a/@)/(d & e) |] P.:/ following 4)
    -- Unions of two such sides in a row are each two sides, however
    -- many there are (they were one side each up to four in a row, and
    -- two from five on). The two routes share the walk up to the first
    -- union they take different sides of: k + 2 alternatives.
    let pairs k = foldl1Ex' (P.:/) (replicate k [path| x/@ | @/x/@ |] :: [P.Path])
    for_ [1 .. 6 :: Int] \k ->
      (k, count (pairs k P.:/ [path| (a/@ | @/a/@)/(d & e) |])) `shouldBe` (k, k + 2)
  it "the order and the grouping of sides never matter" do
    -- random paths with unions anywhere, each also with one label for
    -- every step, so that sides are often written alike
    let cases =
          unGen
            ( (<>)
                <$> vectorOf 700 (choose (2, 9) >>= genScoped True)
                <*> vectorOf 700 genStretchy
            )
            (mkQCGen 20261108)
            30
        within p = fewAlternatives 200 p
        -- and each with a side of every union written twice
        twinned = [q | p <- take 500 cases, hasUnion p, let q = withTwin p, within q]
        unions = [p | p <- cases <> map oneLabel cases, hasUnion p, within p] <> twinned
    length unions `shouldSatisfy` (> 600)
    length twinned `shouldSatisfy` (> 100)
    -- the side written twice is often no side written once
    length [() | q <- twinned, compilePath (dropTwins q) /= compilePath q] `shouldSatisfy` (> 20)
    forM_ unions \p -> do
      (show p, compilePath (reorder reverse p)) `shouldBe` (show p, compilePath p)
      (show p, compilePath (reorder rotate p)) `shouldBe` (show p, compilePath p)
      (show p, compilePath (regroup p)) `shouldBe` (show p, compilePath p)
  where
    hasUnion = \case
      P.Union {} -> True
      l P.:/ r -> hasUnion l || hasUnion r
      l P.:& r -> hasUnion l || hasUnion r
      P.Backwards p -> hasUnion p
      P.WithLimits _ _ p -> hasUnion p
      _ -> False
    sidesOf kind = \case
      P.Union kind' l r | kind' == kind -> sidesOf kind l <> sidesOf kind r
      p -> [p]
    -- a path with each union rebuilt from its sides
    rebuilt :: (P.UnionKind -> [P.Path] -> P.Path) -> P.Path -> P.Path
    rebuilt build = \case
      p@(P.Union kind _ _) -> build kind (map (rebuilt build) (sidesOf kind p))
      l P.:/ r -> rebuilt build l P.:/ rebuilt build r
      l P.:& r -> rebuilt build l P.:& rebuilt build r
      P.Backwards p -> P.Backwards (rebuilt build p)
      P.WithLimits v l p -> P.WithLimits v l (rebuilt build p)
      p -> p
    reorder f = rebuilt \kind sides -> foldl1Ex' (P.Union kind) (f sides)
    rotate xs = drop 1 xs <> take 1 xs
    -- grouped from the right: p | (q | r)
    regroup = rebuilt \kind sides -> foldr1Ex (P.Union kind) sides
    -- with the sides that are written like one before them left out
    dropTwins = rebuilt \kind sides -> foldl1Ex' (P.Union kind) (ordNub sides)
    -- with the first side of every union written again after the others
    withTwin = rebuilt \kind sides -> foldl1Ex' (P.Union kind) (sides <> take 1 sides)
    -- the sides of every union in every order, grouped from the left and
    -- from the right
    orderings :: P.Path -> [P.Path]
    orderings = \case
      p@(P.Union kind _ _) -> ordNub do
        sides <- traverse orderings (sidesOf kind p)
        ordered <- permutations sides
        [foldl1Ex' (P.Union kind) ordered, foldr1Ex (P.Union kind) ordered]
      l P.:/ r -> (P.:/) <$> orderings l <*> orderings r
      l P.:& r -> (P.:&) <$> orderings l <*> orderings r
      P.Backwards p -> P.Backwards <$> orderings p
      P.WithLimits v l p -> P.WithLimits v l <$> orderings p
      p -> [p]
    -- every step with the same label
    oneLabel = \case
      P.Literal _ -> P.Literal "a"
      l P.:/ r -> oneLabel l P.:/ oneLabel r
      l P.:& r -> oneLabel l P.:& oneLabel r
      P.Union k l r -> P.Union k (oneLabel l) (oneLabel r)
      P.Backwards q -> P.Backwards (oneLabel q)
      P.WithLimits v l q -> P.WithLimits v l (oneLabel q)
      q -> q
