-- | Tests for "Models.PathPattern.Compile": the examples in
-- docs/path-pattern-ir.md, and an independent interpreter of the distributed
-- expressions that the glued patterns must agree with.
module Models.PathPattern.CompileSpec where

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
import Models.PathPattern.TestCorpus
import MyPrelude
import Test.QuickCheck.Gen (unGen)
import Test.QuickCheck.Random (mkQCGen)
import Utils.Testing

-- | Evaluate a rooted expression directly, producing each witness as its
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
      Written _ d m -> go input (Step d m)
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
      Alt es -> concatMap (go input) es
      Held {} -> error "interpret: a shared walk not put in its place"
      Sequence p q ->
        [ [ConcreteRoute w.root (w.hops ++ c.hops) | w <- ws, c <- cs]
        | ws <- go input p,
          j <- ordNub (map endOf ws),
          cs <- go (Just j) q
        ]
    endOf r = maybe r.root (.target) (lastMay r.hops)

matchedRoutes :: Graph Text () -> P.Path -> Set [ConcreteRoute]
matchedRoutes g surface =
  setFromList
    [ concreteRoutes r w
    | r <- resolved.resolutions,
      w <- witnesses r
    ]
  where
    resolved =
      resolveQuery corpusCurrent (compilePath surface)
        & runInMemoryGraphMetadataEditing
        & evalStateLocal g
        & runPureEff

spec_compileOracle :: Spec
spec_compileOracle = describe "compiled patterns agree with the interpreter" do
  forM_ corpusCompileQueries \surface -> it (show surface) do
    let exprs = toList (surfaceExprs (pushBackwards surface))
    forM_ corpusGraphs \g ->
      (show g, matchedRoutes g surface)
        `shouldBe` ( show g,
                     setFromList (concatMap (interpretExpr g corpusCurrent) exprs)
                   )

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

-- | Sides are the same when they compile to the same routes in the same
-- order, however they are written.
spec_compileSameSides :: Spec
spec_compileSameSides = describe "a side written twice is one side" do
  let same a b = compilePath a `shouldBe` compilePath b
      n1 = smallNID 1
      p = (P.Literal "a" P.:|| P.ExcludingNIDs (singletonSet n1)) P.:/ [path| b/@ |]
      p' = (P.ExcludingNIDs (singletonSet n1) P.:|| P.Literal "a") P.:/ [path| b/@ |]
      before x = x P.:/ [path| d & e |]
  it "(a/@ | a/@/@)/(d & e)" $
    same [path| (a/@ | a/@/@)/(d & e) |] [path| a/@/(d & e) |]
  it "(p | q) | p and p | q | p are p | q" do
    same (before ((p P.:|| [path| x |]) P.:|| p)) (before (p P.:|| [path| x |]))
    same (before (p P.:|| ([path| x |] P.:|| p))) (before (p P.:|| [path| x |]))
  it "p with its inner union written the other way around" $
    same (before (p P.:|| p')) (before p)
  it "p | p/@ and p | @/p" do
    same (before (p P.:|| (p P.:/ P.One))) (before p)
    same (before (p P.:|| (P.One P.:/ p))) (before p)
  it "but an unjoined side and a joined one differ" $
    compilePath [path| (a | a/@)/(d & e) |]
      `shouldBe` compilePath [path| a/(d & e) | a/@/(d & e) |]
  it "a test repeated after an @ side" do
    same
      [path| ((c | @)/!{@000000000001} | (c | @)/!{@000000000001}/!{@000000000001})/(e & d) |]
      [path| (c | @)/!{@000000000001}/(e & d) |]
    same
      [path| (@/!{@000000000001} | !{@000000000001}/@ | !{@000000000001})/(e & d) |]
      [path| !{@000000000001}/(e & d) |]
  it "a test that an @N side already passes" do
    same
      [path| ((~a | * | @000000000001)/@000000000001 | (~a | * | @000000000001)/@000000000001/!{@000000000002})/(d & b) |]
      [path| (~a | * | @000000000001)/@000000000001/(d & b) |]
    same
      [path| (@000000000001 | @000000000001/@ | @000000000001/@000000000001 | @000000000001/!{@000000000002})/(d & b) |]
      [path| @000000000001/(d & b) |]
  it "but @/@N, which tests the current node, is not the root @N" do
    length (compilePath [path| (@/@000000000001 | @000000000001)/(e & d) |]).alternatives
      `shouldBe` 2
    length (compilePath [path| @000000000001/(e & d) |]).alternatives
      `shouldBe` 1
  it "and conjuncts in another order are another side, as their routes are" do
    length (alternativesUpToOrder [path| (c & * | * & c)/(!{@000000000001} & d) |])
      `shouldBe` 2
    length (alternativesUpToOrder [path| (c & *)/(!{@000000000001} & d) |])
      `shouldBe` 1

-- | A leading @ is dropped only to compare sides: in what a path compiles
-- to it still joins, so that an @N after it tests the current node.
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
  it "sides that differ in a leading @ stay two sides before an @N" do
    -- (c & @)/(@1 & d) jumps to node 1 where @/(c & @)/(@1 & d) tests
    -- the current node, also when the union is inside a conjunct
    let both = [path| ((@/(c & @) | (c & @)) & g)/(@000000000001 & d) |]
        included small =
          (alternativesUpToOrder small `difference` alternativesUpToOrder both)
            `shouldBe` mempty
    included [path| ((c & @) & g)/(@000000000001 & d) |]
    included [path| (@/(c & @) & g)/(@000000000001 & d) |]
    length (compilePath both).alternatives `shouldBe` 3
  it "a leading @ before a side with no output at its input is not a side" do
    compilePath [path| ((@/c | c) & g)/(@000000000001 & d) |]
      `shouldBe` compilePath [path| (c & g)/(@000000000001 & d) |]
    compilePath [path| ((@/(c/@ & b) | c/@ & b) & g)/(e & d) |]
      `shouldBe` compilePath [path| ((c/@ & b) & g)/(e & d) |]

-- | A union of many joined sides costs what its alternatives cost, not a
-- reading for each subset of its sides (docs/path-pattern-ir.md, Unions).
spec_compileManyJoinedSides :: Spec
spec_compileManyJoinedSides = describe "a union of many joined sides" do
  let nodes n = foldl1Ex' (P.:||) [P.Absolute (smallNID i) | i <- [1 .. n]]
      walks n =
        foldl1Ex'
          (P.:||)
          [P.Literal ("x" <> tshow i) P.:/ P.One | i <- [0 .. n - 1 :: Int]]
      counts name p n = it name do
        checked <- timeout 1000000 (evaluate (checkAlternatives p))
        checked `shouldBe` Just (Right ())
        compiled <-
          timeout 2000000 (evaluate (length (compilePath p).alternatives))
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
    486
  it "more alternatives than the limit are still refused at once" do
    checked <-
      timeout 1000000 . evaluate . checkAlternatives $
        nodes 40 P.:/ [path| a & b |]
    checked `shouldBe` Just (Left (TooManyAlternatives defaultMaxAlternatives))
  let refused name p = it name do
        checked <- timeout 5000000 (evaluate (checkAlternatives p))
        checked `shouldBe` Just (Left (TooLargeToCount defaultMaxAlternatives))
  -- unions whose copies choose in more ways than can be listed: the
  -- check stops while the term is being made
  refused "two unions copied by both ends" $
    [path| a & b |]
      P.:/ ([path| f |] P.:|| nodes 30)
      P.:/ ([path| j |] P.:|| nodes 30)
      P.:/ [path| k & l |]
  refused "a union in a conjunct, copied twice" $
    [path| (a & b & c)/(a & b | c) |]
      P.:/ (nodes 200 P.:& [path| c |])
      P.:/ [path| a & b |]

-- | Unions are chosen in every order only where the order can make a
-- difference: unions that are never sequenced with each other, and unions
-- of single chains, cost what their alternatives cost
-- (docs/path-pattern-ir.md, Unions).
spec_compileIndependentUnions :: Spec
spec_compileIndependentUnions = describe "unions whose order cannot matter" do
  let union' i = P.Literal ("a" <> tshow i) P.:|| P.Literal ("b" <> tshow i)
      conjoined n f = foldl1Ex' (P.:&) [f (union' i) | i <- [1 .. n :: Int]]
      chained n = foldl1Ex' (P.:/) [union' i | i <- [1 .. n :: Int]]
      counts name p n = it name do
        checked <- timeout 300000 (evaluate (checkAlternatives p))
        checked `shouldBe` Just (Right ())
        compiled <-
          timeout 500000 (evaluate (length (compilePath p).alternatives))
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
  it "where the order matters every order is still taken" do
    -- each order alone gives 6 of these 8
    length (alternativesUpToOrder [path| ((a & b) | x)/((c & d) | e) |])
      `shouldBe` 8
    -- each order alone gives 176
    let staged = [path| (c & d)/(x/@ | y)/(w/@ | z)/(a & b) |]
    length (alternativesUpToOrder staged) `shouldBe` 221
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
          isRight . checkAlternatives $
            limitedTo 300 x
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
          isRight . checkAlternatives $
            limitedTo 300 x
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
  it "where every copy chose it, a side is also as it is written alone" $
    -- the @ conjunct before (@1 & d) written alone is a root (the compile
    -- examples have it); chosen by the @ copy only, it follows that @
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
-- leading @\@@). A conjunct written without a step (@\@@, @\@N@) is left
-- out: an @\@N@ after a lone @\@@ conjunct is a root, after @\@/…@ a
-- test, so such prefixes differ before some continuation without a union
-- already, and two of them in one conjunction are not told apart.
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
          isRight . checkAlternatives $
            limitedTo 300 x
        checked =
          [ (p, p', c)
          | (p, k, c) <- cases,
            let rewritten = filter (/= p) (ordNub (rewrites p)),
            not (null rewritten),
            let p' = rewritten `indexEx` (k `mod` length rewritten),
            compilePath p == compilePath p',
            and
              [ compilePath (p P.:/ r) == compilePath (p' P.:/ r)
              | r <- readings c
              ],
            withinLimit (p P.:/ c),
            withinLimit (p' P.:/ c)
          ]
    it "random prefixes rewritten at one place" do
      length checked `shouldSatisfy` (> 150)
      forM_ checked \(p, p', c) ->
        (show (p, p', c), compilePath (p P.:/ c))
          `shouldBe` (show (p, p', c), compilePath (p' P.:/ c))
    it "an @N after a lone @ conjunct is why the conjunct is left out" do
      -- the same pattern alone, but not before @1: no counterexample
      compilePath [path| @/(c & @) |] `shouldBe` compilePath [path| c & @ |]
      compilePath [path| @/(c & @)/(@000000000001 & d) |]
        `shouldNotBe` compilePath [path| (c & @)/(@000000000001 & d) |]
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

-- | The alternatives of a path, each with its routes in a fixed order.
alternativesUpToOrder :: P.Path -> Set PatternPath
alternativesUpToOrder surface =
  mapSet routesInOrder (compilePath surface).alternatives

-- | A pattern with its routes in a fixed order.
routesInOrder :: PatternPath -> PatternPath
routesInOrder p =
  canonicalize
    p {routes = impureNonNullToNE (sortOn routeKey (toList p.routes))}
  where
    routeKey r =
      ( case r.root of
          Var _ -> Nothing
          v -> Just v,
        [(h.direction, h.matcher) | h <- r.hops]
      )
    impureNonNullToNE = \case
      x : xs -> x :| xs
      [] -> error "a pattern without routes"

spec_compileAlternativesLimit :: Spec
spec_compileAlternativesLimit = describe "the alternatives limit" do
  let big = [path| (a | b | c)/(d & e & f & g & h & i & j) |]
      limited l = P.WithLimits () (P.PathLimits (Just l) Nothing)
  it "does not limit a path without a conjunction, where nothing copies" $
    checkAlternatives
      [path| (a | b)/(a | b)/(a | b)/(a | b)/(a | b)/(a | b)/(a | b)/(a | b)/(a | b)/(a | b)/(a | b) |]
      `shouldBe` Right ()
  it "gives up on a path that would take too long to count" do
    let labels' = foldl1Ex' (P.:++) [P.Literal (tshow i) | i <- [1 .. 31 :: Int]]
        slow =
          labels'
            P.:/ labels'
            P.:/ [path| (a | b | c)/(e & f & g & h & i & j & k)/@000000000001/@000000000002 |]
    result <- timeout 2000000 (evaluate (checkAlternatives slow))
    result `shouldBe` Just (Left (TooLargeToCount defaultMaxAlternatives))
  it "stops a path with more alternatives than the default" $
    checkAlternatives big
      `shouldBe` Left (TooManyAlternatives defaultMaxAlternatives)
  it "allows it as a shared union" $
    checkAlternatives [path| (a ++ b ++ c)/(d & e & f & g & h & i & j) |]
      `shouldBe` Right ()
  it "an override raises the limit" do
    checkAlternatives (limited (P.AtMostAlternatives 3000) big)
      `shouldBe` Right ()
    checkAlternatives (limited P.UnlimitedAlternatives big) `shouldBe` Right ()
    checkAlternatives
      [path| %override({max-alternatives = 3000}, (a | b | c)/(d & e & f & g & h & i & j)) |]
      `shouldBe` Right ()
  it "an override lowers the limit, and the innermost one wins" do
    checkAlternatives
      [path| %override({max-alternatives = 5}, (a | b)/(c & d & e)) |]
      `shouldBe` Left (TooManyAlternatives 5)
    checkAlternatives
      [path| %override({max-alternatives = 5}, %override({max-alternatives = 8}, (a | b)/(c & d & e))) |]
      `shouldBe` Right ()
  it "the largest limit among the unions applies to the path" $
    checkAlternatives
      [path| %override({max-alternatives = 100}, a | b)/(c | d)/(e & f & g) |]
      `shouldBe` Right ()
  it "the limit does not change what the path means" $
    compilePath (limited (P.AtMostAlternatives 3) [path| (a | b)/(c & d) |])
      `shouldBe` compilePath [path| (a | b)/(c & d) |]

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
          isRight . checkAlternatives $
            limitedTo 100 (x P.:/ P.One)
        checked =
          [ (base, bigger)
          | (p, q, c, d, ctx) <- cases,
            k <- [P.Relational, P.Shared],
            let base = plug ctx c d p
                bigger = plug ctx c d (P.Union k p q),
            withinLimit base && withinLimit bigger
          ]
    length checked `shouldSatisfy` (> 400)
    -- a union of a path with itself, or with %never, is that path
    forM_ cases \(p, _, c, d, ctx) -> when (withinLimit (plug ctx c d p)) do
      let same x =
            (show (p, ctx, c, d), compilePath (plug ctx c d x))
              `shouldBe` (show (p, ctx, c, d), compilePath (plug ctx c d p))
      same (p P.:|| p)
      same (p P.:|| P.Zero)
      same (P.Zero P.:|| p)
      -- ...as is one with a side repeated, regrouped, or with the sides
      -- of every union in it written the other way around
      let other = P.Union P.Relational c d
          sameAs y x =
            (show (x, ctx), compilePath (plug ctx c d x))
              `shouldBe` (show (x, ctx), compilePath (plug ctx c d y))
      when (withinLimit (plug ctx c d (p P.:|| other))) do
        ((p P.:|| other) P.:|| p) `sameAs` (p P.:|| other)
        (p P.:|| (other P.:|| p)) `sameAs` (p P.:|| other)
        (other P.:|| p) `sameAs` (p P.:|| other)
        (p P.:|| swapUnions p) `sameAs` p
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

-- | The path under @%override({max-alternatives = n}, …)@.
limitedTo :: Int -> P.Path -> P.Path
limitedTo n =
  P.WithLimits () (P.PathLimits (Just (P.AtMostAlternatives n)) Nothing)

-- | The path with limit nodes put around random parts of it, up to three
-- deep at one place. The alternatives limits are the default's, so the
-- limit in effect is the same everywhere.
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
          [Nothing, Just (P.AtMostAlternatives defaultMaxAlternatives)]
        <*> elements
          [Nothing, Just (P.AtMostRepairs 5), Just P.UnlimitedRepairs]

spec_compileIgnoresLimits :: Spec
spec_compileIgnoresLimits = describe "limit nodes do not change compilation" do
  let override l = P.WithLimits () l
      repairs = P.PathLimits Nothing (Just (P.AtMostRepairs 5000))
      alternatives = P.PathLimits (Just (P.AtMostAlternatives 50)) Nothing
      plain = [path| (a & b)/(@ | d) |]
  it "two nested around a union after a conjunction" $
    compilePath
      ([path| a & b |] P.:/ override repairs (override alternatives [path| @ | d |]))
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
          P.:/ override alternatives (override repairs [path| a/@ |])
          P.:/ [path| d & e |]
      )
      `shouldBe` compilePath [path| (x & y)/a/@/(d & e) |]
  it "any number anywhere in random paths" . withMaxSuccess 2000 $
    forAll (fst <$> genRepairCase) \p -> forAll (genWithLimits p) \limited ->
      conjoin
        [ checkAlternatives limited === checkAlternatives p,
          checkAlternatives p
            == Right ()
              ==> conjoin
                [ compilePath limited === compilePath p,
                  compileAlternatives limited === compileAlternatives p,
                  surfaceExprs (pushBackwards limited)
                    === surfaceExprs (pushBackwards p)
                ]
        ]
