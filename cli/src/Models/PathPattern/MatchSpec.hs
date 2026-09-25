-- | Compares the matcher, and the effectful loader in front of it, with a
-- brute-force oracle that tries every assignment of variables and labels.
module Models.PathPattern.MatchSpec where

import Effectful.State.Dynamic
import Graph.GraphMetadataEditing (runInMemoryGraphMetadataEditing)
import Graph.PathPattern.Resolve
import Models.Edge
import Models.Graph (Graph, edgeSet, nidSet)
import Models.NID
import Models.Path.TH (path)
import Models.PathPattern
import Models.PathPattern.Compile (compilePath)
import Models.PathPattern.Match
import Models.PathPattern.TestCorpus
import MyPrelude
import Utils.Testing

-- | Every witness of a pattern, by exhaustive search.
bruteForce :: Graph Text () -> NID -> PatternPath -> Set Witness
bruteForce g cur p =
  setFromList
    [ Witness (mapFromList nodes) (mapFromList labels)
    | boundOk,
      nodes <- assignments (patternVars p) allNodes,
      labels <- assignments (keys specs) allLabels,
      let w = Witness (mapFromList nodes) (mapFromList labels)
          value = \case
            Current -> cur
            Fixed n -> n
            Var v -> fromJustEx (lookup v w.nodes),
      all (\v -> testAccepts (vertexTest p v) (value v)) (patternVertices p),
      all (edgeHolds value w) (mapToList specs)
    ]
  where
    specs = patternEdges p
    allNodes = toList (nidSet g)
    edges = edgeSet g
    allLabels = ordNub (map (.transition) (toList edges))
    boundOk =
      all
        (`member` nidSet g)
        [n | v <- patternVertices p, Just n <- [boundNID v]]
    boundNID = \case
      Current -> Just cur
      Fixed n -> Just n
      Var _ -> Nothing
    edgeHolds value w (e, spec) =
      let l = fromJustEx (lookup e w.labels)
          s = value spec.source
          t = value spec.target
       in matcherAccepts spec.matcher l
            && case spec.direction of
              Out -> Edge s l t `member` edges
              In -> Edge t l s `member` edges
    assignments :: [k] -> [v] -> [[(k, v)]]
    assignments ks vs = traverse (\k -> map (k,) vs) ks

resolveIn :: Graph Text () -> PathQuery -> ResolvedPath
resolveIn g q =
  resolveQuery corpusCurrent q
    & runInMemoryGraphMetadataEditing
    & evalStateLocal g
    & runPureEff

spec_matchOracle :: Spec
spec_matchOracle = describe "matching agrees with brute force" do
  forM_ corpusQueries \surface -> it (show surface) do
    let q = compilePath surface
    forM_ corpusGraphs \g -> do
      let resolved = resolveIn g q
          byPattern =
            mapFromList
              [(r.patternPath, r) | r <- resolved.resolutions] ::
              Map PatternPath Resolution
      forM_ (toList q.alternatives) \p -> do
        let expected = bruteForce g corpusCurrent p
            pure' = matchPattern (lookupIn g) (Just corpusCurrent) p
            loaded = lookup p byPattern
            context = show (p, g)
        annotate context do
          -- the pure matcher and the loader agree with the oracle
          fmap (setFromList . witnesses) pure'
            `shouldBe` justIfTrue (not (null expected)) expected
          fmap (setFromList . witnesses) loaded
            `shouldBe` justIfTrue (not (null expected)) expected
          forM_ pure' \r -> do
            let ws = witnesses r
                keyed = map (concreteRoutes r) ws
                oracleRoutes w = concreteRoutes r w
            length ws `shouldBe` length expected
            -- the first witness is the lexicographic minimum
            headMay keyed
              `shouldBe` headMay (sort (map oracleRoutes (toList expected)))
            targets r
              `shouldBe` setFromList
                [ n
                | w <- toList expected,
                  rt <- oracleRoutes w,
                  let n = maybe rt.root (.target) (lastMay rt.hops)
                ]
            setFromList (selectedRoutes r)
              `shouldBe` mapSet
                (\w -> fromJustEx (headMay (oracleRoutes w)))
                expected
            routeLastEdges r
              `shouldBe` lastEdgesOracle r (toList expected)
  where
    annotate :: String -> IO () -> IO ()
    annotate context action =
      action `catch` \(e :: SomeException) ->
        throwIO (userError (context <> "\n" <> show e))

lastEdgesOracle :: Resolution -> [Witness] -> Map Int (Set (Edge Text))
lastEdgesOracle r ws =
  unionsWith (<>) . map (uncurry singletonMap) $
    [ (i, singletonSet (concreteEdge before h))
    | w <- ws,
      (i, rt) <- zip [0 ..] (concreteRoutes r w),
      Just h <- [lastMay rt.hops],
      let before = case reverse rt.hops of
            _ : prev : _ -> prev.target
            _ -> rt.root
    ]

spec_matchExamples :: Spec
spec_matchExamples = describe "matching examples" do
  let g = graphWithEdges
      matched edges surface =
        (resolveIn (g edges) (compilePath surface)).resolutions
  it "a & a has k * k witnesses stored as 2k partials" do
    let edges = [edge' 1 "a" n | n <- [2 .. 11]]
    case matched edges [path| a & a |] of
      [r] -> do
        length (witnesses r) `shouldBe` 100
        sum (map (length . snd) r.components) `shouldBe` 20
      rs -> expectationFailure ("expected one resolution: " <> show rs)
  it "(a & a)/@ is one component" do
    let edges = [edge' 1 "a" n | n <- [2 .. 11]]
    case matched edges [path| (a & a)/@ |] of
      [r] -> do
        length r.components `shouldBe` 1
        length (witnesses r) `shouldBe` 10
      rs -> expectationFailure ("expected one resolution: " <> show rs)
  it "(* & *)/@ chooses labels independently" do
    let edges = [edge' 1 "x" 2, edge' 1 "y" 2]
    case matched edges [path| (* & *)/@ |] of
      [r] -> length (witnesses r) `shouldBe` 4
      rs -> expectationFailure ("expected one resolution: " <> show rs)
  it "*/@/(b & c) makes one label choice for the shared step" do
    let edges =
          [edge' 1 "x" 2, edge' 1 "y" 2, edge' 2 "b" 3, edge' 2 "c" 4]
    case matched edges [path| */@/(b & c) |] of
      [r] -> length (witnesses r) `shouldBe` 2
      rs -> expectationFailure ("expected one resolution: " <> show rs)
  it "a route through a fixed node spans two components" do
    let edges = [edge' 1 "a" 5, edge' 5 "b" 6, edge' 5 "b" 7]
    case matched edges [path| a/@000000000005/b |] of
      [r] -> do
        length r.components `shouldBe` 2
        map (map (.target) . (.hops)) (selectedRoutes r)
          `shouldBe` [[smallNID 5, smallNID 6], [smallNID 5, smallNID 7]]
      rs -> expectationFailure ("expected one resolution: " <> show rs)
  it "a missing fixed node does not match" do
    matched [] [path| @000000000005 |] `shouldBe` []
  it "orders alternatives by their smallest witness" do
    let edges = [edge' 1 "a" 9, edge' 1 "b" 2]
        firsts =
          map
            (map (.label) . (.hops) . firstSelectedRoute)
            (matched edges [path| b + a |])
    firsts `shouldBe` [["a"], ["b"]]
