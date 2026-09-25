module Models.PathPattern.ResiduateSpec where

import Models.Edge
import Models.Graph (Graph, insertEdge, insertNode, maybeNodeLookup)
import Models.NID
import Models.Node (emptyNode)
import Models.Path.Simple qualified as P
import Models.Path.TH (path)
import Models.PathPattern
import Models.PathPattern.Compile (compilePath)
import Models.PathPattern.Match
import Models.PathPattern.Residuate
import Models.PathPattern.TestCorpus
import MyPrelude
import Utils.Testing

-- | Fresh nodes become NIDs that no corpus graph uses.
freshNID :: Int -> NID
freshNID k = smallNID (1000 + k)

concrete :: Binding -> NID
concrete = \case
  Existing nid -> nid
  Fresh k -> freshNID k

applyPatch :: Patch -> Graph Text () -> Graph Text ()
applyPatch patch g =
  foldl'
    (flip insertEdge)
    (foldl' (\acc nid -> insertNode (emptyNode nid) acc) g (map concrete (toList patch.nodes)))
    [Edge (concrete e.source) e.transition (concrete e.sink) | e <- toList patch.edges]

-- | Labels for wildcard and regex edges in the corpus.
corpusLabels :: Matcher -> Maybe Text
corpusLabels m = find (matcherAccepts m) ["a", "b", "a\"b"]

residuateIn :: Graph Text () -> P.Path -> Either ResidualError Residuation
residuateIn g surface =
  residuatePure (`maybeNodeLookup` g) corpusLabels corpusCurrent (compilePath surface)

resolvesIn :: Graph Text () -> P.Path -> Bool
resolvesIn g surface =
  any
    (isJust . matchPattern (`maybeNodeLookup` g) (Just corpusCurrent))
    (compilePath surface).alternatives

spec_residuationProperties :: Spec
spec_residuationProperties = describe "residuation properties" do
  forM_ corpusQueries \surface -> it (show surface) do
    forM_ corpusGraphs \g -> do
      let context = show g
      case residuateIn g surface of
        Left err -> expectationFailure (context <> ": " <> show err)
        Right AlreadyResolves ->
          (context, resolvesIn g surface) `shouldBe` (context, True)
        Right Unsatisfiable ->
          (context, resolvesIn g surface) `shouldBe` (context, False)
        Right res@(Repairs rs) -> do
          (context, resolvesIn g surface) `shouldBe` (context, False)
          -- ambiguity iff at least two survivors
          case chooseRepair res of
            Apply r -> toList rs `shouldBe` [r]
            Ambiguous rs' -> do
              rs' `shouldBe` rs
              length rs `shouldSatisfy` (>= 2)
            other -> expectationFailure (show other)
          forM_ rs \r -> do
            let g' = applyPatch r.patch g
            -- the residual's own binding is a witness after patching
            let witness =
                  Witness
                    (mapFromList [(v, concrete b) | (Var v, b) <- mapToList r.sigma])
                    r.labels
                matched =
                  matchPattern (`maybeNodeLookup` g') (Just corpusCurrent) r.patternPath
            (context, fmap (elem witness . witnesses) matched)
              `shouldBe` (context, Just True)
            -- no-op afterwards, and applying it twice changes nothing
            (context, residuateIn g' surface)
              `shouldBe` (context, Right AlreadyResolves)
            (context, applyPatch r.patch g') `shouldBe` (context, g')

spec_residuationCoverage :: Spec
spec_residuationCoverage =
  it "the corpus exercises repairs, ambiguity and right evidence" do
    let results =
          [ r
          | surface <- corpusQueries,
            g <- corpusGraphs,
            Right r <- [residuateIn g surface]
          ]
        repairs = [rs | Repairs rs <- results]
    length [() | AlreadyResolves <- results] `shouldSatisfy` (> 100)
    length repairs `shouldSatisfy` (> 100)
    length (filter ((>= 2) . length) repairs) `shouldSatisfy` (> 10)
    length [() | rs <- repairs, r <- toList rs, not r.leftOnly]
      `shouldSatisfy` (> 10)

-- | The patches of a residuation, fresh nodes shown as new1, new2, ...
patches :: Graph Text () -> P.Path -> [[Text]]
patches g surface = case residuateIn g surface of
  Right (Repairs rs) ->
    map (\r -> map showEdge (toList r.patch.edges)) (toList rs)
  other -> error ("expected repairs: " <> show other)
  where
    showEdge e = renderBinding e.source <> " " <> e.transition <> " " <> renderBinding e.sink

n :: Int -> Text
n = tshow . smallNID

spec_residuationExamples :: Spec
spec_residuationExamples = describe "residuation examples" do
  let c = n 1
  it "keeps satisfied routes: mk a/b & c" $
    patches
      (graphWithEdges [edge' 1 "a" 2, edge' 1 "a" 3, edge' 2 "b" 4])
      [path| a/b & c |]
      `shouldBe` [[c <> " c new1"]]
  it "left evidence dominates building fresh: mk a/b" $
    patches (graphWithEdges [edge' 1 "a" 2]) [path| a/b |]
      `shouldBe` [[n 2 <> " b new1"]]
  it "right evidence is an alternative after the left-only repair" $
    -- (a/b & c)/@ with C -c-> Q and W -b-> Q (Q = 5, W = 6)
    patches
      (graphWithEdges [edge' 1 "c" 5, edge' 6 "b" 5])
      [path| (a/b & c)/@ |]
      `shouldBe` [[c <> " a new1", "new1 b " <> n 5], [c <> " a " <> n 6]]
  it "several right candidates are each an alternative" $
    patches
      (graphWithEdges [edge' 1 "c" 5, edge' 6 "b" 5, edge' 7 "b" 5])
      [path| (a/b & c)/@ |]
      `shouldBe` [ [c <> " a new1", "new1 b " <> n 5],
                   [c <> " a " <> n 6],
                   [c <> " a " <> n 7]
                 ]
  it "a fixed tail is right evidence: mk a/b/@5" $
    patches (graphWithEdges [edge' 6 "b" 5]) [path| a/b/@000000000005 |]
      `shouldBe` [[c <> " a new1", "new1 b " <> n 5], [c <> " a " <> n 6]]
  it "a & a creates one edge" $
    patches (graphWithEdges []) [path| a & a |] `shouldBe` [[c <> " a new1"]]
  it "a & b creates two nodes" $
    patches (graphWithEdges []) [path| a & b |]
      `shouldBe` [[c <> " a new1", c <> " b new2"]]
  it "a/b & a/c shares the new a node" $
    patches (graphWithEdges []) [path| a/b & a/c |]
      `shouldBe` [[c <> " a new1", "new1 b new2", "new1 c new3"]]
  it "(a & b)/c creates two c nodes" $
    patches (graphWithEdges []) [path| (a & b)/c |]
      `shouldBe` [[c <> " a new1", c <> " b new3", "new1 c new2", "new3 c new4"]]
  it "a/b/@5 & c/b/@5 is ambiguous between separate and shared b sources" $
    length (patches (graphWithEdges [edge' 5 "x" 5]) [path| a/b/@000000000005 & c/b/@000000000005 |])
      `shouldBe` 2
  it "(a & b)/@ joins the new edges" $
    patches (graphWithEdges []) [path| (a & b)/@ |]
      `shouldBe` [[c <> " a new1", c <> " b new1"]]
  it "unions are alternatives: mk a + b" $
    patches (graphWithEdges []) [path| a + b |]
      `shouldBe` [[c <> " a new1"], [c <> " b new1"]]
  it "equivalent alternatives are one repair: mk a + a/@" $
    patches (graphWithEdges []) [path| a + a/@ |] `shouldBe` [[c <> " a new1"]]
  it "a redundant start join is one repair: mk a + @/a" $
    patches (graphWithEdges []) [path| a + @/a |] `shouldBe` [[c <> " a new1"]]
  it "independent endpoints may already coincide" $
    residuateIn (graphWithEdges [edge' 1 "a" 2, edge' 1 "b" 2]) [path| a & b |]
      `shouldBe` Right AlreadyResolves
  it "duplicate conjunction is satisfied by existing edges" $
    residuateIn (graphWithEdges [edge' 1 "a" 2, edge' 1 "a" 3]) [path| a & a |]
      `shouldBe` Right AlreadyResolves
  it "an unsatisfiable test has no repair" $
    residuateIn (graphWithEdges []) [path| (@ & @000000000002)/@ |]
      `shouldBe` Right Unsatisfiable
  describe "labels" do
    let noLabel g surface =
          residuatePure (`maybeNodeLookup` g) (const Nothing) corpusCurrent (compilePath surface)
    it "a wildcard edge needs a label from the chooser" $
      noLabel (graphWithEdges []) [path| a/* |]
        `shouldBe` Left (UnsupportedLabel MWild)
    it "an unrelated existing label is not reused" $
      noLabel (graphWithEdges [edge' 3 "foo" 4]) [path| a/* |]
        `shouldBe` Left (UnsupportedLabel MWild)
    it "an existing wildcard edge satisfies the pattern" $
      noLabel (graphWithEdges [edge' 1 "a" 2, edge' 2 "foo" 3]) [path| a/* |]
        `shouldBe` Right AlreadyResolves
    it "a chooser's label is used" $
      fmap
        (\case Repairs (r :| _) -> map (.transition) (toList r.patch.edges); _ -> [])
        (residuatePure (`maybeNodeLookup` graphWithEdges []) (const (Just "x")) corpusCurrent (compilePath [path| * |]))
        `shouldBe` Right ["x"]
  describe "orphans" do
    let orphansOf g surface = case residuateIn g surface of
          Right (Repairs (r :| _)) -> patchOrphans r.patch
          other -> error (show other)
    it "a missing fixed root with new edges is an orphan" $
      orphansOf (graphWithEdges [edge' 1 "x" 1]) [path| @000000000005/a |]
        `shouldBe` [setFromList [Existing (smallNID 5), Fresh 0]]
    it "creating from an existing node is not an orphan" $
      orphansOf (graphWithEdges [edge' 1 "x" 1]) [path| ~a |] `shouldBe` []
