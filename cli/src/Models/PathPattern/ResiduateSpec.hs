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

-- | Without labels, mk does what it would do with a fresh label, or errors
-- iff a survivor would use that label (docs/path-pattern-ir.md §5).
spec_missingLabelsActAsFreshLabels :: Spec
spec_missingLabelsActAsFreshLabels =
  it "missing labels act as a fresh label over the corpus" do
    forM_ corpusQueries \surface -> forM_ corpusGraphs \g -> do
      let run chooser =
            residuatePure (`maybeNodeLookup` g) chooser corpusCurrent (compilePath surface)
          withFresh = run \m -> find (matcherAccepts m) ["fresh"]
          context = show (surface, g)
          usesFresh = \case
            Right (Repairs rs) ->
              any (any ((== "fresh") . (.transition)) . toList . (.patch.edges)) rs
            _ -> False
      case run (const Nothing) of
        Right r -> (context, withFresh) `shouldBe` (context, Right r)
        Left (UnsupportedLabel m _ _ _)
          | matcherAccepts m "fresh" ->
              (context, usesFresh withFresh) `shouldBe` (context, True)
          | otherwise ->
              (context, isLeft withFresh) `shouldBe` (context, True)

infix 1 `shouldNeedLabel`

-- | The matcher and step (route, hop) of a label error.
shouldNeedLabel ::
  (Show a) => Either ResidualError a -> (Matcher, Int, Int) -> Expectation
shouldNeedLabel result expected = case result of
  Left (UnsupportedLabel m ri hi _) -> (m, ri, hi) `shouldBe` expected
  other -> expectationFailure ("expected a label error: " <> show other)

-- | The survivors listed by a label error.
labelErrorRepairs :: Either ResidualError a -> Maybe Int
labelErrorRepairs = \case
  Left (UnsupportedLabel _ _ _ rs) -> Just (length rs)
  Right _ -> Nothing

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
    patches (graphWithEdges [edge' 5 "x" 5]) [path| a/b/@000000000005 & c/b/@000000000005 |]
      `shouldBe` [ [c <> " a new1", c <> " c new2", "new1 b " <> n 5, "new2 b " <> n 5],
                   [c <> " a new1", c <> " c new1", "new1 b " <> n 5]
                 ]
  it "identifies hops that create the same stored edge from opposite directions" $
    -- l and k/~l/@1 both need the stored edge 1 -l-> x
    patches (graphWithEdges []) [path| l & k/~l/@000000000001 |]
      `shouldBe` [[c <> " k new1", c <> " l new1"]]
  it "a mixed pair that does not shrink the patch is an alternative" $
    patches (graphWithEdges []) [path| l/m & k/~l/@000000000001 |]
      `shouldBe` [ [c <> " k new3", c <> " l new1", c <> " l new3", "new1 m new2"],
                   [c <> " k new1", c <> " l new1", "new1 m new2"]
                 ]
  describe "identifying a fresh vertex with an existing node" do
    let two = edge' 2 "x" 2
    it "is an alternative: mk a & a/@2" $ do
      patches (graphWithEdges [two]) [path| a & a/@000000000002 |]
        `shouldBe` [[c <> " a " <> n 2, c <> " a new1"], [c <> " a " <> n 2]]
      fmap chooseRepair (residuateIn (graphWithEdges [two]) [path| a & a/@000000000002 |])
        `shouldSatisfy` \case
          Right (Ambiguous _) -> True
          _ -> False
    it "also for a missing fixed node: mk a & a/@7" $
      patches (graphWithEdges []) [path| a & a/@000000000007 |]
        `shouldBe` [[c <> " a " <> n 7, c <> " a new1"], [c <> " a " <> n 7]]
    it "continues from the pinned node: mk a/b & a/@2/c" $
      patches (graphWithEdges [two]) [path| a/b & a/@000000000002/c |]
        `shouldBe` [ [ c <> " a " <> n 2,
                       c <> " a new1",
                       n 2 <> " c new3",
                       "new1 b new2"
                     ],
                     [c <> " a " <> n 2, n 2 <> " b new1", n 2 <> " c new2"]
                   ]
    it "takes left evidence from the pinned node: mk a/b & a/@2" $
      patches (graphWithEdges [edge' 2 "b" 3]) [path| a/b & a/@000000000002 |]
        `shouldBe` [ [c <> " a " <> n 2, c <> " a new1", "new1 b new2"],
                     [c <> " a " <> n 2]
                   ]
    it "from the right, including the current node: mk a/~a" $
      patches (graphWithEdges []) [path| a/~a |]
        `shouldBe` [[c <> " a new1", "new2 a new1"], [c <> " a new1"]]
    it "only when the node passes the vertex's tests: mk a/!{@2} & a/@2" $
      patches
        (graphWithEdges [two])
        [path| a/!{@000000000002} & a/@000000000002 |]
        `shouldBe` [[c <> " a " <> n 2, c <> " a new1"]]
    it "not when the shared edge exists: that is evidence" $
      residuateIn (graphWithEdges [edge' 1 "a" 2]) [path| a & a/@000000000002 |]
        `shouldBe` Right AlreadyResolves
    it "an unpinned repair from another alternative still wins" $
      patches (graphWithEdges [two]) [path| a/@000000000002 ++ (a & a/@000000000002) |]
        `shouldBe` [[c <> " a " <> n 2]]
    it "a pin does not eliminate a superset from another alternative" $
      length (patches (graphWithEdges [two]) [path| (a & a/@000000000002) ++ a/@000000000002/b |])
        `shouldBe` 3
    it "a pin does not eliminate left evidence: mk b/a & b/@2/a" $
      length (patches (graphWithEdges [edge' 1 "b" 1, two]) [path| b/a & b/@000000000002/a |])
        `shouldBe` 2
    it "nor an unpinned repair of another alternative" $
      length
        ( patches
            (graphWithEdges [two])
            [path| (b/a & b/@000000000002/a) ++ (b & b/@000000000002/a) |]
        )
        `shouldBe` 2
    it "a pin to the current node: mk (a & b)/@ & a" $
      patches (graphWithEdges [edge' 1 "b" 1]) [path| (a & b)/@ & a |]
        `shouldBe` [[c <> " a " <> c, c <> " a new1"], [c <> " a " <> c]]
    it "other evidence does not let the pin win: mk a & b & b/@3" $
      -- a binds to 1 or 2; either way the unpinned repair is the same
      patches
        (graphWithEdges [edge' 1 "a" 1, edge' 1 "a" 2, edge' 3 "x" 3])
        [path| a & b & b/@000000000003 |]
        `shouldBe` [[c <> " b " <> n 3, c <> " b new1"], [c <> " b " <> n 3]]
    it "other evidence does not let the pin win: mk (a & b)/@/a/@/(a & b)" $
      patches
        (graphWithEdges [edge' 1 "a" 1, edge' 1 "a" 2])
        [path| (a & b)/@/a/@/(a & b) |]
        `shouldBe` [ [c <> " b " <> c, c <> " b new1"],
                     [c <> " b " <> c, n 2 <> " a new1", n 2 <> " b new2"],
                     [ c <> " b " <> n 2,
                       n 2 <> " a new1",
                       "new1 a new2",
                       "new1 b new3"
                     ],
                     [c <> " b " <> c]
                   ]
    it "an equal repair is shown through evidence, not a pin" $
      fmap
        (\case Repairs rs -> map renderResidual (toList rs); _ -> [])
        (residuateIn (graphWithEdges [edge' 1 "a" 2]) [path| (a & ~a/a)/@ |])
        `shouldBe` Right
          [ c
              <> " -\"a\"-> "
              <> n 2
              <> " & "
              <> c
              <> " <-\"a\"- new1 -\"a\"-> "
              <> n 2
              <> "\n    creates: new1 -\"a\"-> "
              <> c
              <> ", new1 -\"a\"-> "
              <> n 2,
            -- the join is 2 (evidence), not 1 (a pin with the same patch)
            c
              <> " -\"a\"-> "
              <> n 2
              <> " & "
              <> c
              <> " <-\"a\"- "
              <> c
              <> " -\"a\"-> "
              <> n 2
              <> "\n    creates: "
              <> c
              <> " -\"a\"-> "
              <> c
          ]
    it "every pin still shares an edge with its node" $
      -- pinning a/c's target to 2 is justified only when the a and d nodes
      -- are identified, so d/c/@2 is the same edge
      patches
        (graphWithEdges [two, edge' 3 "x" 3])
        [path| a/b/@000000000003 & d/b/@000000000003 & a/c & d/c/@000000000002 |]
        `shouldBe` [ [ c <> " a new1",
                       c <> " d new2",
                       "new1 b " <> n 3,
                       "new1 c new3",
                       "new2 b " <> n 3,
                       "new2 c " <> n 2
                     ],
                     [c <> " a new1", c <> " d new1", "new1 b " <> n 3, "new1 c " <> n 2],
                     [ c <> " a new1",
                       c <> " d new1",
                       "new1 b " <> n 3,
                       "new1 c " <> n 2,
                       "new1 c new2"
                     ]
                   ]
    it "finishes quickly with many pins and long shared chains" do
      let count surface = case residuateIn (graphWithEdges [two]) surface of
            Right (Repairs rs) -> length rs
            other -> error ("expected repairs: " <> show other)
      result <-
        timeout 5000000 . evaluate $
          ( count [path| a/b/c/d/e/f/g/h & a/@000000000002/b/c/d/e/f/g/h |],
            count [path| a/~a & b/~b & c/~c & d/~d & e/~e & f/~f |]
          )
      result `shouldBe` Just (2, 64)
  it "a variable without a bound vertex is an internal error" $
    evaluate
      ( residuatePure
          (const Nothing)
          (const Nothing)
          corpusCurrent
          (query [pat [route (var 0) [out 0 "a" (var 1)]]])
      )
      `shouldThrow` anyErrorCall
  it "a join bound by one route's left walk is left-justified" $
    patches (graphWithEdges [edge' 1 "a" 2]) [path| (a & b)/@ |]
      `shouldBe` [[c <> " b " <> n 2]]
  it "orders right-evidence repairs by patch size after the left-only one" $
    -- a/b/c/@5 with 6 -c-> 5 and 7 -b-> 6
    patches (graphWithEdges [edge' 6 "c" 5, edge' 7 "b" 6]) [path| a/b/c/@000000000005 |]
      `shouldBe` [ [c <> " a new1", "new1 b new2", "new2 c " <> n 5],
                   [c <> " a " <> n 7],
                   [c <> " a new1", "new1 b " <> n 6]
                 ]
  it "the label error names the step that needs a label" $
    residuatePure (`maybeNodeLookup` graphWithEdges []) (const Nothing) corpusCurrent (compilePath [path| a & b/* |])
      `shouldNeedLabel` (MWild, 1, 1)
  it "(a & b)/@ joins the new edges" $
    patches (graphWithEdges []) [path| (a & b)/@ |]
      `shouldBe` [[c <> " a new1", c <> " b new1"]]
  it "unions are alternatives: mk a + b" $
    patches (graphWithEdges []) [path| a ++ b |]
      `shouldBe` [[c <> " a new1"], [c <> " b new1"]]
  it "equivalent alternatives are one repair: mk a + a/@" $
    patches (graphWithEdges []) [path| a ++ a/@ |] `shouldBe` [[c <> " a new1"]]
  it "a redundant start join is one repair: mk a + @/a" $
    patches (graphWithEdges []) [path| a ++ @/a |] `shouldBe` [[c <> " a new1"]]
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
        repairLabels = \case
          Repairs rs -> map (map (.transition) . toList . (.patch.edges)) (toList rs)
          other -> error ("expected repairs: " <> show other)
    it "a wildcard edge needs a label from the chooser" $
      noLabel (graphWithEdges []) [path| a/* |]
        `shouldNeedLabel` (MWild, 0, 1)
    it "an unrelated existing label is not reused" $
      noLabel (graphWithEdges [edge' 3 "foo" 4]) [path| a/* |]
        `shouldNeedLabel` (MWild, 0, 1)
    it "an existing wildcard edge satisfies the pattern" $
      noLabel (graphWithEdges [edge' 1 "a" 2, edge' 2 "foo" 3]) [path| a/* |]
        `shouldBe` Right AlreadyResolves
    it "an alternative lacking a label is an error, not dropped: mk a + *" $
      noLabel (graphWithEdges []) [path| a ++ * |]
        `shouldNeedLabel` (MWild, 0, 0)
    it "whichever alternative comes first: mk * + a" $
      noLabel (graphWithEdges []) [path| * ++ a |]
        `shouldNeedLabel` (MWild, 0, 0)
    it "a regex alternative lacking a label is an error: mk a + regex" $
      noLabel (graphWithEdges []) [path| a ++ regex:"^x"|]
        `shouldSatisfy` \case
          Left (UnsupportedLabel (MRegex _) 0 0 _) -> True
          _ -> False
    it "a path that already resolves is a no-op: mk a + *" $ do
      noLabel (graphWithEdges [edge' 1 "a" 2]) [path| a ++ * |]
        `shouldBe` Right AlreadyResolves
      noLabel (graphWithEdges [edge' 1 "x" 2]) [path| a ++ * |]
        `shouldBe` Right AlreadyResolves
      noLabel (graphWithEdges [edge' 1 "x" 2, edge' 2 "y" 3]) [path| b ++ */* |]
        `shouldBe` Right AlreadyResolves
    it "a label-needing superset of another repair is not needed: mk a + a/*" $
      fmap repairLabels (noLabel (graphWithEdges []) [path| a ++ a/* |])
        `shouldBe` Right [["a"]]
    it "an existing edge for * avoids the label: mk * & a" $
      fmap repairLabels (noLabel (graphWithEdges [edge' 1 "x" 2]) [path| * & a |])
        `shouldBe` Right [["a"]]
    it "a fresh * edge still needs a label: mk * & a" $
      noLabel (graphWithEdges []) [path| * & a |]
        `shouldNeedLabel` (MWild, 0, 0)
    it "a surviving label-needing candidate is not dropped for another" $
      -- 1 -a-> 2 would need 2 -*-> 5; right evidence 6 -x-> 5 needs 1 -a-> 6
      noLabel
        (graphWithEdges [edge' 1 "a" 2, edge' 6 "x" 5])
        [path| a/*/@000000000005 |]
        `shouldNeedLabel` (MWild, 0, 1)
    it "the error lists every survivor: mk c/d + c/e + *" $ do
      let result = noLabel (graphWithEdges []) [path| c/d ++ c/e ++ * |]
      result `shouldNeedLabel` (MWild, 0, 0)
      labelErrorRepairs result `shouldBe` Just 3
    it "a plain wildcard lists only its own repair: mk a/*" $
      labelErrorRepairs (noLabel (graphWithEdges []) [path| a/* |])
        `shouldBe` Just 1
    it "an equally good label-free repair is not taken: mk a/* + */b" $ do
      let result = noLabel (graphWithEdges [edge' 1 "a" 2]) [path| a/* ++ */b |]
      result `shouldNeedLabel` (MWild, 0, 1)
      labelErrorRepairs result `shouldBe` Just 2
    it "a missing label equals no literal: mk \"\\0MWild\" + *" $
      labelErrorRepairs
        (noLabel (graphWithEdges []) (P.Literal "\0MWild" P.:++ P.Wild))
        `shouldBe` Just 2
    it "chooser-supplied labels are unaffected: mk a + *" $ do
      let withChooser chooser =
            fmap
              repairLabels
              ( residuatePure
                  (`maybeNodeLookup` graphWithEdges [])
                  chooser
                  corpusCurrent
                  (compilePath [path| a ++ * |])
              )
      withChooser (const (Just "a")) `shouldBe` Right [["a"]]
      withChooser (const (Just "x")) `shouldBe` Right [["a"], ["x"]]
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
