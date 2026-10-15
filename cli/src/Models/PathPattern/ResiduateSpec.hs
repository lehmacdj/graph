module Models.PathPattern.ResiduateSpec where

import Data.Set qualified as Set
import Models.Edge
import Models.Graph
  ( Graph,
    emptyGraph,
    insertEdge,
    insertEdges,
    insertNode,
    maybeNodeLookup,
  )
import Models.NID
import Models.Node (Node, emptyNode)
import Models.Path.Parse (pPath', pSmallNID)
import Models.Path.Simple qualified as P
import Models.Path.TH (path)
import Models.PathPattern
import Models.PathPattern.Admit
import Models.PathPattern.Compile (compilePath)
import Models.PathPattern.Match
import Models.PathPattern.Residuate
import Models.PathPattern.TestCorpus
import MyPrelude
import Utils.Parsing (whitespace)
import Utils.Parsing.Common (eof, runParser)
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

-- | 'corpusNode': the nodes of a graph, and the corpus's current node.
nodeIn :: Graph Text () -> NID -> Maybe (Node Text ())
nodeIn = corpusNode

residuateIn :: Graph Text () -> P.Path -> Either ResidualError Residuation
residuateIn g surface =
  residuatePure (nodeIn g) corpusLabels corpusCurrent (compilePath surface)

resolvesIn :: Graph Text () -> P.Path -> Bool
resolvesIn g surface =
  any
    (isJust . matchPattern (nodeIn g) (Just corpusCurrent))
    (compilePath surface).alternatives

-- | The nodes a path writes as @\@N@ in some alternative.
writtenNodes :: P.Path -> [NID]
writtenNodes p =
  ordNub [n | a <- toList (compilePath p).alternatives, Fixed n <- patternVertices a]

spec_residuationProperties :: Spec
spec_residuationProperties = describe "what mk creates, over the corpus" do
  forM_ corpusQueries \surface -> it (show surface) do
    forM_ corpusGraphs \g -> do
      let context = show g
      case residuateIn g surface of
        -- no node with a written id is created
        Left (MissingNodes absent) -> do
          (context, resolvesIn g surface) `shouldBe` (context, False)
          forM_ absent \n -> do
            (context, n `elem` writtenNodes surface) `shouldBe` (context, True)
            (context, isNothing (nodeIn g n)) `shouldBe` (context, True)
        -- a refused shape is refused whatever the graph holds
        Left (Refused refusal) ->
          (context, Just refusal)
            `shouldBe` (context, headMay (refusals corpusCurrent (compilePath surface)))
        Left err -> expectationFailure (context <> ": " <> show err)
        Right AlreadyResolves ->
          (context, resolvesIn g surface) `shouldBe` (context, True)
        Right Unsatisfiable ->
          (context, resolvesIn g surface) `shouldBe` (context, False)
        Right res@(Repairs rs) -> do
          (context, resolvesIn g surface) `shouldBe` (context, False)
          -- it asks iff there are at least two possibilities
          case chooseRepair res of
            Apply r -> toList rs `shouldBe` [r]
            Ambiguous rs' -> do
              rs' `shouldBe` rs
              length rs `shouldSatisfy` (>= 2)
            other -> expectationFailure (show other)
          forM_ rs \r -> do
            -- only new nodes are created
            (context, [b | b@(Existing _) <- toList r.patch.nodes]) `shouldBe` (context, [])
            let g' = applyPatch r.patch g
            -- where the walk went is a match once it is created
            let witness =
                  Witness
                    (mapFromList [(v, concrete b) | (Var v, b) <- mapToList r.sigma])
                    r.labels
                matched =
                  matchPattern (nodeIn g') (Just corpusCurrent) r.patternPath
            (context, fmap (elem witness . witnesses) matched)
              `shouldBe` (context, Just True)
            -- nothing to do afterwards, and creating it twice changes nothing
            (context, residuateIn g' surface)
              `shouldBe` (context, Right AlreadyResolves)
            (context, applyPatch r.patch g') `shouldBe` (context, g')

-- | Without labels, mk does what it would do with a fresh label, or errors
-- iff a possibility would use that label (docs/path-route-ir.md section 5).
spec_missingLabelsActAsFreshLabels :: Spec
spec_missingLabelsActAsFreshLabels =
  it "missing labels act as a fresh label over the corpus" do
    forM_ corpusQueries \surface -> forM_ corpusGraphs \g -> do
      let run chooser =
            residuatePure (nodeIn g) chooser corpusCurrent (compilePath surface)
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
        Left err@(MissingNodes _) -> (context, withFresh) `shouldBe` (context, Left err)
        Left err@(Refused _) -> (context, withFresh) `shouldBe` (context, Left err)
        Left err@(TooManyRepairs _) -> expectationFailure (context <> ": " <> show err)

infix 1 `shouldNeedLabel`

-- | The matcher and step (route, hop) of a label error.
shouldNeedLabel ::
  (Show a) => Either ResidualError a -> (Matcher, Int, Int) -> Expectation
shouldNeedLabel result expected = case result of
  Left (UnsupportedLabel m ri hi _) -> (m, ri, hi) `shouldBe` expected
  other -> expectationFailure ("expected a label error: " <> show other)

-- | The possibilities listed by a label error.
labelErrorRepairs :: Either ResidualError a -> Maybe Int
labelErrorRepairs = \case
  Left (UnsupportedLabel _ _ _ rs) -> Just (length rs)
  Left _ -> Nothing
  Right _ -> Nothing

spec_residuationCoverage :: Spec
spec_residuationCoverage =
  it "the corpus exercises creating, asking and doing nothing" do
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

-- * Examples

-- | What @mk@ does: nothing, or it creates edges and new nodes, or it
-- lists several such possibilities and asks; or it refuses, as when the
-- path writes nodes that do not exist.
data Does
  = Resolves
  | NeedsNodes [Int]
  | Creates [Text]
  | Asks [[Text]]
  | NeedsLabel [[Text]]
  | CannotBe
  | TooMany
  | -- | refused for its shape, with the raw code of the refusal
    Refuses Text
  deriving stock (Eq, Show)

-- | A node as an example writes it: its number, or @newK@.
shortBinding :: Binding -> Text
shortBinding = \case
  Existing nid -> maybe (tshow nid) tshow (smallNIDIndex nid)
  Fresh k -> "new" <> tshow (k + 1)

-- | A patch as an example writes it: @0 -a-> new1@ for an edge. (A
-- created node that is not new would show as @node 2@: there is none.)
shortPatch :: (Ord l) => (l -> Text) -> Patch' l -> [Text]
shortPatch label patch =
  [ shortBinding e.source <> " -" <> label e.transition <> "-> " <> shortBinding e.sink
  | e <- toList patch.edges
  ]
    <> ["node " <> shortBinding b | b@(Existing _) <- toList patch.nodes]

does :: Either ResidualError Residuation -> Does
does = \case
  Right AlreadyResolves -> Resolves
  Right Unsatisfiable -> CannotBe
  Right (Repairs (r :| [])) -> Creates (shortPatch id r.patch)
  Right (Repairs rs) -> Asks (map (shortPatch id . (.patch)) (toList rs))
  Left (TooManyRepairs _) -> TooMany
  Left (Refused refusal) -> Refuses (refusalCode refusal)
  Left (MissingNodes ns) -> NeedsNodes (mapMaybe smallNIDIndex (toList ns))
  Left (UnsupportedLabel _ _ _ rs) ->
    NeedsLabel (map (shortPatch label . (.patch)) (toList rs))
  where
    label = \case
      Known l -> l
      Missing m -> renderMatcher m

-- | A graph with node 0, these nodes and these edges (@(1, "a", 2)@).
graphOf :: [Int] -> [(Int, Text, Int)] -> Graph Text ()
graphOf nodes edges =
  insertEdges
    [edge' s l t | (s, l, t) <- edges]
    (foldl' (\g k -> insertNode (emptyNode (smallNID k)) g) emptyGraph (0 : nodes))

-- | A path as it is typed for @mk@: @+@ is @++@, and @\@2@ is the node
-- that 'graphOf' calls 2.
typed :: Text -> P.Path
typed t = case runParser (whitespace *> pPath' pSmallNID <* eof) "" t of
  Right parsed
    | Just p <- P.handleDirectivesWith (Just P.Shared) (\_ _ -> Nothing) parsed -> p
  _ -> error ("typed: not a path for mk: " <> unpack t)

-- | What @mk p@ does at node 0, with labels from the chooser.
mkWith :: (Matcher -> Maybe Text) -> Graph Text () -> Text -> Does
mkWith chooser g p =
  does (residuatePure (`maybeNodeLookup` g) chooser (smallNID 0) (compilePath (typed p)))

-- | What @mk p@ does at node 0.
mk :: Graph Text () -> Text -> Does
mk = mkWith (const Nothing)

-- | What @mk p@ creates at node 0: nothing if the path resolves.
patchOf :: Graph Text () -> Text -> Patch
patchOf g p =
  case residuatePure (`maybeNodeLookup` g) (const Nothing) (smallNID 0) (compilePath (typed p)) of
    Right (Repairs (r :| [])) -> r.patch
    Right AlreadyResolves -> Patch mempty mempty
    other -> error ("patchOf: " <> show other)

-- | The graph after @mk p@ at node 0 created what it creates. The k-th
-- node it creates is node @1000 + k@.
having :: Graph Text () -> Text -> Graph Text ()
having g p = applyPatch (patchOf g p) g

-- | What two commands create one after the other, the second on the graph
-- that 'having' gives after the first: the nodes the first creates are
-- new nodes of the whole.
both :: Patch -> Patch -> Patch
both first' next =
  Patch
    (first'.nodes <> Set.map whole next.nodes)
    ( first'.edges
        <> Set.map (\e -> Edge (whole e.source) e.transition (whole e.sink)) next.edges
    )
  where
    created = length [() | Fresh _ <- toList first'.nodes]
    whole = \case
      Fresh k -> Fresh (created + k)
      Existing n -> case [k | k <- [0 .. created - 1], freshNID k == n] of
        k : _ -> Fresh k
        [] -> Existing n

-- | What @mk p@ and then @mk q@ create together at node 0.
staged :: Graph Text () -> Text -> Text -> Patch
staged g p q = both (patchOf g p) (patchOf (having g p) q)

-- | Whether two patches create the same, up to the names of new nodes.
samePatch :: (Ord l) => Patch' l -> Patch' l -> Bool
samePatch a b = patchSubsumes a b && patchSubsumes b a

spec_walkExamples :: Spec
spec_walkExamples = describe "what mk creates" do
  let empty' = graphOf [] []
  describe "a step follows an edge that exists or that this command creates" do
    it "mk a & a/@2 creates only 0 -a-> 2" $
      mk (graphOf [2] []) "a & a/@2" `shouldBe` Creates ["0 -a-> 2"]
    it "mk a/~a creates only 0 -a-> new1: it is still going left to right" $
      mk empty' "a/~a" `shouldBe` Creates ["0 -a-> new1"]
    it "mk a/a stays a chain of two" $
      mk empty' "a/a" `shouldBe` Creates ["0 -a-> new1", "new1 -a-> new2"]
    it "mk a & a is one edge" $
      mk empty' "a & a" `shouldBe` Creates ["0 -a-> new1"]
    it "mk a/b & a/c shares the a node" $
      mk empty' "a/b & a/c"
        `shouldBe` Creates ["0 -a-> new1", "new1 -b-> new2", "new1 -c-> new3"]
    it "mk a/b & a/@2/c puts b and c under node 2" $
      mk (graphOf [2] []) "a/b & a/@2/c"
        `shouldBe` Creates ["0 -a-> 2", "2 -b-> new1", "2 -c-> new2"]
    it "mk a/~a/b puts b under the current node" $
      mk empty' "a/~a/b" `shouldBe` Creates ["0 -a-> new1", "0 -b-> new2"]
    it "mk a/~a & b/~b & c/~c creates three edges" $
      mk empty' "a/~a & b/~b & c/~c"
        `shouldBe` Creates ["0 -a-> new1", "0 -b-> new2", "0 -c-> new3"]
    it "mk a/b with an a edge puts b under its node" $
      mk (graphOf [] [(0, "a", 1)]) "a/b" `shouldBe` Creates ["1 -b-> new1"]
    it "a reverse step into a written node has no choice: mk ~a/@2" do
      mk (graphOf [2] []) "~a/@2" `shouldBe` Creates ["2 -a-> 0"]
      mk (graphOf [2] [(3, "a", 0)]) "~a/@2/b"
        `shouldBe` Creates ["2 -a-> 0", "2 -b-> new1"]
    it "an edge to an excluded node does not count: mk a/!{@1}" $
      mk (graphOf [] [(0, "a", 1)]) "a/!{@1}"
        `shouldBe` Creates ["0 -a-> new1"]
    it "unions are tried on their own: mk a + b asks" $
      mk empty' "a ++ b" `shouldBe` Asks [["0 -a-> new1"], ["0 -b-> new1"]]
    it "mk a/~a + (zz & a/~a) creates 0 -a-> new1, either way round" do
      mk empty' "a/~a ++ (zz & a/~a)" `shouldBe` Creates ["0 -a-> new1"]
      mk empty' "(zz & a/~a) ++ a/~a" `shouldBe` Creates ["0 -a-> new1"]
    it "mk (a & a/@3)/~x/(a + b) with 0 -x-> 3 creates 0 -a-> 3" $
      mk (graphOf [] [(0, "x", 3)]) "(a & a/@3)/~x/(a ++ b)"
        `shouldBe` Creates ["0 -a-> 3"]
    it "mk (a ++ b) & (b/c & b/@2/c) creates 0 -b-> 2 and c under 2" $
      mk (graphOf [2] []) "(a ++ b) & (b/c & b/@2/c)"
        `shouldBe` Creates ["0 -b-> 2", "2 -c-> new1"]
    it "an unsatisfiable test cannot be constructed" $
      mk (graphOf [2] []) "(@ & @2)/@" `shouldBe` CannotBe
  describe "nothing is decided from the right" do
    it "mk a/b/@2 & c/b/@2 creates two nodes" $
      mk (graphOf [2] []) "a/b/@2 & c/b/@2"
        `shouldBe` Creates
          ["0 -a-> new1", "0 -c-> new2", "new1 -b-> 2", "new2 -b-> 2"]
    it "mk (a & c)/@/b/@2 creates one" $
      mk (graphOf [2] []) "(a & c)/@/b/@2"
        `shouldBe` Creates ["0 -a-> new1", "0 -c-> new1", "new1 -b-> 2"]
    it "with 1 -b-> 2, mk a/b/@2 creates 0 -a-> new1 -b-> 2" $
      mk (graphOf [] [(1, "b", 2)]) "a/b/@2"
        `shouldBe` Creates ["0 -a-> new1", "new1 -b-> 2"]
    it "write a/@1/b/@2 for node 1" $
      mk (graphOf [] [(1, "b", 2)]) "a/@1/b/@2"
        `shouldBe` Creates ["0 -a-> 1"]
    it "mk (a/b & b)/@2 creates the three edges" $
      mk (graphOf [2] []) "(a/b & b)/@2"
        `shouldBe` Creates ["0 -a-> new1", "0 -b-> 2", "new1 -b-> 2"]
    it "mk (~b/b & ~a/b)/@ creates two sources" $
      mk empty' "(~b/b & ~a/b)/@"
        `shouldBe` Creates ["new1 -b-> 0", "new2 -a-> 0", "new2 -b-> 0"]
  describe "several edges to follow: mk creates nothing and asks" do
    let twoAs = graphOf [] [(0, "a", 1), (0, "a", 2)]
    it "mk a/b with two a edges lists both" $
      mk twoAs "a/b" `shouldBe` Asks [["1 -b-> new1"], ["2 -b-> new1"]]
    it "does nothing once one of them has b, and mk a/b/c lists two" do
      let g = graphOf [] [(0, "a", 1), (0, "a", 2), (1, "b", 3)]
      mk g "a/b" `shouldBe` Resolves
      mk g "a/b/c"
        `shouldBe` Asks [["3 -c-> new1"], ["2 -b-> new1", "new1 -c-> new2"]]
    it "two a steps with different tests could follow each other's edge: mk a/!{@1}/b & a/c is refused" do
      let g = graphOf [] [(0, "a", 1)]
      mk g "a/!{@1}/b & a/c" `shouldBe` Refuses "CircularSuppliers"
      mk g "a/!{@1} & a/b" `shouldBe` Refuses "CircularSuppliers"
      -- what it asked between is written with the node of the second a
      mk g "a/!{@1}/b & a/@1/c"
        `shouldBe` Creates ["0 -a-> new1", "1 -c-> new3", "new1 -b-> new2"]
      mk g "a/!{@1}/(b & c)"
        `shouldBe` Creates ["0 -a-> new1", "new1 -b-> new2", "new1 -c-> new3"]
    it "mk a/@2/x & a/@3/y & a/z puts z under 2 or under 3" $
      mk (graphOf [2, 3] []) "a/@2/x & a/@3/y & a/z"
        `shouldMatchPossibilities` [ ["0 -a-> 2", "0 -a-> 3", "2 -x-> new1", "2 -z-> new3", "3 -y-> new2"],
                                     ["0 -a-> 2", "0 -a-> 3", "2 -x-> new1", "3 -y-> new2", "3 -z-> new3"]
                                   ]
    it "mk (a | a/@2)/(a & b) lists two" $
      possibilities (mk (graphOf [2] []) "(a | a/@2)/(a & b)") `shouldBe` 2
    it "mk a/b & c/~c with 0 -a-> 0 and 0 -a-> 4 lists two" $
      possibilities (mk (graphOf [] [(0, "a", 0), (0, "a", 4)]) "a/b & c/~c")
        `shouldBe` 2
    it "mk (a & ~a/a)/@ with 0 -a-> 2" $
      mk (graphOf [] [(0, "a", 2)]) "(a & ~a/a)/@"
        `shouldMatchPossibilities` [ ["new1 -a-> 0", "new1 -a-> 2"],
                                     ["0 -a-> 0", "new1 -a-> 0"]
                                   ]
    it "each choice is walked on its own: mk a/b & a/b/c lists two of four walks" $
      possibilities (mk twoAs "a/b & a/b/c") `shouldBe` 2
  describe "joins" do
    it "a join takes a node one of its steps could follow an edge to" $
      mk (graphOf [] [(0, "a", 1)]) "(a & c)/@" `shouldBe` Creates ["0 -c-> 1"]
    it "and asks when they could follow edges to different nodes" $
      mk (graphOf [] [(0, "a", 1), (0, "c", 5)]) "(a & c)/@"
        `shouldMatchPossibilities` [["0 -c-> 1"], ["0 -a-> 5"]]
    it "with none it creates one node with an edge for each step" $
      mk empty' "(a & c)/@" `shouldBe` Creates ["0 -a-> new1", "0 -c-> new1"]
    it "a join and a step that could follow its edge: mk (a & b)/@ & a is refused" do
      let g = graphOf [] [(0, "b", 0)]
      mk g "(a & b)/@ & a" `shouldBe` Refuses "CircularSuppliers"
      mk g "(a & b)/@" `shouldBe` Creates ["0 -a-> 0"]
  describe "which possibilities are listed" do
    it "not one that creates everything another creates and more" $
      -- of the four walks, the two that also create a b under node 2
      mk (graphOf [] [(0, "a", 1), (0, "a", 2), (1, "b", 3)]) "a/b & a/c"
        `shouldMatchPossibilities` [["1 -c-> new1"], ["2 -c-> new1"]]
    it "equal ones count once: mk a + a/@" $
      mk empty' "a ++ a/@" `shouldBe` Creates ["0 -a-> new1"]
    it "one that creates nothing means the path resolves" $
      mk (graphOf [] [(0, "a", 1), (0, "a", 2), (1, "b", 3)]) "a/b ++ zz"
        `shouldBe` Resolves
    it "independent ends may already coincide" $
      mk (graphOf [] [(0, "a", 2), (0, "b", 2)]) "a & b" `shouldBe` Resolves
  describe "no node with a written id is created" do
    it "mk a/@7 without node 7 is refused, and says which node" $
      mk empty' "a/@7" `shouldBe` NeedsNodes [7]
    it "every written node that does not exist is named: mk a/@7/b/@8 & c/@2" $
      mk (graphOf [2] []) "a/@7/b/@8 & c/@2" `shouldBe` NeedsNodes [7, 8]
    it "also a node the path starts at: mk @7/x" $
      mk empty' "@7/x" `shouldBe` NeedsNodes [7]
    it "a node that is only excluded is no obstacle: mk a/!{@7}" $
      mk empty' "a/!{@7}" `shouldBe` Creates ["0 -a-> new1"]
    it "a side of a union that needs one is no possibility; the others are" do
      mk empty' "a/@7 ++ b" `shouldBe` Creates ["0 -b-> new1"]
      mk empty' "(a/@7 | b)" `shouldBe` Creates ["0 -b-> new1"]
      mk empty' "(a | a/@2)/(a & b)"
        `shouldBe` Creates ["0 -a-> new1", "new1 -a-> new2", "new1 -b-> new3"]
      mk empty' "a/@7 ++ b/@8" `shouldBe` NeedsNodes [7, 8]
    it "a path that resolves does nothing, whatever another side writes" $
      mk (graphOf [] [(0, "b", 1)]) "a/@7 ++ b" `shouldBe` Resolves
    it "nor is the node it stands on created" $
      does (residuatePure (const Nothing) (const Nothing) (smallNID 0) (compilePath (typed "a")))
        `shouldBe` NeedsNodes [0]
  describe "the routes are walked at the same pace" do
    -- a step follows an edge this command creates only if the step that
    -- creates it is no further from the start of its route
    it "mk a & b/~b/a/@2 creates all three: the first a is one step along, the edge is created three along" $
      mk (graphOf [2] []) "a & b/~b/a/@2"
        `shouldBe` Creates ["0 -a-> 2", "0 -a-> new1", "0 -b-> new2"]
    it "the other way round the step follows: mk x/~x/a & a/@2" $
      mk (graphOf [2] []) "x/~x/a & a/@2" `shouldBe` Creates ["0 -a-> 2", "0 -x-> new1"]
    it "whichever route is written first: mk a/@2 & x/~x/a" $
      mk (graphOf [2] []) "a/@2 & x/~x/a" `shouldBe` Creates ["0 -a-> 2", "0 -x-> new1"]
    it "the distance is counted along the route, also past a written node" do
      mk (graphOf [2] []) "x/@0/a & a/@2" `shouldBe` Creates ["0 -a-> 2", "0 -x-> 0"]
      mk (graphOf [2] []) "a & x/@0/a/@2"
        `shouldBe` Creates ["0 -a-> 2", "0 -a-> new1", "0 -x-> 0"]
    it "mk a/(b & ~a/a/@2) creates a node for a and puts b under it" $
      mk (graphOf [2] []) "a/(b & ~a/a/@2)"
        `shouldBe` Creates ["0 -a-> 2", "0 -a-> new1", "new1 -b-> new2"]
    it "a join is as far as the furthest of its steps: mk (x/y & a)/@ & a/@2" do
      mk (graphOf [2] []) "(x/y & a)/@ & a/@2"
        `shouldBe` Creates ["0 -a-> 2", "0 -x-> new1", "new1 -y-> 2"]
      mk (graphOf [2] []) "a/@2 & (x/y & a)/@"
        `shouldBe` Creates ["0 -a-> 2", "0 -x-> new1", "new1 -y-> 2"]
    it "the join's w and a lone w could follow each other's edge: mk (x/y/z & w)/@ & w is refused" do
      mk empty' "(x/y/z & w)/@ & w" `shouldBe` Refuses "CircularSuppliers"
      mk empty' "(x/y/z & w)/@"
        `shouldBe` Creates ["0 -w-> new3", "0 -x-> new1", "new1 -y-> new2", "new2 -z-> new3"]
    it "not an edge created further along the same route: mk a/~a/a/@2" $
      mk (graphOf [2] []) "a/~a/a/@2"
        `shouldBe` Creates ["0 -a-> 2", "0 -a-> new1"]
    it "nor one into a node that is placed from where the step goes: mk (a/x & a)/!{@5}" $
      -- the second a creates 0 -a-> new1 into the join, which is placed from
      -- where the first a goes, so it comes after the first a, which goes
      -- along 0 -a-> 5
      mk (graphOf [] [(0, "a", 5)]) "(a/x & a)/!{@5}"
        `shouldBe` Creates ["0 -a-> new1", "5 -x-> new1"]
    it "a step back does not follow an edge created further along: mk a/@2 & (a & b)/!{@0} & ~b & c/@0/~b/@3" $
      -- the lone ~b is one step along; the only b edge into 0 is 3 -b-> 0,
      -- which the ~b of the last route creates three steps along, so the
      -- lone ~b creates an edge of its own
      mk (graphOf [2, 3] []) "a/@2 & (a & b)/!{@0} & ~b & c/@0/~b/@3"
        `shouldBe` Creates ["0 -a-> 2", "0 -b-> 2", "0 -c-> 0", "3 -b-> 0", "new1 -b-> 0"]
    it "on a path of three long routes over a graph with loops" $
      -- found among random paths: the a of the first route is three steps
      -- along, and 2 -a-> 0 is created four along the second route, so
      -- that a does not follow it; it follows 2 -a-> new2, which the third
      -- route creates three along, and the ~c after it creates an edge
      mk
        (graphOf [1, 2] [(0, "a", 2), (1, "a", 1), (2, "b", 2), (2, "c", 2)])
        "b/c/@2/a/~c & ~c/@2/~a/@0/c/@2/a/@0 & b/~c/~a/@2/~c/@2"
        `shouldBe` Creates
          [ "0 -b-> new1",
            "0 -c-> 2",
            "2 -a-> 0",
            "2 -a-> new2",
            "2 -c-> 0",
            "new1 -c-> 2",
            "new2 -c-> new1",
            "new3 -c-> new2"
          ]
    it "an edge another step finds does not make that step one of its creators" $
      -- l/@2 at the end of the first route creates 0 -l-> 2, three steps
      -- along. The l/@2 of the second route, two along, comes later (after
      -- the *, which comes after the p steps) and finds that edge there, so
      -- it creates nothing and is not one of its creators. So the l of the
      -- last route, two along, does not follow that edge: it creates one
      mk (graphOf [2, 3] [(0, "q", 0)]) "q/@0/q/@0/l/@2 & */@0/l/@2 & p/@3 & p & q/@0/l"
        `shouldBe` Creates ["0 -l-> 2", "0 -l-> new1", "0 -p-> 3"]
    it "steps that could follow each other's edges in a circle are refused" do
      -- the c of the first route could follow the join's 0 -c-> 1, and the
      -- join could follow that c's 0 -c-> new1
      let g = graphOf [1, 2] []
      mk g "c/!{@2}/a & (a & c)/@ & a/@1 & c/@2" `shouldBe` Refuses "CircularSuppliers"
      -- each of the four ways it could be meant is written with its nodes
      mk g "c/@1/a & a/@1 & c/@2"
        `shouldBe` Creates ["0 -a-> 1", "0 -c-> 1", "0 -c-> 2", "1 -a-> new1"]
      mk g "(a & c)/@2 & c/!{@2}/a & a/@1"
        `shouldBe` Creates ["0 -a-> 1", "0 -a-> 2", "0 -c-> 2", "0 -c-> new1", "new1 -a-> new2"]
      mk g "(a & c)/!{@1,@2}/a & a/@1 & c/@2"
        `shouldBe` Creates ["0 -a-> 1", "0 -a-> new1", "0 -c-> 2", "0 -c-> new1", "new1 -a-> new2"]
      mk g "a/@1 & c/@1 & c/@2 & c/!{@1,@2}/a"
        `shouldBe` Creates ["0 -a-> 1", "0 -c-> 1", "0 -c-> 2", "0 -c-> new1", "new1 -a-> new2"]
    it "whatever the graph holds, and with a step beside them" do
      mk (graphOf [2, 3, 4] [(0, "b", 0), (0, "c", 2), (4, "a", 0)]) "(c & a)/@ & a/~a/c/@3 & (c & b)/@ & (a & b)/@"
        `shouldBe` Refuses "CircularSuppliers"
      mk (graphOf [1, 2] []) "c/!{@2}/a & (a & c)/@ & a/@1 & c/@2 & */!{@1, @2}/z"
        `shouldBe` Refuses "CircularSuppliers"
  describe "* and regex steps are steps like any other" do
    it "mk * & a creates only 0 -a-> new1" $
      mk empty' "* & a" `shouldBe` Creates ["0 -a-> new1"]
    it "mk a/~* creates only a" $
      mk empty' "a/~*" `shouldBe` Creates ["0 -a-> new1"]
    it "mk a & b & */c asks: c under the new a node or the new b node" $
      mk empty' "a & b & */c"
        `shouldMatchPossibilities` [ ["0 -a-> new1", "0 -b-> new2", "new1 -c-> new3"],
                                     ["0 -a-> new1", "0 -b-> new2", "new2 -c-> new3"]
                                   ]
    it "mk c & */b with 0 -a-> 1 asks" $
      mk (graphOf [] [(0, "a", 1)]) "c & */b"
        `shouldMatchPossibilities` [ ["0 -c-> new1", "1 -b-> new2"],
                                     ["0 -c-> new1", "new1 -b-> new2"]
                                   ]
    it "mk */b & c/~c with 0 -a-> 2 asks" $
      possibilities (mk (graphOf [] [(0, "a", 2)]) "*/b & c/~c") `shouldBe` 2
    it "a regex step walks back along a created edge: mk a/~regex with b given" $
      mkWith (const (Just "b")) empty' "a/~regex:\"^[ab]$\""
        `shouldBe` Creates ["0 -a-> new1"]
    it "mk a/c/@ & */c/@ with b given creates only the a route" $
      mkWith (const (Just "b")) empty' "a/c/@ & */c/@"
        `shouldBe` Creates ["0 -a-> new1", "new1 -c-> new2"]
    it "a step that has to create one needs a label: mk *" $
      mk empty' "*" `shouldBe` NeedsLabel [["0 -*-> new1"]]
    it "also between two written nodes: mk */@2/~b/@0 does not go along the b edge created after it" do
      mk (graphOf [2] []) "*/@2/~b/@0" `shouldBe` NeedsLabel [["0 -b-> 2", "0 -*-> 2"]]
      mk (graphOf [2] []) "*/@2/x/~x/~b/@0"
        `shouldBe` NeedsLabel [["0 -b-> 2", "0 -*-> 2", "2 -x-> new1"]]
    it "but along one created at its own distance, whichever is written first" do
      mk (graphOf [2] []) "*/@2 & a/@2" `shouldBe` Creates ["0 -a-> 2"]
      mk (graphOf [2] []) "a/@2 & */@2" `shouldBe` Creates ["0 -a-> 2"]
    it "a * is not one of several steps into an unnamed node: mk (~* & b)/@ is refused" do
      let g = graphOf [7] [(0, "b", 7)]
      mk g "(~* & b)/@ & y/@7/c/@0" `shouldBe` Refuses "WildcardJoin"
      mk g "(~* & b)/@ & y/@7/c/@0 & (p/q/r & ~w)/@" `shouldBe` Refuses "WildcardJoin"
      -- with the node written, the ~* does not go along 7 -c-> 0, which is
      -- created two steps along
      mk g "(~* & b)/@7 & y/@7/c/@0"
        `shouldBe` NeedsLabel [["0 -y-> 7", "7 -c-> 0", "7 -*-> 0"]]
    it "mk a/*/@5 with 0 -a-> 2 and 6 -x-> 5 needs a label for 2 -*-> 5" $
      mk (graphOf [] [(0, "a", 2), (6, "x", 5)]) "a/*/@5"
        `shouldBe` NeedsLabel [["2 -*-> 5"]]
  describe "labels" do
    let noLabel g surface =
          residuatePure (nodeIn g) (const Nothing) corpusCurrent (compilePath surface)
        repairLabels = \case
          Repairs rs -> map (map (.transition) . toList . (.patch.edges)) (toList rs)
          other -> error ("expected possibilities: " <> show other)
    it "the label error names the step that needs a label" $
      noLabel (graphWithEdges []) [path| a & b/* |] `shouldNeedLabel` (MWild, 1, 1)
    it "a wildcard edge needs a label from the chooser" $
      noLabel (graphWithEdges []) [path| a/* |] `shouldNeedLabel` (MWild, 0, 1)
    it "an unrelated existing label is not reused" $
      noLabel (graphWithEdges [edge' 3 "foo" 4]) [path| a/* |]
        `shouldNeedLabel` (MWild, 0, 1)
    it "an existing wildcard edge satisfies the pattern" $
      noLabel (graphWithEdges [edge' 1 "a" 2, edge' 2 "foo" 3]) [path| a/* |]
        `shouldBe` Right AlreadyResolves
    it "an alternative lacking a label is an error, not dropped: mk a + *" $
      noLabel (graphWithEdges []) [path| a ++ * |] `shouldNeedLabel` (MWild, 0, 0)
    it "whichever alternative comes first: mk * + a" $
      noLabel (graphWithEdges []) [path| * ++ a |] `shouldNeedLabel` (MWild, 0, 0)
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
    it "but a refused path is refused also when it resolves: mk b + */*" $
      does (noLabel (graphWithEdges [edge' 1 "x" 2, edge' 2 "y" 3]) [path| b ++ */* |])
        `shouldBe` Refuses "SeveralWildcards"
    it "a label-needing superset of another possibility is not needed: mk a + a/*" $
      fmap repairLabels (noLabel (graphWithEdges []) [path| a ++ a/* |])
        `shouldBe` Right [["a"]]
    it "an existing edge for * avoids the label: mk * & a" $
      fmap repairLabels (noLabel (graphWithEdges [edge' 1 "x" 2]) [path| * & a |])
        `shouldBe` Right [["a"]]
    it "the error lists every possibility: mk c/d + c/e + *" $ do
      let result = noLabel (graphWithEdges []) [path| c/d ++ c/e ++ * |]
      result `shouldNeedLabel` (MWild, 0, 0)
      labelErrorRepairs result `shouldBe` Just 3
    it "a plain wildcard lists only its own possibility: mk a/*" $
      labelErrorRepairs (noLabel (graphWithEdges []) [path| a/* |])
        `shouldBe` Just 1
    it "an equally good label-free possibility is not taken: mk a/* + */b" $ do
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
                  (nodeIn (graphWithEdges []))
                  chooser
                  corpusCurrent
                  (compilePath [path| a ++ * |])
              )
      withChooser (const (Just "a")) `shouldBe` Right [["a"]]
      withChooser (const (Just "x")) `shouldBe` Right [["a"], ["x"]]
    it "a chooser's label is used" $
      fmap
        (\case Repairs (r :| _) -> map (.transition) (toList r.patch.edges); _ -> [])
        (residuatePure (nodeIn (graphWithEdges [])) (const (Just "x")) corpusCurrent (compilePath [path| * |]))
        `shouldBe` Right ["x"]
  describe "staging: mk p and then mk p/q" do
    it "leaves what mk p/q alone leaves: mk a then mk a/b/~b/c" do
      let g = graphOf [] [(0, "x", 5)]
      mk (having g "a") "a/b/~b/c"
        `shouldBe` Creates ["1000 -b-> new1", "1000 -c-> new2"]
      shortPatch id (staged g "a" "a/b/~b/c")
        `shouldBe` ["0 -a-> new1", "new1 -b-> new2", "new1 -c-> new3"]
      mk g "a/b/~b/c"
        `shouldBe` Creates ["0 -a-> new1", "new1 -b-> new2", "new1 -c-> new3"]
    it "but not cut just before @N: mk a then mk a/@2/(a & b) leaves an extra edge" do
      let g = graphOf [2] []
      mk g "a/@2/(a & b)"
        `shouldBe` Creates ["0 -a-> 2", "2 -a-> new1", "2 -b-> new2"]
      shortPatch id (staged g "a" "a/@2/(a & b)")
        `shouldBe` ["0 -a-> 2", "0 -a-> new1", "2 -a-> new2", "2 -b-> new3"]
    it "nor when the first command adds an edge beside one that a step of its own follows" do
      -- with 0 -a-> 0, mk a/a/@3 creates 0 -a-> 3; then the first a has two
      let g = graphOf [3] [(0, "a", 0)]
      mk g "a/a/@3/a" `shouldBe` Creates ["0 -a-> 3", "3 -a-> new1"]
      mk g "a/a/@3" `shouldBe` Creates ["0 -a-> 3"]
      mk (having g "a/a/@3") "a/a/@3/a"
        `shouldMatchPossibilities` [["3 -a-> 3"], ["3 -a-> new1"]]
    it "at the same pace the rest of the path creates no edge that a step of the prefix follows" do
      -- an exception when a created edge counted whenever it was created
      let g = graphOf [3] []
      mk g "(b & a)/~b/a/@3"
        `shouldBe` Creates ["0 -a-> 3", "0 -a-> new2", "0 -b-> new1", "new3 -a-> 3", "new3 -b-> new2"]
      samePatch (staged g "b & a" "(b & a)/~b/a/@3") (patchOf g "(b & a)/~b/a/@3")
        `shouldBe` True
  it "a variable without a bound vertex is an internal error" $
    evaluate
      ( residuatePure
          (const Nothing)
          (const Nothing)
          corpusCurrent
          (query [pat [route (var 0) [out 0 "a" (var 1)]]])
      )
      `shouldThrow` anyErrorCall

-- | The examples the design of @mk@ was decided on: every row of the
-- table of the designer's report. A name, the nodes besides node 0, the
-- edges, the path as typed, and what @mk@ does with it at node 0: for a
-- path whose shape is refused, the code of the refusal; for every other
-- path what the report gives, up to the names of the new nodes.
designExamples :: [(Text, [Int], [(Int, Text, Int)], Text, Does)]
designExamples =
  [ ("D1", [2], [], "a & a/@2", Creates ["0 -a-> 2"]),
    ("D2", [], [], "a/~a", Creates ["0 -a-> new1"]),
    ("D3", [], [], "a/a", Creates ["0 -a-> new1", "new1 -a-> new2"]),
    ("D4", [2], [], "a/b/@2 & c/b/@2", Creates ["0 -a-> new1", "0 -c-> new2", "new1 -b-> 2", "new2 -b-> 2"]),
    ("D5", [2], [], "(a & c)/@/b/@2", Creates ["0 -a-> new1", "0 -c-> new1", "new1 -b-> 2"]),
    ("D6", [1, 2], [(1, "b", 2)], "a/b/@2", Creates ["0 -a-> new1", "new1 -b-> 2"]),
    ("D7", [1, 2], [(1, "b", 2)], "a/@1/b/@2", Creates ["0 -a-> 1"]),
    ("D8", [2], [], "a & b/~b/a/@2", Creates ["0 -a-> 2", "0 -a-> new1", "0 -b-> new2"]),
    ("D9", [], [], "* & a", Creates ["0 -a-> new1"]),
    ("D10", [], [], "a & b & */c", Asks [["0 -a-> new1", "0 -b-> new2", "new1 -c-> new3"], ["0 -a-> new1", "0 -b-> new2", "new2 -c-> new3"]]),
    ("D11", [], [], "*", NeedsLabel [["0 -*-> new1"]]),
    ("D12", [], [], "b/@7", NeedsNodes [7]),
    ("D13", [1, 2], [(0, "a", 1), (0, "a", 2)], "a/b", Asks [["1 -b-> new1"], ["2 -b-> new1"]]),
    ("D14", [2], [], "a/~a/a/@2", Creates ["0 -a-> 2", "0 -a-> new1"]),
    ("H1", [], [], "(c & a/c)/@ & (~c & a)/@", Refuses "CircularSuppliers"),
    ("H2", [4, 5], [(0, "b", 4), (4, "c", 5)], "(a & b/c)/@ & a", Refuses "CircularSuppliers"),
    ("H3", [5], [(0, "b", 5)], "(a & b)/@ & a", Refuses "CircularSuppliers"),
    ("H4", [4, 5, 7], [(0, "b", 4), (4, "c", 5), (0, "x", 7)], "(a & b/c)/@ & a & (x & x/x)/@ & x/~x", Refuses "CircularSuppliers"),
    ("H4b", [4, 5], [(0, "b", 4), (4, "c", 5)], "(a & b/c)/@ & a & (x & x/x)/@ & x/~x", Refuses "CircularSuppliers"),
    ("H5", [4, 5, 7], [(0, "b", 4), (4, "c", 5)], "(a & b/c)/@ & a & x/@7 & */@7", Refuses "CircularSuppliers"),
    ("H6", [4, 5, 7], [(0, "b", 4), (4, "c", 5)], "(a & b/c)/@ & a & */@7 & x/@7", Refuses "CircularSuppliers"),
    ("H7", [1, 2], [], "c/!{@2}/a & (a & c)/@ & a/@1 & c/@2", Refuses "CircularSuppliers"),
    ("H8", [2, 3], [], "a/~a/a/@2 & a/~a/a/@3", Creates ["0 -a-> 2", "0 -a-> 3", "0 -a-> new1"]),
    ("H9", [], [], "(* & a)/@", Refuses "WildcardJoin"),
    ("H10", [], [], "a/@7 + b", Creates ["0 -b-> new1"]),
    ("H11", [5, 7], [(0, "x", 5), (5, "y", 7)], "(x/y & a)/@ & a", Refuses "CircularSuppliers"),
    ("E1", [2], [], "a/b & a/@2/c", Creates ["0 -a-> 2", "2 -b-> new1", "2 -c-> new2"]),
    ("E2", [], [], "a/~a/b", Creates ["0 -a-> new1", "0 -b-> new2"]),
    ("E3", [2], [], "(a/b & b)/@2", Creates ["0 -a-> new1", "0 -b-> 2", "new1 -b-> 2"]),
    ("E4", [1], [(0, "a", 1)], "a/!{@1}/b & a/c", Refuses "CircularSuppliers"),
    ("E5", [2, 3], [], "a/@2/x & a/@3/y & a/z", Asks [["0 -a-> 2", "0 -a-> 3", "2 -x-> new1", "2 -z-> new3", "3 -y-> new2"], ["0 -a-> 2", "0 -a-> 3", "2 -x-> new1", "3 -y-> new2", "3 -z-> new3"]]),
    ("E6", [], [], "a/~a + (zz & a/~a)", Creates ["0 -a-> new1"]),
    ("E7", [], [], "(zz & a/~a) + a/~a", Creates ["0 -a-> new1"]),
    ("E8", [3], [(0, "x", 3)], "(a & a/@3)/~x/(a + b)", Creates ["0 -a-> 3"]),
    ("E9", [2], [], "(a ++ b) & (b/c & b/@2/c)", Creates ["0 -b-> 2", "2 -c-> new1"]),
    ("E10", [1], [(0, "a", 1)], "(a & c)/@", Creates ["0 -c-> 1"]),
    ("E11", [1, 5], [(0, "a", 1), (0, "c", 5)], "(a & c)/@", Asks [["0 -c-> 1"], ["0 -a-> 5"]]),
    ("E12", [1, 2, 3], [(0, "a", 1), (0, "a", 2), (1, "b", 3)], "a/b & a/c", Asks [["1 -c-> new1"], ["2 -c-> new1"]]),
    ("E13", [2], [], "*/@2/~b/@0", NeedsLabel [["0 -b-> 2", "0 -*-> 2"]]),
    ("E14", [2], [], "*/@2 & a/@2", Creates ["0 -a-> 2"]),
    ("X1", [], [], "a & a", Creates ["0 -a-> new1"]),
    ("X2", [], [], "a/!{@0} & b/!{@0}", Creates ["0 -a-> new1", "0 -b-> new2"]),
    ("X3", [], [], "regex:\"^a$\" & a", Creates ["0 -a-> new1"]),
    ("X4", [], [], "regex:\"^z$\" & a", NeedsLabel [["0 -a-> new2", "0 -regex:\"^z$\"-> new1"]]),
    ("X5", [], [], "(a | b)/(c & d)", Asks [["0 -a-> new1", "new1 -c-> new2", "new1 -d-> new3"], ["0 -b-> new1", "new1 -c-> new2", "new1 -d-> new3"], ["0 -a-> new1", "0 -b-> new3", "new1 -c-> new2", "new3 -d-> new4"], ["0 -a-> new3", "0 -b-> new1", "new1 -c-> new2", "new3 -d-> new4"]]),
    ("X6", [], [], "(a ++ b)/(c & d)", Asks [["0 -a-> new1", "new1 -c-> new2", "new1 -d-> new3"], ["0 -b-> new1", "new1 -c-> new2", "new1 -d-> new3"]]),
    ("X7", [], [], "(x/@ | y)/(c & d)", Asks [["0 -x-> new1", "new1 -c-> new2", "new1 -d-> new3"], ["0 -y-> new1", "new1 -c-> new2", "new1 -d-> new3"], ["0 -x-> new1", "0 -y-> new3", "new1 -c-> new2", "new3 -d-> new4"], ["0 -x-> new3", "0 -y-> new1", "new1 -c-> new2", "new3 -d-> new4"]]),
    ("X8", [], [], "(a/@ | a/@)/(b & c)", Creates ["0 -a-> new1", "new1 -b-> new2", "new1 -c-> new3"]),
    ("X9", [], [], "(a & b)/@ & (b & c)/@ & (c & a)/@", Refuses "CircularSuppliers"),
    ("N1", [], [], "b & (* & a)/@", Refuses "WildcardJoin"),
    ("N2", [1], [(0, "a", 1)], "c & */b", Asks [["0 -c-> new1", "1 -b-> new2"], ["0 -c-> new1", "new1 -b-> new2"]]),
    ("N3", [], [], "(c & ~a/*)/@ & c", Refuses "WildcardJoin"),
    ("N4", [], [], "(c & */b)/@ & c", Refuses "CircularSuppliers"),
    ("N5", [1, 2], [(0, "b", 1), (1, "c", 0), (2, "c", 0)], "(~c & b & a)/@", Asks [["0 -a-> 1"], ["0 -a-> 2", "0 -b-> 2"]]),
    ("N6", [1], [], "a", Creates ["0 -a-> new1"]),
    ("N7", [1, 2], [(0, "b", 2), (1, "c", 2)], "*/c", Creates ["2 -c-> new1"]),
    ("N8", [1, 2, 3], [(0, "a", 1), (0, "a", 2), (1, "b", 3)], "a/b/c", Asks [["3 -c-> new1"], ["2 -b-> new1", "new1 -c-> new2"]]),
    ("N9", [2], [], "a & x/@0/a/@2", Creates ["0 -a-> 2", "0 -a-> new1", "0 -x-> 0"]),
    ("N10", [2], [], "a/b & a/@2", Creates ["0 -a-> 2", "2 -b-> new1"]),
    ("N11", [1, 2, 3], [(0, "a", 1), (0, "a", 2), (1, "b", 3)], "a/b", Resolves),
    ("N12", [1, 2], [], "a/@1 | ~*", NeedsLabel [["0 -a-> 1"], ["new1 -*-> 0"]]),
    ("N13", [2], [], "*/@2 & (a & *)/@", Refuses "WildcardJoin"),
    ("N14", [], [], "regex:\"^a$\" & (* & a)/@", Refuses "WildcardJoin"),
    ("N15", [], [], "(* & a)/@ & (b & regex:\"^a$\")/@", Refuses "WildcardJoin"),
    ("F1", [], [], "a & (b & a & b/~a)/@", Refuses "CircularSuppliers"),
    ("F2", [1, 2, 3], [(0, "a", 1), (0, "e", 1), (1, "f", 2), (2, "g", 3)], "(a/(b & f)/@/g & a & e/b/r)/@", Refuses "CircularSuppliers"),
    ("F3", [1, 2], [(0, "a", 1)], "a/@2 & a/b", Asks [["0 -a-> 2", "1 -b-> new1"], ["0 -a-> 2", "2 -b-> new1"]]),
    ("F4", [1], [(0, "c", 0)], "@0/b & */~b/@1", Asks [["0 -b-> new1", "1 -b-> new1"], ["0 -b-> new1", "1 -b-> 0"]]),
    ("F5", [4], [(0, "b", 4), (4, "a", 4)], "(a & b/*)/@ & a", Refuses "WildcardJoin"),
    ("T1", [1], [], "a/@1 & b/@1 & *", Creates ["0 -a-> 1", "0 -b-> 1"]),
    ("T2", [1], [], "b/@1 & a/@1 & *", Creates ["0 -a-> 1", "0 -b-> 1"]),
    ("T3", [], [], "a & b & (a & b)/@", Refuses "CircularSuppliers"),
    ("T4", [], [], "a & b & (b & a)/@", Refuses "CircularSuppliers"),
    ("T5", [1], [(0, "a", 1)], "a/!{@1} & a/b", Refuses "CircularSuppliers"),
    ("Y1", [], [], "a/@/(@ & ~a/a)/@", Refuses "ReturnsToMeetingPoint"),
    ("Y2", [], [], "a/@/(@ & b/~*)/@", Refuses "ReturnsToMeetingPoint"),
    ("Y3", [], [], "a/@/(@ & a)/@", Refuses "ReturnsToMeetingPoint"),
    ("Y4", [], [(0, "a", 0)], "a/@/(@ & a)/@", Refuses "ReturnsToMeetingPoint"),
    ("Y5", [1], [], "a/b/@1 & a/@1", Creates ["0 -a-> 1", "1 -b-> 1"]),
    ("Y6", [], [], "*/@0 & (a & *)/@", Refuses "WildcardJoin"),
    ("Y7", [], [], "a & b & (* & a)/@", Refuses "WildcardJoin"),
    ("Y8", [], [], "a/@0 | c/!{@0}", Asks [["0 -a-> 0"], ["0 -c-> new1"]]),
    ("Y9", [], [], "(a/@0 | c/!{@0}) & d/!{@0}", Asks [["0 -a-> 0", "0 -d-> new1"], ["0 -c-> new1", "0 -d-> new2"]]),
    ("Z1", [], [(0, "x", 0)], "x/b & a/~b/@0", Creates ["0 -a-> new1", "0 -b-> new1"]),
    ("Z2", [], [(0, "x", 0)], "x/~b & a/b/@0", Creates ["0 -a-> new1", "new1 -b-> 0"]),
    ("Z3", [], [(0, "a", 0)], "b/@0 & a/~b/@0 & ~b/c", Creates ["0 -b-> 0", "0 -c-> new1"]),
    ("Z4", [1], [(0, "c", 0)], "b & ~c/~b/@0/b/a/@1", Asks [["0 -a-> 1", "0 -b-> 0", "0 -b-> new1"], ["0 -b-> 0", "0 -b-> new1", "new1 -a-> 1"]]),
    ("Z5", [], [(0, "b", 0)], "~a & ~*/a/@0", Creates ["new1 -a-> 0"]),
    ("P1", [], [], "a/b & a/c", Creates ["0 -a-> new1", "new1 -b-> new2", "new1 -c-> new3"]),
    ("P2", [2], [], "x/~x/a & a/@2", Creates ["0 -a-> 2", "0 -x-> new1"]),
    ("P3", [2], [], "(x/y & a)/@ & a/@2", Creates ["0 -a-> 2", "0 -x-> new1", "new1 -y-> 2"]),
    ("P4", [5], [(0, "a", 5)], "(a/x & a)/!{@5}", Creates ["0 -a-> new1", "5 -x-> new1"]),
    ("P5", [], [], "(a & b)/@ & (a & c)/@", Refuses "CircularSuppliers"),
    ("P6", [5, 7], [(0, "b", 5)], "(a & b)/@ & a & x/@7", Refuses "CircularSuppliers"),
    ("P7", [], [], "a/@0/b/@0 | c/!{@0}", Asks [["0 -a-> 0", "0 -b-> 0"], ["0 -c-> new1"]]),
    ("P8", [], [], "(a/@0/b/@0 | c/!{@0}) & d/!{@0}", Asks [["0 -a-> 0", "0 -b-> 0", "0 -d-> new1"], ["0 -c-> new1", "0 -d-> new2"]]),
    ("P9", [1, 2], [(0, "a", 1), (1, "a", 2)], "a/@/(@ & a)/@", Refuses "ReturnsToMeetingPoint"),
    ("P10", [1, 2, 3, 4], [(0, "a", 1), (0, "b", 2), (0, "c", 3), (0, "d", 4)], "(a & b)/@ & (c & d)/@", Asks [["0 -b-> 1", "0 -d-> 3"], ["0 -b-> 1", "0 -c-> 4"], ["0 -a-> 2", "0 -d-> 3"], ["0 -a-> 2", "0 -c-> 4"]])
  ]

spec_designExamples :: Spec
spec_designExamples = describe "the examples the design was decided on" do
  let refused = \case
        Refuses _ -> True
        _ -> False
      example (name, nodes, edges, p, expected) =
        it (unpack (name <> ": mk " <> p)) $
          mk (graphOf nodes edges) p `shouldBe` expected
  describe "built" $
    for_ [e | e@(_, _, _, _, expected) <- designExamples, not (refused expected)] example
  describe "refused for their shape" $
    for_ [e | e@(_, _, _, _, expected) <- designExamples, refused expected] example
  it "a refused path is refused whatever the graph holds" $
    for_ [e | e@(_, _, _, _, expected) <- designExamples, refused expected] \(_, _, _, p, expected) ->
      for_ corpusGraphs \g ->
        does (residuatePure (nodeIn g) (const Nothing) corpusCurrent (compilePath (typed p)))
          `shouldBe` expected
  it "there are 65 built and 33 refused" $
    (length (filter (\(_, _, _, _, e) -> not (refused e)) designExamples), length designExamples)
      `shouldBe` (65, 98)

-- | The possibilities listed, in any order.
shouldMatchPossibilities :: Does -> [[Text]] -> Expectation
shouldMatchPossibilities result expected = case result of
  Asks listed -> listed `shouldMatchList` expected
  other -> expectationFailure ("expected to be asked: " <> show other)

infix 1 `shouldMatchPossibilities`

-- | How many possibilities are listed.
possibilities :: Does -> Int
possibilities = \case
  Asks listed -> length listed
  _ -> 0

-- * Selection against its reference

-- | Every walk of a path in a graph, before selection.
candidatesIn ::
  (Matcher -> Maybe Text) -> Graph Text () -> PathQuery -> [Candidate]
candidatesIn chooser g q =
  fromMaybe (error "candidatesIn: no limit") $
    candidatesUpTo Nothing chooser g q

-- | The walks, unless there are more than the limit.
candidatesUpTo ::
  Maybe Int ->
  (Matcher -> Maybe Text) ->
  Graph Text () ->
  PathQuery ->
  Maybe [Candidate]
candidatesUpTo limit chooser g q =
  either (const Nothing) Just . runIdentity $
    candidatesWithin
      limit
      (pure . nodeIn g)
      (pure . chooser)
      corpusCurrent
      q

-- | 'select' keeps what 'selectReference' keeps, in its order.
selectsAsReference :: [Candidate] -> Expectation
selectsAsReference cs =
  map (.residual.patch) (select cs)
    `shouldBe` map (.residual.patch) (selectReference cs)

-- | A path and a graph over nodes 1 to 4 (5 never exists) and labels a, b
-- and c: random paths, and shapes with steps back along created edges,
-- steps into a written node, missing labels, unions and steps with many
-- matches, alone or combined with a random path.
genRepairCase :: Gen (P.Path, Graph Text ())
genRepairCase = do
  surface <-
    frequency
      [ (4, genRepairPath =<< choose (1, 7)),
        (2, genShape),
        (1, (P.:&) <$> genShape <*> (genRepairPath =<< choose (1, 3))),
        (1, (P.:/) <$> genShape <*> (genRepairPath =<< choose (1, 3))),
        (1, (P.:/) <$> (genRepairPath =<< choose (1, 3)) <*> genShape)
      ]
  nodes <- sublistOf [1 .. 4 :: Int]
  edges <- flip vectorOf genEdge =<< frequency [(3, choose (0, 6)), (1, choose (7, 14))]
  -- mostly with the nodes the path writes: without them mk refuses
  written <- frequency [(5, pure (withWritten surface)), (1, pure id)]
  pure
    ( surface,
      written $
        insertEdges
          edges
          (foldl' (\g k -> insertNode (emptyNode (smallNID k)) g) emptyGraph nodes)
    )
  where
    genEdge = edge' <$> choose (1, 4) <*> genLabel <*> choose (1, 4)
    genLabel = elements ["a", "b", "c"]
    genNode = P.Absolute . smallNID <$> choose (1, 5)
    genStep =
      frequency
        [ (8, P.Literal <$> genLabel),
          (1, pure P.Wild),
          (1, pure (P.RegexMatch [re|^[ab]$|]))
        ]
    conj = foldl1Ex' (P.:&)
    genShape = do
      x <- genStep
      y <- genStep
      z <- genStep
      fixedNode <- genNode
      k <- choose (2, 4)
      steps <- vectorOf k genStep
      elements
        [ -- steps back along created edges
          x P.:/ P.Backwards x P.:& y P.:/ P.Backwards y,
          conj [s P.:/ P.Backwards s | s <- steps],
          conj steps P.:/ P.Backwards x P.:/ conj steps P.:/ P.Backwards y,
          conj [P.Backwards s P.:/ x | s <- steps],
          -- several routes into a written node
          x P.:/ z P.:/ fixedNode P.:& y P.:/ z P.:/ fixedNode,
          conj [s P.:/ z P.:/ fixedNode | s <- steps],
          -- a step beside one into a written node
          x P.:& x P.:/ fixedNode,
          x P.:/ y P.:& x P.:/ fixedNode P.:/ z,
          -- an edge created at the end of a route, where another begins
          x P.:& y P.:/ P.Backwards y P.:/ x P.:/ fixedNode,
          conj [s P.:/ P.Backwards s P.:/ x P.:/ fixedNode | s <- steps],
          -- alternatives
          (x P.:|| y) P.:/ conj steps,
          (x P.:++ y P.:/ fixedNode) P.:/ conj steps,
          conj steps P.:/ (x P.:|| y P.:|| P.One),
          -- steps with many matches
          conj [P.Wild P.:/ s | s <- steps],
          conj [P.RegexMatch [re|^[abc]$|] P.:/ s | s <- steps],
          P.Wild P.:/ P.Wild P.:/ x
        ]
    genRepairPath :: Int -> Gen P.Path
    genRepairPath size
      | size <= 1 =
          frequency
            [ (10, genStep),
              (1, pure P.One),
              (2, genNode),
              ( 1,
                P.ExcludingNIDs . setFromList . map smallNID
                  <$> (sublistOf [1 .. 3] `suchThat` (not . null))
              )
            ]
      | otherwise = do
          k <- choose (1, size - 1)
          let l = genRepairPath k
              r = genRepairPath (size - k)
          frequency
            [ (5, (P.:/) <$> l <*> r),
              (4, (P.:&) <$> l <*> r),
              (1, (P.:||) <$> l <*> r),
              (1, (P.:++) <$> l <*> r),
              (1, P.Backwards <$> genRepairPath (size - 1))
            ]

-- | The labels a chooser gives in a random case: none, the corpus's, or
-- one for wildcards only.
data Labels = NoLabels | CorpusLabels | WildLabel
  deriving stock (Eq, Show, Enum, Bounded)

labelsOf :: Labels -> Matcher -> Maybe Text
labelsOf = \case
  NoLabels -> const Nothing
  CorpusLabels -> corpusLabels
  WildLabel -> \case
    MWild -> Just "w"
    _ -> Nothing

spec_selectMatchesReference :: Spec
spec_selectMatchesReference = describe "select keeps what its reference keeps" do
  it "over the corpus, with every chooser used on it" $
    forM_ corpusQueries \surface -> do
      let q = compilePath surface
          literal = \case
            MLiteral _ -> True
            _ -> False
          -- a chooser is only asked about matchers that are not literals
          choosers
            | all (all (literal . (.matcher)) . patternEdges) q.alternatives =
                [const Nothing]
            | otherwise =
                [ const Nothing,
                  corpusLabels,
                  \m -> find (matcherAccepts m) ["fresh"]
                ]
      -- a path of a refused shape has no walks to select from
      forM_ corpusGraphs \g -> forM_ choosers \chooser ->
        for_ (candidatesUpTo Nothing chooser g q) selectsAsReference
  describe "with created nodes that are interchangeable" do
    let symmetric name g surface =
          it name . selectsAsReference $
            candidatesIn (const Nothing) g (compilePath surface)
        bare = graphWithEdges [edge' 2 "x" 2]
    symmetric "(~a & ~b)/c" bare [path| (~a & ~b)/c |]
    symmetric "(~a & ~b & ~c)/x & (~a & ~b & ~c)/y" bare $
      [path| (~a & ~b & ~c)/x & (~a & ~b & ~c)/y |]
    symmetric
      "(a | b)/~a/(a & b)/~b with a loop"
      (graphWithEdges [edge' 1 "a" 1, edge' 2 "b" 1])
      [path| (a | b)/~a/(a & b)/~b |]
    symmetric
      "a/x & a/y & a/z over four children"
      (graphWithEdges [edge' 1 "a" (10 + i) | i <- [0 .. 3]])
      [path| a/x & a/y & a/z |]
    symmetric
      "*/(x & y & z) over four children"
      (graphWithEdges [edge' 1 (singleton l) (10 + i) | (i, l) <- zip [0 ..] "abcd"])
      [path| */@/(x & y & z) |]
  it "on random paths and graphs" . withMaxSuccess 1500 $
    forAll genRepairCase \(surface, g) -> forAll arbitraryBoundedEnum \labels ->
      fewAlternatives 1000 surface
        ==> case candidatesUpTo (Just 300) (labelsOf labels) g (compilePath surface) of
          Nothing -> discard
          Just cs ->
            map (.residual.patch) (select cs)
              === map (.residual.patch) (selectReference cs)

-- * The limit on walks

residuateUpTo ::
  P.RepairsLimit ->
  Graph Text () ->
  P.Path ->
  Either ResidualError Residuation
residuateUpTo limit g surface =
  runIdentity $
    residuateWithin
      limit
      (pure . nodeIn g)
      (pure . const Nothing)
      corpusCurrent
      (compilePath surface)

-- | What building an admitted path took (walks, batches), if it took at
-- most this many walks. Only a count of the walks bounds them: @mk@
-- answers a path that resolves by matching it once its walks pass the
-- limit, so what it answers says nothing of how many walks there are.
walkCostUpTo :: Int -> Graph Text () -> P.Path -> Maybe WalkCost
walkCostUpTo limit g surface =
  case runIdentity (walkedWithin (Just limit) (pure . nodeIn g) (pure . const Nothing) corpusCurrent (compilePath surface)) of
    Right walked -> Just walked.cost
    Left (TooManyRepairs _) -> Nothing
    Left other -> error ("walkCost: " <> show other)

-- | What building an admitted path of at most the default number of walks
-- took.
walkCost :: Graph Text () -> P.Path -> WalkCost
walkCost g surface =
  fromMaybe (error "walkCost: more walks than the default limit") $
    walkCostUpTo defaultMaxRepairs g surface

-- | The graph with every node that the path writes as @\@N@.
withWritten :: P.Path -> Graph Text () -> Graph Text ()
withWritten p g =
  foldl' (\acc n -> insertNode (emptyNode n) acc) g (filter (isNothing . (`maybeNodeLookup` g)) (writtenNodes p))

-- | How many possibilities there are, or the error.
repairCount :: Either ResidualError Residuation -> Either ResidualError Int
repairCount = fmap \case
  Repairs rs -> length rs
  AlreadyResolves -> 0
  Unsatisfiable -> 0

spec_repairsLimit :: Spec
spec_repairsLimit = describe "the limit on walks" do
  let -- node 1 with this many children by edges a, and one by each of
      -- the first letters after a
      children k =
        graphWithEdges $
          [edge' 1 "a" (10 + i) | i <- [1 .. k]]
            <> [edge' 1 (singleton l) (100 + i) | (i, l) <- zip [0 ..] (take 10 ['b' ..])]
      -- k conjuncts, each a step that every child matches, then its own
      -- step: children ^ k walks
      anyChild k =
        foldl1Ex'
          (P.:&)
          [ P.Literal "a" P.:/ P.Literal ("x" <> tshow i)
          | i <- [1 .. k :: Int]
          ]
      finishing micros = timeout micros . evaluate
      -- the fewest children with which four such conjuncts are over the
      -- default limit
      tooManyChildren =
        headEx [k | k <- [1 ..], k ^ (4 :: Int) > defaultMaxRepairs]
  describe "counts walks" do
    it "at the limit the possibilities are those without one" do
      let p = anyChild 3
      repairCount (residuateUpTo (P.AtMostRepairs 8) (children 2) p) `shouldBe` Right 8
      residuateUpTo (P.AtMostRepairs 8) (children 2) p
        `shouldBe` residuateUpTo P.UnlimitedRepairs (children 2) p
    it "one under it is an error" $
      residuateUpTo (P.AtMostRepairs 7) (children 2) (anyChild 3)
        `shouldBe` Left (TooManyRepairs 7)
    it "walks that are not listed count" do
      -- 2 * 2 walks, of which the two that also create a b are dropped
      let g = graphWithEdges [edge' 1 "a" 2, edge' 1 "a" 3, edge' 2 "b" 4]
          p = [path| a/b & a/c |]
      repairCount (residuateUpTo (P.AtMostRepairs 4) g p) `shouldBe` Right 2
      residuateUpTo (P.AtMostRepairs 3) g p `shouldBe` Left (TooManyRepairs 3)
    it "a step with one edge to follow is one walk, however many there are" do
      -- ten existing conjuncts and a new one
      let p =
            foldl1Ex' (P.:&) $
              [P.Literal (singleton l) | l <- take 10 ['b' ..]]
                <> [P.Literal "new"]
      repairCount (residuateUpTo (P.AtMostRepairs 1) (children 0) p)
        `shouldBe` Right 1
    it "nothing is done twice: a path with one walk passes a limit of one" do
      -- the * comes after the a, and follows its edge
      let g = graphWithEdges []
      (walkCost g [path| * & a |]).walks `shouldBe` 1
      repairCount (residuateUpTo (P.AtMostRepairs 1) g [path| * & a |]) `shouldBe` Right 1
      -- two ways, of two walks
      let g' = graphWithEdges [edge' 1 "a" 2, edge' 1 "a" 3]
          p = [path| a/b |]
      (walkCost g' p).walks `shouldBe` 2
      repairCount (residuateUpTo (P.AtMostRepairs 2) g' p) `shouldBe` Right 2
      residuateUpTo (P.AtMostRepairs 1) g' p `shouldBe` Left (TooManyRepairs 1)
    it "a refused path is refused whatever the limit" $
      residuateUpTo (P.AtMostRepairs 1) (graphWithEdges [edge' 1 "a" 2]) [path| a/!{@000000000002}/b & a/c |]
        `shouldSatisfy` \case
          Left (Refused (CircularSuppliers _ _)) -> True
          _ -> False
    it "nor do steps back along created edges multiply them" $
      repairCount
        ( residuateUpTo
            (P.AtMostRepairs 1)
            (children 0)
            [path| a/~a & b/~b & c/~c & d/~d & e/~e & f/~f |]
        )
        `shouldBe` Right 1
  describe "the default" do
    it "stops a path with more walks at once" do
      result <-
        finishing 2000000 $
          residuateIn (children tooManyChildren) (anyChild 4)
      result `shouldBe` Just (Left (TooManyRepairs defaultMaxRepairs))
    it "allows one with fewer" do
      result <-
        finishing 20000000 . repairCount $
          residuateIn (children (tooManyChildren - 1)) (anyChild 4)
      result `shouldBe` Just (Right ((tooManyChildren - 1) ^ (4 :: Int)))
    it "no limit allows the first" do
      result <-
        finishing 30000000 . repairCount $
          residuateUpTo P.UnlimitedRepairs (children tooManyChildren) $
            anyChild 4
      result `shouldBe` Just (Right (tooManyChildren ^ (4 :: Int)))
  describe "a path that resolves is a no-op whatever the limit" do
    it "with more walks than the default" do
      -- every one of 2 * k steps has two edges to follow
      let k = headEx [i | i <- [1 ..], (2 :: Int) ^ i > defaultMaxRepairs]
          g =
            graphWithEdges
              [ edge' 1 (singleton l) t
              | l <- take k ['a' ..],
                t <- [2, 3]
              ]
          p = foldl1Ex' (P.:&) [P.Literal (singleton l) | l <- take k ['a' ..]]
      result <- finishing 5000000 (residuateIn g p)
      result `shouldBe` Just (Right AlreadyResolves)
    it "with a limit of one" $
      residuateUpTo
        (P.AtMostRepairs 1)
        (graphWithEdges [edge' 1 "a" 2, edge' 1 "a" 3])
        [path| a & a |]
        `shouldBe` Right AlreadyResolves
    it "on random paths and graphs" . withMaxSuccess 1000 $
      forAll genRepairCase \(surface, g) ->
        fewAlternatives 1000 surface
          ==> case candidatesUpTo
            (Just 300)
            (const Nothing)
            g
            (compilePath surface) of
            Nothing -> discard
            Just cs ->
              let unlimited = residuateUpTo P.UnlimitedRepairs g surface
                  overLimit k
                    | unlimited == Right AlreadyResolves = unlimited
                    | otherwise = Left (TooManyRepairs k)
                  made = length cs
               in conjoin
                    [ -- fewer than the walks is always too few
                      made
                        <= 1
                          .||. residuateUpTo (P.AtMostRepairs (made - 1)) g surface
                          === overLimit (made - 1),
                      residuateUpTo (P.AtMostRepairs 100000) g surface === unlimited,
                      -- as many as the walks is enough: none is made twice
                      residuateUpTo (P.AtMostRepairs made) g surface === unlimited
                    ]
