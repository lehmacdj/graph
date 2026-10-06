-- | 'suggestPaths' against its reference, which residuates every
-- candidate path (docs/path-pattern-ir.md §5, Suggested paths).
module Models.PathPattern.SuggestSpec where

import Control.Monad.State.Strict qualified as S
import Models.Graph (Graph, maybeNodeLookup)
import Models.NID (NID)
import Models.Path.Simple qualified as P
import Models.Path.TH (path)
import Models.PathPattern.Compile (checkAlternatives, compilePath)
import Models.PathPattern.Residuate
import Models.PathPattern.ResiduateSpec (genRepairCase)
import Models.PathPattern.Suggest
import Models.PathPattern.TestCorpus
import MyPrelude
import Utils.Testing

-- | Counts the nodes read, as a measure of the residuations done.
type Counting = S.State Int

countingLookup :: Graph Text () -> NodeLookup Counting
countingLookup g nid = S.modify' (+ 1) $> maybeNodeLookup nid g

-- | The survivors of an ambiguous path, if it is one within the limit.
ambiguousIn ::
  P.RepairsLimit -> Graph Text () -> P.Path -> Maybe (NonEmpty Residual)
ambiguousIn limit g p =
  case runIdentity
    ( residuateWithin
        limit
        (pure . (`maybeNodeLookup` g))
        noLabels
        corpusCurrent
        (compilePath p)
    ) of
    Right (Repairs rs@(_ :| _ : _)) -> Just rs
    _ -> Nothing

-- | The suggestions for the first five survivors, as shown, and the nodes
-- read to find them.
suggestionsWith ::
  ( NodeLookup Counting ->
    LabelChooser Counting ->
    NID ->
    P.Path ->
    NonEmpty Residual ->
    [Residual] ->
    Counting [Maybe P.Path]
  ) ->
  Graph Text () ->
  P.Path ->
  NonEmpty Residual ->
  ([Maybe Text], Int)
suggestionsWith suggest g p rs =
  first (map (fmap renderSurfacePath)) $
    S.runState
      (suggest (countingLookup g) noLabels corpusCurrent p rs (take 5 (toList rs)))
      0

spec_suggestPathsMatchesReference :: Spec
spec_suggestPathsMatchesReference =
  describe "suggestPaths suggests what its reference suggests" do
    let same g p = case ambiguousIn (P.AtMostRepairs defaultMaxRepairs) g p of
          Nothing -> expectationFailure ("not ambiguous: " <> show p)
          Just rs ->
            fst (suggestionsWith suggestPaths g p rs)
              `shouldBe` fst (suggestionsWith suggestPathsReference g p rs)
        bare = graphWithEdges [edge' 1 "zz" 9]
    it "over the corpus" $
      forM_ corpusQueries \p -> forM_ corpusGraphs \g ->
        forM_ (ambiguousIn (P.AtMostRepairs defaultMaxRepairs) g p) \rs ->
          (show (p, g), fst (suggestionsWith suggestPaths g p rs))
            `shouldBe` ( show (p, g),
                         fst (suggestionsWith suggestPathsReference g p rs)
                       )
    it "where no step can be rewritten: many pins" $
      same bare [path| (a & b & c)/~a/(a & b & c)/~b |]
    it "where created nodes are shared from the right" $
      same
        (graphWithEdges [edge' 5 "x" 5])
        [path| a/b/@000000000005 & c/b/@000000000005 & d/b/@000000000005 |]
    it "where an alternative has several of the repairs" do
      same bare [path| a/~a & b/~b ++ c/~c |]
      same bare [path| (a | b)/~a/(a & b)/~b |]
      same
        (graphWithEdges [edge' 1 "a" 2, edge' 1 "a" 3])
        [path| a/b ++ a/c ++ d |]
    it "on random ambiguous paths and graphs" . withMaxSuccess 400 $
      forAll genRepairCase \(p, g) ->
        checkAlternatives p
          == Right ()
            ==> case ambiguousIn (P.AtMostRepairs 150) g p of
              Nothing -> discard
              Just rs ->
                fst (suggestionsWith suggestPaths g p rs)
                  === fst (suggestionsWith suggestPathsReference g p rs)

spec_suggestPathsCost :: Spec
spec_suggestPathsCost = describe "suggestPaths residuates no more than it must" do
  let bare = graphWithEdges [edge' 1 "zz" 9]
      reads suggest g p = case ambiguousIn P.UnlimitedRepairs g p of
        Nothing -> error ("not ambiguous: " <> show p)
        Just rs -> snd (suggestionsWith suggest g p rs)
      -- the nodes one residuation of the path reads
      residuationReads g p =
        S.execState
          (residuate (countingLookup g) noLabels corpusCurrent (compilePath p))
          0
  it "not at all for a path that pins nothing: shared created nodes" do
    -- no step of these repairs can be rewritten, so each candidate is the
    -- path itself, which has all the repairs still
    let g = graphWithEdges [edge' 5 "x" 5]
        p =
          [path| a/b/@000000000005 & c/b/@000000000005 & d/b/@000000000005
                 & e/b/@000000000005 |]
    reads suggestPaths g p `shouldBe` 0
    reads suggestPathsReference g p `shouldBe` 5 * residuationReads g p
  it "once for a candidate tried for several repairs" do
    let p = [path| (a & b & c)/~a/(a & b & c)/~b |]
    reads suggestPaths bare p
      `shouldSatisfy` (< reads suggestPathsReference bare p `div` 2)
  it "finishes for many pins in about the time of one residuation" do
    -- five residuations of the path itself before, 10s; now none, and one
    -- of a candidate that is over the limit
    let p = [path| (a & b & c & d & e)/~a/(a & b & c)/~b |]
        suggested = do
          rs <- ambiguousIn P.UnlimitedRepairs bare p
          pure (fst (suggestionsWith suggestPaths bare p rs))
    result <- timeout 30000000 (evaluate (force suggested))
    result `shouldBe` Just (Just (replicate 5 Nothing))
