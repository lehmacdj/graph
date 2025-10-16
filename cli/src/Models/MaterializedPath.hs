module Models.MaterializedPath
  ( MaterializedPath (..),
    getTargets,
    finalNonLoopEdges,
    leftmostConnects,
    spec_leftmostConnects,
    module X,
  )
where

import Models.Augmentation.IsThin as X
import Models.Common
import Models.Connect
import Models.Edge
import Models.Graph
import Models.NID
import Models.NormalizedPath
import Models.NormalizedPath.TH
import MyPrelude
import Utils.Testing

data MaterializedPath t = MaterializedPath
  { path :: NormalizedPath NID t,
    graph :: Graph t IsThin,
    nonexistentNodes :: Set NID
  }
  deriving (Eq, Show, Generic)

dpTarget :: DeterministicPath NID t -> NID
dpTarget = \case
  Pointlike PointlikeDeterministicPath {..} ->
    anchor
  Rooted RootedDeterministicPath {target = PointlikeDeterministicPath {..}} ->
    anchor

-- | Get the set of all nodes targeted by the NormalizedPath
-- maybe this actually should belong in some Models.NormalizedPath submodule;
-- it's here because typically this is used downstream of materializing a path
getTargets :: (ValidTransition t) => NormalizedPath NID t -> Set NID
getTargets (NormalizedPath dps) = foldMap (singletonSet . dpTarget) dps

-- | Get the final connects targeted by the NormalizedPath.
-- This returns two layers of Set:
-- * the outer set represents disjunction of DeterministicPaths
-- * the inner set represents conjunction of edges within a DeterministicPath
-- maybe this actually should belong in some Models.NormalizedPath submodule;
-- it's here because typically this is used downstream of materializing a path
finalNonLoopEdges ::
  forall t. (ValidTransition t) => NormalizedPath NID t -> Set (Set (Edge t))
finalNonLoopEdges (NormalizedPath dps) =
  foldMap (singletonSet . dpFinalNonLoopEdges) dps
  where
    dpFinalNonLoopEdges :: DeterministicPath NID t -> Set (Edge t)
    dpFinalNonLoopEdges = \case
      Pointlike _ -> mempty
      Rooted RootedDeterministicPath {rootBranches, target = PointlikeDeterministicPath {..}} ->
        ifoldMapOf
          (itraversed <. folded)
          (\i b -> branchFinalNonLoopEdges (dpTarget i) anchor b)
          rootBranches

    branchFinalNonLoopEdges :: NID -> NID -> DPBranch NID t -> Set (Edge t)
    branchFinalNonLoopEdges branchStart target = \case
      DPIncoming (DPLiteral l) -> singletonSet (Edge branchStart l target)
      DPOutgoing (DPLiteral l) -> singletonSet (Edge target l branchStart)
      DPSequence _ midpoint b2 -> foldMap (branchFinalNonLoopEdges (dpTarget (Pointlike midpoint)) target) b2
      DPIncoming DPWild -> error "invalid in NormalizedPath NID"
      DPOutgoing DPWild -> error "invalid in NormalizedPath NID"
      DPIncoming (DPRegex _) -> error "invalid in NormalizedPath NID"
      DPOutgoing (DPRegex _) -> error "invalid in NormalizedPath NID"

leftmostConnects :: (Ord t) => NormalizedPath NID t -> Set (NID, [Connect t])
leftmostConnects (NormalizedPath dps) =
  setFromList $ map dpLeftmostConnects (toList dps)

dpLeftmostConnects ::
  forall t.
  (Ord t) =>
  DeterministicPath NID t ->
  (NID, [Connect t])
dpLeftmostConnects = \case
  Pointlike PointlikeDeterministicPath {..} ->
    (anchor, [])
  Rooted RootedDeterministicPath {rootBranches, target = PointlikeDeterministicPath {..}} ->
    -- Take the first (leftmost) root in the OMap
    case headMay (mapToList rootBranches) of
      Nothing -> error "broken invariant: rootBranches cannot be empty"
      Just (root, branches) ->
        case headMay (toList branches) of
          Nothing -> error "broken invariant: branches cannot be empty"
          Just branch ->
            let (rootNID, rootConnects) = dpLeftmostConnects root
             in (rootNID, rootConnects ++ branchToConnects branch anchor)
  where
    branchToConnects :: DPBranch NID t -> NID -> [Connect t]
    branchToConnects branch finalTarget = case branch of
      DPIncoming (DPLiteral l) -> [Connect l finalTarget]
      DPOutgoing (DPLiteral l) -> [Connect l finalTarget]
      DPSequence branches1 midpoint branches2 ->
        -- Take the first branch from the first set
        case headMay (toList branches1) of
          Nothing -> error "broken invariant: sequence branches cannot be empty"
          Just firstBranch ->
            let midNID = midpoint.anchor
                connects1 = branchToConnects firstBranch midNID
                connects2 = branchToConnects (headEx (toList branches2)) finalTarget
             in connects1 ++ connects2
      DPIncoming DPWild -> error "invalid in NormalizedPath NID"
      DPOutgoing DPWild -> error "invalid in NormalizedPath NID"
      DPIncoming (DPRegex _) -> error "invalid in NormalizedPath NID"
      DPOutgoing (DPRegex _) -> error "invalid in NormalizedPath NID"

spec_leftmostConnects :: Spec
spec_leftmostConnects = do
  it "just pointlike works" $
    leftmostConnects [npathNID| @0 |]
      `shouldBe` setFromList [(smallNID 0, [])]
  it "loops are irrelevant" $
    leftmostConnects [npathNID| @0[a & b] |]
      `shouldBe` setFromList [(smallNID 0, [])]
  it "simple sequence works" $
    leftmostConnects [npathNID| [@0<a/@1|b]>@2 |]
      `shouldBe` setFromList [(smallNID 0, [Connect "a" (smallNID 1), Connect "b" (smallNID 2)])]
  it "all unioned things included" $
    leftmostConnects [npathNID| [@0<a/@1|b]>@2 + [@3<c/@4|d]>@5 + @6[e] |]
      `shouldBe` setFromList
        [ (smallNID 0, [Connect "a" (smallNID 1), Connect "b" (smallNID 2)]),
          (smallNID 3, [Connect "c" (smallNID 4), Connect "d" (smallNID 5)]),
          (smallNID 6, [])
        ]
  it "takes leftmost part of intersections" $
    leftmostConnects [npathNID| [@0<a & @1<b]>@2|]
      `shouldBe` setFromList [(smallNID 0, [Connect "a" (smallNID 2)])]
