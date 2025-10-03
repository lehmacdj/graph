module Models.MaterializedPath
  ( MaterializedPath (..),
    targets,
    finalNonLoopEdges,
    module X,
  )
where

import Models.Augmentations.IsThin as X
import Models.Common
import Models.Edge
import Models.Graph
import Models.NID
import Models.NormalizedPath
import MyPrelude

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
targets :: (ValidTransition t) => NormalizedPath NID t -> Set NID
targets (NormalizedPath dps) = foldMap (singletonSet . dpTarget) dps

-- | Get the final connects targeted by the NormalizedPath.
-- maybe this actually should belong in some Models.NormalizedPath submodule;
-- it's here because typically this is used downstream of materializing a path
finalNonLoopEdges :: forall t. (ValidTransition t) => NormalizedPath NID t -> Set (Set (Edge t))
finalNonLoopEdges (NormalizedPath dps) = foldMap (singletonSet . dpFinalNonLoopEdges) dps
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
