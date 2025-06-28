module Graph.MaterializePath where

import Graph.GraphMetadataEditing
import Models.Connect (Connect (..))
import Models.Graph (Graph, emptyGraph, insertNode, nodesMatchedBy, nodesMatching, singletonGraph)
import Models.NID
import Models.Node
import Models.NormalizedPath
import Models.Path
import MyPrelude hiding ((\\))
import Polysemy.State

data IsThin = Thin | Fetched
  deriving (Eq, Show, Ord, Generic)

instance ShowableAugmentation IsThin where
  augmentationLabel = Nothing
  defaultShowAugmentation = \case
    Thin -> "thin"
    Fetched -> "fetched"
  shouldShowStandaloneAugmentation = True

data MaterializedPath t = MaterializedPath
  { path :: NormalizedPath ConcreteAnchor t,
    graph :: Graph t IsThin,
    nonexistentNodes :: Set NID
  }
  deriving (Eq, Show, Generic)

-- | Traverse a path, fetching node metadata and noting which nodes are missing.
materializePath ::
  forall t r.
  ( Members '[GraphMetadataReading t] r,
    ValidTransition t
  ) =>
  -- | The node to start from
  NID ->
  Path t ->
  Sem r (MaterializedPath t)
materializePath firstNid op = do
  let normalizedPath = leastConstrainedNormalizedPath $ normalizePath op
  traverse (traverseDeterministicPath firstNid) (toList normalizedPath.union)
    & fmap (NormalizedPath . setFromList . concat)
    & runState emptyGraph
    & runState mempty
    & fmap \(nonexistentNodes, (graph, path)) -> MaterializedPath {..}
  where
    traverseDeterministicPath ::
      NID ->
      DeterministicPath FullyAnchored t ->
      Sem
        (State (Graph t IsThin) : State (Set NID) : r)
        [DeterministicPath ConcreteAnchor t]
    traverseDeterministicPath nid = \case
      Pointlike p -> fmap Pointlike <$> traversePointlike nid p
      Rooted r -> fmap Rooted <$> traverseRooted nid r

    traversePointlike ::
      NID ->
      PointlikeDeterministicPath FullyAnchored t ->
      Sem
        (State (Graph t IsThin) : State (Set NID) : r)
        [PointlikeDeterministicPath ConcreteAnchor t]
    traversePointlike nid PointlikeDeterministicPath {..} = undefined

    traverseRooted ::
      NID ->
      RootedDeterministicPath FullyAnchored t ->
      Sem
        (State (Graph t IsThin) : State (Set NID) : r)
        [RootedDeterministicPath ConcreteAnchor t]
    traverseRooted nid RootedDeterministicPath {..} = undefined

    traverseBranch ::
      NID ->
      DPBranch FullyAnchored t ->
      Sem
        (State (Graph t IsThin) : State (Set NID) : r)
        [(DPBranch ConcreteAnchor t, NID)]
    traverseBranch nid = \case
      DPLiteral l -> undefined
      DPWild -> undefined
      DPSequence {} -> undefined
