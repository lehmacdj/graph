module Graph.MaterializePath where

import Graph.GraphMetadataEditing
import Models.Connect (Connect (..), matchConnect)
import Models.Graph (Graph, emptyGraph, insertNode, nodesMatchedBy, nodesMatching, singletonGraph)
import Models.NID
import Models.Node
import Models.NormalizedPath
import Models.Path
import MyPrelude hiding ((\\))
import Polysemy.State

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
    & fmap (NormalizedPath . setFromList . concatMap toNullable)
    & cachingReadingInState
    & runState emptyGraph
    & runState mempty
    & fmap \(nonexistentNodes, (graph, path)) -> MaterializedPath {..}
  where
    traverseDeterministicPath ::
      NID ->
      DeterministicPath FullyAnchored t ->
      Sem
        (GraphMetadataReading t : State (Graph t IsThin) : State (Set NID) : r)
        (NonNull [DeterministicPath ConcreteAnchor t])
    traverseDeterministicPath nid = \case
      Pointlike p -> mapNonNull Pointlike <$> traversePointlike nid p
      Rooted r -> mapNonNull Rooted <$> traverseRooted nid r

    traversePointlike ::
      NID ->
      PointlikeDeterministicPath FullyAnchored t ->
      Sem
        (GraphMetadataReading t : State (Graph t IsThin) : State (Set NID) : r)
        (NonNull [PointlikeDeterministicPath ConcreteAnchor t])
    traversePointlike nid PointlikeDeterministicPath {..} = undefined

    traverseRooted ::
      NID ->
      RootedDeterministicPath FullyAnchored t ->
      Sem
        (GraphMetadataReading t : State (Graph t IsThin) : State (Set NID) : r)
        (NonNull [RootedDeterministicPath ConcreteAnchor t])
    traverseRooted nid RootedDeterministicPath {..} = undefined

    traverseBranch ::
      ( Members [GraphMetadataReading t, State (Graph t IsThin), State (Set NID)] q,
        HasCallStack
      ) =>
      NID ->
      DPBranch FullyAnchored t ->
      Sem q (NonNull [(DPBranch ConcreteAnchor t, ConcreteAnchor)])
    traverseBranch nid = \case
      DPLiteral l -> do
        let def = singletonNN (DPLiteral l, NotInGraph)
        getNodeMetadata nid <&> \case
          Nothing -> def
          Just n ->
            fromMaybe def . fromNullable $
              [(DPLiteral l, CSpecific nid') | nid' <- matchConnect l n.outgoing]
      DPWild -> do
        let def = singletonNN (DPWild, NotInGraph)
        getNodeMetadata nid <&> \case
          Nothing -> def
          Just n ->
            fromMaybe def . fromNullable $
              [(DPWild, CSpecific nid') | nid' <- n ^.. #outgoing . folded . #node]
      DPSequence bs1 midpoint bs2 -> undefined
