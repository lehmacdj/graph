module Graph.MaterializePath where

import Graph.GraphMetadataEditing
import Models.Connect (Connect (..), matchConnect)
import Models.Graph (Graph, emptyGraph)
import Models.MaterializedPath
import Models.NID
import Models.Node
import Models.NormalizedPath
import Models.Path.Simple
import MyPrelude hiding ((\\))
import Polysemy.State

materializePath ::
  forall r.
  ( Members '[GraphMetadataReading] r,
    HasCallStack
  ) =>
  NID ->
  Path ->
  Sem r MaterializedPath
materializePath nid path =
  materializeNPath nid (leastConstrainedNormalizedPath (normalizePath path))

-- | Traverse a path, fetching node metadata and noting which nodes are missing.
materializeNPath ::
  forall r.
  ( Members '[GraphMetadataReading] r,
    HasCallStack
  ) =>
  -- | The node to start from
  NID ->
  NormalizedPath FullyAnchored ->
  Sem r MaterializedPath
materializeNPath firstNid normalizedPath =
  traverseViaTransitions firstNid unifyAnchor traverseDirection normalizedPath
    & cachingReadingInState
    & runState emptyGraph
    & runState mempty
    & fmap \(nonexistentNodes, (graph, path)) -> MaterializedPath {..}
  where
    unifyAnchor :: Bool -> NID -> FullyAnchored -> Maybe NID
    unifyAnchor isRoot nid = \case
      FSpecific fnid
        | not isRoot && nid /= fnid -> Nothing
        | otherwise -> pure fnid
      FJoinPoint {..}
        | nid `notMember` excluding -> pure nid
        | otherwise -> Nothing

    traverseDirection ::
      ( Members [GraphMetadataReading, State (Graph Text IsThin), State (Set NID)] q,
        HasCallStack
      ) =>
      NID ->
      DPDirection ->
      Sem q [(DPDirection, NID)]
    traverseDirection nid = \case
      DPIncoming' t -> traverseTransition nid (.incoming) DPIncoming' t
      DPOutgoing' t -> traverseTransition nid (.outgoing) DPOutgoing' t

    traverseTransition ::
      ( Members [GraphMetadataReading, State (Graph Text IsThin), State (Set NID)] q,
        HasCallStack
      ) =>
      NID ->
      (Node Text () -> Set (Connect Text)) ->
      (DPTransition -> DPDirection) ->
      DPTransition ->
      Sem q [(DPDirection, NID)]
    traverseTransition nid getConnections mkBranch = \case
      DPLiteral t -> withEarlyReturn do
        n <- getNodeMetadata nid `onNothingM` returnEarly []
        pure $
          getConnections n
            & matchConnect t
            & map (mkBranch (DPLiteral t),)
      DPWild -> withEarlyReturn do
        n <- getNodeMetadata nid `onNothingM` returnEarly []
        pure $
          getConnections n
            & toList
            & map (\Connect {..} -> (mkBranch (DPLiteral transition), node))
      DPRegex r -> withEarlyReturn do
        n <- getNodeMetadata nid `onNothingM` returnEarly []
        pure $
          getConnections n
            & toListOf (folded . filteredBy (#transition . textRegexing r))
            & map (\Connect {..} -> (mkBranch (DPLiteral transition), node))
