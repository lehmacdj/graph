module Graph.Resolve where

import Graph.GraphMetadataEditing
import Models.Connect (Connect (..), matchConnect)
import Models.Graph (Graph, emptyGraph)
import Models.NID
import Models.Node
import Models.NormalizedPath
import Models.NormalizedPath.Traversals
import Models.Path.Simple
import Models.ResolvedPath
import MyPrelude hiding ((\\))
import Polysemy.State

resolvePath ::
  forall r.
  ( Members '[GraphMetadataReading] r,
    HasCallStack
  ) =>
  NID ->
  Path ->
  Sem r ResolvedPath
resolvePath nid path =
  resolveNPath nid (leastConstrainedNormalizedPath (normalizePath path))

-- | Traverse a path, fetching node metadata and noting which nodes are missing.
resolveNPath ::
  forall r.
  ( Members '[GraphMetadataReading] r,
    HasCallStack
  ) =>
  -- | The node to start from
  NID ->
  NormalizedPath FullyAnchored ->
  Sem r ResolvedPath
resolveNPath firstNid normalizedPath =
  traverseViaTransitions firstNid unifyAnchor traverseDirection normalizedPath
    & cachingReadingInState
    & runState emptyGraph
    & runState mempty
    & fmap \(nonexistentNodes, (graph, path)) -> ResolvedPath {..}
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
      FullyAnchored ->
      DPDirection ->
      FullyAnchored ->
      Sem q [(DPDirection, NID)]
    traverseDirection nid _startAnchor direction _target = case direction of
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
