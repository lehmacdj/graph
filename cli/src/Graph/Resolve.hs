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

-- | Whether at least one deterministic member of the path resolves.
-- Unlike 'resolveNPath', this computes only possible landing NIDs: branch
-- intersections become set intersections, so no resolved-path cartesian
-- products need to be constructed.
resolveNPathExists ::
  forall r.
  ( Members '[GraphMetadataReading] r,
    HasCallStack
  ) =>
  NID ->
  NormalizedPath FullyAnchored ->
  Sem r Bool
resolveNPathExists firstNid normalizedPath =
  goMembers (toList normalizedPath.union)
  where
    goMembers = \case
      [] -> pure False
      (dp : rest) -> do
        landings <- resolveDp firstNid dp
        if null landings then goMembers rest else pure True

    resolveDp :: NID -> DeterministicPath FullyAnchored -> Sem r (Set NID)
    resolveDp nid = \case
      Pointlike p -> resolvePointlike True nid p
      Rooted rooted -> resolveRooted nid rooted

    resolveRooted ::
      NID -> RootedDeterministicPath FullyAnchored -> Sem r (Set NID)
    resolveRooted nid RootedDeterministicPath {..} = do
      perRoot <- forM (mapToList rootBranches) \(root, branches) -> do
        roots <- resolveDp nid root
        unions <$> traverse (`resolveBranches` branches) (toList roots)
      let common = intersectSets perRoot
      unions
        <$> traverse
          (\landing -> resolvePointlike False landing target)
          (toList common)

    resolvePointlike ::
      Bool -> NID -> PointlikeDeterministicPath FullyAnchored -> Sem r (Set NID)
    resolvePointlike isRoot nid PointlikeDeterministicPath {..} =
      case unifyFullyAnchored isRoot nid anchor of
        Nothing -> pure mempty
        Just unified -> do
          landings <- resolveBranches unified loops
          pure $
            if unified `member` landings
              then singletonSet unified
              else mempty

    resolveBranches ::
      NID -> OSet (DPBranch FullyAnchored) -> Sem r (Set NID)
    resolveBranches nid branches
      | null branches = pure (singletonSet nid)
      | otherwise =
          intersectSets
            <$> traverse (resolveBranch nid) (toList branches)

    resolveBranch ::
      NID -> DPBranch FullyAnchored -> Sem r (Set NID)
    resolveBranch nid = \case
      DPSingle direction -> followDirection nid direction
      DPSequence before midpoint after -> do
        beforeLandings <- resolveBranches nid before
        midpointLandings <-
          unions
            <$> traverse
              (\landing -> resolvePointlike False landing midpoint)
              (toList beforeLandings)
        unions
          <$> traverse
            (`resolveBranches` after)
            (toList midpointLandings)

    followDirection :: NID -> DPDirection -> Sem r (Set NID)
    followDirection nid direction = do
      node <- getNodeMetadata nid
      pure case node of
        Nothing -> mempty
        Just n -> case direction of
          DPIncoming' transition -> matchingNodes n.incoming transition
          DPOutgoing' transition -> matchingNodes n.outgoing transition

    matchingNodes :: Set (Connect Text) -> DPTransition -> Set NID
    matchingNodes connections = \case
      DPLiteral transition -> setFromList (matchConnect transition connections)
      DPWild -> setFromList (map (.node) (toList connections))
      DPRegex regex ->
        setFromList
          [ connection.node
          | connection <- toList connections,
            has (textRegexing regex) connection.transition
          ]

    intersectSets :: [Set NID] -> Set NID
    intersectSets = \case
      [] -> mempty
      (x : xs) -> foldl' intersection x xs

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
  traverseViaTransitions
    firstNid
    unifyFullyAnchored
    traverseDirection
    normalizedPath
    & cachingReadingInState
    & runState emptyGraph
    & runState mempty
    & fmap \(nonexistentNodes, (graph, path)) -> ResolvedPath {..}
  where
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
