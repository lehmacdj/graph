{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

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

{-# HLINT ignore materializeNPath "Functor law" #-}

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
materializeNPath firstNid normalizedPath = do
  traverse (traverseDeterministicPath firstNid) (toList normalizedPath.union)
    & fmap (NormalizedPath . setFromList . concat)
    & cachingReadingInState
    & runState emptyGraph
    & runState mempty
    & fmap \(nonexistentNodes, (graph, path)) -> MaterializedPath {..}
  where
    traverseDeterministicPath ::
      ( Members [GraphMetadataReading, State (Graph Text IsThin), State (Set NID)] q,
        HasCallStack
      ) =>
      NID ->
      DeterministicPath FullyAnchored ->
      Sem q [DeterministicPath NID]
    traverseDeterministicPath nid = \case
      Pointlike p -> map Pointlike <$> traversePointlike True nid p
      Rooted r -> map Rooted <$> traverseRooted nid r

    traversePointlike ::
      ( Members [GraphMetadataReading, State (Graph Text IsThin), State (Set NID)] q,
        HasCallStack
      ) =>
      Bool ->
      NID ->
      PointlikeDeterministicPath FullyAnchored ->
      Sem q [PointlikeDeterministicPath NID]
    traversePointlike isRoot nid PointlikeDeterministicPath {..} = withEarlyReturn do
      nid' <- case anchor of
        FSpecific fnid
          | not isRoot && nid /= fnid -> returnEarly []
          | otherwise -> pure fnid
        FJoinPoint {..}
          | nid `notMember` excluding -> pure nid
          | otherwise -> returnEarly []
      loops' <- traverseBranches nid' loops
      pure $
        loops'
          & filter ((== nid') . snd)
          & map (PointlikeDeterministicPath nid' . fst)

    ensureSameAnchors ::
      (HasCallStack) => NonNull [(a, NID)] -> Maybe (NonNull [a], NID)
    ensureSameAnchors xs =
      let example = snd (head xs)
          as = mapNonNull fst xs
       in justIfTrue
            (all ((== example) . snd) $ toNullable xs)
            (as, example)

    traverseRooted ::
      ( Members [GraphMetadataReading, State (Graph Text IsThin), State (Set NID)] q,
        HasCallStack
      ) =>
      NID ->
      RootedDeterministicPath FullyAnchored ->
      Sem q [RootedDeterministicPath NID]
    traverseRooted nid RootedDeterministicPath {..} = do
      rootBranches' ::
        [ [ ( DeterministicPath NID,
              OSet (DPBranch FullyAnchored)
            )
          ]
        ] <-
        rootBranches
          & mapToList
          & (traverse . _1) (traverseDeterministicPath nid)
          <&> map (\(rs, bs) -> (,bs) <$> rs)
          <&> choices
      rootBranches'' ::
        [(OMap (DeterministicPath NID) (OSet (DPBranch NID)), NID)] <-
        rootBranches'
          & (traverse . traverse)
            (\(dp, bs) -> (dp,) <$> traverseBranches dp.target.anchor bs)
          <&> map (map \(r, bas) -> (\(b, a) -> ((r, b), a)) <$> bas)
          <&> concatMap choices
          <&> mapMaybe (ensureSameAnchors . impureNonNull)
          <&> ordNub
          <&> over (mapped . _1) (mapFromList . toNullable)
      rootBranches''
        & (traverse . _2) (\x -> traversePointlike False x target)
        <&> concatMap (\(rbs, ts) -> (rbs,) <$> ts)
        <&> map (uncurry smartBuildRootedDeterministicPath)

    traverseBranches ::
      ( Members [GraphMetadataReading, State (Graph Text IsThin), State (Set NID)] q,
        HasCallStack
      ) =>
      NID ->
      OSet (DPBranch FullyAnchored) ->
      Sem q [(OSet (DPBranch NID), NID)]
    traverseBranches nid branches = withEarlyReturn do
      -- in the case where the OSet is empty (only when loops),
      -- we return the same anchor as the input
      when (null branches) $ returnEarly [(mempty, nid)]
      branches' <- traverse (traverseBranch nid) (toList branches)
      pure $
        branches'
          & choices
          & mapMaybe (ensureSameAnchors . impureNonNull)
          & over (mapped . _1) (setFromList . toNullable)

    traverseTransition ::
      ( Members [GraphMetadataReading, State (Graph Text IsThin), State (Set NID)] q,
        HasCallStack
      ) =>
      NID ->
      (Node Text () -> Set (Connect Text)) ->
      (DPTransition -> DPBranch NID) ->
      DPTransition ->
      Sem q [(DPBranch NID, NID)]
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
            & toListOf (folded . filteredBy (#transition . regexing' r))
            & map (\Connect {..} -> (mkBranch (DPLiteral transition), node))

    traverseBranch ::
      ( Members [GraphMetadataReading, State (Graph Text IsThin), State (Set NID)] q,
        HasCallStack
      ) =>
      NID ->
      DPBranch FullyAnchored ->
      Sem q [(DPBranch NID, NID)]
    traverseBranch nid = \case
      DPOutgoing transition -> traverseTransition nid (.outgoing) DPOutgoing transition
      DPIncoming transition -> traverseTransition nid (.incoming) DPIncoming transition
      DPSequence bs1 midpoint bs2 -> do
        bs1's <- traverseBranches nid bs1
        uptoMidpoints :: [(OSet (DPBranch NID), PointlikeDeterministicPath NID)] <-
          bs1's
            & (traverse . _2) (\x -> traversePointlike False x midpoint)
            <&> concatMap (\(bs1', ms) -> (bs1',) <$> ms)
        uptoMidpoints
          & (traverse . _2) (\p -> (p,) <$> traverseBranches p.anchor bs2)
          <&> concatMap (\(bs1', (p, bs2'ts)) -> first (DPSequence bs1' p) <$> bs2'ts)
