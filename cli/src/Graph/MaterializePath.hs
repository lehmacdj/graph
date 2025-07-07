{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

module Graph.MaterializePath where

import Graph.GraphMetadataEditing
import Models.Connect (Connect (..), matchConnect)
import Models.Graph (Graph, emptyGraph)
import Models.NID
import Models.Node
import Models.NormalizedPath
import Models.Path
import MyPrelude hiding ((\\))
import Polysemy.State

data MaterializedPath t = MaterializedPath
  { path :: NormalizedPath NID t,
    graph :: Graph t IsThin,
    nonexistentNodes :: Set NID
  }
  deriving (Eq, Show, Generic)

{-# HLINT ignore materializePath "Functor law" #-}

-- | Traverse a path, fetching node metadata and noting which nodes are missing.
materializePath ::
  forall t r.
  ( Members '[GraphMetadataReading t] r,
    ValidTransition t,
    HasCallStack
  ) =>
  -- | The node to start from
  NID ->
  Path t ->
  Sem r (MaterializedPath t)
materializePath firstNid op = do
  let normalizedPath = leastConstrainedNormalizedPath $ normalizePath op
  traverse (traverseDeterministicPath firstNid) (toList normalizedPath.union)
    & fmap (NormalizedPath . setFromList . concat)
    & cachingReadingInState
    & runState emptyGraph
    & runState mempty
    & fmap \(nonexistentNodes, (graph, path)) -> MaterializedPath {..}
  where
    traverseDeterministicPath ::
      ( Members [GraphMetadataReading t, State (Graph t IsThin), State (Set NID)] q,
        HasCallStack
      ) =>
      NID ->
      DeterministicPath FullyAnchored t ->
      Sem q [DeterministicPath NID t]
    traverseDeterministicPath nid = \case
      Pointlike p -> map Pointlike <$> traversePointlike nid p
      Rooted r -> map Rooted <$> traverseRooted nid r

    traversePointlike ::
      ( Members [GraphMetadataReading t, State (Graph t IsThin), State (Set NID)] q,
        HasCallStack
      ) =>
      NID ->
      PointlikeDeterministicPath FullyAnchored t ->
      Sem q [PointlikeDeterministicPath NID t]
    traversePointlike nid PointlikeDeterministicPath {..} = withEarlyReturn do
      when (has (#_FSpecific . only nid) anchor) $ returnEarly []
      loops' <- traverseBranches nid loops
      pure $
        loops'
          & filter ((== nid) . snd)
          & map (PointlikeDeterministicPath nid . fst)

    ensureSameAnchors ::
      (HasCallStack) => NonNull [(a, NID)] -> Maybe (NonNull [a], NID)
    ensureSameAnchors xs =
      let example = snd (head xs)
          as = mapNonNull fst xs
       in justIfTrue
            (all ((== example) . snd) $ toNullable xs)
            (as, example)

    traverseRooted ::
      ( Members [GraphMetadataReading t, State (Graph t IsThin), State (Set NID)] q,
        HasCallStack
      ) =>
      NID ->
      RootedDeterministicPath FullyAnchored t ->
      Sem q [RootedDeterministicPath NID t]
    traverseRooted nid RootedDeterministicPath {..} = do
      rootBranches' ::
        [ [ ( DeterministicPath NID t,
              OSet (DPBranch FullyAnchored t)
            )
          ]
        ] <-
        rootBranches
          & mapToList
          & (traverse . _1) (traverseDeterministicPath nid)
          <&> map (\(rs, bs) -> (,bs) <$> rs)
          <&> choices
      rootBranches'' ::
        [(OMap (DeterministicPath NID t) (OSet (DPBranch NID t)), NID)] <-
        rootBranches'
          & (traverse . traverse)
            (\(dp, bs) -> (dp,) <$> traverseBranches dp.target.anchor bs)
          <&> map (map \(r, bas) -> (\(b, a) -> ((r, b), a)) <$> bas)
          <&> concatMap choices
          <&> mapMaybe (ensureSameAnchors . impureNonNull)
          <&> ordNub
          <&> over (mapped . _1) (mapFromList . toNullable)
      rootBranches''
        & (traverse . _2) (`traversePointlike` target)
        <&> concatMap (\(rbs, ts) -> (rbs,) <$> ts)
        <&> map (uncurry smartBuildRootedDeterministicPath)

    traverseBranches ::
      ( Members [GraphMetadataReading t, State (Graph t IsThin), State (Set NID)] q,
        HasCallStack
      ) =>
      NID ->
      OSet (DPBranch FullyAnchored t) ->
      Sem q [(OSet (DPBranch NID t), NID)]
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

    traverseBranch ::
      ( Members [GraphMetadataReading t, State (Graph t IsThin), State (Set NID)] q,
        HasCallStack
      ) =>
      NID ->
      DPBranch FullyAnchored t ->
      Sem q [(DPBranch NID t, NID)]
    traverseBranch nid = \case
      DPLiteral t -> withEarlyReturn do
        n <- getNodeMetadata nid `onNothingM` returnEarly []
        pure $
          n.outgoing
            & matchConnect t
            & map (DPLiteral t,)
      DPWild -> withEarlyReturn do
        n <- getNodeMetadata nid `onNothingM` returnEarly []
        pure $
          n.outgoing
            & toList
            & map (\Connect {..} -> (DPLiteral transition, node))
      DPSequence bs1 midpoint bs2 -> do
        bs1's <- traverseBranches nid bs1
        uptoMidpoints :: [(OSet (DPBranch NID t), PointlikeDeterministicPath NID t)] <-
          bs1's
            & (traverse . _2) (`traversePointlike` midpoint)
            <&> concatMap (\(bs1', ms) -> (bs1',) <$> ms)
        uptoMidpoints
          & (traverse . _2) (\p -> (p,) <$> traverseBranches p.anchor bs2)
          <&> concatMap (\(bs1', (p, bs2'ts)) -> first (DPSequence bs1' p) <$> bs2'ts)
