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
  { path :: NormalizedPath ConcreteAnchor t,
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
  traverse (traverseDeterministicPath (CSpecific firstNid)) (toList normalizedPath.union)
    & fmap (NormalizedPath . setFromList . concatMap toNullable)
    & cachingReadingInState
    & runState emptyGraph
    & runState mempty
    & fmap \(nonexistentNodes, (graph, path)) -> MaterializedPath {..}
  where
    traverseDeterministicPath ::
      ( Members [GraphMetadataReading t, State (Graph t IsThin), State (Set NID)] q,
        HasCallStack
      ) =>
      ConcreteAnchor ->
      DeterministicPath FullyAnchored t ->
      Sem q (NonNull [DeterministicPath ConcreteAnchor t])
    traverseDeterministicPath nid = \case
      Pointlike p -> mapNonNull Pointlike <$> traversePointlike nid p
      Rooted r -> mapNonNull Rooted <$> traverseRooted nid r

    traversePointlike ::
      ( Members [GraphMetadataReading t, State (Graph t IsThin), State (Set NID)] q,
        HasCallStack
      ) =>
      ConcreteAnchor ->
      PointlikeDeterministicPath FullyAnchored t ->
      Sem q (NonNull [PointlikeDeterministicPath ConcreteAnchor t])
    traversePointlike canchor PointlikeDeterministicPath {..} = do
      let canchor' = case (canchor, anchor) of
            (UnsatisfiedLoops _, _) ->
              error "can't have UnsatisfiedLoops when traversing PointlikeDeterministicPath"
            (CSpecific nid1, FSpecific nid2)
              | nid1 == nid2 -> CSpecific nid1
              | otherwise -> NotInGraph
            (CSpecific nid1, FJoinPoint) -> CSpecific nid1
            (NotInGraph, FJoinPoint) -> NotInGraph
            (NotInGraph, FSpecific _) -> NotInGraph
      loops' <- traverseBranches canchor' loops
      let unsatisfiedAnchor = case canchor' of
            UnsatisfiedLoops _ ->
              error "can't have UnsatisfiedLoops when traversing PointlikeDeterministicPath"
            CSpecific nid -> UnsatisfiedLoops nid
            NotInGraph -> NotInGraph
      pure $
        loops'
          & nfilter ((== canchor') . snd)
          & map (PointlikeDeterministicPath canchor' . fst)
          & fromNullable
          & fromMaybe (mapNonNull (PointlikeDeterministicPath unsatisfiedAnchor . fst) loops')

    ensureSameAnchors ::
      (HasCallStack) => NonNull [(a, ConcreteAnchor)] -> (NonNull [a], ConcreteAnchor)
    ensureSameAnchors xs =
      let example = snd (head xs)
          as = mapNonNull fst xs
       in if all ((== example) . snd) $ toNullable xs
            then (as, example)
            else (as, NotInGraph)

    traverseRooted ::
      ( Members [GraphMetadataReading t, State (Graph t IsThin), State (Set NID)] q,
        HasCallStack
      ) =>
      ConcreteAnchor ->
      RootedDeterministicPath FullyAnchored t ->
      Sem q (NonNull [RootedDeterministicPath ConcreteAnchor t])
    traverseRooted nid RootedDeterministicPath {..} = do
      rootBranches' ::
        [ [ ( DeterministicPath ConcreteAnchor t,
              OSet (DPBranch FullyAnchored t)
            )
          ]
        ] <-
        rootBranches
          & mapToList
          & (traverse . _1) (traverseDeterministicPath nid)
          <&> map (\(rs, bs) -> (,bs) <$> toNullable rs)
          <&> choices
      rootBranches'' ::
        [ ( OMap
              (DeterministicPath ConcreteAnchor t)
              (OSet (DPBranch ConcreteAnchor t)),
            ConcreteAnchor
          )
        ] <-
        rootBranches'
          & (traverse . traverse)
            ( \(dp, bs) ->
                (dp,) <$> traverseBranches dp.target.anchor bs
            )
          <&> map (map \(r, bas) -> (\(b, a) -> ((r, b), a)) <$> toNullable bas)
          <&> concatMap choices
          <&> map (ensureSameAnchors . impureNonNull)
          <&> ordNub
          <&> over (mapped . _1) (mapFromList . toNullable)
      rootBranches''
        & (traverse . _2) (`traversePointlike` target)
        <&> concatMap (\(rbs, ts) -> (rbs,) <$> toNullable ts)
        <&> map (uncurry smartBuildRootedDeterministicPath)
        <&> impureNonNull

    traverseBranches ::
      ( Members [GraphMetadataReading t, State (Graph t IsThin), State (Set NID)] q,
        HasCallStack
      ) =>
      ConcreteAnchor ->
      OSet (DPBranch FullyAnchored t) ->
      Sem q (NonNull [(OSet (DPBranch ConcreteAnchor t), ConcreteAnchor)])
    traverseBranches nid =
      fmap
        ( impureNonNull
            . ordNub
            . over (mapped . _1) (setFromList . toNullable)
            . map (ensureSameAnchors . impureNonNull)
            . choices
            . map toNullable
        )
        . traverse (traverseBranch nid)
        . toList

    traverseBranch ::
      ( Members [GraphMetadataReading t, State (Graph t IsThin), State (Set NID)] q,
        HasCallStack
      ) =>
      ConcreteAnchor ->
      DPBranch FullyAnchored t ->
      Sem q (NonNull [(DPBranch ConcreteAnchor t, ConcreteAnchor)])
    traverseBranch anchor = \case
      DPLiteral t -> withEarlyReturn do
        nid <- case anchor of
          CSpecific n -> pure n
          UnsatisfiedLoops n -> pure n
          NotInGraph -> returnEarly (singletonNN (DPLiteral t, NotInGraph))
        n <- getNodeMetadata nid `onNothingM` returnEarly (singletonNN (DPLiteral t, NotInGraph))
        pure $
          n.outgoing
            & matchConnect t
            & map (\nid' -> (DPLiteral t, CSpecific nid'))
            & fromNullable
            & fromMaybe (singletonNN (DPLiteral t, NotInGraph))
      DPWild -> withEarlyReturn do
        nid <- case anchor of
          CSpecific n -> pure n
          UnsatisfiedLoops n -> pure n
          NotInGraph -> returnEarly (singletonNN (DPWild, NotInGraph))
        n <- getNodeMetadata nid `onNothingM` returnEarly (singletonNN (DPWild, NotInGraph))
        pure $
          n.outgoing
            & toList
            & map (\Connect {..} -> (DPLiteral transition, CSpecific node))
            & fromNullable
            & fromMaybe (singletonNN (DPWild, NotInGraph))
      DPSequence bs1 midpoint bs2 -> do
        bs1's <- traverseBranches anchor bs1 <&> toNullable
        uptoMidpoints :: [(OSet (DPBranch ConcreteAnchor t), PointlikeDeterministicPath ConcreteAnchor t)] <-
          bs1's
            & (traverse . _2) (`traversePointlike` midpoint)
            <&> concatMap (\(bs1', ms) -> (bs1',) <$> toNullable ms)
        uptoMidpoints
          & (traverse . _2) (\p -> (p,) <$> traverseBranches p.anchor bs2)
          <&> concatMap (\(bs1', (p, bs2'ts)) -> first (DPSequence bs1' p) <$> toNullable bs2'ts)
          <&> impureNonNull
