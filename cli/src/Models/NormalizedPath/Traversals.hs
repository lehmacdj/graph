{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

module Models.NormalizedPath.Traversals where

import Models.NormalizedPath.Types
import MyPrelude

traverseAnchors ::
  forall f a b.
  (Applicative f, Ord b, Ord a) =>
  (a -> f b) ->
  NormalizedPath a ->
  f (NormalizedPath b)
traverseAnchors f (NormalizedPath paths) =
  NormalizedPath . setFromList <$> traverse convertDeterministicPath (toList paths)
  where
    convertDeterministicPath :: DeterministicPath a -> f (DeterministicPath b)
    convertDeterministicPath = \case
      Rooted p -> Rooted <$> convertRooted p
      Pointlike p -> Pointlike <$> convertPointlike p

    convertRooted :: RootedDeterministicPath a -> f (RootedDeterministicPath b)
    convertRooted (RootedDeterministicPath rootBranches target) =
      smartBuildRootedDeterministicPath . mapFromList
        <$> traverse convertRootBranch (mapToList rootBranches)
        <*> convertPointlike target
      where
        convertRootBranch :: (DeterministicPath a, OSet (DPBranch a)) -> f (DeterministicPath b, OSet (DPBranch b))
        convertRootBranch (dp, branches) =
          (,)
            <$> convertDeterministicPath dp
            <*> (setFromList <$> traverse convertBranch (toList branches))

    convertPointlike :: PointlikeDeterministicPath a -> f (PointlikeDeterministicPath b)
    convertPointlike (PointlikeDeterministicPath anchor loops) =
      PointlikeDeterministicPath
        <$> f anchor
        <*> (setFromList <$> traverse convertBranch (toList loops))

    convertBranch :: DPBranch a -> f (DPBranch b)
    convertBranch = \case
      DPOutgoing t -> pure $ DPOutgoing t
      DPIncoming t -> pure $ DPIncoming t
      DPSequence as1 midpoint as2 ->
        smartBuildSequence . setFromList
          <$> traverse convertBranch (toList as1)
          <*> convertPointlike midpoint
          <*> (setFromList <$> traverse convertBranch (toList as2))

{-# HLINT ignore traverseViaTransitions "Functor law" #-}

-- | Traverse a normalized path by its transitions starting from a specified
-- anchor.
--
-- See resolveNPath/createNPath for examples that use this in different ways.
traverseViaTransitions ::
  forall f a b.
  (Monad f, Ord a, Ord b) =>
  -- | The initial anchor (type @b@) to start traversal from
  b ->
  -- | Anchor unification function.
  --
  -- Parameters:
  -- * @isRoot@ - whether this is the root anchor of the path
  -- * @current@ - the current anchor we've reached (type @b@)
  -- * @expected@ - the anchor specified in the path structure (type @a@)
  --
  -- Returns @Just@ the unified anchor if compatible, @Nothing@ if incompatible
  (Bool -> b -> a -> Maybe b) ->
  -- | Transition callback.
  --
  -- Parmameters:
  -- * @start@ - the anchor we're starting from after unification (type @b@)
  -- * @startAnchor@ - the starting anchor from the path structure (type @a@)
  -- * @direction@ - the direction and transition label to follow
  -- * @target@ - the anchor we should end up at (type @a@)
  --
  -- Returns a list of possible (direction, end) pairs representing successful
  -- traversals
  (b -> a -> DPDirection -> a -> f [(DPDirection, b)]) ->
  NormalizedPath a ->
  f (NormalizedPath b)
traverseViaTransitions initial fanchor ftransition normalizedPath =
  traverse (traverseDeterministicPath initial) (toList normalizedPath.union)
    <&> NormalizedPath
      . setFromList
      . concat
  where
    traverseDeterministicPath ::
      ( Applicative f,
        HasCallStack
      ) =>
      b ->
      DeterministicPath a ->
      f [DeterministicPath b]
    traverseDeterministicPath nid = \case
      Pointlike p -> map Pointlike <$> traversePointlike True nid p
      Rooted r -> map Rooted <$> traverseRooted nid r

    traversePointlike ::
      (HasCallStack) =>
      Bool ->
      b ->
      PointlikeDeterministicPath a ->
      f [PointlikeDeterministicPath b]
    traversePointlike isRoot nid PointlikeDeterministicPath {..} = do
      case fanchor isRoot nid anchor of
        Nothing -> pure []
        Just nid' -> do
          loops' <- traverseBranches nid' anchor anchor loops
          pure $
            loops'
              & filter ((== nid') . snd)
              & map (PointlikeDeterministicPath nid' . fst)

    ensureSameAnchors ::
      forall x. (HasCallStack) => NonNull [(x, b)] -> Maybe (NonNull [x], b)
    ensureSameAnchors xs =
      let example = snd (head xs)
          as = mapNonNull fst xs
       in justIfTrue
            (all ((== example) . snd) $ toNullable xs)
            (as, example)

    traverseRooted ::
      (HasCallStack) =>
      b ->
      RootedDeterministicPath a ->
      f [RootedDeterministicPath b]
    traverseRooted nid RootedDeterministicPath {..} = do
      rootBranches' ::
        [ [ ( DeterministicPath a,
              DeterministicPath b,
              OSet (DPBranch a)
            )
          ]
        ] <-
        rootBranches
          & mapToList
          & traverse
            ( \(dpa, bs) -> do
                dpbs <- traverseDeterministicPath nid dpa
                pure $ map (dpa,,bs) dpbs
            )
      rootBranches'' ::
        [(OMap (DeterministicPath b) (OSet (DPBranch b)), b)] <-
        rootBranches'
          & (traverse . traverse)
            ( \(dpa, dpb, bs) ->
                (dpb,) <$> traverseBranches (dpTarget dpb).anchor (dpTarget dpa).anchor target.anchor bs
            )
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
      (HasCallStack) =>
      b ->
      a ->
      a ->
      OSet (DPBranch a) ->
      f [(OSet (DPBranch b), b)]
    traverseBranches nid startAnchor target branches
      | -- in the case where the OSet is empty (only when loops),
        -- we return the same anchor as the input
        null branches =
          pure [(mempty, nid)]
      | otherwise = do
          branches' <- traverse (traverseBranch nid startAnchor target) (toList branches)
          pure $
            branches'
              & choices
              & mapMaybe (ensureSameAnchors . impureNonNull)
              & over (mapped . _1) (setFromList . toNullable)

    traverseBranch ::
      (HasCallStack) =>
      b ->
      a ->
      a ->
      DPBranch a ->
      f [(DPBranch b, b)]
    traverseBranch nid startAnchor target = \case
      DPSingle direction ->
        over (mapped . _1) DPSingle <$> ftransition nid startAnchor direction target
      DPSequence bs1 midpoint bs2 -> do
        bs1's <- traverseBranches nid startAnchor midpoint.anchor bs1
        uptoMidpoints :: [(OSet (DPBranch b), PointlikeDeterministicPath b)] <-
          bs1's
            & (traverse . _2) (\x -> traversePointlike False x midpoint)
            <&> concatMap (\(bs1', ms) -> (bs1',) <$> ms)
        uptoMidpoints
          & (traverse . _2) (\p -> (p,) <$> traverseBranches p.anchor midpoint.anchor target bs2)
          <&> concatMap (\(bs1', (p, bs2'ts)) -> first (DPSequence bs1' p) <$> bs2'ts)

-- | Like 'traverseViaTransitions', but only performs side effects via the
-- transition function and ignores the resulting/path.
traverseViaTransitions_ ::
  forall f a.
  (Monad f, Ord a) =>
  (a -> DPDirection -> a -> f ()) ->
  NormalizedPath a ->
  f ()
traverseViaTransitions_ ftransition normalizedPath =
  void $
    traverseViaTransitions
      ()
      (\_ _ _ -> Just ())
      (\_ a dir b -> ftransition a dir b $> [(dir, ())])
      normalizedPath
