{-# LANGUAGE ApplicativeDo #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

module Models.NormalizedPath where

import Data.Monoid (Sum (..))
import GHC.Records
import Models.NID
import Models.Path.Simple
import MyPrelude

data Anchor
  = Unanchored
  | JoinPoint {excluding :: Set NID}
  | Specific NID
  deriving stock (Eq, Ord, Show, Generic, Lift)
  deriving anyclass (NFData)

data FullyAnchored
  = FJoinPoint {excluding :: Set NID}
  | FSpecific NID
  deriving stock (Eq, Ord, Show, Generic, Lift)
  deriving anyclass (NFData)

data DeterministicPath a
  = Rooted (RootedDeterministicPath a)
  | Pointlike (PointlikeDeterministicPath a)
  deriving stock (Eq, Ord, Show, Generic, Lift)
  deriving anyclass (NFData)

instance HasField "target" (DeterministicPath a) (PointlikeDeterministicPath a) where
  getField (Rooted p) = p.target
  getField (Pointlike p) = p

-- | Represents a path that targets a single node (i.e. is deterministic)
--
-- Invariants:
-- - the sets of branches in rootBranches must be nonempty
-- - a key in rootBranches should only be Rooted if the nested rootBranches has
--   at least two distinct roots (otherwise it should be Pointlike and the
--   branches should go in a DPSequence in the branch set)
data RootedDeterministicPath a = RootedDeterministicPath
  { rootBranches :: OMap (DeterministicPath a) (OSet (DPBranch a)),
    target :: PointlikeDeterministicPath a
  }
  deriving stock (Eq, Ord, Show, Generic, Lift)
  deriving anyclass (NFData)

-- | A pointlike deterministic path. This is a path that has a single Root
-- and the target is the same as the Root.
data PointlikeDeterministicPath a
  = PointlikeDeterministicPath
  { -- | this anchor can only be unanchored if the loops set is empty
    anchor :: a,
    -- | each of these can also be inverted, see `invertLoop` for details
    loops :: OSet (DPBranch a)
  }
  deriving stock (Eq, Ord, Show, Generic, Lift)
  deriving anyclass (NFData)

unanchored :: PointlikeDeterministicPath Anchor
unanchored = PointlikeDeterministicPath Unanchored mempty

joinPoint :: PointlikeDeterministicPath Anchor
joinPoint = PointlikeDeterministicPath (JoinPoint mempty) mempty

specific :: NID -> PointlikeDeterministicPath Anchor
specific nid = PointlikeDeterministicPath (Specific nid) mempty

-- | Type of transition (literal or wild).
-- TODO: give this a similar trees that grow treatment to Models.Path
-- We can eliminate the invariant that NormalizedPath NID doesn't contain
-- DPWild/DPRegex if we do that, or alternately include the original transition
-- but include the capture groups/matched transitions in the result
data DPTransition
  = DPLiteral Text
  | DPWild
  | DPRegex CheckedRegex
  deriving stock (Eq, Ord, Show, Generic, Lift)
  deriving anyclass (NFData)

data DPDirection
  = DPIncoming' DPTransition
  | DPOutgoing' DPTransition
  deriving stock (Eq, Ord, Show, Generic, Lift)
  deriving anyclass (NFData)

pattern DPIncoming :: DPTransition -> DPBranch a
pattern DPIncoming t = DPSingle (DPIncoming' t)

pattern DPOutgoing :: DPTransition -> DPBranch a
pattern DPOutgoing t = DPSingle (DPOutgoing' t)

{-# COMPLETE DPIncoming, DPOutgoing, DPSequence #-}

-- | A branch in the path.
data DPBranch a
  = DPSingle DPDirection
  | -- | A concatenation of two intersections of branches.
    --
    -- Invariants:
    -- - both intersections are nonempty
    -- - sequences of non-intersected branches are right associated (e.g.
    --   @Sequence (singleton a) m (Sequence (singleton b) n (singleton c))@
    --   instead of
    --   @Sequence (Sequence (singleton a) m (singleton b)) n (singleton c)@)
    DPSequence
      (OSet (DPBranch a))
      (PointlikeDeterministicPath a)
      (OSet (DPBranch a))
  deriving stock (Eq, Ord, Show, Generic, Lift)
  deriving anyclass (NFData)

newtype NormalizedPath a = NormalizedPath {union :: Set (DeterministicPath a)}
  deriving stock (Eq, Ord, Show, Generic, Lift)
  deriving anyclass (NFData)

pointify ::
  RootedDeterministicPath Anchor ->
  Maybe (PointlikeDeterministicPath Anchor)
pointify (branchify -> (roots, branches, target)) = do
  protoPoint <- foldlM1 mergePointlike (target `ncons` toList roots)
  Just
    protoPoint
      { loops = protoPoint.loops <> branches,
        anchor = fromJustEx $ mergeAnchor (JoinPoint mempty) protoPoint.anchor
      }

-- | Split the root off each Rooted key in the rootBranches and convert the
-- corresponding rootBranch into a DPSequence.
-- This essentially "reassociates" the rootBranches so that we have a
-- set of all of the roots and a set of all of the branches.
-- We do this before merging the roots to a single root when
-- sequencing two Rooted paths.
branchify ::
  RootedDeterministicPath Anchor ->
  ( OSet (PointlikeDeterministicPath Anchor),
    OSet (DPBranch Anchor),
    PointlikeDeterministicPath Anchor
  )
branchify RootedDeterministicPath {..} =
  let (rooteds, pointlikes) = partitionRootedPointlike (mapToList rootBranches)
      (rootedRoots, rootedBranches) =
        rooteds
          & over (mapped . _1) branchify
          & map convertToBranches
          & unzip
          & bimap unions unions
      (pointlikeRoots, pointlikeBranches) =
        pointlikes
          & unzip
          & bimap setFromList unions
   in ( pointlikeRoots <> rootedRoots,
        pointlikeBranches <> rootedBranches,
        target
      )
  where
    convertToBranches ::
      ( ( OSet (PointlikeDeterministicPath Anchor),
          OSet (DPBranch Anchor),
          PointlikeDeterministicPath Anchor
        ),
        OSet (DPBranch Anchor)
      ) ->
      ( OSet (PointlikeDeterministicPath Anchor),
        OSet (DPBranch Anchor)
      )
    convertToBranches ((w, x, y), z) =
      (w, singletonSet $ smartBuildSequence x y z)
    partitionRootedPointlike ::
      [(DeterministicPath a, OSet (DPBranch a))] ->
      ( [(RootedDeterministicPath a, OSet (DPBranch a))],
        [(PointlikeDeterministicPath a, OSet (DPBranch a))]
      )
    partitionRootedPointlike =
      partitionEithers . map \case
        (Rooted r, x) -> Left (r, x)
        (Pointlike p, x) -> Right (p, x)

intersectDeterministicPaths ::
  DeterministicPath Anchor ->
  DeterministicPath Anchor ->
  Maybe (DeterministicPath Anchor)
intersectDeterministicPaths (Rooted p1) (Rooted p2) = do
  newTarget <- mergePointlike p1.target p2.target
  let newSourceBranches = unionWith (<>) p1.rootBranches p2.rootBranches
  Just . Rooted $ smartBuildRootedDeterministicPath newSourceBranches newTarget
intersectDeterministicPaths (Pointlike p1) (Pointlike p2) = do
  Pointlike <$> mergePointlike p1 p2
intersectDeterministicPaths (Rooted (pointify -> maybeP1)) (Pointlike p2) = do
  p1 <- maybeP1
  Pointlike <$> mergePointlike p1 p2
intersectDeterministicPaths (Pointlike p1) (Rooted (pointify -> maybeP2)) = do
  p2 <- maybeP2
  Pointlike <$> mergePointlike p1 p2

-- | Join on the semi-lattice of anchors. Every Specific NID is a top element
mergeAnchor :: Anchor -> Anchor -> Maybe Anchor
mergeAnchor Unanchored a = Just a
mergeAnchor a Unanchored = Just a
mergeAnchor JoinPoint {..} (Specific nid)
  | nid `notMember` excluding = Just (Specific nid)
  | otherwise = Nothing
mergeAnchor (Specific nid) JoinPoint {..}
  | nid `notMember` excluding = Just (Specific nid)
  | otherwise = Nothing
mergeAnchor JoinPoint {excluding = e1} JoinPoint {excluding = e2} =
  Just $ JoinPoint (e1 `union` e2)
mergeAnchor (Specific nid1) (Specific nid2)
  | nid1 == nid2 = Just (Specific nid1)
  | otherwise = Nothing

-- | Merge two PointlikeDeterministicPaths by combining their loops and
-- merging their anchors.
mergePointlike ::
  PointlikeDeterministicPath Anchor ->
  PointlikeDeterministicPath Anchor ->
  Maybe (PointlikeDeterministicPath Anchor)
mergePointlike p1 p2 = do
  mergedAnchor <- mergeAnchor p1.anchor p2.anchor
  Just $
    PointlikeDeterministicPath
      { anchor = mergedAnchor,
        loops = mapOSet isoLoop (p1.loops <> p2.loops)
      }

smartBuildSequence ::
  (HasCallStack, Ord a) =>
  OSet (DPBranch a) ->
  PointlikeDeterministicPath a ->
  OSet (DPBranch a) ->
  DPBranch a
smartBuildSequence as1 midpoint bs2
  | null as1 || null bs2 =
      error "invariant broken: both branch sets must be nonempty"
  | otherwise = case toList as1 of
      [DPSequence leftBs leftMid leftRightBs] ->
        DPSequence leftBs leftMid . singletonSet $
          smartBuildSequence leftRightBs midpoint bs2
      _ -> DPSequence as1 midpoint bs2

smartBuildRootedDeterministicPath ::
  (HasCallStack, Ord a) =>
  OMap (DeterministicPath a) (OSet (DPBranch a)) ->
  PointlikeDeterministicPath a ->
  RootedDeterministicPath a
smartBuildRootedDeterministicPath rootBranches target
  | null rootBranches =
      error "invariant broken: rootBranches must be nonempty"
  | otherwise = case mapToList rootBranches of
      [(r@(Rooted (RootedDeterministicPath (length -> 1) _)), b)] ->
        smartBuildRootedDeterministicPath
          (uncurry singletonMap (smartBuildRootBranch r b))
          target
      _ -> RootedDeterministicPath rootBranches target

smartBuildRootBranch ::
  (HasCallStack, Ord a) =>
  DeterministicPath a ->
  OSet (DPBranch a) ->
  ( DeterministicPath a,
    OSet (DPBranch a)
  )
smartBuildRootBranch
  (Rooted (RootedDeterministicPath (mapToList -> [(dp, branches)]) midpoint))
  branchExtensions =
    (dp, singletonSet $ smartBuildSequence branches midpoint branchExtensions)
smartBuildRootBranch dp branches = (dp, branches)

sequenceDeterministicPaths ::
  DeterministicPath Anchor ->
  DeterministicPath Anchor ->
  Maybe (DeterministicPath Anchor)
sequenceDeterministicPaths (Pointlike p1) (Pointlike p2) =
  Pointlike <$> mergePointlike p1 p2
sequenceDeterministicPaths
  (Pointlike p1)
  (Rooted (branchify -> (roots, branches, target))) = do
    newRoot <- foldlM1 mergePointlike (p1 `ncons` toList roots)
    Just $
      Rooted $
        smartBuildRootedDeterministicPath
          (singletonMap (Pointlike newRoot) branches)
          target
sequenceDeterministicPaths (Rooted p1) (Pointlike p2) = do
  newTarget <- mergePointlike p1.target p2
  Just $ Rooted $ p1 {target = newTarget}
sequenceDeterministicPaths
  (Rooted p1)
  (Rooted (branchify -> (roots, p2Branches, p2Target))) = do
    midpoint <- foldlM1 mergePointlike (p1.target `ncons` toList roots)
    Just . Rooted $
      smartBuildRootedDeterministicPath
        (singletonMap (Rooted p1 {target = midpoint}) p2Branches)
        p2Target

invertBranch :: (Ord a) => DPBranch a -> DPBranch a
invertBranch = \case
  DPOutgoing trans -> DPIncoming trans
  DPIncoming trans -> DPOutgoing trans
  DPSequence as1 midpoint bs2 ->
    DPSequence
      (mapOSet invertBranch as1)
      (invertPointlike midpoint)
      (mapOSet invertBranch bs2)

flipBranch :: (Ord a) => DPBranch a -> DPBranch a
flipBranch = \case
  DPOutgoing trans -> DPOutgoing trans
  DPIncoming trans -> DPIncoming trans
  DPSequence as1 m12 bs23 ->
    case flipBranch <$> toList bs23 of
      [] -> error "invariant broken: both branch sets must be nonempty"
      [DPSequence bs2 m23 bs3] ->
        smartBuildSequence bs3 (invertPointlike m23) $
          singletonSet (smartBuildSequence bs2 (invertPointlike m12) as1)
      xs -> smartBuildSequence (setFromList xs) (invertPointlike m12) as1

backwardsCount :: DPBranch a -> Int
backwardsCount = \case
  DPOutgoing _ -> 0
  DPIncoming _ -> 1
  DPSequence as1 _ bs2 ->
    alaf Sum foldMap backwardsCount as1
      + alaf Sum foldMap backwardsCount bs2

-- | There's two valid ways to flip a branch if it's the loop of a pointlike
-- path: invert (like we do for Rooted paths) or flip the sequence so the last
-- transition becomes the first. We need both, so that we can compare &
-- determine one (relatively arbitrary) cannonical form
invertLoop :: (Ord a) => DPBranch a -> DPBranch a
invertLoop =
  minOn (\x -> (backwardsCount x, x))
    <$> invertBranch
    <*> flipBranch

isoLoop :: (Ord a) => DPBranch a -> DPBranch a
isoLoop =
  minOn (\x -> (backwardsCount x, x))
    <$> id
    <*> invertBranch . flipBranch

worseLoops :: (Ord a) => DPBranch a -> DPBranch a
worseLoops =
  maxOn (\x -> (backwardsCount x, x))
    <$> id
    <*> invertBranch . flipBranch

invertPointlike ::
  (Ord a) =>
  PointlikeDeterministicPath a ->
  PointlikeDeterministicPath a
invertPointlike (PointlikeDeterministicPath anchor loops) =
  PointlikeDeterministicPath anchor (mapOSet invertLoop loops)

invertDeterministicPath ::
  DeterministicPath Anchor -> Maybe (DeterministicPath Anchor)
invertDeterministicPath (Pointlike p) = Just . Pointlike $ invertPointlike p
invertDeterministicPath
  (Rooted (branchify -> (roots, branches, target))) = do
    newTarget <- foldlM1 mergePointlike =<< fromNullable (toList roots)
    let newBranches = mapOSet invertBranch branches
    Just . Rooted $
      smartBuildRootedDeterministicPath
        (singletonMap (Pointlike target) newBranches)
        newTarget

normalizePath :: Path -> NormalizedPath Anchor
normalizePath = \case
  Zero ->
    NormalizedPath mempty
  One ->
    NormalizedPath . singletonSet . Pointlike $ joinPoint
  Absolute nid ->
    NormalizedPath . singletonSet . Pointlike $ specific nid
  ExcludingNIDs nids ->
    NormalizedPath . singletonSet . Pointlike $
      PointlikeDeterministicPath (JoinPoint nids) mempty
  Wild ->
    NormalizedPath . singletonSet . Rooted $
      smartBuildRootedDeterministicPath
        (singletonMap (Pointlike unanchored) (singletonSet (DPOutgoing DPWild)))
        unanchored
  Literal t ->
    NormalizedPath . singletonSet . Rooted $
      smartBuildRootedDeterministicPath
        (singletonMap (Pointlike unanchored) (singletonSet (DPOutgoing (DPLiteral t))))
        unanchored
  RegexMatch r ->
    NormalizedPath . singletonSet . Rooted $
      smartBuildRootedDeterministicPath
        (singletonMap (Pointlike unanchored) (singletonSet (DPOutgoing (DPRegex r))))
        unanchored
  Backwards p ->
    let np = normalizePath p
     in NormalizedPath $
          np.union
            & toList
            & mapMaybe invertDeterministicPath
            & setFromList
  p1 :+ p2 ->
    let np1 = normalizePath p1
        np2 = normalizePath p2
     in NormalizedPath $ np1.union <> np2.union
  p1 :& p2 ->
    let np1 = normalizePath p1
        np2 = normalizePath p2
     in cartesianProductSet np1.union np2.union
          & asSet
          & toList
          -- maybe better to handle errors here than to just ignore them?
          & mapMaybe (uncurry intersectDeterministicPaths)
          & setFromList
          & NormalizedPath
  p1 :/ p2 ->
    let np1 = normalizePath p1
        np2 = normalizePath p2
     in cartesianProductSet np1.union np2.union
          & asSet
          & toList
          & mapMaybe (uncurry sequenceDeterministicPaths)
          & setFromList
          & NormalizedPath

-- | Expand all Unanchored anchors to produce the least constrained path
-- possible. This may result in a very large number of paths
-- (e.g. @(a & b)/(c & d) => (a/c & a/d & b/c & b/d)@).
leastConstrainedNormalizedPath ::
  NormalizedPath Anchor ->
  NormalizedPath FullyAnchored
leastConstrainedNormalizedPath =
  over (_Unwrapped . setmapped) convertDeterministicPath
  where
    convertDeterministicPath = \case
      Rooted p -> Rooted $ convertRooted p
      Pointlike p -> Pointlike $ convertPointlike p

    convertRooted ::
      (HasCallStack) =>
      RootedDeterministicPath Anchor ->
      RootedDeterministicPath FullyAnchored
    convertRooted (RootedDeterministicPath rootBranches target) =
      let newTarget = convertPointlike target
          newRootBranches =
            rootBranches
              & mapToList
              & concatMap explodeUnanchored
              & map (uncurry singletonMap)
              & unionsWith (<>)
       in smartBuildRootedDeterministicPath
            newRootBranches
            newTarget

    explodeUnanchored ::
      (DeterministicPath Anchor, OSet (DPBranch Anchor)) ->
      [(DeterministicPath FullyAnchored, OSet (DPBranch FullyAnchored))]
    explodeUnanchored = \case
      ( Rooted
          rdp@RootedDeterministicPath {target = PointlikeDeterministicPath {anchor = Unanchored}},
        branchExtensions
        ) ->
          let RootedDeterministicPath {..} = convertRooted rdp
              rootBranches' :: [(DeterministicPath FullyAnchored, DPBranch FullyAnchored)] =
                rootBranches
                  & mapToList
                  & over (mapped . _2) toList
                  & concatMap (\(root, bs) -> map (root,) bs)
              branchExtensions' :: OSet (DPBranch FullyAnchored) = unions (mapOSet convertBranch branchExtensions)
           in (rootBranches' `cartesianProduct` toList branchExtensions')
                & map
                  ( \((root, b1), b2) ->
                      smartBuildRootBranch
                        root
                        ( singletonSet $
                            smartBuildSequence
                              (singletonSet b1)
                              (PointlikeDeterministicPath (FJoinPoint mempty) mempty)
                              (singletonSet b2)
                        )
                  )
      (dp, bs) ->
        singleton $
          smartBuildRootBranch
            (convertDeterministicPath dp)
            (unions $ mapOSet convertBranch bs)

    convertAnchor = \case
      Unanchored -> FJoinPoint mempty
      JoinPoint {..} -> FJoinPoint {..}
      Specific nid -> FSpecific nid

    convertPointlike (PointlikeDeterministicPath Unanchored (toList -> (_ : _))) =
      error "convertPointlike: Unanchored path with loops"
    convertPointlike (PointlikeDeterministicPath anchor loops) =
      PointlikeDeterministicPath
        (convertAnchor anchor)
        (unions (mapOSet convertBranch loops))

    convertBranch ::
      (HasCallStack) =>
      DPBranch Anchor ->
      OSet (DPBranch FullyAnchored)
    convertBranch = \case
      DPOutgoing t -> singletonSet $ DPOutgoing t
      DPIncoming t -> singletonSet $ DPIncoming t
      DPSequence as1 midpoint bs2 -> do
        let as1' = unions (mapOSet convertBranch as1)
        let bs2' = unions (mapOSet convertBranch bs2)
        case midpoint.anchor of
          Unanchored
            | null midpoint.loops ->
                mapOSet
                  ( \(x, y) ->
                      smartBuildSequence
                        (singletonSet x)
                        (convertPointlike joinPoint)
                        (singletonSet y)
                  )
                  (as1' `cartesianProductSet` bs2')
            | otherwise -> error "broken invariant: Unanchored path with loops"
          _ -> singletonSet $ DPSequence as1' (convertPointlike midpoint) bs2'

-- | Assign one JoinPoint to each Unanchored anchor
leastNodesNormalizedPath ::
  NormalizedPath Anchor ->
  NormalizedPath FullyAnchored
leastNodesNormalizedPath path =
  runIdentity $ traverseAnchors (Identity . fullyAnchor) path

fullyAnchor :: Anchor -> FullyAnchored
fullyAnchor = \case
  Unanchored -> FJoinPoint mempty
  JoinPoint {..} -> FJoinPoint {..}
  Specific nid -> FSpecific nid

fullySpecific :: Anchor -> Maybe NID
fullySpecific = \case
  Unanchored -> Nothing
  JoinPoint {} -> Nothing
  Specific nid -> Just nid

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
    <&> NormalizedPath . setFromList . concat
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
          & traverse (\(dpa, bs) -> do
              dpbs <- traverseDeterministicPath nid dpa
              pure $ map (dpa, , bs) dpbs)
      rootBranches'' ::
        [(OMap (DeterministicPath b) (OSet (DPBranch b)), b)] <-
        rootBranches'
          & (traverse . traverse) (\(dpa, dpb, bs) ->
              (dpb,) <$> traverseBranches dpb.target.anchor dpa.target.anchor target.anchor bs)
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
