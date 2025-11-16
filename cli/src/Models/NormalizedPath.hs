{-# LANGUAGE UndecidableInstances #-}

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
  deriving stock (Eq, Ord, Show, Generic)

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
  { rootBranches ::
      NonNull
        (OMap
          (DeterministicPath a)
          (NonNull (OSet (DPBranch a)))
        ),
    target :: PointlikeDeterministicPath a
  }
  deriving stock (Eq, Ord, Show, Generic)

-- | A pointlike deterministic path. This is a path that has a single Root
-- and the target is the same as the Root.
data PointlikeDeterministicPath a
  = PointlikeDeterministicPath
  { -- | this anchor can only be unanchored if the loops set is empty
    anchor :: a,
    -- | each of these can also be inverted, see `invertLoop` for details
    loops :: OSet (DPBranch a)
  }
  deriving stock (Eq, Ord, Show, Generic)

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

-- | A branch in the path.
data DPBranch a
  = DPIncoming DPTransition
  | DPOutgoing DPTransition
  | -- | A concatenation of two intersections of branches.
    --
    -- Invariants:
    -- - both intersections are nonempty
    -- - sequences of non-intersected branches are right associated (e.g.
    --   @Sequence (singleton a) m (Sequence (singleton b) n (singleton c))@
    --   instead of
    --   @Sequence (Sequence (singleton a) m (singleton b)) n (singleton c)@)
    DPSequence
      (NonNull (OSet (DPBranch a)))
      (PointlikeDeterministicPath a)
      (NonNull (OSet (DPBranch a)))
  deriving stock (Eq, Ord, Show, Generic)

newtype NormalizedPath a = NormalizedPath {union :: Set (DeterministicPath a)}
  deriving stock (Eq, Ord, Show, Generic)

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
  ( NonNull (OSet (PointlikeDeterministicPath Anchor)),
    NonNull (OSet (DPBranch Anchor)),
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
   in ( impureNonNull $ pointlikeRoots <> rootedRoots,
        impureNonNull $ pointlikeBranches <> rootedBranches,
        target
      )
  where
    convertToBranches ::
      ( ( NonNull (OSet (PointlikeDeterministicPath Anchor)),
          NonNull (OSet (DPBranch Anchor)),
          PointlikeDeterministicPath Anchor
        ),
        NonNull (OSet (DPBranch Anchor))
      ) ->
      ( OSet (PointlikeDeterministicPath Anchor),
        NonNull (OSet (DPBranch Anchor))
      )
    convertToBranches ((w, x, y), z) =
      (toNullable w, singletonNNSet $ smartBuildSequence x y z)
    partitionRootedPointlike ::
      [(DeterministicPath a, NonNull (OSet (DPBranch a)))] ->
      ( [(RootedDeterministicPath a, NonNull (OSet (DPBranch a)))],
        [(PointlikeDeterministicPath a, NonNull (OSet (DPBranch a)))]
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
  NonNull (OSet (DPBranch a)) ->
  PointlikeDeterministicPath a ->
  NonNull (OSet (DPBranch a)) ->
  DPBranch a
smartBuildSequence as1 midpoint bs2 = case toList as1 of
  [DPSequence leftBs leftMid leftRightBs] ->
    DPSequence leftBs leftMid $
      singletonNNSet $
        smartBuildSequence leftRightBs midpoint bs2
  _ -> DPSequence as1 midpoint bs2

smartBuildRootedDeterministicPath ::
  (HasCallStack, Ord a) =>
  NonNull (OMap (DeterministicPath a) (NonNull (OSet (DPBranch a)))) ->
  PointlikeDeterministicPath a ->
  RootedDeterministicPath a
smartBuildRootedDeterministicPath rootBranches target =
  case mapToList rootBranches of
    [(r@(Rooted (RootedDeterministicPath (olength -> 1) _)), b)] ->
      let (dp, branches) = smartBuildRootBranch r b
       in smartBuildRootedDeterministicPath
            (singletonNNMap dp branches)
            target
    _ -> RootedDeterministicPath rootBranches target

smartBuildRootBranch ::
  (HasCallStack, Ord a) =>
  DeterministicPath a ->
  NonNull (OSet (DPBranch a)) ->
  ( DeterministicPath a,
    NonNull (OSet (DPBranch a))
  )
smartBuildRootBranch
  (Rooted (RootedDeterministicPath (mapToList -> [(dp, branches)]) midpoint))
  branchExtensions =
    (dp, singletonNNSet $ smartBuildSequence branches midpoint branchExtensions)
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
          (singletonNNMap (Pointlike newRoot) branches)
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
        (singletonNNMap (Rooted p1 {target = midpoint}) p2Branches)
        p2Target

invertBranch :: (Ord a) => DPBranch a -> DPBranch a
invertBranch = \case
  DPOutgoing trans -> DPIncoming trans
  DPIncoming trans -> DPOutgoing trans
  DPSequence as1 midpoint bs2 ->
    DPSequence
      (mapOSetNN invertBranch as1)
      (invertPointlike midpoint)
      (mapOSetNN invertBranch bs2)

flipBranch :: (Ord a) => DPBranch a -> DPBranch a
flipBranch = \case
  DPOutgoing trans -> DPOutgoing trans
  DPIncoming trans -> DPIncoming trans
  DPSequence as1 m12 bs23 ->
    case flipBranch <$> toList bs23 of
      [DPSequence bs2 m23 bs3] ->
        smartBuildSequence bs3 (invertPointlike m23) $
          singletonNNSet (smartBuildSequence bs2 (invertPointlike m12) as1)
      xs -> smartBuildSequence (impureNonNull $ setFromList xs) (invertPointlike m12) as1

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
    let newBranches = mapOSetNN invertBranch branches
    Just . Rooted $
      smartBuildRootedDeterministicPath
        (singletonNNMap (Pointlike target) newBranches)
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
        (singletonNNMap (Pointlike unanchored) (singletonNNSet (DPOutgoing DPWild)))
        unanchored
  Literal t ->
    NormalizedPath . singletonSet . Rooted $
      smartBuildRootedDeterministicPath
        (singletonNNMap (Pointlike unanchored) (singletonNNSet (DPOutgoing (DPLiteral t))))
        unanchored
  RegexMatch r ->
    NormalizedPath . singletonSet . Rooted $
      smartBuildRootedDeterministicPath
        (singletonNNMap (Pointlike unanchored) (singletonNNSet (DPOutgoing (DPRegex r))))
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
            (impureNonNull newRootBranches)
            newTarget

    explodeUnanchored ::
      (DeterministicPath Anchor, NonNull (OSet (DPBranch Anchor))) ->
      [(DeterministicPath FullyAnchored, NonNull (OSet (DPBranch FullyAnchored)))]
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
                        ( singletonNNSet $
                            smartBuildSequence
                              (singletonNNSet b1)
                              (PointlikeDeterministicPath (FJoinPoint mempty) mempty)
                              (singletonNNSet b2)
                        )
                  )
      (dp, bs) ->
        singleton $
          smartBuildRootBranch
            (convertDeterministicPath dp)
            (impureNonNull $ unions $ mapOSet convertBranch bs)

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
                        (singletonNNSet x)
                        (convertPointlike joinPoint)
                        (singletonNNSet y)
                  )
                  (as1' `cartesianProductSet` bs2')
            | otherwise -> error "broken invariant: Unanchored path with loops"
          _ -> singletonSet $ DPSequence (impureNonNull as1') (convertPointlike midpoint) (impureNonNull bs2')

-- | Assign one JoinPoint to each Unanchored anchor
leastNodesNormalizedPath ::
  NormalizedPath Anchor ->
  NormalizedPath FullyAnchored
leastNodesNormalizedPath path =
  runIdentity $ traverseAnchors (Identity . convertAnchor) path
  where
    convertAnchor :: Anchor -> FullyAnchored
    convertAnchor = \case
      Unanchored -> FJoinPoint mempty
      JoinPoint {..} -> FJoinPoint {..}
      Specific nid -> FSpecific nid

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
      smartBuildRootedDeterministicPath . impureNonNull . mapFromList
        <$> traverse convertRootBranch (mapToList rootBranches)
        <*> convertPointlike target
      where
        convertRootBranch :: (DeterministicPath a, NonNull (OSet (DPBranch a))) -> f (DeterministicPath b, NonNull (OSet (DPBranch b)))
        convertRootBranch (dp, branches) =
          (,)
            <$> convertDeterministicPath dp
            <*> traverseNonNull convertBranch branches

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
        smartBuildSequence
          <$> traverseNonNull convertBranch as1
          <*> convertPointlike midpoint
          <*> traverseNonNull convertBranch as2
