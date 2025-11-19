module Models.NormalizedPath.Normalize where

import Data.Monoid (Sum (..))
import Models.NormalizedPath.Anchor
import Models.NormalizedPath.Types
import Models.Path.Simple
import MyPrelude

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
