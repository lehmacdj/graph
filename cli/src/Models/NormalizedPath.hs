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

data DeterministicPath a t
  = Rooted (RootedDeterministicPath a t)
  | Pointlike (PointlikeDeterministicPath a t)
  deriving stock (Eq, Ord, Show, Generic, Lift)
  deriving anyclass (NFData)

instance HasField "target" (DeterministicPath a t) (PointlikeDeterministicPath a t) where
  getField (Rooted p) = p.target
  getField (Pointlike p) = p

-- | Represents a path that targets a single node (i.e. is deterministic)
--
-- Invariants:
-- - the sets of branches in rootBranches must be nonempty
-- - a key in rootBranches should only be Rooted if the nested rootBranches has
--   at least two distinct roots (otherwise it should be Pointlike and the
--   branches should go in a DPSequence in the branch set)
data RootedDeterministicPath a t = RootedDeterministicPath
  { rootBranches :: OMap (DeterministicPath a t) (OSet (DPBranch a t)),
    target :: PointlikeDeterministicPath a t
  }
  deriving stock (Eq, Ord, Show, Generic, Lift)
  deriving anyclass (NFData)

-- | A pointlike deterministic path. This is a path that has a single Root
-- and the target is the same as the Root.
data PointlikeDeterministicPath a t
  = PointlikeDeterministicPath
  { -- | this anchor can only be unanchored if the loops set is empty
    anchor :: a,
    -- | each of these can also be inverted, see `invertLoop` for details
    loops :: OSet (DPBranch a t)
  }
  deriving stock (Eq, Ord, Show, Generic, Lift)
  deriving anyclass (NFData)

unanchored :: (Ord t) => PointlikeDeterministicPath Anchor t
unanchored = PointlikeDeterministicPath Unanchored mempty

joinPoint :: (Ord t) => PointlikeDeterministicPath Anchor t
joinPoint = PointlikeDeterministicPath (JoinPoint mempty) mempty

specific :: (Ord t) => NID -> PointlikeDeterministicPath Anchor t
specific nid = PointlikeDeterministicPath (Specific nid) mempty

-- | Type of transition (literal or wild).
-- TODO: give this a similar trees that grow treatment to Models.Path
-- We can eliminate the invariant that NormalizedPath NID doesn't contain
-- DPWild/DPRegex if we do that, or alternately include the original transition
-- but include the capture groups/matched transitions in the result
data DPTransition t
  = DPLiteral t
  | DPWild
  | DPRegex CheckedRegex
  deriving stock (Eq, Ord, Show, Generic, Lift)
  deriving anyclass (NFData)

-- | A branch in the path.
data DPBranch a t
  = DPIncoming (DPTransition t)
  | DPOutgoing (DPTransition t)
  | -- | A concatenation of two intersections of branches.
    --
    -- Invariants:
    -- - both intersections are nonempty
    -- - sequences of non-intersected branches are right associated (e.g.
    --   @Sequence (singleton a) m (Sequence (singleton b) n (singleton c))@
    --   instead of
    --   @Sequence (Sequence (singleton a) m (singleton b)) n (singleton c)@)
    DPSequence
      (OSet (DPBranch a t))
      (PointlikeDeterministicPath a t)
      (OSet (DPBranch a t))
  deriving stock (Eq, Ord, Show, Generic, Lift)
  deriving anyclass (NFData)

newtype NormalizedPath a t = NormalizedPath {union :: Set (DeterministicPath a t)}
  deriving stock (Eq, Ord, Show, Generic, Lift)
  deriving anyclass (NFData)

pointify ::
  (Ord t) =>
  RootedDeterministicPath Anchor t ->
  Maybe (PointlikeDeterministicPath Anchor t)
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
  forall t.
  (Ord t) =>
  RootedDeterministicPath Anchor t ->
  ( OSet (PointlikeDeterministicPath Anchor t),
    OSet (DPBranch Anchor t),
    PointlikeDeterministicPath Anchor t
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
      ( ( OSet (PointlikeDeterministicPath Anchor t),
          OSet (DPBranch Anchor t),
          PointlikeDeterministicPath Anchor t
        ),
        OSet (DPBranch Anchor t)
      ) ->
      ( OSet (PointlikeDeterministicPath Anchor t),
        OSet (DPBranch Anchor t)
      )
    convertToBranches ((w, x, y), z) =
      (w, singletonSet $ smartBuildSequence x y z)
    partitionRootedPointlike ::
      (Ord t) =>
      [(DeterministicPath a t, OSet (DPBranch a t))] ->
      ( [(RootedDeterministicPath a t, OSet (DPBranch a t))],
        [(PointlikeDeterministicPath a t, OSet (DPBranch a t))]
      )
    partitionRootedPointlike =
      partitionEithers . map \case
        (Rooted r, x) -> Left (r, x)
        (Pointlike p, x) -> Right (p, x)

intersectDeterministicPaths ::
  (Ord t) =>
  DeterministicPath Anchor t ->
  DeterministicPath Anchor t ->
  Maybe (DeterministicPath Anchor t)
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
  (Ord t) =>
  PointlikeDeterministicPath Anchor t ->
  PointlikeDeterministicPath Anchor t ->
  Maybe (PointlikeDeterministicPath Anchor t)
mergePointlike p1 p2 = do
  mergedAnchor <- mergeAnchor p1.anchor p2.anchor
  Just $
    PointlikeDeterministicPath
      { anchor = mergedAnchor,
        loops = mapOSet isoLoop (p1.loops <> p2.loops)
      }

smartBuildSequence ::
  (HasCallStack, Ord a, Ord t) =>
  OSet (DPBranch a t) ->
  PointlikeDeterministicPath a t ->
  OSet (DPBranch a t) ->
  DPBranch a t
smartBuildSequence as1 midpoint bs2
  | null as1 || null bs2 =
      error "invariant broken: both branch sets must be nonempty"
  | otherwise = case toList as1 of
      [DPSequence leftBs leftMid leftRightBs] ->
        DPSequence leftBs leftMid . singletonSet $
          smartBuildSequence leftRightBs midpoint bs2
      _ -> DPSequence as1 midpoint bs2

smartBuildRootedDeterministicPath ::
  (HasCallStack, Ord t, Ord a) =>
  OMap (DeterministicPath a t) (OSet (DPBranch a t)) ->
  PointlikeDeterministicPath a t ->
  RootedDeterministicPath a t
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
  (HasCallStack, Ord t, Ord a) =>
  DeterministicPath a t ->
  OSet (DPBranch a t) ->
  ( DeterministicPath a t,
    OSet (DPBranch a t)
  )
smartBuildRootBranch
  (Rooted (RootedDeterministicPath (mapToList -> [(dp, branches)]) midpoint))
  branchExtensions =
    (dp, singletonSet $ smartBuildSequence branches midpoint branchExtensions)
smartBuildRootBranch dp branches = (dp, branches)

sequenceDeterministicPaths ::
  (Ord t) =>
  DeterministicPath Anchor t ->
  DeterministicPath Anchor t ->
  Maybe (DeterministicPath Anchor t)
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

invertBranch :: (Ord a, Ord t) => DPBranch a t -> DPBranch a t
invertBranch = \case
  DPOutgoing trans -> DPIncoming trans
  DPIncoming trans -> DPOutgoing trans
  DPSequence as1 midpoint bs2 ->
    DPSequence
      (mapOSet invertBranch as1)
      (invertPointlike midpoint)
      (mapOSet invertBranch bs2)

flipBranch :: (Ord a, Ord t) => DPBranch a t -> DPBranch a t
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

backwardsCount :: DPBranch a t -> Int
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
invertLoop :: (Ord a, Ord t) => DPBranch a t -> DPBranch a t
invertLoop =
  minOn (\x -> (backwardsCount x, x))
    <$> invertBranch
    <*> flipBranch

isoLoop :: (Ord a, Ord t) => DPBranch a t -> DPBranch a t
isoLoop =
  minOn (\x -> (backwardsCount x, x))
    <$> id
    <*> invertBranch . flipBranch

worseLoops :: (Ord a, Ord t) => DPBranch a t -> DPBranch a t
worseLoops =
  maxOn (\x -> (backwardsCount x, x))
    <$> id
    <*> invertBranch . flipBranch

invertPointlike ::
  (Ord a, Ord t) =>
  PointlikeDeterministicPath a t ->
  PointlikeDeterministicPath a t
invertPointlike (PointlikeDeterministicPath anchor loops) =
  PointlikeDeterministicPath anchor (mapOSet invertLoop loops)

invertDeterministicPath ::
  (Ord t) => DeterministicPath Anchor t -> Maybe (DeterministicPath Anchor t)
invertDeterministicPath (Pointlike p) = Just . Pointlike $ invertPointlike p
invertDeterministicPath
  (Rooted (branchify -> (roots, branches, target))) = do
    newTarget <- foldlM1 mergePointlike =<< fromNullable (toList roots)
    let newBranches = mapOSet invertBranch branches
    Just . Rooted $
      smartBuildRootedDeterministicPath
        (singletonMap (Pointlike target) newBranches)
        newTarget

normalizePath :: (Ord t) => Path t -> NormalizedPath Anchor t
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
  forall t.
  (Ord t) =>
  NormalizedPath Anchor t ->
  NormalizedPath FullyAnchored t
leastConstrainedNormalizedPath =
  over (_Unwrapped . setmapped) convertDeterministicPath
  where
    convertDeterministicPath = \case
      Rooted p -> Rooted $ convertRooted p
      Pointlike p -> Pointlike $ convertPointlike p

    convertRooted ::
      (HasCallStack) =>
      RootedDeterministicPath Anchor t ->
      RootedDeterministicPath FullyAnchored t
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
      (Ord t) =>
      (DeterministicPath Anchor t, OSet (DPBranch Anchor t)) ->
      [(DeterministicPath FullyAnchored t, OSet (DPBranch FullyAnchored t))]
    explodeUnanchored = \case
      ( Rooted
          rdp@RootedDeterministicPath {target = PointlikeDeterministicPath {anchor = Unanchored}},
        branchExtensions
        ) ->
          let RootedDeterministicPath {..} = convertRooted rdp
              rootBranches' :: [(DeterministicPath FullyAnchored t, DPBranch FullyAnchored t)] =
                rootBranches
                  & mapToList
                  & over (mapped . _2) toList
                  & concatMap (\(root, bs) -> map (root,) bs)
              branchExtensions' :: OSet (DPBranch FullyAnchored t) = unions (mapOSet convertBranch branchExtensions)
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
      DPBranch Anchor t ->
      OSet (DPBranch FullyAnchored t)
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
  forall t.
  (Ord t) =>
  NormalizedPath Anchor t ->
  NormalizedPath FullyAnchored t
leastNodesNormalizedPath path =
  runIdentity $ traverseAnchors (Identity . convertAnchor) path
  where
    convertAnchor :: Anchor -> FullyAnchored
    convertAnchor = \case
      Unanchored -> FJoinPoint mempty
      JoinPoint {..} -> FJoinPoint {..}
      Specific nid -> FSpecific nid

traverseAnchors ::
  forall f t a b.
  (Applicative f, Ord t, Ord b, Ord a) =>
  (a -> f b) ->
  NormalizedPath a t ->
  f (NormalizedPath b t)
traverseAnchors f (NormalizedPath paths) =
  NormalizedPath . setFromList <$> traverse convertDeterministicPath (toList paths)
  where
    convertDeterministicPath :: DeterministicPath a t -> f (DeterministicPath b t)
    convertDeterministicPath = \case
      Rooted p -> Rooted <$> convertRooted p
      Pointlike p -> Pointlike <$> convertPointlike p

    convertRooted :: RootedDeterministicPath a t -> f (RootedDeterministicPath b t)
    convertRooted (RootedDeterministicPath rootBranches target) =
      smartBuildRootedDeterministicPath . mapFromList
        <$> traverse convertRootBranch (mapToList rootBranches)
        <*> convertPointlike target
      where
        convertRootBranch :: (DeterministicPath a t, OSet (DPBranch a t)) -> f (DeterministicPath b t, OSet (DPBranch b t))
        convertRootBranch (dp, branches) =
          (,)
            <$> convertDeterministicPath dp
            <*> (setFromList <$> traverse convertBranch (toList branches))

    convertPointlike :: PointlikeDeterministicPath a t -> f (PointlikeDeterministicPath b t)
    convertPointlike (PointlikeDeterministicPath anchor loops) =
      PointlikeDeterministicPath
        <$> f anchor
        <*> (setFromList <$> traverse convertBranch (toList loops))

    convertBranch :: DPBranch a t -> f (DPBranch b t)
    convertBranch = \case
      DPOutgoing t -> pure $ DPOutgoing t
      DPIncoming t -> pure $ DPIncoming t
      DPSequence as1 midpoint as2 ->
        smartBuildSequence . setFromList
          <$> traverse convertBranch (toList as1)
          <*> convertPointlike midpoint
          <*> (setFromList <$> traverse convertBranch (toList as2))
