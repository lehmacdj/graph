{-# LANGUAGE UndecidableInstances #-}

module Models.NormalizedPath where

import Models.NID
import Models.Path
import MyPrelude

data Anchor = Unanchored | JoinPoint | Specific NID
  deriving stock (Eq, Ord, Show, Generic, Lift)
  deriving anyclass (NFData)

data FullyAnchored = FJoinPoint | FSpecific NID
  deriving stock (Eq, Ord, Show, Generic, Lift)
  deriving anyclass (NFData)

data ConcreteAnchor = CSpecific NID | NotInGraph
  deriving stock (Eq, Ord, Show, Generic, Lift)
  deriving anyclass (NFData)

data DeterministicPath t
  = Rooted (RootedDeterministicPath t)
  | Pointlike (PointlikeDeterministicPath t)
  deriving stock (Eq, Ord, Show, Generic, Lift)
  deriving anyclass (NFData)

-- | Represents a path that targets a single node (i.e. is deterministic)
--
-- Invariants:
-- - the sets of branches in rootBranches must be nonempty
-- - the keys of rootBranches may not be the same Anchor as target.anchor
data RootedDeterministicPath t = RootedDeterministicPath
  { rootBranches :: Map (PointlikeDeterministicPath t) (Set (DPBranch t)),
    target :: PointlikeDeterministicPath t
  }
  deriving stock (Eq, Ord, Show, Generic, Lift)
  deriving anyclass (NFData)

-- | A pointlike deterministic path. This is a path that has a single Root
-- and the target is the same as the Root.
data PointlikeDeterministicPath t
  = PointlikeDeterministicPath
  { -- | this anchor can't be Unanchored
    anchor :: Anchor,
    loops :: Set (DPBranch t)
  }
  deriving stock (Eq, Ord, Show, Generic, Lift)
  deriving anyclass (NFData)

unanchored :: (Ord t) => PointlikeDeterministicPath t
unanchored = PointlikeDeterministicPath Unanchored mempty

joinPoint :: (Ord t) => PointlikeDeterministicPath t
joinPoint = PointlikeDeterministicPath JoinPoint mempty

specific :: (Ord t) => NID -> PointlikeDeterministicPath t
specific nid = PointlikeDeterministicPath (Specific nid) mempty

-- | A branch in the path.
data DPBranch t
  = DPLiteral t
  | DPWild
  | -- | A concatenation of two intersections of branches.
    --
    -- Invariants:
    -- - both intersections are nonempty
    -- - sequences of non-intersected branches are right associated (e.g.
    --   @Sequence (singleton a) m (Sequence (singleton b) n (singleton c))@
    --   instead of
    --   @Sequence (Sequence (singleton a) m (singleton b)) n (singleton c)@)
    DPSequence (Set (DPBranch t)) (PointlikeDeterministicPath t) (Set (DPBranch t))
  deriving stock (Eq, Ord, Show, Generic, Lift)
  deriving anyclass (NFData)

newtype NormalizedPath t = NormalizedPath {union :: Set (DeterministicPath t)}
  deriving stock (Eq, Ord, Show, Generic, Lift)
  deriving anyclass (NFData)

pointify ::
  (Ord t) =>
  RootedDeterministicPath t ->
  Maybe (PointlikeDeterministicPath t)
pointify RootedDeterministicPath {..} = do
  protoPoint <- foldlM1 mergePointlike (target `ncons` keys rootBranches)
  let extraLoops = mconcat $ toList rootBranches
  Just
    protoPoint
      { loops = protoPoint.loops <> extraLoops,
        anchor = fromJustEx $ mergeAnchor JoinPoint protoPoint.anchor
      }

intersectDeterministicPaths ::
  (Ord t) =>
  DeterministicPath t ->
  DeterministicPath t ->
  Maybe (DeterministicPath t)
intersectDeterministicPaths (Rooted p1) (Rooted p2) = do
  newTarget <- mergePointlike p1.target p2.target
  let newSourceBranches = unionWith (<>) p1.rootBranches p2.rootBranches
  Just . Rooted $ RootedDeterministicPath newSourceBranches newTarget
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
mergeAnchor JoinPoint (Specific nid) = Just (Specific nid)
mergeAnchor (Specific nid) JoinPoint = Just (Specific nid)
mergeAnchor JoinPoint JoinPoint = Just JoinPoint
mergeAnchor (Specific nid1) (Specific nid2)
  | nid1 == nid2 = Just (Specific nid1)
  | otherwise = Nothing

-- | Merge two PointlikeDeterministicPaths by combining their loops and
-- merging their anchors.
mergePointlike ::
  (Ord t) =>
  PointlikeDeterministicPath t ->
  PointlikeDeterministicPath t ->
  Maybe (PointlikeDeterministicPath t)
mergePointlike p1 p2 = do
  mergedAnchor <- mergeAnchor p1.anchor p2.anchor
  Just $
    PointlikeDeterministicPath
      { anchor = mergedAnchor,
        loops = p1.loops <> p2.loops
      }

smartBuildSequence ::
  (HasCallStack, Ord t) =>
  Set (DPBranch t) ->
  PointlikeDeterministicPath t ->
  Set (DPBranch t) ->
  DPBranch t
smartBuildSequence bs1 midpoint bs2
  | null (bs1 <> bs2) = error "smartBuildSequence: both branch sets must be nonempty"
  | otherwise = case toList bs1 of
      [DPSequence leftBs leftMid leftRightBs] ->
        DPSequence leftBs leftMid . singletonSet $
          smartBuildSequence leftRightBs midpoint bs2
      _ -> DPSequence bs1 midpoint bs2

sequenceDeterministicPaths ::
  (Ord t) =>
  DeterministicPath t ->
  DeterministicPath t ->
  Maybe (DeterministicPath t)
sequenceDeterministicPaths (Pointlike p1) (Pointlike p2) =
  Pointlike <$> mergePointlike p1 p2
sequenceDeterministicPaths (Pointlike p1) (Rooted p2) = do
  newRoot <- foldlM1 mergePointlike (p1 `ncons` keys p2.rootBranches)
  let newBranches = mconcat $ toList p2.rootBranches
  Just $ Rooted $ p2 {rootBranches = singletonMap newRoot newBranches}
sequenceDeterministicPaths (Rooted p1) (Pointlike p2) = do
  newTarget <- mergePointlike p1.target p2
  Just $ Rooted $ p1 {target = newTarget}
sequenceDeterministicPaths (Rooted p1) (Rooted p2) = do
  midpoint <- foldlM1 mergePointlike (p1.target `ncons` keys p2.rootBranches)
  let p2Branches = mconcat $ toList p2.rootBranches
  let newRootBranches =
        p1.rootBranches <&> \branches ->
          singletonSet $
            smartBuildSequence branches midpoint p2Branches
  Just . Rooted $ RootedDeterministicPath newRootBranches p2.target

normalizePath :: (Ord t) => Path t -> NormalizedPath t
normalizePath = \case
  Zero ->
    NormalizedPath mempty
  One ->
    NormalizedPath . singletonSet . Pointlike $ joinPoint
  Absolute nid ->
    NormalizedPath . singletonSet . Pointlike $ specific nid
  Wild ->
    NormalizedPath . singletonSet . Rooted $
      RootedDeterministicPath
        (singletonMap unanchored (singletonSet DPWild))
        unanchored
  Literal t ->
    NormalizedPath . singletonSet . Rooted $
      RootedDeterministicPath
        (singletonMap unanchored (singletonSet (DPLiteral t)))
        unanchored
  p1 :+ p2 ->
    let np1 = normalizePath p1
        np2 = normalizePath p2
     in NormalizedPath $ np1.union <> np2.union
  p1 :& p2 ->
    let np1 = normalizePath p1
        np2 = normalizePath p2
     in cartesianProductSet np1.union np2.union
          & toList
          -- maybe better to handle errors here than to just ignore them?
          & mapMaybe (uncurry intersectDeterministicPaths)
          & setFromList
          & NormalizedPath
  p1 :/ p2 ->
    let np1 = normalizePath p1
        np2 = normalizePath p2
     in cartesianProductSet np1.union np2.union
          & toList
          & mapMaybe (uncurry sequenceDeterministicPaths)
          & setFromList
          & NormalizedPath

-- | Expand all Unanchored anchors to produce the least constrained path
-- possible. This may result in a very large number of paths
-- (e.g. @(a & b)/(c & d) => (a/c & a/d & b/c & b/d)@).
leastConstrainedNormalizedPath :: NormalizedPath t -> NormalizedPath t
leastConstrainedNormalizedPath = undefined

-- | Assign one JoinPoint to each Unanchored anchor
leastNodesNormalizedPath :: NormalizedPath t -> NormalizedPath t
leastNodesNormalizedPath = undefined
