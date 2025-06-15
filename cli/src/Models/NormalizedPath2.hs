{-# LANGUAGE PatternGuards #-}
{-# LANGUAGE UndecidableInstances #-}

module NormalizedPath2 where

import Models.NID
import Models.Path
import MyPrelude

data Anchor = Unanchored | JoinPoint | Specific NID
  deriving stock (Eq, Ord, Show, Generic)
  deriving anyclass (NFData)

-- | Represents a path that targets a single node (i.e. is deterministic)
--
-- Invariants:
-- - if nonSelfLoopSources is empty, hasSelfLoop must be True
-- - if `hasSelfLoop`, the target can't be Unanchored
-- - the sets of branches in nonSelfLoopSources must be nonempty
-- - the keys of nonSelfLoopSources may not be the same Anchor.Specific as the
--   target
data RootedDeterministicPath t = RootedDeterministicPath
  { nonSelfLoopSources :: Map (PointlikeDeterministicPath t) (Set (DPBranch t)),
    target :: PointlikeDeterministicPath t,
    hasSelfLoop :: Bool
  }
  deriving stock (Eq, Ord, Show, Generic)
  deriving anyclass (NFData)

-- | Try to coerce a 'RootedDeterministicPath' into a
-- 'PointlikeDeterministicPath'. This is only possible if the path is
-- "pointlike". A path is pointlike if it has a single source and the target is
-- the same as the source.
asPointlike ::
  (HasCallStack) =>
  RootedDeterministicPath t ->
  Maybe (PointlikeDeterministicPath t)
asPointlike dp@RootedDeterministicPath {..}
  | hasSelfLoop,
    null nonSelfLoopSources,
    target.anchor /= Unanchored || error "broken invariant" =
      Just dp.target
  | otherwise = Nothing

-- | A pointlike deterministic path. This is a path that has a single source
-- and the target is the same as the source.
data PointlikeDeterministicPath t
  = PointlikeDeterministicPath
  { -- | this anchor can't be Unanchored
    anchor :: Anchor,
    loops :: Set (DPBranch t)
  }
  deriving stock (Eq, Ord, Show, Generic)
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
  deriving stock (Eq, Ord, Show, Generic)
  deriving anyclass (NFData)

newtype NormalizedPath t = NormalizedPath {union :: Set (RootedDeterministicPath t)}

intersectDeterministicPaths ::
  RootedDeterministicPath t ->
  RootedDeterministicPath t ->
  Maybe (RootedDeterministicPath t)
intersectDeterministicPaths dp1 dp2 = undefined

sequenceDeterministicPaths ::
  RootedDeterministicPath t ->
  RootedDeterministicPath t ->
  Maybe (RootedDeterministicPath t)
sequenceDeterministicPaths dp1 dp2 = do
  undefined

normalizePath :: (Ord t) => Path t -> Maybe (NormalizedPath t)
normalizePath = \case
  One ->
    Just . NormalizedPath . singletonSet $
      RootedDeterministicPath
        (singletonMap joinPoint mempty)
        joinPoint
        True
  Absolute nid ->
    Just . NormalizedPath . singletonSet $
      RootedDeterministicPath
        (singletonMap (specific nid) mempty)
        (specific nid)
        True
  Wild ->
    Just . NormalizedPath . singletonSet $
      RootedDeterministicPath
        (singletonMap unanchored (singletonSet DPWild))
        unanchored
        False
  Literal t ->
    Just . NormalizedPath . singletonSet $
      RootedDeterministicPath
        (singletonMap unanchored (singletonSet (DPLiteral t)))
        unanchored
        False
  p1 :+ p2 -> do
    np1 <- normalizePath p1
    np2 <- normalizePath p2
    Just $ NormalizedPath $ np1.union <> np2.union
  p1 :& p2 -> do
    np1 <- normalizePath p1
    np2 <- normalizePath p2
    Just $
      cartesianProductSet np1.union np2.union
        & toList
        -- maybe better to handle errors here than to just ignore them?
        & mapMaybe (uncurry intersectDeterministicPaths)
        & setFromList
        & NormalizedPath
  p1 :/ p2 -> do
    np1 <- normalizePath p1
    np2 <- normalizePath p2
    Just $
      cartesianProductSet np1.union np2.union
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
