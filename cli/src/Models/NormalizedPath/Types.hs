module Models.NormalizedPath.Types where

import Models.NID
import Models.NormalizedPath.Anchor
import MyPrelude

newtype NormalizedPath a = NormalizedPath {union :: Set (DeterministicPath a)}
  deriving stock (Show, Eq, Ord, Generic, Lift)
  deriving anyclass (NFData)

data DeterministicPath a
  = Rooted (RootedDeterministicPath a)
  | Pointlike (PointlikeDeterministicPath a)
  deriving stock (Show, Eq, Ord, Generic, Lift)
  deriving anyclass (NFData)

dpTarget :: DeterministicPath a -> PointlikeDeterministicPath a
dpTarget (Rooted p) = p.target
dpTarget (Pointlike p) = p

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
  deriving stock (Show, Eq, Ord, Generic, Lift)
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
  deriving stock (Show, Eq, Ord, Generic, Lift)
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
  deriving stock (Show, Eq, Ord, Generic, Lift)
  deriving anyclass (NFData)

data DPDirection
  = DPIncoming' DPTransition
  | DPOutgoing' DPTransition
  deriving stock (Show, Eq, Ord, Generic, Lift)
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
  deriving stock (Show, Eq, Ord, Generic, Lift)
  deriving anyclass (NFData)

-- * Smart, invariant-preserving constructors

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
