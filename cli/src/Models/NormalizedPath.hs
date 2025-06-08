{-# LANGUAGE UndecidableInstances #-}

module Models.NormalizedPath where

import Data.IxSet.Typed (Indexable (indices), IxSet, ixFun, ixList)
import Data.IxSet.Typed qualified as IxSet
import Data.Map.Merge.Strict (dropMissing, merge)
import Models.NID
import Models.Path
import MyPrelude

-- | Path components that denote a set of NIDs
data LocationSpecifier t
  = -- | The default location specifier, which allows / to distribute over &
    SuperPosition
  | -- | A point where it is possible for & to collapse forcing intersection
    JoinPoint
  | -- | Some specific set of NIDs (e.g. caused by Absolute)
    Specific (Set NID)

-- | A @DeterministicPath t MacroDeterministic@ is only "macro deterministic".
-- Such paths, have all `+` nodes factored out, but because they still have
-- unresolved `Literal`/`Wild` transitions, they are not fully deterministic.
-- Duplicating/unioning on any non-determinism in the `Literal`/`Wild`
-- transitions produces the correct result truly deterministic path.
data MacroDeterministic = MDJoinPoint | MDSpecific NID
  deriving (Eq, Ord, Show, Generic, Hashable)

-- | Used as an intermediate step in the normalization process to capture
-- whether a path has been anchored to a specific node (preventing `&` from
-- distributing over `/` or not).
data PreDeterministic = PDUnanchored | PDJoinPoint | PDSpecific NID
  deriving (Eq, Ord, Show, Generic, Hashable)

data Deterministic = DSpecific NID | DNew
  deriving (Eq, Ord, Show, Generic, Hashable)

data Rootedness = Rooted | Unrooted

type DPIndexes :: Rootedness -> Type -> [Type]
type family DPIndexes r d

newtype DPITarget d = DPITarget {target :: d}
  deriving (Eq, Ord, Show, Generic)

newtype DPIRoot d = DPIRoot {root :: d}
  deriving (Eq, Ord, Show, Generic)

type DPIndexRooted d = [DPRootedKey d, DPITarget d, DPIRoot d]

type instance DPIndexes Rooted d = DPIndexRooted d

type DPIndexUnrooted d = '[d]

type instance DPIndexes Unrooted d = DPIndexUnrooted d

data DPRootedKey d = DPRootedKey {root :: d, target :: Maybe d}
  deriving (Eq, Ord, Show, Generic)

type DPValue :: Rootedness -> Type -> Type -> Type
type family DPValue r t d

type instance DPValue Unrooted t d = TargetingDeterministicPathBranch t d

type instance DPValue Rooted t d = RootedDeterministicPathBranch t d

data TargetingDeterministicPathBranch t d = TargetingDeterministicPathBranch
  { branch :: DeterministicPathBranch t d,
    target :: d
  }
  deriving (Eq, Ord, Show, Generic)

data RootedDeterministicPathBranch t d = RootedDeterministicPathBranch
  { root :: d,
    branchTarget :: Maybe (TargetingDeterministicPathBranch t d)
  }
  deriving (Eq, Ord, Show, Generic)

instance
  (Ord d, Ord t) =>
  Indexable (DPIndexUnrooted d) (TargetingDeterministicPathBranch t d)
  where
  indices = ixList (ixFun (singleton . (.target)))

instance
  (Ord d, Ord t) =>
  Indexable (DPIndexRooted d) (RootedDeterministicPathBranch t d)
  where
  indices = ixList (ixFun mkKey) (ixFun mkTarget) (ixFun mkRoot)
    where
      mkKey = \case
        RootedDeterministicPathBranch r Nothing ->
          singleton $ DPRootedKey r Nothing
        RootedDeterministicPathBranch r (Just tdp) ->
          singleton $ DPRootedKey r (Just tdp.target)
      mkTarget = \case
        RootedDeterministicPathBranch r Nothing ->
          singleton $ DPITarget r
        RootedDeterministicPathBranch _ (Just tdp) ->
          singleton $ DPITarget tdp.target
      mkRoot (RootedDeterministicPathBranch r _) = singleton $ DPIRoot r

-- | Path that has all `+` nodes/superposition factored out.
-- @d@ should be either `MacroDeterministic` or `Deterministic` for specifying
-- a truly deterministic path.
--
-- Mutually recursively defined as it is so that only valid combinations of
-- intersection/sequence are possible.
newtype DeterministicPath r t d = DeterministicPath
  {intersection :: NonNull (IxSet (DPIndexes r d) (DPValue r t d))}
  deriving (Generic)

deriving newtype instance (Indexable (DPIndexes r d) (DPValue r t d)) => Eq (DeterministicPath r t d)

deriving newtype instance (Indexable (DPIndexes r d) (DPValue r t d), Show (DPValue r t d)) => Show (DeterministicPath r t d)

deriving newtype instance (Indexable (DPIndexes r d) (DPValue r t d)) => Ord (DeterministicPath r t d)

deriving newtype instance (Indexable (DPIndexes r d) (DPValue r t d)) => Semigroup (DeterministicPath r t d)

data DeterministicPathBranch t d
  = DPWild
  | DPLiteral t
  | DPSequence
      (NonNull [DeterministicPath Unrooted t d])
      (DeterministicPath Unrooted t ())
  deriving (Eq, Ord, Show, Generic)

newtype NormalizedPath t d = NormalizedPath
  {union :: Set (DeterministicPath Rooted t d)}
  deriving stock (Eq, Show, Ord, Generic)

deriving newtype instance (Semigroup (Set (DeterministicPath Rooted t d))) => (Semigroup (NormalizedPath t d))

deriving newtype instance (Monoid (Set (DeterministicPath Rooted t d))) => (Monoid (NormalizedPath t d))

data NormalizationError t = E
  deriving (Eq, Ord, Show, Generic)

type NormalizationErrors t = NonEmpty (NormalizationError t)

-- fancier anchors:
-- a/(@1/b & @) => a/@1/b & a/@
-- a/@/(@1) => a/@1
-- ((x/@1 & y) & z)/@ => x/@1 & y/@1 & z/@1
-- ((x/@1 & y) & z)/(b + @) => x/@1 & y/@1 & z/@1 + x/@1/b & y/b & z/b
-- a/(@1 + @2) => a/@1 + a/@2
-- a/(@1 + @2/b) => a/@1 + a/@2/b
-- (a/@ & b/@3)/@1 => error because impossible
-- (a & b/@3)/@1 => a/@1 & b/@3

-- Key thing to figure out to make progress:
-- (a & b/@)/(@1 & c) => ???

sequenceDeterministicPathBranches ::
  (Ord t) =>
  RootedDeterministicPathBranch t PreDeterministic ->
  RootedDeterministicPathBranch t PreDeterministic ->
  Maybe (RootedDeterministicPathBranch t PreDeterministic)
sequenceDeterministicPathBranches = undefined

sequenceDeterministicPaths ::
  (Ord t) =>
  DeterministicPath Rooted t PreDeterministic ->
  DeterministicPath Rooted t PreDeterministic ->
  Either (NormalizationErrors t) (DeterministicPath Rooted t PreDeterministic)
sequenceDeterministicPaths (DeterministicPath bs1) (DeterministicPath bs2) = do
  let bs1ByTarget =
        IxSet.groupBy @(DPITarget PreDeterministic) (toNullable bs1)
      bs2ByRoot =
        IxSet.groupBy @(DPIRoot PreDeterministic) (toNullable bs2)
  let cp = cartesianProduct bs1ByTarget bs2ByRoot
  undefined

-- let leadingMap
-- IxSet.groupBy @PreDeterministic . toNullable -> bs1
-- IxSet.groupBy @PreDeterministic . toNullable -> bs2    undefined

prenormalizePath ::
  (Ord t) =>
  Path t ->
  Either (NormalizationErrors t) (NormalizedPath t PreDeterministic)
prenormalizePath = \case
  One ->
    Right . NormalizedPath . singletonSet . DeterministicPath . singletonNNIxSet $
      RootedDeterministicPathBranch PDJoinPoint Nothing
  Absolute nid ->
    Right . NormalizedPath . singletonSet . DeterministicPath . singletonNNIxSet $
      RootedDeterministicPathBranch (PDSpecific nid) Nothing
  Wild ->
    Right . NormalizedPath . singletonSet . DeterministicPath . singletonNNIxSet $
      RootedDeterministicPathBranch
        PDUnanchored
        (Just (TargetingDeterministicPathBranch DPWild PDUnanchored))
  Literal t ->
    Right . NormalizedPath . singletonSet . DeterministicPath . singletonNNIxSet $
      RootedDeterministicPathBranch
        PDUnanchored
        (Just (TargetingDeterministicPathBranch (DPLiteral t) PDUnanchored))
  p1 :+ p2 -> prenormalizePath p1 <!> prenormalizePath p2
  p1 :& p2 -> do
    (NormalizedPath u1, NormalizedPath u2) <-
      combineErrors (prenormalizePath p1) (prenormalizePath p2)
    Right . NormalizedPath $ mapSet (uncurry (<>)) $ cartesianProductSet u1 u2
  p1 :/ p2 -> do
    (NormalizedPath u1, NormalizedPath u2) <-
      combineErrors (prenormalizePath p1) (prenormalizePath p2)
    let cp = cartesianProductSet u1 u2
    let merged = traverseSet (Validation . uncurry sequenceDeterministicPaths) cp
    NormalizedPath <$> merged.either

-- ((x/@1 & y) & z)/z => x/@1/z/@ & y/@/z/@ & z/@/z/@
-- (x & y)/z => x/@/z/@ & y/@/z/@
-- (x & y)/@/z => (x & y)/@/z

deprenormalize :: PreDeterministic -> MacroDeterministic
deprenormalize = \case
  PDUnanchored -> MDJoinPoint
  PDJoinPoint -> MDJoinPoint
  PDSpecific nid -> MDSpecific nid

deprenormalizeTargetingDeterministicPathBranch ::
  (Ord t) =>
  TargetingDeterministicPathBranch t PreDeterministic ->
  TargetingDeterministicPathBranch t MacroDeterministic
deprenormalizeTargetingDeterministicPathBranch
  TargetingDeterministicPathBranch {..} =
    TargetingDeterministicPathBranch
      { branch = deprenormalizeDeterministicPathBranch branch,
        target = deprenormalize target
      }

deprenormalizeRootedDeterministicPathBranch ::
  (Ord t) =>
  RootedDeterministicPathBranch t PreDeterministic ->
  RootedDeterministicPathBranch t MacroDeterministic
deprenormalizeRootedDeterministicPathBranch
  RootedDeterministicPathBranch {..} =
    RootedDeterministicPathBranch
      { root = deprenormalize root,
        branchTarget =
          deprenormalizeTargetingDeterministicPathBranch <$> branchTarget
      }

deprenormalizeDeterministicPathBranch ::
  (Ord t) =>
  DeterministicPathBranch t PreDeterministic ->
  DeterministicPathBranch t MacroDeterministic
deprenormalizeDeterministicPathBranch = \case
  DPLiteral t -> DPLiteral t
  DPWild -> DPWild
  DPSequence ps finalPath ->
    DPSequence
      ( over
          (nonNull . traverse . _Unwrapped . nonNull . ixsetmapped)
          deprenormalizeTargetingDeterministicPathBranch
          ps
      )
      finalPath

deprenormalizePath ::
  (Ord t) =>
  NormalizedPath t PreDeterministic ->
  NormalizedPath t MacroDeterministic
deprenormalizePath =
  over
    (#union . setmapped . _Unwrapped . nonNull . ixsetmapped)
    deprenormalizeRootedDeterministicPathBranch

normalizePath ::
  (Ord t) =>
  Path t ->
  Either (NormalizationErrors t) (NormalizedPath t MacroDeterministic)
normalizePath = prenormalizePath >=> (Right . deprenormalizePath)

-- (a/@1 & b/@2)/

-- splitRoots ::
--   (Ord t) =>
--   Path t ->
--   Either (NormalizationErrors t) (Set (Root, Maybe (Path t)))
-- splitRoots = splitRoots' Nothing

-- splitRoots' ::
--   (Ord t) =>
--   -- | The topmost intersection that causes initial location specifiers to be
--   -- illegal
--   Maybe (Path t) ->
--   Path t ->
--   Either (NormalizationErrors t) (Set (Root, Maybe (Path t)))
-- splitRoots' topmostIntersection = \case
--   One | isJust topmostIntersection -> CannotIntersectStartingPoints topmostIntersection
--   One -> Right $ singletonSet (RCurrentLocation, Nothing)
--   Wild | isJust topmostIntersection -> CannotIntersectStartingPoints topmostIntersection
--   Wild -> Right $ singletonSet (RCurrentLocation, Just Wild)
--   Literal t -> Right $ singletonSet (RCurrentLocation, Just $ Literal t)
--   Absolute nid -> Right $ singletonSet (RSpecific nid, Nothing)
--   p1 :/ p2 ->
--     over (_Right . setmapped . _2) (Just . maybe p2 (:/ p2)) $ splitRoots p1
--   p1 :+ p2 -> monoidCombineErrors (splitRoots p1) (splitRoots p2)
--   p@(p1 :& p2) -> CannotIntersectStartingPoints t
--   p@(_ :& Absolute _) -> CannotIntersectStartingPoints t

-- (@1 + @2)/hi
-- (@1 & @2)/hi => illegal?
-- (@1/c & @3/d) => definitely should be okay
-- @1 & b => @1 & @/b/@
-- b/@1
--
-- @/(a & b)/@/(c & d)/@ & @/e/@
-- (a & b)/@/c & d/@/(e & f)

-- under superposition interpretation of `/` when interpreting as a set of NIDs
-- (a & b)/* => a/*/@ & b/*/@
-- (a & b)/(c + d) => (a/c & b/c)/@ + (a/d & b/c)/@ + (a/c & b/d)/@ + (a/d & b/d)/@
-- (a + b) & (c + d) => (a & c)/@ + (a & d)/@ + (b & c)/@ + (b & d)/@
-- (a & b) + (c & d) => (a & b)/@ + (c & d)/@
-- ((a & b)/@ + c) & d => ((a & b)/@ + c/@) & d/@ => (a & b & d)/@ + (c & d)/@
-- a/(@ + b) => a/@ + a/b/@
-- a/(@ & b) => a/@ & a/b/@
-- (a + b)/(@ & c) => (a/@ + b/@) & (a/c/@ + b/c/@)
-- a/@ & b/@ => (a & b)/@
-- (a & b)/@/(c & d)
-- (a & b)/@/(c + d) => (a & b)/@/c + (a & b)/@/d
