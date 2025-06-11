{-# LANGUAGE UndecidableInstances #-}

module Models.NormalizedPath where

import Data.IxSet.Typed (Indexable (indices), IxSet, ixFun, ixList)
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

type family DPBranches r d x where
  DPBranches Rooted d x = NonNull (Map d (Maybe (NonNull (Set x))))
  DPBranches Unrooted d x = NonNull (Set x)

type DeterministicPath r t d = DeterministicPath' r t d d

-- | Path that has all `+` nodes/superposition factored out.
-- @d@ should be either `MacroDeterministic` or `Deterministic` for specifying
-- a truly deterministic path.
--
-- Mutually recursively defined as it is so that only valid combinations of
-- intersection/sequence are possible.
data DeterministicPath' r transition determinacy target = DeterministicPath
  { branches ::
      DPBranches r determinacy (DeterministicPathBranch transition determinacy),
    target :: target
  }
  deriving (Generic)

deriving stock instance
  ( Eq tr,
    Eq d,
    Eq ta,
    Eq (DPBranches r d (DeterministicPathBranch tr d))
  ) =>
  Eq (DeterministicPath' r tr d ta)

deriving stock instance
  ( Ord tr,
    Ord d,
    Ord ta,
    Ord (DPBranches r d (DeterministicPathBranch tr d))
  ) =>
  Ord (DeterministicPath' r tr d ta)

deriving stock instance
  ( Show tr,
    Show d,
    Show ta,
    Show (DPBranches r d (DeterministicPathBranch tr d))
  ) =>
  Show (DeterministicPath' r tr d ta)

data DeterministicPathBranch t d
  = DPWild
  | DPLiteral t
  | DPSequence
      (NonNull [DeterministicPath Unrooted t d])
      (DeterministicPath' Unrooted t d ())
  deriving (Eq, Ord, Show, Generic)

newtype NormalizedPath t d = NormalizedPath
  {union :: Set (DeterministicPath Rooted t d)}
  deriving stock (Eq, Show, Ord, Generic)

deriving newtype instance (Semigroup (Set (DeterministicPath Rooted t d))) => (Semigroup (NormalizedPath t d))

deriving newtype instance (Monoid (Set (DeterministicPath Rooted t d))) => (Monoid (NormalizedPath t d))

data NormalizationError t = NoPossibleJoinPointWhileSequencing
  { prefix :: DeterministicPath Rooted t PreDeterministic,
    suffix :: DeterministicPath Rooted t PreDeterministic
  }
  deriving (Eq, Ord, Show, Generic)

type NormalizationErrors t = NonEmpty (NormalizationError t)

mergePreDeterminacy ::
  PreDeterministic -> PreDeterministic -> Maybe PreDeterministic
mergePreDeterminacy PDUnanchored PDUnanchored = Just PDUnanchored
mergePreDeterminacy PDUnanchored PDJoinPoint = Just PDJoinPoint
mergePreDeterminacy PDJoinPoint PDUnanchored = Just PDJoinPoint
mergePreDeterminacy PDJoinPoint PDJoinPoint = Just PDJoinPoint
mergePreDeterminacy PDUnanchored (PDSpecific nid) = Just (PDSpecific nid)
mergePreDeterminacy (PDSpecific nid) PDUnanchored = Just (PDSpecific nid)
mergePreDeterminacy PDJoinPoint (PDSpecific nid) = Just (PDSpecific nid)
mergePreDeterminacy (PDSpecific nid) PDJoinPoint = Just (PDSpecific nid)
mergePreDeterminacy (PDSpecific nid1) (PDSpecific nid2)
  | nid1 == nid2 = Just (PDSpecific nid1)
  | otherwise = Nothing

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
-- a & b/@ => a/@ & b/@
-- c/(@1 & b)

intersectDeterministicPaths ::
  (Ord t) =>
  DeterministicPath Rooted t PreDeterministic ->
  DeterministicPath Rooted t PreDeterministic ->
  Maybe (DeterministicPath Rooted t PreDeterministic)
intersectDeterministicPaths = undefined

-- (a & b)/c => a/@/c & b/@/c
-- a/(b & c) => a/@/b & a/@/c
-- (a & b)/(c & d) => a/@/c & a/@/d & b/@/c & b/@/d
-- (a & b/@)/(c & d) => (a & b)/@/(c & d)

sequenceDeterministicPathBranches ::
  (Ord t) =>
  DeterministicPathBranch t PreDeterministic ->
  DeterministicPathBranch t PreDeterministic ->
  DeterministicPathBranch t PreDeterministic
sequenceDeterministicPathBranches p1 p2 =
  DPSequence
    (singletonNN (DeterministicPath (singletonNNSet p1) PDJoinPoint))
    (DeterministicPath (singletonNNSet p2) ())

-- (a & @)/b ???=> a/@/b & b or (a & @)/@/b

sequenceDeterministicPaths ::
  forall t.
  (Ord t) =>
  DeterministicPath Rooted t PreDeterministic ->
  DeterministicPath Rooted t PreDeterministic ->
  Either (NormalizationErrors t) (DeterministicPath Rooted t PreDeterministic)
sequenceDeterministicPaths dp1@(DeterministicPath rb1 t1) dp2@(DeterministicPath rb2 t2) = do
  anchor <-
    foldlM1 mergePreDeterminacy (t1 `ncons` keys (toNullable rb2))
      `injectError` NoPossibleJoinPointWhileSequencing dp1 dp2
  -- need to also consider roots with no branch for rb1 when merging
  -- pre-determinacy because these are effectively the target of the first path
  case anchor of
    PDUnanchored -> do
      -- this won't actually merge any values because we know all the roots
      -- were unanchored
      let b2Set :: Set (Maybe (NonNull (Set (DeterministicPathBranch t PreDeterministic))))
          b2Set = rb2 & toNullable & toList & setFromList & asSet
      let b2HasNothing = Nothing `member` b2Set
      let b2 :: [DeterministicPathBranch t PreDeterministic]
          b2 = b2Set & toList & catMaybes & concatMap toList
      let rb1' =
            (rb1 & toNullable & mapToList)
              & over (mapped . _2 . _Just) (toList . toNullable)
      let distributeB2 :: ((x, Maybe [y]), z) -> [(x, Either z (y, z))]
          distributeB2 ((x, Nothing), z) = [(x, Left z)]
          distributeB2 ((x, Just ys), z) = ys <&> ((x,) . Right . (,z))
      let nonB2NothingBranches :: [(PreDeterministic, DeterministicPathBranch t PreDeterministic)]
          nonB2NothingBranches =
            cartesianProduct rb1' b2
              & concatMap distributeB2
              & over (mapped . _2 . _Right) (uncurry sequenceDeterministicPathBranches)
              & over (mapped . _2) codiagonal
      let b2NothingBranches = rb1'
      undefined
  -- targets of 1st + roots of 2nd must match. otherwise error
  --
  -- either:
  -- - if any is joinpoint/specific, just return a single DPSequence where the
  --    midpoint is the joinpoint/specific node
  -- - if all are unanchored, stitch together cartesian product of all branches
  undefined

prenormalizePath ::
  (Ord t) =>
  Path t ->
  Either (NormalizationErrors t) (NormalizedPath t PreDeterministic)
prenormalizePath = \case
  One ->
    Right . NormalizedPath . singletonSet $
      DeterministicPath (singletonNNMap PDJoinPoint Nothing) PDUnanchored
  Absolute nid ->
    Right . NormalizedPath . singletonSet $
      DeterministicPath (singletonNNMap (PDSpecific nid) Nothing) PDUnanchored
  Wild ->
    Right . NormalizedPath . singletonSet $
      DeterministicPath
        (singletonNNMap PDUnanchored (Just (singletonNNSet DPWild)))
        PDUnanchored
  Literal t ->
    Right . NormalizedPath . singletonSet $
      DeterministicPath
        (singletonNNMap PDUnanchored (Just (singletonNNSet (DPLiteral t))))
        PDUnanchored
  p1 :+ p2 -> prenormalizePath p1 <!> prenormalizePath p2
  p1 :& p2 -> do
    (NormalizedPath u1, NormalizedPath u2) <-
      combineErrors (prenormalizePath p1) (prenormalizePath p2)
    Right . NormalizedPath $
      setFromList . mapMaybe (uncurry intersectDeterministicPaths) . toList $
        cartesianProductSet u1 u2
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

deprenormalizeDeterministicPath ::
  (Ord t) =>
  DeterministicPath Rooted t PreDeterministic ->
  DeterministicPath Rooted t MacroDeterministic
deprenormalizeDeterministicPath
  DeterministicPath {..} =
    DeterministicPath
      { branches =
          branches
            & toNullable
            & mapToList
            & over (traverse . _1) deprenormalize
            & over
              (traverse . _2 . _Just . nonNull . setmapped)
              deprenormalizeDeterministicPathBranch
            & mapFromList
            & impureNonNull,
        target = deprenormalize target
      }

deprenormalizeUnrootedDeterministicPath ::
  (Ord t) =>
  DeterministicPath Unrooted t PreDeterministic ->
  DeterministicPath Unrooted t MacroDeterministic
deprenormalizeUnrootedDeterministicPath
  DeterministicPath {..} =
    DeterministicPath
      { branches =
          branches
            & over (nonNull . setmapped) deprenormalizeDeterministicPathBranch,
        target = deprenormalize target
      }

deprenormalizeUnrootedDeterministicPath' ::
  (Ord t) =>
  DeterministicPath' Unrooted t PreDeterministic () ->
  DeterministicPath' Unrooted t MacroDeterministic ()
deprenormalizeUnrootedDeterministicPath'
  DeterministicPath {..} =
    DeterministicPath
      { branches =
          branches
            & over (nonNull . setmapped) deprenormalizeDeterministicPathBranch,
        target = ()
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
          (nonNull . traverse)
          deprenormalizeUnrootedDeterministicPath
          ps
      )
      (deprenormalizeUnrootedDeterministicPath' finalPath)

-- deprenormalizeDeterministicPathBranch ::
--   (Ord t) =>
--   DeterministicPathBranch t PreDeterministic ->
--   DeterministicPathBranch t MacroDeterministic
-- deprenormalizeDeterministicPathBranch = \case
--   DPLiteral t -> DPLiteral t
--   DPWild -> DPWild
--   DPSequence ps finalPath ->
--     DPSequence
--       ( over
--           (nonNull . traverse . _Unwrapped . nonNull . ixsetmapped)
--           deprenormalizeTargetingDeterministicPathBranch
--           ps
--       )
--       finalPath

deprenormalizePath ::
  (Ord t) =>
  NormalizedPath t PreDeterministic ->
  NormalizedPath t MacroDeterministic
deprenormalizePath =
  over (#union . setmapped) deprenormalizeDeterministicPath

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
