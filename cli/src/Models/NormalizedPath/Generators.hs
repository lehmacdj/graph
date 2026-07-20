{-# OPTIONS_GHC -Wno-orphans #-}

-- | QuickCheck generators for NormalizedPath types that maintain invariants.
module Models.NormalizedPath.Generators
  ( -- * Basic generators
    genNID,
    genDPTransition,
    genLiteralTransition,
    genDPDirection,
    genLiteralDirection,
    genAnchor,

    -- * Composite generators
    genPointlikeDeterministicPath,
    genPointlikeDeterministicPathLiteralOnly,
    genDPBranch,
    genDPBranchLiteralOnly,
    genDeterministicPath,
    genDeterministicPathLiteralOnly,
    genRootedDeterministicPath,
    genRootedDeterministicPathLiteralOnly,
    genNormalizedPath,
    genNormalizedPathLiteralOnly,

    -- * Arbitrary instances
    -- $instances

    -- * Configuration
    GenConfig (..),
    defaultGenConfig,
    shallowGenConfig,

    -- * Sampling (for interactive inspection)
    sampleGenerators,

    -- * Tests
    spec_Generators,
  )
where

import Models.NID
import Models.NormalizedPath.Anchor
import Models.NormalizedPath.Types
import MyPrelude
import Utils.Testing

-- | Configuration for controlling generator depth/complexity
data GenConfig = GenConfig
  { -- | Maximum depth for recursive structures (sequences, nested roots)
    maxDepth :: Int,
    -- | Maximum number of branches in an intersection
    maxBranches :: Int,
    -- | Maximum number of loops in a pointlike path
    maxLoops :: Int,
    -- | Maximum number of paths in a union
    maxUnionSize :: Int,
    -- | Maximum number of NIDs in a JoinPoint exclusion set
    maxExcluding :: Int
  }
  deriving (Eq, Show)

defaultGenConfig :: GenConfig
defaultGenConfig =
  GenConfig
    { maxDepth = 3,
      maxBranches = 3,
      maxLoops = 2,
      maxUnionSize = 3,
      maxExcluding = 2
    }

-- | Shallow config for simpler generated values
shallowGenConfig :: GenConfig
shallowGenConfig =
  GenConfig
    { maxDepth = 1,
      maxBranches = 2,
      maxLoops = 1,
      maxUnionSize = 2,
      maxExcluding = 1
    }

-- * NID generation

-- | Generate a small NID for testing
genNID :: Gen NID
genNID = smallNID <$> choose (1, 100)

-- | Generate a set of NIDs with bounded size
genNIDSet :: Int -> Gen (Set NID)
genNIDSet maxSize = do
  size <- choose (0, maxSize)
  setFromList <$> vectorOf size genNID

-- * Basic generators

-- | Generate a DPTransition (literal or wild, no regex for simplicity)
genDPTransition :: Gen DPTransition
genDPTransition =
  frequency
    [ (5, DPLiteral <$> genTransitionLabel),
      (1, pure DPWild)
      -- Note: DPRegex not generated as it requires CheckedRegex construction
    ]

-- | Generate a simple transition label
genTransitionLabel :: Gen Text
genTransitionLabel =
  elements ["a", "b", "c", "d", "e", "foo", "bar", "baz", "x", "y", "z"]

-- | Generate a DPDirection
genDPDirection :: Gen DPDirection
genDPDirection =
  oneof
    [ DPIncoming' <$> genDPTransition,
      DPOutgoing' <$> genDPTransition
    ]

-- | Generate a literal-only transition (no wildcards or regex)
genLiteralTransition :: Gen DPTransition
genLiteralTransition = DPLiteral <$> genTransitionLabel

-- | Generate a literal-only direction
genLiteralDirection :: Gen DPDirection
genLiteralDirection =
  oneof
    [ DPIncoming' <$> genLiteralTransition,
      DPOutgoing' <$> genLiteralTransition
    ]

-- | Generate an Anchor
genAnchor :: GenConfig -> Gen Anchor
genAnchor cfg =
  frequency
    [ (2, pure Unanchored),
      (2, JoinPoint <$> genNIDSet cfg.maxExcluding),
      (3, Specific <$> genNID)
    ]

-- * Composite generators

-- | Generate a PointlikeDeterministicPath
-- Invariant: anchor can only be Unanchored if loops is empty
genPointlikeDeterministicPath :: GenConfig -> Gen (PointlikeDeterministicPath Anchor)
genPointlikeDeterministicPath cfg = do
  loops <- genLoops cfg
  anchor <-
    if null loops
      then genAnchor cfg
      else -- If we have loops, anchor cannot be Unanchored
        frequency
          [ (2, JoinPoint <$> genNIDSet cfg.maxExcluding),
            (3, Specific <$> genNID)
          ]
  pure $ PointlikeDeterministicPath anchor loops

-- | Generate a pointlike that is valid as a midpoint (can be unanchored)
genMidpoint :: GenConfig -> Gen (PointlikeDeterministicPath Anchor)
genMidpoint cfg = do
  -- Midpoints typically have no loops
  useLoops <- frequency [(1, pure True), (4, pure False)]
  if useLoops
    then genPointlikeDeterministicPath cfg
    else do
      anchor <- genAnchor cfg
      pure $ PointlikeDeterministicPath anchor mempty

-- | Generate loops for a pointlike path
genLoops :: GenConfig -> Gen (OSet (DPBranch Anchor))
genLoops cfg
  | cfg.maxDepth <= 0 = pure mempty
  | otherwise = do
      numLoops <- choose (0, cfg.maxLoops)
      if numLoops == 0
        then pure mempty
        else do
          -- Loops should be simple to avoid deep recursion
          let loopCfg = cfg {maxDepth = 0, maxBranches = 1}
          branches <- vectorOf numLoops (genDPBranch loopCfg)
          pure $ setFromList branches

-- | Generate a DPBranch
-- Invariants:
-- - DPSequence: both branch sets must be nonempty
-- - Sequences are right-associated (enforced by smartBuildSequence)
genDPBranch :: GenConfig -> Gen (DPBranch Anchor)
genDPBranch cfg
  | cfg.maxDepth <= 0 =
      -- At max depth, only generate single directions
      DPSingle <$> genDPDirection
  | otherwise =
      frequency
        [ (3, DPSingle <$> genDPDirection),
          (1, genSequenceBranch cfg)
        ]

-- | Generate a DPSequence branch using the smart constructor
genSequenceBranch :: GenConfig -> Gen (DPBranch Anchor)
genSequenceBranch cfg = do
  let subCfg = cfg {maxDepth = cfg.maxDepth - 1}
  -- Generate nonempty branch sets
  bs1 <- genNonEmptyBranchSet subCfg
  midpoint <- genMidpoint subCfg
  bs2 <- genNonEmptyBranchSet subCfg
  pure $ smartBuildSequence bs1 midpoint bs2

-- | Generate a nonempty set of branches
genNonEmptyBranchSet :: GenConfig -> Gen (OSet (DPBranch Anchor))
genNonEmptyBranchSet cfg = do
  numBranches <- choose (1, max 1 cfg.maxBranches)
  branches <- vectorOf numBranches (genDPBranch cfg)
  pure $ setFromList branches

-- | Generate a DPBranch with literal-only transitions (no wildcards or regex)
-- Supports full complexity: sequences, loops, etc.
genDPBranchLiteralOnly :: GenConfig -> Gen (DPBranch Anchor)
genDPBranchLiteralOnly cfg
  | cfg.maxDepth <= 0 =
      DPSingle <$> genLiteralDirection
  | otherwise =
      frequency
        [ (3, DPSingle <$> genLiteralDirection),
          (1, genSequenceBranchLiteralOnly cfg)
        ]

-- | Generate a DPSequence branch with literal-only transitions
genSequenceBranchLiteralOnly :: GenConfig -> Gen (DPBranch Anchor)
genSequenceBranchLiteralOnly cfg = do
  let subCfg = cfg {maxDepth = cfg.maxDepth - 1}
  bs1 <- genNonEmptyBranchSetLiteralOnly subCfg
  midpoint <- genMidpointLiteralOnly subCfg
  bs2 <- genNonEmptyBranchSetLiteralOnly subCfg
  pure $ smartBuildSequence bs1 midpoint bs2

-- | Generate a nonempty set of literal-only branches
genNonEmptyBranchSetLiteralOnly :: GenConfig -> Gen (OSet (DPBranch Anchor))
genNonEmptyBranchSetLiteralOnly cfg = do
  numBranches <- choose (1, max 1 cfg.maxBranches)
  branches <- vectorOf numBranches (genDPBranchLiteralOnly cfg)
  pure $ setFromList branches

-- | Generate loops with literal-only transitions
genLoopsLiteralOnly :: GenConfig -> Gen (OSet (DPBranch Anchor))
genLoopsLiteralOnly cfg
  | cfg.maxDepth <= 0 = pure mempty
  | otherwise = do
      numLoops <- choose (0, cfg.maxLoops)
      if numLoops == 0
        then pure mempty
        else do
          let loopCfg = cfg {maxDepth = 0, maxBranches = 1}
          branches <- vectorOf numLoops (genDPBranchLiteralOnly loopCfg)
          pure $ setFromList branches

-- | Generate a PointlikeDeterministicPath with literal-only transitions
genPointlikeDeterministicPathLiteralOnly ::
  GenConfig -> Gen (PointlikeDeterministicPath Anchor)
genPointlikeDeterministicPathLiteralOnly cfg = do
  loops <- genLoopsLiteralOnly cfg
  anchor <-
    if null loops
      then genAnchor cfg
      else
        frequency
          [ (2, JoinPoint <$> genNIDSet cfg.maxExcluding),
            (3, Specific <$> genNID)
          ]
  pure $ PointlikeDeterministicPath anchor loops

-- | Generate a midpoint with literal-only transitions
genMidpointLiteralOnly :: GenConfig -> Gen (PointlikeDeterministicPath Anchor)
genMidpointLiteralOnly cfg = do
  useLoops <- frequency [(1, pure True), (4, pure False)]
  if useLoops
    then genPointlikeDeterministicPathLiteralOnly cfg
    else do
      anchor <- genAnchor cfg
      pure $ PointlikeDeterministicPath anchor mempty

-- | Generate a RootedDeterministicPath
-- Invariants:
-- - rootBranches must be nonempty
-- - Each value in rootBranches must be nonempty
-- - Nested Rooted keys should only occur when they have multiple roots
genRootedDeterministicPath :: GenConfig -> Gen (RootedDeterministicPath Anchor)
genRootedDeterministicPath cfg
  | cfg.maxDepth <= 0 = do
      -- Simple rooted path with pointlike root
      root <- Pointlike <$> genPointlikeDeterministicPath cfg
      branches <- genNonEmptyBranchSet cfg
      target <- genPointlikeDeterministicPath cfg
      pure $ smartBuildRootedDeterministicPath (singletonMap root branches) target
  | otherwise = do
      let subCfg = cfg {maxDepth = cfg.maxDepth - 1}
      numRoots <- choose (1, max 1 cfg.maxBranches)
      rootBranches <- mapFromList <$> vectorOf numRoots (genRootBranchPair subCfg)
      target <- genPointlikeDeterministicPath subCfg
      pure $ smartBuildRootedDeterministicPath rootBranches target

-- | Generate a (root, branches) pair for a rooted path
genRootBranchPair ::
  GenConfig ->
  Gen (DeterministicPath Anchor, OSet (DPBranch Anchor))
genRootBranchPair cfg = do
  root <- genDeterministicPathAsRoot cfg
  branches <- genNonEmptyBranchSet cfg
  pure (root, branches)

-- | Generate a DeterministicPath suitable as a root in a RootedDeterministicPath
-- Note: Nested Rooted paths should only be used when they have multiple roots
genDeterministicPathAsRoot :: GenConfig -> Gen (DeterministicPath Anchor)
genDeterministicPathAsRoot cfg
  | cfg.maxDepth <= 0 =
      Pointlike <$> genPointlikeDeterministicPath cfg
  | otherwise =
      frequency
        [ (4, Pointlike <$> genPointlikeDeterministicPath cfg),
          (1, genNestedRootedAsRoot cfg)
        ]

-- | Generate a nested Rooted path that's valid as a root
-- (should have at least 2 distinct roots to be valid)
genNestedRootedAsRoot :: GenConfig -> Gen (DeterministicPath Anchor)
genNestedRootedAsRoot cfg = do
  let subCfg = cfg {maxDepth = cfg.maxDepth - 1, maxBranches = max 2 cfg.maxBranches}
  -- Generate with at least 2 roots
  numRoots <- choose (2, subCfg.maxBranches)
  rootBranches <- mapFromList <$> vectorOf numRoots (genRootBranchPair subCfg)
  target <- genPointlikeDeterministicPath subCfg
  pure $ Rooted $ smartBuildRootedDeterministicPath rootBranches target

-- | Generate a DeterministicPath
genDeterministicPath :: GenConfig -> Gen (DeterministicPath Anchor)
genDeterministicPath cfg =
  frequency
    [ (2, Pointlike <$> genPointlikeDeterministicPath cfg),
      (1, Rooted <$> genRootedDeterministicPath cfg)
    ]

-- | Generate a RootedDeterministicPath with literal-only transitions
genRootedDeterministicPathLiteralOnly ::
  GenConfig -> Gen (RootedDeterministicPath Anchor)
genRootedDeterministicPathLiteralOnly cfg
  | cfg.maxDepth <= 0 = do
      root <- Pointlike <$> genPointlikeDeterministicPathLiteralOnly cfg
      branches <- genNonEmptyBranchSetLiteralOnly cfg
      target <- genPointlikeDeterministicPathLiteralOnly cfg
      pure $ smartBuildRootedDeterministicPath (singletonMap root branches) target
  | otherwise = do
      let subCfg = cfg {maxDepth = cfg.maxDepth - 1}
      numRoots <- choose (1, max 1 cfg.maxBranches)
      rootBranches <-
        mapFromList <$> vectorOf numRoots (genRootBranchPairLiteralOnly subCfg)
      target <- genPointlikeDeterministicPathLiteralOnly subCfg
      pure $ smartBuildRootedDeterministicPath rootBranches target

-- | Generate a (root, branches) pair with literal-only transitions
genRootBranchPairLiteralOnly ::
  GenConfig ->
  Gen (DeterministicPath Anchor, OSet (DPBranch Anchor))
genRootBranchPairLiteralOnly cfg = do
  root <- genDeterministicPathAsRootLiteralOnly cfg
  branches <- genNonEmptyBranchSetLiteralOnly cfg
  pure (root, branches)

-- | Generate a DeterministicPath suitable as a root with literal-only transitions
genDeterministicPathAsRootLiteralOnly ::
  GenConfig -> Gen (DeterministicPath Anchor)
genDeterministicPathAsRootLiteralOnly cfg
  | cfg.maxDepth <= 0 =
      Pointlike <$> genPointlikeDeterministicPathLiteralOnly cfg
  | otherwise =
      frequency
        [ (4, Pointlike <$> genPointlikeDeterministicPathLiteralOnly cfg),
          (1, genNestedRootedAsRootLiteralOnly cfg)
        ]

-- | Generate a nested Rooted path with literal-only transitions
genNestedRootedAsRootLiteralOnly :: GenConfig -> Gen (DeterministicPath Anchor)
genNestedRootedAsRootLiteralOnly cfg = do
  let subCfg = cfg {maxDepth = cfg.maxDepth - 1, maxBranches = max 2 cfg.maxBranches}
  numRoots <- choose (2, subCfg.maxBranches)
  rootBranches <-
    mapFromList <$> vectorOf numRoots (genRootBranchPairLiteralOnly subCfg)
  target <- genPointlikeDeterministicPathLiteralOnly subCfg
  pure $ Rooted $ smartBuildRootedDeterministicPath rootBranches target

-- | Generate a DeterministicPath with literal-only transitions
-- Supports full complexity: loops, nested rooted paths, sequences, etc.
genDeterministicPathLiteralOnly :: GenConfig -> Gen (DeterministicPath Anchor)
genDeterministicPathLiteralOnly cfg =
  frequency
    [ (2, Pointlike <$> genPointlikeDeterministicPathLiteralOnly cfg),
      (1, Rooted <$> genRootedDeterministicPathLiteralOnly cfg)
    ]

-- | Generate a NormalizedPath with literal-only transitions
genNormalizedPathLiteralOnly :: GenConfig -> Gen (NormalizedPath Anchor)
genNormalizedPathLiteralOnly cfg = do
  numPaths <- choose (0, cfg.maxUnionSize)
  paths <- vectorOf numPaths (genDeterministicPathLiteralOnly cfg)
  pure $ NormalizedPath $ setFromList paths

-- | Generate a NormalizedPath
genNormalizedPath :: GenConfig -> Gen (NormalizedPath Anchor)
genNormalizedPath cfg = do
  numPaths <- choose (0, cfg.maxUnionSize)
  paths <- vectorOf numPaths (genDeterministicPath cfg)
  pure $ NormalizedPath $ setFromList paths

-- * Arbitrary instances

-- $instances
-- These instances use 'defaultGenConfig' for generation.
-- For custom configuration, use the explicit generator functions.

instance Arbitrary DPTransition where
  arbitrary = genDPTransition
  shrink = \case
    DPLiteral t -> DPLiteral <$> shrinkText t
    DPWild -> []
    DPRegex _ -> [DPWild] -- Shrink regex to wild

instance Arbitrary DPDirection where
  arbitrary = genDPDirection
  shrink = \case
    DPIncoming' t -> DPIncoming' <$> shrink t
    DPOutgoing' t -> DPOutgoing' <$> shrink t

instance Arbitrary NID where
  arbitrary = genNID

instance Arbitrary Anchor where
  arbitrary = genAnchor defaultGenConfig
  shrink = \case
    Unanchored -> []
    JoinPoint ex
      | null ex -> [Unanchored]
      | otherwise -> [Unanchored, JoinPoint mempty]
    Specific _ -> [Unanchored, JoinPoint mempty]

instance Arbitrary (PointlikeDeterministicPath Anchor) where
  arbitrary = genPointlikeDeterministicPath defaultGenConfig
  shrink pdp
    | null pdp.loops = case pdp.anchor of
        Unanchored -> []
        _ -> [PointlikeDeterministicPath Unanchored mempty]
    | otherwise =
        -- Remove loops or shrink anchor
        [ PointlikeDeterministicPath pdp.anchor mempty,
          PointlikeDeterministicPath (JoinPoint mempty) pdp.loops
        ]

instance Arbitrary (DPBranch Anchor) where
  arbitrary = genDPBranch defaultGenConfig
  shrink = \case
    DPSingle dir -> DPSingle <$> shrink dir
    DPSequence bs1 _mid _bs2 ->
      -- Shrink to just the first branch of bs1
      case toList bs1 of
        (b : _) -> [b]
        [] -> []

instance Arbitrary (RootedDeterministicPath Anchor) where
  arbitrary = genRootedDeterministicPath defaultGenConfig

instance Arbitrary (DeterministicPath Anchor) where
  arbitrary = genDeterministicPath defaultGenConfig
  shrink = \case
    Pointlike p -> Pointlike <$> shrink p
    Rooted r ->
      -- Shrink to a simple pointlike
      [Pointlike r.target]

instance Arbitrary (NormalizedPath Anchor) where
  arbitrary = genNormalizedPath defaultGenConfig
  shrink (NormalizedPath paths)
    | null paths = []
    | otherwise =
        -- Shrink by removing paths or shrinking individual paths
        NormalizedPath mempty
          : [NormalizedPath (setFromList ps) | ps <- shrink (toList paths)]

-- * Helper functions

shrinkText :: Text -> [Text]
shrinkText t
  | length t <= 1 = []
  | otherwise = [take (length t `div` 2) t]

-- * Tests

spec_Generators :: Spec
spec_Generators = do
  describe "generators produce valid values" do
    it "genDPTransition produces valid transitions" $ property $ do
      t <- genDPTransition
      pure $ case t of
        DPLiteral _ -> True
        DPWild -> True
        DPRegex _ -> True

    it "genDPDirection produces valid directions" $ property $ do
      d <- genDPDirection
      pure $ case d of
        DPIncoming' _ -> True
        DPOutgoing' _ -> True

    it "genAnchor produces valid anchors" $ property $ do
      a <- genAnchor defaultGenConfig
      pure $ case a of
        Unanchored -> True
        JoinPoint _ -> True
        Specific _ -> True

    it "genDPBranch produces valid branches" $ property $ do
      b <- genDPBranch defaultGenConfig
      pure $ branchIsValid b

    it "genPointlikeDeterministicPath produces valid pointlikes" $ property $ do
      pdp <- genPointlikeDeterministicPath defaultGenConfig
      pure $ pointlikeIsValid pdp

    it "genRootedDeterministicPath produces valid rooted paths" $ property $ do
      r <- genRootedDeterministicPath defaultGenConfig
      pure $ rootedIsValid r

    it "genDeterministicPath produces valid paths" $ property $ do
      dp <- genDeterministicPath defaultGenConfig
      pure $ deterministicPathIsValid dp

    it "genNormalizedPath produces valid normalized paths" $ property $ do
      np <- genNormalizedPath defaultGenConfig
      pure $ normalizedPathIsValid np

-- * Sampling

-- | Sample generators to visually inspect output quality.
-- Run this in GHCi: @sampleGenerators@
sampleGenerators :: IO ()
sampleGenerators = do
  putStrLn "=== DeterministicPath (shallow) ==="
  replicateM_ 10 $ generate (genDeterministicPath shallowGenConfig) >>= print

  putStrLn "\n=== DeterministicPath (default) ==="
  replicateM_ 10 $ generate (genDeterministicPath defaultGenConfig) >>= print

-- * Validation functions

branchIsValid :: DPBranch Anchor -> Bool
branchIsValid = \case
  DPSingle _ -> True
  DPSequence bs1 mid bs2 ->
    not (null bs1)
      && not (null bs2)
      && pointlikeIsValid mid
      && all branchIsValid (toList bs1)
      && all branchIsValid (toList bs2)
      && isRightAssociated (DPSequence bs1 mid bs2)

-- | Check that sequences are right-associated
isRightAssociated :: DPBranch Anchor -> Bool
isRightAssociated = \case
  DPSingle _ -> True
  DPSequence bs1 _ bs2 ->
    -- bs1 should not contain a single DPSequence (that would be left-associated)
    case toList bs1 of
      [DPSequence {}] -> False
      _ -> all isRightAssociated (toList bs1) && all isRightAssociated (toList bs2)

pointlikeIsValid :: PointlikeDeterministicPath Anchor -> Bool
pointlikeIsValid pdp =
  -- Anchor can only be Unanchored if loops is empty
  case pdp.anchor of
    Unanchored -> null pdp.loops
    _ -> all branchIsValid (toList pdp.loops)

rootedIsValid :: RootedDeterministicPath Anchor -> Bool
rootedIsValid r =
  not (null r.rootBranches)
    && all (not . null) (toList r.rootBranches)
    && all deterministicPathIsValid (keys r.rootBranches)
    && all (all branchIsValid . toList) (toList r.rootBranches)
    && pointlikeIsValid r.target

deterministicPathIsValid :: DeterministicPath Anchor -> Bool
deterministicPathIsValid = \case
  Pointlike p -> pointlikeIsValid p
  Rooted r -> rootedIsValid r

normalizedPathIsValid :: NormalizedPath Anchor -> Bool
normalizedPathIsValid (NormalizedPath paths) =
  all deterministicPathIsValid (toList paths)
