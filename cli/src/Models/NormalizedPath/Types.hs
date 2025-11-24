module Models.NormalizedPath.Types where

import Models.NID
import Models.NormalizedPath.Anchor
import MyPrelude
import Utils.Testing

newtype NormalizedPath a = NormalizedPath {union :: Set (DeterministicPath a)}
  deriving stock (Eq, Ord, Generic, Lift)
  deriving anyclass (NFData)

data DeterministicPath a
  = Rooted (RootedDeterministicPath a)
  | Pointlike (PointlikeDeterministicPath a)
  deriving stock (Eq, Ord, Generic, Lift)
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
  deriving stock (Eq, Ord, Generic, Lift)
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
  deriving stock (Eq, Ord, Generic, Lift)
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
  deriving stock (Eq, Ord, Generic, Lift)
  deriving anyclass (NFData)

data DPDirection
  = DPIncoming' DPTransition
  | DPOutgoing' DPTransition
  deriving stock (Eq, Ord, Generic, Lift)
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
  deriving stock (Eq, Ord, Generic, Lift)
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

-- * Show instances

-- Precedence levels:
-- 0: NormalizedPath union (+)
-- 1: DeterministicPath
-- 2: Branch intersection (&)
-- 3: Branch sequence (/|)
-- 4: Atomic elements (transitions, anchors)

instance (Show a, Ord a) => Show (NormalizedPath a) where
  showsPrec p np = showParen (p > 0) $ showsNormalizedPath np

showsNormalizedPath :: (Show a, Ord a) => NormalizedPath a -> ShowS
showsNormalizedPath (NormalizedPath paths)
  | null paths = showString "%never"
  | otherwise =
      let pathList = toList paths
       in foldl' (.) id $ intersperse (showString " + ") $ map (showsPrec 1) pathList

instance (Show a, Ord a) => Show (DeterministicPath a) where
  showsPrec p dp = showsDeterministicPath p dp

showsDeterministicPath :: (Show a, Ord a) => Int -> DeterministicPath a -> ShowS
showsDeterministicPath p = \case
  Pointlike pdp -> showsPrec p pdp
  Rooted r -> showsPrec p r

instance (Show a, Ord a) => Show (PointlikeDeterministicPath a) where
  showsPrec _ = showsPointlike

showsPointlike :: (Show a, Ord a) => PointlikeDeterministicPath a -> ShowS
showsPointlike (PointlikeDeterministicPath anchor loops)
  | null loops = shows anchor
  | otherwise =
      shows anchor
        . showString "["
        . showsBranchList (toList loops)
        . showString "]"
  where
    showsBranchList [] = id
    showsBranchList [b] = showsPrec 3 b
    showsBranchList (b : bs) = showsPrec 3 b . showString " & " . showsBranchList bs

instance (Show a, Ord a) => Show (RootedDeterministicPath a) where
  showsPrec _ = showsRooted

showsRooted :: (Show a, Ord a) => RootedDeterministicPath a -> ShowS
showsRooted (RootedDeterministicPath rootBranches target) =
  showString "["
    . showsRootBranches (mapToList rootBranches)
    . showString "]"
    . showsTarget
  where
    showsTarget :: ShowS
    showsTarget
      | isDefaultTarget target = id
      | otherwise = showString ">" . showsPointlike target

    isDefaultTarget :: (Show a) => PointlikeDeterministicPath a -> Bool
    isDefaultTarget (PointlikeDeterministicPath anchor loops) =
      null loops && null (show anchor)

    showsRootBranches [] = id
    showsRootBranches [rb] = showsRootBranch rb
    showsRootBranches (rb : rbs) =
      showsRootBranch rb . showString " & " . showsRootBranches rbs

    showsRootBranch (root, branches) =
      showsBranchSet root branches

    showsRootPrefix dp@(Pointlike p)
      | null p.loops && null (show p.anchor) = id
      | otherwise = showsDeterministicPath 0 dp . showString "<"
    showsRootPrefix dp = showsDeterministicPath 0 dp . showString "<"

    showsBranchSet root branches =
      case toList branches of
        [single] -> showsRootPrefix root . showsPrec 2 single
        multiple -> showsBranchList root multiple

    showsBranchList _ [] = id
    showsBranchList root [b] = showsRootPrefix root . showsPrec 3 b
    showsBranchList root (b : bs) =
      showsRootPrefix root . showsPrec 3 b . showString " & " . showsBranchList root bs

instance Show DPTransition where
  showsPrec _ = showsDPTransition

showsDPTransition :: DPTransition -> ShowS
showsDPTransition = \case
  DPLiteral t -> shows t
  DPWild -> showString "*"
  DPRegex r -> shows r

instance Show DPDirection where
  showsPrec _ = showsDPDirection

showsDPDirection :: DPDirection -> ShowS
showsDPDirection = \case
  DPOutgoing' t -> showsDPTransition t
  DPIncoming' t -> showString "~" . showsDPTransition t

instance (Show a, Ord a) => Show (DPBranch a) where
  showsPrec p branch = showsBranch p branch

showsBranch :: (Show a, Ord a) => Int -> DPBranch a -> ShowS
showsBranch _p (DPSingle dir) = showsDPDirection dir
showsBranch p (DPSequence as1 midpoint bs2) =
  showParen (p > 3) $
    showsBranchSet as1
      . showString "/"
      . showsMidpoint midpoint
      . showString "|"
      . showsBranchSet bs2
  where
    showsMidpoint pdp
      | null pdp.loops = shows pdp.anchor
      | otherwise = showsPointlike pdp

    showsBranchSet branches =
      case toList branches of
        [single] -> showsBranchSingle single
        multiple ->
          showString "("
            . showsBranchList multiple
            . showString ")"

    showsBranchSingle (DPSingle dir) = showsDPDirection dir
    showsBranchSingle other = showsPrec 3 other

    showsBranchList [] = id
    showsBranchList [b] = showsPrec 3 b
    showsBranchList (b : bs) = showsPrec 3 b . showString " & " . showsBranchList bs

spec_showNormalizedPath :: Spec
spec_showNormalizedPath = do
  it "empty path" $
    show (NormalizedPath mempty :: NormalizedPath Anchor)
      `shouldBe` "%never"

  describe "pointlike" do
    it "simple join point" $
      show (NormalizedPath (singletonSet (Pointlike joinPoint)))
        `shouldBe` "@"
    it "pointlike with NID only" $
      show
        ( NormalizedPath
            ( singletonSet
                (Pointlike (specific (unsafeNID "000000000abc")))
            )
        )
        `shouldBe` "@000000000abc"
    it "loop" $
      show
        ( NormalizedPath
            ( singletonSet $
                Pointlike $
                  PointlikeDeterministicPath
                    (JoinPoint mempty)
                    (singletonSet $ DPOutgoing (DPLiteral "foo"))
            )
        )
        `shouldBe` [rq|@["foo"]|]
    it "incoming loop" $
      show
        ( NormalizedPath
            ( singletonSet $
                Pointlike $
                  PointlikeDeterministicPath
                    (JoinPoint mempty)
                    (singletonSet $ DPIncoming (DPLiteral "bar"))
            )
        )
        `shouldBe` [rq|@[~"bar"]|]
    it "two loops" $
      show
        ( NormalizedPath
            ( singletonSet $
                Pointlike $
                  PointlikeDeterministicPath
                    (JoinPoint mempty)
                    ( setFromList
                        [ DPOutgoing DPWild,
                          DPIncoming (DPLiteral "bar")
                        ]
                    )
            )
        )
        `shouldBe` [rq|@[* & ~"bar"]|]

  it "wild loop" $
    show
      ( NormalizedPath
          ( singletonSet $
              Pointlike $
                PointlikeDeterministicPath
                  (JoinPoint mempty)
                  (singletonSet $ DPOutgoing DPWild)
          )
      )
      `shouldBe` "@[*]"

  it "excluding NIDs" $
    show
      ( NormalizedPath
          ( singletonSet $
              Pointlike $
                PointlikeDeterministicPath
                  ( JoinPoint
                      ( setFromList
                          [ unsafeNID "000000000abc",
                            unsafeNID "000000000def"
                          ]
                      )
                  )
                  mempty
          )
      )
      `shouldBe` "!{@000000000abc, @000000000def}"

  it "union of paths" $
    show
      ( NormalizedPath
          ( setFromList
              [ Pointlike joinPoint,
                Pointlike (specific (unsafeNID "000000000abc"))
              ]
          )
      )
      `shouldBe` "@ + @000000000abc"

  describe "rooted" do
    it "simple" $
      show
        ( NormalizedPath
            ( singletonSet $
                Rooted $
                  RootedDeterministicPath
                    (singletonMap (Pointlike unanchored) (singletonSet $ DPOutgoing (DPLiteral "foo")))
                    unanchored
            )
        )
        `shouldBe` [rq|["foo"]|]
    it "anchored destination" $
      show
        ( NormalizedPath
            ( singletonSet $
                Rooted $
                  RootedDeterministicPath
                    (singletonMap (Pointlike unanchored) (singletonSet $ DPOutgoing (DPLiteral "foo")))
                    joinPoint
            )
        )
        `shouldBe` [rq|["foo"]>@|]
    it "two branches" $
      show
        ( NormalizedPath
            ( singletonSet $
                Rooted $
                  RootedDeterministicPath
                    ( singletonMap
                        (Pointlike unanchored)
                        ( setFromList
                            [ DPOutgoing (DPLiteral "foo"),
                              DPOutgoing (DPLiteral "bar")
                            ]
                        )
                    )
                    unanchored
            )
        )
        `shouldBe` [rq|["foo" & "bar"]|]
    it "sequence with /| operator" $
      show
        ( NormalizedPath
            ( singletonSet $
                Rooted $
                  RootedDeterministicPath
                    ( singletonMap
                        (Pointlike unanchored)
                        ( singletonSet $
                            DPSequence
                              (singletonSet $ DPOutgoing (DPLiteral "a"))
                              unanchored
                              (singletonSet $ DPOutgoing (DPLiteral "b"))
                        )
                    )
                    unanchored
            )
        )
        `shouldBe` [rq|["a"/|"b"]|]
    it "sequence with midpoint" $
      show
        ( NormalizedPath
            ( singletonSet $
                Rooted $
                  RootedDeterministicPath
                    ( singletonMap
                        (Pointlike unanchored)
                        ( singletonSet $
                            DPSequence
                              (singletonSet $ DPOutgoing (DPLiteral "a"))
                              joinPoint
                              (singletonSet $ DPOutgoing (DPLiteral "b"))
                        )
                    )
                    unanchored
            )
        )
        `shouldBe` [rq|["a"/@|"b"]|]

    it "nesting/rooting" $
      show
        ( RootedDeterministicPath
            ( mapFromList
                [ (Pointlike $ PointlikeDeterministicPath (JoinPoint mempty) (singletonSet (DPOutgoing DPWild)), singletonSet (DPOutgoing (DPLiteral "a"))),
                  (Pointlike unanchored, singletonSet (DPOutgoing (DPLiteral "b")))
                ]
            )
            joinPoint
        )
        `shouldBe` [rq|[@[*]<"a" & "b"]>@|]

    it "specific" $
      show
        ( RootedDeterministicPath
            ( singletonMap
                (Pointlike $ specific (unsafeNID "000000000xyz"))
                (singletonSet (DPOutgoing (DPLiteral "a")))
            )
            (specific (unsafeNID "000000000abc"))
        )
        `shouldBe` [rq|[@000000000xyz<"a"]>@000000000abc|]

    it "specific intersected" $
      show
        ( RootedDeterministicPath
            ( singletonMap
                (Pointlike $ specific (unsafeNID "000000000xyz"))
                ( setFromList
                    [ DPOutgoing (DPLiteral "a"),
                      DPOutgoing (DPLiteral "b")
                    ]
                )
            )
            (specific (unsafeNID "000000000abc"))
        )
        `shouldBe` [rq|[@000000000xyz<"a" & @000000000xyz<"b"]>@000000000abc|]
