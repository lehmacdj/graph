-- | Test utilities for NormalizedPath, including graph generation for testing.
module Models.NormalizedPath.TestUtils
  ( graphResolvingPath,
    graphResolvingPathFrom,
    graphEdges,
    requiresUnanchoredJoin,
    coveredPathFrom,
  )
where

import Models.Edge
import Models.Graph
import Models.NID
import Models.Node (emptyNode)
import Models.NormalizedPath.Anchor
import Models.NormalizedPath.Traversals
import Models.NormalizedPath.Types
import MyPrelude
import Polysemy.State
import System.Random

-- | Generate a graph that fully resolves the given NormalizedPath.
-- Uses a random NID as the starting point.
graphResolvingPath ::
  StdGen ->
  NormalizedPath Anchor ->
  Graph Text ()
graphResolvingPath gen path =
  let (startNid, gen') = uniform gen
   in graphResolvingPathFrom gen' startNid path

-- | Generate a graph that fully resolves the given NormalizedPath from a
-- specific starting NID.
--
-- Anchors are resolved to concrete NIDs up front (see 'resolveAnchorNids'),
-- one NID per anchor occurrence. Resolving each occurrence exactly once is
-- what makes the generated graph actually resolve the path:
--
-- * a loop branch's closing transition targets the loop's own root instead
--   of a fresh node, and
-- * a join point shared by several branches resolves to a single node, so
--   the branches meet.
--
-- Edges are then minted for every transition of the NID-resolved path, and
-- a node is created for every resolved anchor (so even transition-less
-- paths like @\@5@ resolve).
graphResolvingPathFrom ::
  StdGen ->
  NID ->
  NormalizedPath Anchor ->
  Graph Text ()
graphResolvingPathFrom gen startNid path =
  let (edges, (_, pathNids)) =
        run
          . runState @[Edge Text] []
          . runState gen
          $ do
            pathNids <- resolveAnchorNids startNid path
            traverseViaTransitions_ emitTransition pathNids
            pure pathNids
      (anchorNids, _) = traverseAnchors (\nid -> ([nid], nid)) pathNids
   in emptyGraph
        & insertNodes (map emptyNode (ordNub anchorNids))
        & insertEdges edges
  where
    emitTransition ::
      (Member (State StdGen) r, Member (State [Edge Text]) r) =>
      NID ->
      DPDirection ->
      NID ->
      Sem r ()
    emitTransition source direction target = do
      let emit label = modify @[Edge Text] $ case direction of
            DPOutgoing' _ -> (Edge source label target :)
            -- Incoming means the edge goes FROM target TO source
            DPIncoming' _ -> (Edge target label source :)
      case directionTransition direction of
        DPLiteral t -> emit t
        DPWild -> emit =<< generateLabel
        -- simplified: assume the regex matches "match"
        DPRegex _ -> emit "match"

    directionTransition = \case
      DPOutgoing' t -> t
      DPIncoming' t -> t

    generateLabel :: (Member (State StdGen) r) => Sem r Text
    generateLabel = do
      idx :: Int <- embedStateful $ uniformR (0, 25)
      pure $ singleton $ toEnum (fromEnum 'a' + idx)

-- | Resolve every anchor of a path to a concrete NID, one NID per anchor
-- occurrence. Root-position anchors (the positions a traversal starts from)
-- resolve to the start NID, matching how resolution unifies them with the
-- current position; other Unanchored/JoinPoint anchors get fresh random
-- NIDs. JoinPoint exclusion sets are ignored: collisions with random NIDs
-- are negligible.
resolveAnchorNids ::
  (Member (State StdGen) r) =>
  NID ->
  NormalizedPath Anchor ->
  Sem r (NormalizedPath NID)
resolveAnchorNids startNid (NormalizedPath dps) =
  NormalizedPath . setFromList <$> traverse convertDp (toList dps)
  where
    convertDp = \case
      Pointlike p -> Pointlike <$> convertPointlike True p
      Rooted r -> Rooted <$> convertRooted r

    convertRooted (RootedDeterministicPath rootBranches target) =
      -- distinct roots can resolve to the same NID (e.g. a JoinPoint root
      -- and an Unanchored root both resolve to the start NID); merge their
      -- branch sets instead of letting mapFromList drop one
      smartBuildRootedDeterministicPath
        . unionsWith (<>)
        . map (uncurry singletonMap)
        <$> traverse convertRootBranch (mapToList rootBranches)
        <*> convertPointlike False target

    convertRootBranch (root, branches) =
      (,)
        <$> convertDp root
        <*> (setFromList <$> traverse convertBranch (toList branches))

    convertPointlike isRoot (PointlikeDeterministicPath anchor loops) =
      PointlikeDeterministicPath
        <$> resolveAnchor isRoot anchor
        <*> (setFromList <$> traverse convertBranch (toList loops))

    convertBranch = \case
      DPSingle d -> pure $ DPSingle d
      DPSequence bs1 midpoint bs2 ->
        smartBuildSequence
          <$> (setFromList <$> traverse convertBranch (toList bs1))
          <*> convertPointlike False midpoint
          <*> (setFromList <$> traverse convertBranch (toList bs2))

    resolveAnchor isRoot = \case
      Specific nid -> pure nid
      _
        | isRoot -> pure startNid
        | otherwise -> embedStateful uniform

-- | Whether resolving this path requires several branches to meet at an
-- Unanchored target. 'leastConstrainedNormalizedPath' turns a rooted path's
-- Unanchored target into a join point, so an intersection landing on one
-- must meet there to resolve — but residuals never pin Unanchored anchors,
-- so the residual of such a path may not be sufficient to make it resolve.
-- Paths matching this predicate are exempt from the residual invariant and
-- are excluded from the property tests; see
-- docs/pathResidualsUnresolvedBy-spec.md.
--
-- Only the top-level target is checked, but the terminal count must follow
-- 'leastConstrainedNormalizedPath''s explosion: an unanchored midpoint
-- multiplies its sequence per (as1, bs2) branch pair, and a nested root
-- with an Unanchored target multiplies its outer branches per nested
-- (root, branch) pair — every exploded copy terminates at the top-level
-- target and must meet there.
requiresUnanchoredJoin :: DeterministicPath Anchor -> Bool
requiresUnanchoredJoin = \case
  Pointlike _ -> False
  Rooted r -> r.target.anchor == Unanchored && terminalCount r > 1
  where
    terminalCount :: RootedDeterministicPath Anchor -> Int
    terminalCount r =
      sum
        [ rootMultiplier root * sum (map branchTerminals (toList bs))
        | (root, bs) <- mapToList r.rootBranches
        ]
    -- the number of exploded (root, branch) pairs a nested root with an
    -- Unanchored target contributes as prefixes of each outer branch
    rootMultiplier = \case
      Rooted nested
        | nested.target.anchor == Unanchored ->
            sum
              [ rootMultiplier root * sum (map branchCopies (toList bs))
              | (root, bs) <- mapToList nested.rootBranches
              ]
      _ -> 1
    -- how many branches this branch explodes into
    branchCopies = \case
      DPSingle _ -> 1
      b@(DPSequence as1 _ bs2)
        | isPairMidpoint b ->
            sum (map branchCopies (toList as1)) * sum (map branchCopies (toList bs2))
        | otherwise -> 1
    -- how many terminals this branch has after explosion
    branchTerminals = \case
      DPSingle _ -> 1
      b@(DPSequence as1 _ bs2)
        | isPairMidpoint b ->
            sum (map branchCopies (toList as1))
              * sum (map branchTerminals (toList bs2))
        | otherwise -> sum (map branchTerminals (toList bs2))
    isPairMidpoint = \case
      DPSequence _ m _ -> m.anchor == Unanchored && null m.loops
      DPSingle _ -> False

-- | Rewrite a generated path so the residual property tests' preconditions
-- hold by construction, instead of being filtered for with
-- 'Test.QuickCheck.suchThat' / '==>':
--
-- * a path falling under the residual invariant's carve-out (see
--   'requiresUnanchoredJoin') has its top-level Unanchored target
--   normalized to @JoinPoint ∅@ — the spec's "append @>\@@" normalization,
--   and exactly how 'leastConstrainedNormalizedPath' already interprets
--   such a target at resolution time;
-- * @startNid@ is removed from every JoinPoint exclusion set: a
--   root-position JoinPoint excluding the start NID can never resolve from
--   it no matter which edges are created, so such paths fail the "the
--   generated graph resolves the path" precondition. (Non-root exclusion
--   sets are stripped too for simplicity; the generator resolves those
--   anchors to fresh uniformly random NIDs, so their exclusions essentially
--   never bite either way.)
coveredPathFrom :: NID -> DeterministicPath Anchor -> DeterministicPath Anchor
coveredPathFrom startNid dp =
  dp & normalizeUnanchoredJoin & allowStartNid
  where
    normalizeUnanchoredJoin = \case
      d@(Rooted r)
        | requiresUnanchoredJoin d ->
            Rooted r {target = r.target {anchor = JoinPoint mempty}}
      d -> d
    allowStartNid d =
      NormalizedPath (singletonSet d)
        & traverseAnchors (Identity . dropStart)
        & runIdentity
        & \case
          NormalizedPath (toList -> [d']) -> d'
          _ -> error "coveredPathFrom: singleton union changed size"
    dropStart = \case
      JoinPoint excluding -> JoinPoint (deleteSet startNid excluding)
      a -> a

-- | Extract all edges from a graph
graphEdges :: Graph Text () -> [Edge Text]
graphEdges g =
  [ outgoingEdge (node ^. #nid) conn
  | node <- nodesOf g,
    conn <- toList (node ^. #outgoing)
  ]
