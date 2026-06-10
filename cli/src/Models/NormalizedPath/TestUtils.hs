-- | Test utilities for NormalizedPath, including graph generation for testing.
module Models.NormalizedPath.TestUtils
  ( graphResolvingPath,
    graphResolvingPathFrom,
    graphEdges,
    requiresUnanchoredJoin,
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
-- Unanchored midpoints and Unanchored targets of nested roots don't require
-- a meet ('leastConstrainedNormalizedPath' splits them per branch pair), so
-- only the top-level target is checked.
requiresUnanchoredJoin :: DeterministicPath Anchor -> Bool
requiresUnanchoredJoin = \case
  Pointlike _ -> False
  Rooted r -> r.target.anchor == Unanchored && terminalCount r > 1
  where
    terminalCount r =
      sum [sum (map branchTerminals (toList bs)) | bs <- toList r.rootBranches]
    branchTerminals = \case
      DPSingle _ -> 1
      DPSequence _ _ bs2 -> sum (map branchTerminals (toList bs2))

-- | Extract all edges from a graph
graphEdges :: Graph Text () -> [Edge Text]
graphEdges g =
  [ outgoingEdge (node ^. #nid) conn
  | node <- nodesOf g,
    conn <- toList (node ^. #outgoing)
  ]
