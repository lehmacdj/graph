{-# LANGUAGE OverloadedRecordDot #-}

module Models.NormalizedPath.UnresolvedResiduals
  ( pathResidualsUnresolvedBy,
    spec_pathResidualsUnresolvedBy,
  )
where

import Effectful.State.Dynamic
import Graph.GraphMetadataEditing (runInMemoryGraphMetadataEditing)
import Graph.Resolve (resolveNPathExists)
import Models.Connect (Connect, Connect' (..), matchConnect)
import Models.Graph
import Models.NID
import Models.Node
import Models.NormalizedPath
import Models.NormalizedPath.Generators
import Models.NormalizedPath.Parse (pNormalizedPath)
import Models.NormalizedPath.TestUtils
import MyPrelude
import System.Random
import Utils.Testing

-- | The residual alternatives of a path: each element of the result is a
-- complete description of the edges whose creation makes the path
-- resolvable from the given start NID in the given graph. For generality
-- this operates on `NormalizedPath Anchor` rather than `FullyAnchored` or
-- `NID`.
--
-- The result is a meta-union: ANY ONE alternative suffices (the preferred
-- one is first; the list is deduplicated). Within one alternative the
-- ordinary NormalizedPath union means "create every member". The result is
-- exactly @pure %never@ iff the path already fully resolves (every union
-- member, every root). Alternatives arise where the graph — not the path —
-- is ambiguous about what is missing (several recovered seams, fan-outs,
-- arrival choices); see docs/pathResidualsUnresolvedBy-spec.md (v2).
--
-- This can be used to get the residual of a path that wasn't fully
-- resolved by resolvePath if you pass the non-thin graph from the
-- ResolvedPath and the same starting NID that was used to resolve it, and
-- then to create the new nodes/transitions that would have been necessary
-- for it to be resolved (the future @mk@ command).
--
-- Note: behavior for intersections whose target is Unanchored is dubious:
-- residuals never pin Unanchored anchors, but resolution requires such
-- branches to meet at a join point, so the residual may not be sufficient
-- to make the path resolve. Callers that want join semantics must anchor
-- the target (e.g. by appending @>\@@). See
-- docs/pathResidualsUnresolvedBy-spec.md.
pathResidualsUnresolvedBy ::
  NID ->
  Graph Text () ->
  NormalizedPath Anchor ->
  NonEmpty (NormalizedPath Anchor)
pathResidualsUnresolvedBy startNid graph (NormalizedPath dps) =
  let memberAlts = [residualOfDp startNid graph dp | dp <- toList dps]
      alts =
        ordNub
          [ NormalizedPath (setFromList (concat combo))
          | combo <- combineAlts memberAlts
          ]
   in case alts of
        (a : as) -> a :| as
        [] -> error "pathResidualsUnresolvedBy: no alternatives"

-- | Combine per-site alternatives with the full cartesian product (§4.2).
-- The all-preferred combination comes first, followed by combinations
-- varying one site, then two, and so on. Within one tier, earlier sites
-- vary first. Higher-order combinations are required when the exact repair
-- uses non-preferred graph-derived choices at independent ambiguity sites.
-- Enumerating by tier directly is important: sorting the full product would
-- force every later combination before the preferred one can be consumed.
combineAlts :: [[a]] -> [[a]]
combineAlts xss
  | any null xss = []
  | otherwise =
      [ materialize varied
      | variedCount <- [0 .. length xss],
        varied <- variations variedCount 0
      ]
  where
    -- Each variation is ordered by its list of (site, choice) pairs, which
    -- is the same ordering the former sort key imposed within a tier.
    variations 0 _ = [[]]
    variations count firstSite = do
      site <- [firstSite .. length xss - count]
      (choice, value) <- drop 1 (zip [0 :: Int ..] (xss `indexEx` site))
      rest <- variations (count - 1) (site + 1)
      pure ((site, choice, value) : rest)

    materialize = go 0 xss
      where
        go _ [] _ = []
        go site (xs : rest) changes = case changes of
          ((changedSite, _, value) : remaining)
            | changedSite == site -> value : go (site + 1) rest remaining
          _ -> headEx xs : go (site + 1) rest changes

-- | The residual alternatives of one deterministic path; each alternative
-- is the list of union members to create.
residualOfDp ::
  NID -> Graph Text () -> DeterministicPath Anchor -> [[DeterministicPath Anchor]]
residualOfDp startNid g dp
  | dpUnsatisfiable startNid dp = [[dp]]
  | otherwise =
      let alts = case dp of
            Pointlike p -> residualPointlikeAlts startNid g p
            Rooted r -> residualRootedAlts startNid g r
       in if any (isJust . (.landed)) alts
            then [[]]
            else case ordNub [dpAltMembers a | a <- alts, a.progress] of
              [] -> [[dp]]
              ms -> ms

-- | A deterministic path that no edge creation can make resolve: a root
-- (recursively including nested Rooted roots) that is a JoinPoint
-- excluding the start NID can never unify with it, and resolution is
-- conjunctive across roots (§3 of the spec). Such paths are returned
-- unchanged; their branches are not dropped.
dpUnsatisfiable :: NID -> DeterministicPath Anchor -> Bool
dpUnsatisfiable startNid = \case
  Pointlike p -> rootAnchorUnsat p.anchor
  Rooted r -> any (dpUnsatisfiable startNid) (keys r.rootBranches)
  where
    rootAnchorUnsat = \case
      JoinPoint exc -> startNid `member` exc
      _ -> False

-- * Anchors

-- | Unify a root-position anchor with the start NID (§5 of the spec):
-- Specific anchors jump, JoinPoint/Unanchored unify with the start NID.
unifyRootAnchor :: NID -> Anchor -> Maybe NID
unifyRootAnchor startNid = \case
  Specific n -> Just n
  JoinPoint exc -> justIfTrue (startNid `notMember` exc) startNid
  Unanchored -> Just startNid

-- | Pin a JoinPoint anchor to the NID it unified with; Unanchored is never
-- pinned. Pinned anchors only end up in the result when the containing
-- deterministic path made progress: a no-progress path is returned
-- unchanged before any pinned anchor is used.
pinAnchor :: Anchor -> NID -> Anchor
pinAnchor a n = case a of
  Unanchored -> Unanchored
  JoinPoint _ -> Specific n
  Specific m -> Specific m

-- | The end context of a midpoint position. An Unanchored midpoint is an
-- implicit pair point: 'leastConstrainedNormalizedPath' explodes it per
-- branch pair, but intersections meeting it must still agree on a landing.
anchorCtx :: Anchor -> EndCtx
anchorCtx = \case
  Unanchored -> EndPair
  JoinPoint exc -> EndJoin exc
  Specific n -> EndAt (singletonSet n)

-- | The end context of a rooted path's target. An Unanchored target is
-- fully open: the residual never pins it (§3 carve-out).
targetEndCtx :: Anchor -> EndCtx
targetEndCtx = \case
  Unanchored -> EndOpen
  JoinPoint exc -> EndJoin exc
  Specific n -> EndAt (singletonSet n)

anchorAdmits :: Anchor -> NID -> Bool
anchorAdmits a v = case a of
  JoinPoint exc -> v `notMember` exc
  Specific n -> v == n
  Unanchored -> True

specificPdp :: NID -> PointlikeDeterministicPath Anchor
specificPdp n = PointlikeDeterministicPath (Specific n) mempty

specificDp :: NID -> DeterministicPath Anchor
specificDp = Pointlike . specificPdp

-- * Layer 0: bidirectional segment resolution

-- | What a linear run must arrive at.
data EndCtx
  = -- | unconstrained landing (an Unanchored target, never pinned)
    EndOpen
  | -- | an implicit pair point inside a branch: unconstrained for
    -- matching, but an intersection ending here must agree on one landing
    EndPair
  | -- | land anywhere outside the exclusion set (unpinned join point)
    EndJoin (Set NID)
  | -- | land exactly at one of these
    EndAt (Set NID)

-- | The ends a gap that runs all the way to the end context can take, one
-- alternative per element: a gap into a Specific end lands exactly there
-- (per-NID alternatives when several); a gap into an open/join end runs to
-- the (possibly unpinned) end position.
gapEndAlts :: EndCtx -> [Maybe NID]
gapEndAlts = \case
  EndAt s -> [Just n | n <- toList s]
  EndJoin _ -> [Nothing]
  EndOpen -> [Nothing]
  EndPair -> [Nothing]

data SegResult
  = -- | frontiers met; the meeting NIDs, and the subset reachable without
    -- any self-loop edge matching a non-loop step (§5.1 — empty when the
    -- resolution depends on a coincidental self-loop match)
    SegResolved (Set NID) (Set NID)
  | -- | maximal-consumption splits, preferred first: either split's
    -- repair suffices, so ties are alternatives (§3)
    SegGaps [SegGap]

-- | One split of a gapped run: fwd consumed count, fwd frontier, gap
-- steps, bwd consumed count, and the recovered seams paired with the
-- landings their resolved suffix reaches (for reach pinning, §5.2).
data SegGap = SegGap Int (Set NID) [DPDirection] Int [(NID, Set NID)]

matchTransition :: Set (Connect Text) -> DPTransition -> Set NID
matchTransition conns = \case
  DPLiteral l -> setFromList (matchConnect l conns)
  DPWild -> setFromList (map (.node) (toList conns))
  DPRegex r ->
    setFromList [c.node | c <- toList conns, has (textRegexing r) c.transition]

stepForward :: Graph Text () -> NID -> DPDirection -> Set NID
stepForward g nid dir = case maybeLookupNode g nid of
  Nothing -> mempty
  Just n -> case dir of
    DPOutgoing' t -> matchTransition n.outgoing t
    DPIncoming' t -> matchTransition n.incoming t

-- | The exact inverse of 'stepForward' (§2 of the spec), nothing more: an
-- edge matched in the wrong orientation would produce residuals that
-- 'resolveNPath' cannot actually resolve.
stepBackward :: Graph Text () -> NID -> DPDirection -> Set NID
stepBackward g nid dir = case maybeLookupNode g nid of
  Nothing -> mempty
  Just n -> case dir of
    DPOutgoing' t -> matchTransition n.incoming t
    DPIncoming' t -> matchTransition n.outgoing t

-- | Frontiers after each consumed step, stopping before the first empty one.
frontierTrail ::
  (NID -> DPDirection -> Set NID) ->
  Set NID ->
  [DPDirection] ->
  [Set NID]
frontierTrail step = go
  where
    go f = \case
      [] -> []
      (d : ds) ->
        let f' = unions [step n d | n <- toList f]
         in if null f' then [] else f' : go f' ds

-- | Bidirectionally match one linear run of directed steps between a forward
-- frontier and the end context.
--
-- When @openBwd@ is set (the run ends its branch), an open/join end still
-- gets a backward pass starting from every node of the graph: matching the
-- run's suffix anywhere recovers a concrete seam node for the residual.
--
-- Split selection (§4.1.1): among (k forward, j backward) splits, maximize
-- k + j; ties prefer the split whose progress does not depend on a
-- self-loop edge matching a non-loop step (§5.1); remaining ties prefer
-- the larger k (forward wins). Every maximal split is returned — the
-- preferred one first, the rest as alternatives, since either split's
-- repair suffices (§3). A gap always keeps at least one step
-- (k + j <= length - 1); consuming every step but missing the backward
-- frontier is not progress — the run failed as a whole.
resolveSegment ::
  Graph Text () -> Bool -> Set NID -> [DPDirection] -> EndCtx -> SegResult
resolveSegment g openBwd start steps end
  | kAll == len,
    Just met <- meets fwdFinal =
      let metClean
            | length fwdsClean == len = fromMaybe mempty (meets (lastEx fwdsClean))
            | otherwise = mempty
       in SegResolved met metClean
  | otherwise =
      let maxSum = maximumEx [k + j | (k, j) <- splits]
          maximal =
            sortOn (\s -> (not (clean s), negate (fst s))) $
              filter (\(k, j) -> k + j == maxSum) splits
          gapAt (k, j) =
            let fF = if k == 0 then start else fwds `indexEx` (k - 1)
                seams
                  | j == 0 = []
                  | otherwise =
                      [(s, reachOf j s) | s <- toList (bwds `indexEx` (j - 1))]
             in SegGap k fF (take (len - k - j) (drop k steps)) j seams
       in SegGaps (map gapAt maximal)
  where
    len = length steps
    fwdStep = stepForward g
    bwdStep = stepBackward g
    noSelf step n d = step n d `difference` singletonSet n
    fwds = frontierTrail fwdStep start steps
    kAll = length fwds
    fwdFinal = fromMaybe start (lastMay fwds)
    meets f = case end of
      EndOpen -> Just f
      EndPair -> Just f
      EndJoin exc ->
        let ok = f `difference` exc
         in justIfTrue (not (null ok)) ok
      EndAt s ->
        let ok = f `intersection` s
         in justIfTrue (not (null ok)) ok
    bwdSeed = case end of
      EndAt s -> Just s
      EndJoin exc -> justIfTrue openBwd (nidSet g `difference` exc)
      EndOpen -> justIfTrue openBwd (nidSet g)
      EndPair -> justIfTrue openBwd (nidSet g)
    bwds = maybe [] (\s -> frontierTrail bwdStep s (reverse steps)) bwdSeed
    fwdsClean = frontierTrail (noSelf fwdStep) start steps
    bwdsClean = maybe [] (\s -> frontierTrail (noSelf bwdStep) s (reverse steps)) bwdSeed
    clean (k, j) = k <= length fwdsClean && j <= length bwdsClean
    splits =
      [ (k, j)
      | k <- [0 .. min kAll (len - 1)],
        j <- [0 .. min (length bwds) (len - 1 - k)]
      ]
    -- where the suffix the backward pass consumed lands when resolved
    -- forward from the seam, filtered to admissible landings
    reachOf j s =
      let landing =
            fromMaybe (singletonSet s) $
              lastMay (frontierTrail fwdStep (singletonSet s) (drop (len - j) steps))
       in case end of
            EndAt s0 -> landing `intersection` s0
            EndJoin exc -> landing `difference` exc
            EndOpen -> landing
            EndPair -> landing

-- * Residual building blocks

type ABranches = OSet (DPBranch Anchor)

type ARootsMap = OMap (DeterministicPath Anchor) ABranches

-- | Where a residual gap resumes from.
data GapRoots
  = -- | nothing was consumed before the gap: it begins at the position the
    -- branch started from, and the enclosing structure decides how to
    -- anchor it (root form, station key, ...)
    AtStart ABranches
  | -- | concrete resume points with the unresolved branches from each
    AtRoots ARootsMap
  deriving stock (Eq, Ord)

data Gap = Gap
  { roots :: GapRoots,
    -- | Nothing = the gap runs to the (possibly unpinned) end position
    end :: Maybe NID
  }
  deriving stock (Eq, Ord)

-- | One alternative of resolving a branch.
data BrRes = BrRes
  { progress :: Bool,
    dones :: [DeterministicPath Anchor],
    outcome :: Either [Gap] (Set NID),
    -- | even when the outcome is gapped, the branch's terminal position may
    -- have been reached by its fully-resolved suffix (reach pinning, §5.2)
    reach :: Maybe (Set NID),
    -- | for a resolved outcome: the subset of the landing reachable
    -- without any self-loop edge matching a non-loop step (§5.1);
    -- Nothing = the whole landing is clean
    cleanLanding :: Maybe (Set NID),
    -- | Instantiating this gap toward a fresh enclosing target would lose
    -- an internal meet with graph structure that was omitted from the
    -- residual. Such a gap must be recreated wholesale when the enclosing
    -- JoinPoint stays unpinned.
    freshTargetUnsafe :: Bool
  }
  deriving stock (Eq, Ord)

-- | A 'BrRes' with no terminal reach or self-loop-cleanliness information.
brRes :: Bool -> [DeterministicPath Anchor] -> Either [Gap] (Set NID) -> BrRes
brRes p ds o = BrRes p ds o Nothing Nothing False

-- | Merge the fully-resolving alternatives of one site into a single
-- first alternative and drop the gapped ones (§4.2.2): a landing union is
-- valid resolution semantics (each landing is individually achievable),
-- and applying this at every alternative-producing site keeps resolution
-- detection compositional under the lazy tiered 'combineAlts' enumeration.
collapseResolved :: [BrRes] -> [BrRes]
collapseResolved alts =
  case [(r, ls) | r <- alts, Right ls <- [r.outcome]] of
    [] -> alts
    [(r, _)] -> [r]
    resolved ->
      [ BrRes
          (any ((.progress) . fst) resolved)
          (ordNub (concatMap ((.dones) . fst) resolved))
          (Right (unions (map snd resolved)))
          Nothing
          (Just (unions [fromMaybe ls r.cleanLanding | (r, ls) <- resolved]))
          (any ((.freshTargetUnsafe) . fst) resolved)
      ]

-- | The terminal position a branch result got to, resolved or not.
resReach :: BrRes -> Maybe (Set NID)
resReach r = either (const r.reach) Just r.outcome

-- | The gaps of a branch result; empty for a fully-resolved outcome.
resGaps :: BrRes -> [Gap]
resGaps r = fromLeft [] r.outcome

-- | The landing of the first fully-resolved alternative, if any. By the
-- collapse rule (fully-resolving alternatives merge into one, see
-- 'stationFromArrivals') there is at most one.
firstRight :: [BrRes] -> Maybe (Set NID)
firstRight rs = headMay [ls | r <- rs, Right ls <- [r.outcome]]

isAtStartGap :: Gap -> Bool
isAtStartGap gp = case gp.roots of
  AtStart _ -> True
  AtRoots _ -> False

gapRootsMap :: Gap -> ARootsMap
gapRootsMap gp = case gp.roots of
  AtRoots rm -> rm
  AtStart _ -> error "gapRootsMap: unanchored gap escaped its position"

mkRootedDp ::
  ARootsMap -> PointlikeDeterministicPath Anchor -> DeterministicPath Anchor
mkRootedDp rm t =
  Rooted . flip smartBuildRootedDeterministicPath t . unionsWith (<>) $
    [uncurry singletonMap (smartBuildRootBranch k v) | (k, v) <- mapToList rm]

upsertAssoc :: (Eq k) => (v -> v -> v) -> k -> v -> [(k, v)] -> [(k, v)]
upsertAssoc f k v = \case
  [] -> [(k, v)]
  ((k', v') : rest)
    | k' == k -> (k', f v' v) : rest
    | otherwise -> (k', v') : upsertAssoc f k v rest

intersectAll :: [Set NID] -> Set NID
intersectAll = \case
  [] -> mempty
  (x : xs) -> foldl' intersection x xs

combineLandings :: EndCtx -> [Set NID] -> Maybe (Set NID)
combineLandings ctx ls = case ctx of
  -- an Unanchored target imposes no meet (§3 carve-out)
  EndOpen -> Just (unions ls)
  _ ->
    let m = intersectAll ls
     in justIfTrue (not (null m)) m

-- * Branch structure flattening

-- | A branch viewed as maximal linear runs separated by boundaries.
data Part
  = PRun [DPDirection]
  | PMid (PointlikeDeterministicPath Anchor)
  | PSplit ABranches

flattenBranch :: DPBranch Anchor -> [Part]
flattenBranch = mergeRuns . go
  where
    go = \case
      DPSingle d -> [PRun [d]]
      DPSequence bs1 m bs2 -> goSet bs1 <> midPart m <> goSet bs2
    goSet bs = case toList bs of
      [b] -> go b
      _ -> [PSplit bs]
    midPart m
      | Unanchored <- m.anchor, null m.loops = []
      | otherwise = [PMid m]
    mergeRuns = \case
      (PRun a : PRun b : rest) -> mergeRuns (PRun (a <> b) : rest)
      (p : rest) -> p : mergeRuns rest
      [] -> []

runToBranch :: [DPDirection] -> DPBranch Anchor
runToBranch = \case
  [] -> error "runToBranch: empty run"
  [d] -> DPSingle d
  (d : ds) ->
    smartBuildSequence
      (singletonSet (DPSingle d))
      unanchored
      (singletonSet (runToBranch ds))

invertDirection :: DPDirection -> DPDirection
invertDirection = \case
  DPOutgoing' t -> DPIncoming' t
  DPIncoming' t -> DPOutgoing' t

invertedRun :: [DPDirection] -> [DPDirection]
invertedRun = reverse . map invertDirection

-- | Invert a branch that is one pure linear run.
invertPureRun :: DPBranch Anchor -> Maybe (DPBranch Anchor)
invertPureRun b = case flattenBranch b of
  [PRun ds] -> Just (runToBranch (invertedRun ds))
  _ -> Nothing

-- | Reverse a branch while preserving all of the edges it denotes. This is
-- used for a nested Unanchored root's exact-repair alternative: rooting the
-- reversed residual at the recovered landing avoids specializing the
-- original Unanchored target.
reverseBranch :: DPBranch Anchor -> DPBranch Anchor
reverseBranch = \case
  DPSingle direction -> DPSingle (invertDirection direction)
  DPSequence before midpoint suffix ->
    smartBuildSequence
      (mapOSet reverseBranch suffix)
      (reversePointlike midpoint)
      (mapOSet reverseBranch before)
  where
    reversePointlike PointlikeDeterministicPath {..} =
      PointlikeDeterministicPath anchor (mapOSet reverseBranch loops)

-- | Reverse every pointlike-rooted member of a residual between its
-- recovered target and concrete roots. The result creates exactly the same
-- edges, but the Unanchored target occurrence is gone rather than pinned.
invertResidualAt ::
  NID -> NID -> DeterministicPath Anchor -> Maybe [DeterministicPath Anchor]
invertResidualAt startNid landing = \case
  Rooted RootedDeterministicPath {..} -> do
    traverse reverseRoot (mapToList rootBranches)
    where
      reverseRoot (Pointlike root, branches) = do
        rootNid <- unifyRootAnchor startNid root.anchor
        let reversedRoot =
              PointlikeDeterministicPath
                (Specific landing)
                (mapOSet reverseBranch target.loops)
            reversedTarget =
              PointlikeDeterministicPath
                (Specific rootNid)
                (mapOSet reverseBranch root.loops)
        pure $
          Rooted $
            smartBuildRootedDeterministicPath
              ( singletonMap
                  (Pointlike reversedRoot)
                  (mapOSet reverseBranch branches)
              )
              reversedTarget
      reverseRoot (Rooted _, _) = Nothing
  Pointlike _ -> Nothing

rebuildParts :: [Part] -> ABranches
rebuildParts = \case
  [] -> error "rebuildParts: empty parts"
  [PRun ds] -> singletonSet (runToBranch ds)
  [PSplit bs] -> bs
  (PRun ds : PMid m : rest) ->
    singletonSet $
      smartBuildSequence (singletonSet (runToBranch ds)) m (rebuildParts rest)
  (PRun ds : rest) ->
    singletonSet $
      smartBuildSequence
        (singletonSet (runToBranch ds))
        unanchored
        (rebuildParts rest)
  (PSplit bs : PMid m : rest) ->
    singletonSet $ smartBuildSequence bs m (rebuildParts rest)
  (PSplit bs : rest) ->
    singletonSet $ smartBuildSequence bs unanchored (rebuildParts rest)
  (PMid _ : _) -> error "rebuildParts: leading midpoint"

-- * Gap orientation and assembly (§6)

-- | Orientation of deconstructed loop gaps at a station node @n@. The
-- original (forward) orientation is preferred; a k=0 gap (rooted at @n@
-- itself, ending at a recovered seam) is re-expressed inverted — rooted at
-- the seam, ending at @n@ — only when that buys structure: chaining
-- through the station into a continuation that leaves it, or merging with
-- other gaps that end at @n@ into one deterministic path.
orientGaps :: NID -> Bool -> [Gap] -> [Gap]
orientGaps n contAtN gaps
  | doInvert = map (\gp -> fromMaybe gp (invertAtN gp)) gaps
  | otherwise = gaps
  where
    endsAtN gp = gp.end == Just n
    invertAtN gp = do
      guard (not (endsAtN gp))
      s <- gp.end
      rm <- case gp.roots of
        AtRoots rm -> Just rm
        AtStart _ -> Nothing
      (rk, bs) <- case mapToList rm of
        [entry] -> Just entry
        _ -> Nothing
      guard (rk == specificDp n)
      ibs <- traverse invertPureRun (toList bs)
      pure (Gap (AtRoots (singletonMap (specificDp s) (setFromList ibs))) (Just n))
    nEligible = length (filter (isJust . invertAtN) gaps)
    nAtN = length (filter endsAtN gaps)
    doInvert = nEligible > 0 && (contAtN || nAtN > 0 || nEligible >= 2)

-- | Assemble the pieces at a pointlike station (a root pointlike, a
-- midpoint, or a nested root's landing): orient its loop gaps, chain the
-- ones ending at the station into the continuation gaps that leave it
-- (AtStart continuation gaps root at the station key), and emit the form
-- standalone when nothing roots at it but it still carries structure.
assembleStation ::
  -- | station node, when concrete
  Maybe NID ->
  -- | root key for AtStart continuation gaps when no loop chaining applies
  DeterministicPath Anchor ->
  -- | emit the form standalone even without AtStart gaps
  Bool ->
  -- | attached (zero-consumption) loops, already part of the form
  ABranches ->
  -- | deconstructed loop gaps
  [Gap] ->
  -- | continuation gaps, in branch declaration order
  [Gap] ->
  ([Gap], [DeterministicPath Anchor])
assembleStation mn form formEmit attached loopGaps contGaps =
  let hasAtStart = any isAtStartGap contGaps
      oriented = case mn of
        Just n -> orientGaps n hasAtStart loopGaps
        Nothing -> loopGaps
      (chainable, offGaps) = case mn of
        Just n
          | hasAtStart -> partition ((== Just n) . (.end)) oriented
        _ -> ([], oriented)
      key = case (chainable, mn) of
        (_ : _, Just n) ->
          mkRootedDp
            (unionsWith (<>) (map gapRootsMap chainable))
            (PointlikeDeterministicPath (Specific n) attached)
        _ -> form
      convert gp = case gp.roots of
        AtStart bs -> gp {roots = AtRoots (singletonMap key bs)}
        AtRoots _ -> gp
      formDone = [form | not hasAtStart, formEmit || not (null attached)]
   in (map convert contGaps <> offGaps, formDone)

-- | Merge a gap that ends at a concrete node into the (unique) gap whose
-- root map resumes from exactly that node (§6 preference 1: one connected
-- deterministic path per alternative wherever ends allow).
chainGaps :: [Gap] -> [Gap]
chainGaps = go
  where
    go gaps = maybe gaps go (findChain gaps)
    findChain gaps = headMay do
      (i, gi) <- zip [0 :: Int ..] gaps
      x <- maybeToList gi.end
      rmi <- case gi.roots of
        AtRoots rm -> [rm]
        AtStart _ -> []
      -- gi must be the only gap ending at x
      guard (length (filter ((== Just x) . (.end)) gaps) == 1)
      (jIdx, gj) <- zip [0 :: Int ..] gaps
      guard (jIdx /= i)
      rmj <- case gj.roots of
        AtRoots rm -> [rm]
        AtStart _ -> []
      bs <- maybeToList (lookup (specificDp x) rmj)
      -- and gj the only gap resuming from x
      guard
        ( length
            [ ()
            | (kIdx, gk) <- zip [0 :: Int ..] gaps,
              kIdx /= i,
              AtRoots rmk <- [gk.roots],
              specificDp x `member` rmk
            ]
            == 1
        )
      let newEntry =
            uncurry singletonMap (smartBuildRootBranch (mkRootedDp rmi (specificPdp x)) bs)
          rmj' =
            unionsWith (<>) $
              [ if k == specificDp x then newEntry else singletonMap k v
              | (k, v) <- mapToList rmj
              ]
          gj' = gj {roots = AtRoots rmj'}
      pure
        [ if idx == jIdx then gj' else gk
        | (idx, gk) <- zip [0 :: Int ..] gaps,
          idx /= i
        ]

-- | Build the residual members from a target pointlike and the collected
-- gaps: gaps that run to the target (or end at its pin) merge into one
-- main rooted path; the rest group by their end node into standalones.
buildMembers ::
  PointlikeDeterministicPath Anchor ->
  Maybe NID ->
  [Gap] ->
  (Maybe (DeterministicPath Anchor), [DeterministicPath Anchor])
buildMembers targetPdp targetPin gaps =
  let gaps' = chainGaps gaps
      isMain gp = case gp.end of
        Nothing -> True
        Just x -> Just x == targetPin
      (mainGaps, offGaps) = partition isMain gaps'
      main =
        justIfTrue (not (null mainGaps)) $
          mkRootedDp (unionsWith (<>) (map gapRootsMap mainGaps)) targetPdp
      grouped =
        foldl'
          ( \acc gp ->
              upsertAssoc (\a b -> unionsWith (<>) [a, b]) gp.end (gapRootsMap gp) acc
          )
          []
          offGaps
      offMembers = [mkRootedDp rm (specificPdp x) | (Just x, rm) <- grouped]
   in (main, offMembers)

-- * Loops

-- | One alternative of resolving all loops of a pointlike at a node.
data LoopsAlt = LoopsAlt
  { progress :: Bool,
    resolved :: Bool,
    -- | zero-consumption loops stay attached to their pointlike (§6)
    attached :: ABranches,
    -- | partially-consumed loops deconstruct into gaps with concrete roots
    gaps :: [Gap],
    dones :: [DeterministicPath Anchor]
  }
  deriving stock (Eq, Ord)

-- | Resolve the loops of a pointlike at a concrete node; alternatives per
-- loop combine by cartesian product. A loop that consumed zero steps stays
-- attached in every position; a fully-resolved loop drops out.
processLoops :: Graph Text () -> NID -> ABranches -> [LoopsAlt]
processLoops g n loops =
  ordNub (map (foldl' merge emptyLoopsAlt) (combineAlts (map perLoop (toList loops))))
  where
    emptyLoopsAlt = LoopsAlt False True mempty [] []
    merge a p =
      LoopsAlt
        (a.progress || p.progress)
        (a.resolved && p.resolved)
        (a.attached <> p.attached)
        (a.gaps <> p.gaps)
        (a.dones <> p.dones)
    perLoop :: DPBranch Anchor -> [LoopsAlt]
    perLoop b =
      let rs = resolveBranch g True True (singletonSet n) b (EndAt (singletonSet n))
       in case [r | r <- rs, isRight r.outcome] of
            (r : _) -> [LoopsAlt True True mempty [] r.dones]
            [] ->
              ordNub
                [ if r.progress
                    then
                      LoopsAlt
                        True
                        False
                        mempty
                        (map toRooted (resGaps r))
                        r.dones
                    else LoopsAlt False False (singletonSet b) [] []
                | r <- rs
                ]
    toRooted gp = case gp.roots of
      AtStart bs -> gp {roots = AtRoots (singletonMap (specificDp n) bs)}
      AtRoots _ -> gp

-- * Layer 1: structural recursion over a branch

-- | Resolve one branch from a frontier toward an end context, computing
-- the gap alternatives that remain. The result is a nonempty list of
-- alternatives, preferred first; if any alternative fully resolves it is
-- the only one (fully-resolving alternatives merge, §4.2.2).
resolveBranch ::
  Graph Text () ->
  -- | whether the branch's final run may seed its backward pass from every
  -- node of the graph (§4.1.2); False inside implicit split midpoints
  Bool ->
  -- | origin: the frontier is exactly the branch's start position, so gaps
  -- with nothing consumed are reported 'AtStart'
  Bool ->
  Set NID ->
  DPBranch Anchor ->
  EndCtx ->
  [BrRes]
resolveBranch g openBwd origin0 start branch endCtx =
  collapseResolved (ordNub (goParts origin0 start (flattenBranch branch)))
  where
    boundaryCtx :: [Part] -> EndCtx
    boundaryCtx = \case
      [] -> endCtx
      (PMid m : _) -> anchorCtx m.anchor
      _ -> EndPair

    finalEndAlts :: [Maybe NID]
    finalEndAlts = gapEndAlts endCtx

    goParts :: Bool -> Set NID -> [Part] -> [BrRes]
    goParts origin f parts = collapseResolved (goParts' origin f parts)

    goParts' :: Bool -> Set NID -> [Part] -> [BrRes]
    goParts' origin f = \case
      [] -> [brRes False [] (Right f)]
      (PRun ds : rest) ->
        case resolveSegment g (null rest && openBwd) f ds (boundaryCtx rest) of
          SegResolved f' mc ->
            let regular = afterUnit (Just mc) True [] f' rest
                regularResolved = any (isRight . (.outcome)) regular
                -- If none of the current arrivals completes the remaining
                -- tail, also try the seam positions where that tail does
                -- resolve. This recovers an arrival supplied by a missing
                -- edge. It is also the self-loop-free interpretation when
                -- the current boundary was reached only by a self-loop.
                recoveredLandings
                  | not (null rest), not regularResolved = tailLandings rest
                  | otherwise = []
                recovered
                  | not (null recoveredLandings) =
                      recoverOrChainFrom
                        recoveredLandings
                        origin
                        0
                        f
                        ds
                        rest
                  | otherwise = []
                alternatives
                  | null mc = recovered <> regular
                  | otherwise = regular <> recovered
             in collapseResolved alternatives
          SegGaps gaps ->
            ordNub $
              concat
                [ afterGap origin k fF mid j seams rest
                | SegGap k fF mid j seams <- gaps
                ]
      (PSplit bs : rest) -> goSplit origin f bs rest
      (PMid _ : _) -> error "goParts: branch begins with a midpoint"

    -- continue after a fully-resolved leading unit
    afterUnit ::
      Maybe (Set NID) -> Bool -> [DeterministicPath Anchor] -> Set NID -> [Part] -> [BrRes]
    afterUnit clean prog ds f' = \case
      [] -> [BrRes prog ds (Right f') Nothing clean False]
      (PMid m : rest') -> map (addProg prog ds) (stationFromArrivals f' m rest')
      rest -> map (addProg prog ds) (goParts False f' rest)

    addProg :: Bool -> [DeterministicPath Anchor] -> BrRes -> BrRes
    addProg prog ds r =
      BrRes
        (prog || r.progress)
        (ds <> r.dones)
        r.outcome
        r.reach
        r.cleanLanding
        r.freshTargetUnsafe

    -- root alternatives for a gap whose forward consumption stopped at fF:
    -- a fan-out followed by a gap is one alternative per frontier node (§3)
    gapRootAlts :: Bool -> Int -> Set NID -> ABranches -> [GapRoots]
    gapRootAlts origin k fF bs
      | k == 0 && origin = [AtStart bs]
      | otherwise = [AtRoots (singletonMap (specificDp v) bs) | v <- toList fF]

    afterGap ::
      Bool ->
      Int ->
      Set NID ->
      [DPDirection] ->
      Int ->
      [(NID, Set NID)] ->
      [Part] ->
      [BrRes]
    afterGap origin k fF mid j seams rest = case rest of
      [] ->
        let bs = singletonSet (runToBranch mid)
            prog = k > 0 || j > 0
         in ordNub
              [ BrRes prog [] (Left [Gap roots e]) reach Nothing False
              | roots <- gapRootAlts origin k fF bs,
                (e, reach) <-
                  if j > 0
                    then [(Just s, justIfTrue (not (null r)) r) | (s, r) <- seams]
                    else [(e, Nothing) | e <- finalEndAlts]
              ]
      (PMid m : rest')
        | Just n <- specificMidNid m ->
            let bs = singletonSet (runToBranch mid)
                prog = k > 0 || j > 0
             in ordNub
                  [ BrRes
                      (prog || st.progress)
                      st.dones
                      (Left (Gap roots e : resGaps st))
                      (resReach st)
                      Nothing
                      st.freshTargetUnsafe
                  | roots <- gapRootAlts origin k fF bs,
                    e <-
                      if j > 0
                        then [Just s | (s, _) <- seams]
                        else [Just n],
                    st <- stationAt n m rest'
                  ]
        | otherwise -> recoverOrChain origin k fF mid rest
      _ -> recoverOrChain origin k fF mid rest

    -- the boundary's NID is unknown (§4.2.1): look for positions the whole
    -- remaining tail resolves from to recover a concrete seam, otherwise
    -- keep one connected structural gap
    recoverOrChain ::
      Bool -> Int -> Set NID -> [DPDirection] -> [Part] -> [BrRes]
    recoverOrChain origin k fF mid parts =
      recoverOrChainFrom (tailLandings parts) origin k fF mid parts

    recoverOrChainFrom ::
      [(NID, Set NID)] ->
      Bool ->
      Int ->
      Set NID ->
      [DPDirection] ->
      [Part] ->
      [BrRes]
    recoverOrChainFrom landings origin k fF mid parts =
      let cands = setFromList (map fst landings)
          chained =
            [ brRes (k > 0) [] (Left [Gap roots e])
            | roots <- gapRootAlts origin k fF (rebuildParts (PRun mid : parts)),
              e <- finalEndAlts
            ]
       in if null cands
            then chained
            else case resolveSegment g True fF mid (EndAt cands) of
              SegResolved met _ ->
                map (addProg True []) (continueTail met parts)
              SegGaps gaps2 ->
                ordNub $
                  gaps2 >>= \(SegGap k2 fF2 mid2 j2 seams2) ->
                    let bs = singletonSet (runToBranch mid2)
                        seamReach cs =
                          let ls = unions (mapMaybe (`lookup` landings) (toList cs))
                           in justIfTrue (not (null ls)) ls
                     in ordNub
                          [ BrRes True [] (Left [Gap roots (Just s)]) reach Nothing False
                          | roots <- gapRootAlts origin k2 fF2 bs,
                            (s, reach) <-
                              if j2 > 0
                                then [(s, seamReach r) | (s, r) <- seams2]
                                else [(c, lookup c landings) | c <- toList cands]
                          ]

    continueTail :: Set NID -> [Part] -> [BrRes]
    continueTail met = \case
      (PMid m : rest') -> stationFromArrivals met m rest'
      parts -> goParts False met parts

    tailLandings :: [Part] -> [(NID, Set NID)]
    tailLandings parts =
      [ (v, ls)
      | v <- toList (nidSet g),
        Just ls <- [tailLandingFrom parts v]
      ]

    -- where the tail lands when fully resolved from @v@
    tailLandingFrom :: [Part] -> NID -> Maybe (Set NID)
    tailLandingFrom parts v = case parts of
      (PMid m : rest')
        | anchorAdmits m.anchor v -> firstRight (stationAt v m rest')
        | otherwise -> Nothing
      _ -> firstRight (goParts False (singletonSet v) parts)

    specificMidNid :: PointlikeDeterministicPath Anchor -> Maybe NID
    specificMidNid m = case m.anchor of
      Specific n -> Just n
      _ -> Nothing

    excOf :: PointlikeDeterministicPath Anchor -> Set NID
    excOf m = case m.anchor of
      JoinPoint exc -> exc
      _ -> mempty

    -- a midpoint position reached at a concrete node: resolve its loops
    -- there and continue with the rest of the branch
    stationAt :: NID -> PointlikeDeterministicPath Anchor -> [Part] -> [BrRes]
    stationAt n m rest = collapseResolved $ ordNub do
      la <- processLoops g n m.loops
      cr <- goParts True (singletonSet n) rest
      case (la.resolved, cr.outcome) of
        (True, Right f') ->
          pure $
            BrRes
              (la.progress || cr.progress)
              (la.dones <> cr.dones)
              (Right f')
              Nothing
              cr.cleanLanding
              cr.freshTargetUnsafe
        _ -> do
          let contGaps = resGaps cr
              stationPdp =
                PointlikeDeterministicPath (pinAnchor m.anchor n) la.attached
              (gaps', formDones) =
                assembleStation
                  (Just n)
                  (Pointlike stationPdp)
                  False
                  la.attached
                  la.gaps
                  contGaps
          pure $
            BrRes
              (la.progress || cr.progress)
              (la.dones <> cr.dones <> formDones)
              (Left gaps')
              (resReach cr)
              Nothing
              cr.freshTargetUnsafe

    -- a midpoint reached at several NIDs (§4.2.2): all fully-resolving
    -- arrivals merge into one alternative; otherwise each progressing
    -- arrival is its own alternative
    stationFromArrivals ::
      Set NID -> PointlikeDeterministicPath Anchor -> [Part] -> [BrRes]
    stationFromArrivals arrivals m rest =
      let perArrival = [stationAt n m rest | n <- toList arrivals]
          resolveds =
            [(r, ls) | rs <- perArrival, r <- rs, Right ls <- [r.outcome]]
       in case resolveds of
            (_ : _) ->
              [ BrRes
                  (any ((.progress) . fst) resolveds)
                  (ordNub (concatMap ((.dones) . fst) resolveds))
                  (Right (unions (map snd resolveds)))
                  Nothing
                  (Just (unions [fromMaybe ls r.cleanLanding | (r, ls) <- resolveds]))
                  (any ((.freshTargetUnsafe) . fst) resolveds)
              ]
            [] ->
              let allAlts = concat perArrival
               in ordNub case filter (.progress) allAlts of
                    [] -> allAlts
                    ps -> ps

    -- an intersection's members all start at the same node, so a fan-out
    -- frontier entering a split is one alternative per node (§3)
    goSplit :: Bool -> Set NID -> ABranches -> [Part] -> [BrRes]
    goSplit origin f bs rest
      | length (toList f) > 1 =
          collapseResolved
            (ordNub (concat [goSplit1 origin (singletonSet v) bs rest | v <- toList f]))
      | otherwise = goSplit1 origin f bs rest

    goSplit1 :: Bool -> Set NID -> ABranches -> [Part] -> [BrRes]
    goSplit1 origin f bs rest = collapseResolved $ ordNub do
      let subEnd = boundaryCtx rest
          subOpenBwd = null rest && openBwd
          chainAll =
            [ brRes False [] (Left [Gap roots e])
            | roots <- gapRootAlts origin 0 f (rebuildParts (PSplit bs : rest)),
              e <- finalEndAlts
            ]
      combo <-
        combineAlts
          [ map (b,) (resolveBranch g subOpenBwd origin f b subEnd)
          | b <- toList bs
          ]
      let landed = [(b, ls) | (b, r) <- combo, Right ls <- [r.outcome]]
          failed = concat [gs | (_, r) <- combo, Left gs <- [r.outcome]]
          anyFailed = any (isLeft . (.outcome) . snd) combo
          subDones = concatMap ((.dones) . snd) combo
          subProgress = any ((.progress) . snd) combo
          disagreementReaches =
            let landings = map snd landed
                cleanConstraints =
                  [ fromMaybe ls r.cleanLanding
                  | (_, r) <- combo,
                    Right ls <- [r.outcome]
                  ]
                meetClean = case filter (not . null) cleanConstraints of
                  [] -> mempty
                  cs -> intersectAll cs
                leaveOneOutMeets =
                  unions
                    [ m
                    | (i, _) <- zip [0 :: Int ..] landings,
                      let rest' = take i landings <> drop (i + 1) landings,
                      not (null rest'),
                      let m = intersectAll rest',
                      not (null m)
                    ]
                admitted = case subEnd of
                  EndJoin exc ->
                    (meetClean `union` leaveOneOutMeets) `difference` exc
                  EndPair -> meetClean `union` leaveOneOutMeets
                  _ -> mempty
             in justIfTrue (not (null admitted)) admitted
      case rest of
        [] ->
          if not anyFailed
            then case combineLandings subEnd (map snd landed) of
              Just ls ->
                pure $
                  BrRes
                    subProgress
                    subDones
                    (Right ls)
                    Nothing
                    Nothing
                    (any ((.freshTargetUnsafe) . snd) combo)
              Nothing ->
                [ r
                    { progress = subProgress,
                      reach = disagreementReaches
                    }
                | r <- chainAll
                ]
            else case subEnd of
              -- at a pair point the intersection's members must agree on
              -- one landing; failed members re-aim at the resolved
              -- members' agreed landing (reuse), and with no agreed
              -- landing the whole intersection is recreated as one gap so
              -- its members meet at one fresh pair point
              EndPair
                | not (null landed),
                  ms <- intersectAll (map snd landed),
                  not (null ms) -> do
                    v <- toList ms
                    reaimed <-
                      combineAlts
                        [ resolveBranch g False origin f b (EndAt (singletonSet v))
                        | (b, r) <- combo,
                          isLeft r.outcome
                        ]
                    let gaps = concat [resGaps r | r <- reaimed]
                        ds = concatMap (.dones) reaimed
                    pure $
                      BrRes
                        (subProgress || any (.progress) reaimed)
                        (subDones <> ds)
                        (Left gaps)
                        (Just (singletonSet v))
                        Nothing
                        ( any ((.freshTargetUnsafe) . snd) combo
                            || any (.freshTargetUnsafe) reaimed
                            || (not (null landed) && anyFailed)
                        )
                | otherwise ->
                    [ brRes subProgress subDones (Left [Gap roots Nothing])
                    | roots <- gapRootAlts origin 0 f bs
                    ]
              _ ->
                pure $
                  BrRes
                    subProgress
                    subDones
                    (Left failed)
                    ( let knownReaches = mapMaybe (resReach . snd) combo
                          ms = intersectAll knownReaches
                       in justIfTrue
                            (not (null knownReaches) && not (null ms))
                            ms
                    )
                    Nothing
                    ( any ((.freshTargetUnsafe) . snd) combo
                        || (not (null landed) && anyFailed)
                    )
        (PMid m : rest')
          | Just n <- specificMidNid m ->
              if not anyFailed && not (null landed)
                then
                  [ (addProg subProgress subDones r)
                      { freshTargetUnsafe =
                          r.freshTargetUnsafe
                            || any ((.freshTargetUnsafe) . snd) combo
                      }
                  | r <- afterUnit Nothing False [] (singletonSet n) rest
                  ]
                else do
                  st <- stationAt n m rest'
                  if not subProgress && not st.progress
                    then chainAll
                    else
                      pure $
                        BrRes
                          (subProgress || st.progress)
                          (subDones <> st.dones)
                          (Left (failed <> resGaps st))
                          (resReach st)
                          Nothing
                          ( any ((.freshTargetUnsafe) . snd) combo
                              || st.freshTargetUnsafe
                              || (not (null landed) && anyFailed)
                          )
          | otherwise ->
              joinMidRecover origin f combo landed anyFailed subDones subProgress m rest' chainAll
        _ ->
          let runImplicit combo' =
                let landed' =
                      [(b, ls) | (b, res) <- combo', Right ls <- [res.outcome]]
                    failed' =
                      concat [gs | (_, res) <- combo', Left gs <- [res.outcome]]
                    failedBranches' =
                      [b | (b, res) <- combo', isLeft res.outcome]
                    failedReaches' =
                      [ rs
                      | (_, res) <- combo',
                        isLeft res.outcome,
                        Just rs <- [res.reach]
                      ]
                    dones' = concatMap ((.dones) . snd) combo'
                    progress' = any ((.progress) . snd) combo'
                    unsafe' =
                      any ((.freshTargetUnsafe) . snd) combo'
                        || (not (null landed') && not (null failedBranches'))
                 in goSplitImplicit
                      origin
                      f
                      landed'
                      failed'
                      failedBranches'
                      failedReaches'
                      dones'
                      progress'
                      unsafe'
                      rest
                      chainAll
              agreed = intersectAll (map snd landed)
              alternatePairs =
                fromMaybe mempty disagreementReaches `difference` agreed
           in runImplicit combo
                <> do
                  p <- toList alternatePairs
                  rerun <-
                    combineAlts
                      [ map (b,) $
                          resolveBranch
                            g
                            False
                            origin
                            f
                            b
                            (EndAt (singletonSet p))
                      | (b, _) <- combo
                      ]
                  runImplicit rerun

    -- an explicit join midpoint after an intersection: every branch must
    -- agree on it; when they don't, try every node the whole tail resolves
    -- from as an alternative seam (§4.2.1), else keep one connected gap
    joinMidRecover ::
      Bool ->
      Set NID ->
      [(DPBranch Anchor, BrRes)] ->
      [(DPBranch Anchor, Set NID)] ->
      Bool ->
      [DeterministicPath Anchor] ->
      Bool ->
      PointlikeDeterministicPath Anchor ->
      [Part] ->
      [BrRes] ->
      [BrRes]
    joinMidRecover origin f combo landed anyFailed subDones subProgress m rest' chainAll =
      let landings = map snd landed
          meet = intersectAll landings `difference` excOf m
          cleanConstraints =
            [ fromMaybe ls res.cleanLanding
            | (_, res) <- combo,
              Right ls <- [res.outcome]
            ]
          meetClean = case filter (not . null) cleanConstraints of
            [] -> mempty
            cs -> intersectAll cs `difference` excOf m
          leaveOneOutMeets =
            unions
              [ m'
              | (i, _) <- zip [0 :: Int ..] landings,
                let rest = take i landings <> drop (i + 1) landings,
                not (null rest),
                let m' = intersectAll rest `difference` excOf m,
                not (null m')
              ]
          alternateMeets =
            (meetClean `union` leaveOneOutMeets) `difference` meet
          perCand v = do
            rs <-
              combineAlts
                [ resolveBranch g False origin f b (EndAt (singletonSet v))
                | (b, _) <- combo
                ]
            st <- stationAt v m rest'
            let gaps =
                  concat [resGaps r | r <- rs]
                    <> resGaps st
                ds = concatMap (.dones) rs <> st.dones
                prog = any (.progress) rs || st.progress
            guard prog
            pure $
              BrRes
                True
                ds
                (Left gaps)
                (resReach st)
                Nothing
                ( any (.freshTargetUnsafe) rs
                    || st.freshTargetUnsafe
                    || (not (null landed) && anyFailed)
                )
       in if not (null landed) && not anyFailed && not (null meet)
            then
              [ (addProg subProgress subDones r)
                  { freshTargetUnsafe =
                      r.freshTargetUnsafe
                        || any ((.freshTargetUnsafe) . snd) combo
                  }
              | r <- stationFromArrivals meet m rest'
              ]
                <> concatMap perCand (toList alternateMeets)
            else
              let cands = map fst (tailLandings (PMid m : rest'))
               in case ordNub (concatMap perCand cands) of
                    [] -> chainAll
                    alts -> alts

    -- continuation after an implicit (unanchored, exploded per pair)
    -- midpoint: each landing set continues independently, all required
    goSplitImplicit ::
      Bool ->
      Set NID ->
      [(DPBranch Anchor, Set NID)] ->
      [Gap] ->
      -- \| original members that failed before the pair point; once a
      -- working junction is known, rerun them toward it so a surviving
      -- suffix can be consumed backward
      [DPBranch Anchor] ->
      -- \| terminal positions gapped members' residuals land at: their
      -- pair-continuations must exist from there too
      [Set NID] ->
      -- \| residual members and progress inherited from the left split
      [DeterministicPath Anchor] ->
      Bool ->
      -- \| whether a later fresh target would invalidate reused meet structure
      Bool ->
      [Part] ->
      [BrRes] ->
      [BrRes]
    goSplitImplicit
      origin
      f
      landed
      failed
      failedBranches
      failedReaches
      subDones
      subProgress
      subUnsafe
      rest'
      chainAll = do
        let agreedLandings = intersectAll (map snd landed)
            landingSets
              | null failed,
                not (null landed),
                not (null agreedLandings) =
                  [agreedLandings]
              | otherwise = ordNub (map snd landed <> failedReaches)
            -- A frontier may contain several possible pair-point arrivals.
            -- Try them independently so a continuation that resolves from
            -- only one arrival records that actual junction, rather than an
            -- arbitrary member of the pre-continuation frontier.
            continuations ls =
              let perArrival =
                    [ (singletonSet n, r)
                    | n <- toList ls,
                      r <- goParts False (singletonSet n) rest'
                    ]
                  resolved =
                    [(src, r) | (src, r) <- perArrival, isRight r.outcome]
               in case resolved of
                    (_ : _) ->
                      [ ( unions (map fst resolved),
                          headEx (collapseResolved (map snd resolved))
                        )
                      ]
                    [] -> case filter ((.progress) . snd) perArrival of
                      [] -> perArrival
                      progressing -> progressing
        contCombo <-
          combineAlts [continuations ls | ls <- landingSets]
        let contRights = [(ls, f') | (ls, r) <- contCombo, Right f' <- [r.outcome]]
            contProgress = any ((.progress) . snd) contCombo
            contDones = concatMap ((.dones) . snd) contCombo
            contGaps = concat [resGaps r | (_, r) <- contCombo]
            extendBs bs' =
              singletonSet (smartBuildSequence bs' unanchored (rebuildParts rest'))
            extendGap gp =
              gp
                { roots = case gp.roots of
                    AtStart bs' -> AtStart (extendBs bs')
                    AtRoots rm -> AtRoots (omap extendBs rm)
                }
            retargetTo jn gp
              | isNothing gp.end = gp {end = Just jn}
              | otherwise = gp
            compatibleTailJunctions finalLandings =
              [ jn
              | (jn, ls) <- tailLandings rest',
                isJust (combineLandings endCtx (ls : finalLandings))
              ]
        if not subProgress && not contProgress
          then chainAll
          else case contRights of
            ((jls, _) : _) -> do
              -- a junction with a working continuation exists; failed
              -- branches end at it and branches that landed where the
              -- continuation fails are re-aimed at it
              let junctions =
                    ordNub
                      ( toList jls
                          <> if null failedBranches
                            then []
                            else compatibleTailJunctions (map snd contRights)
                      )
                  failingSets = [ls | (ls, r) <- contCombo, isLeft r.outcome]
              jn <- junctions
              reaims <-
                combineAlts
                  [ resolveBranch g False origin f b (EndAt (singletonSet jn))
                  | (b, ls) <- landed,
                    any (not . null . intersection ls) failingSets
                  ]
              failedReaims <-
                combineAlts
                  [ resolveBranch g False origin f b (EndAt (singletonSet jn))
                  | b <- failedBranches
                  ]
              let reaimGaps = concat [resGaps r | r <- reaims]
                  reaimDones = concatMap (.dones) reaims
                  failedGaps =
                    concat [resGaps r | r <- failedReaims]
                  failedDones = concatMap (.dones) failedReaims
                  -- a gapped member whose residual lands it at a reach node
                  -- still needs its pair continuation from there; the
                  -- landed members' pairs go through jn instead
                  reachContGaps =
                    concat
                      [ resGaps r
                      | (ls, r) <- contCombo,
                        any (not . null . intersection ls) failedReaches
                      ]
              if null failedGaps && null reaimGaps && null reachContGaps
                then case combineLandings endCtx (map snd contRights) of
                  Just ls ->
                    pure $
                      BrRes
                        (subProgress || contProgress)
                        (subDones <> contDones <> reaimDones <> failedDones)
                        (Right ls)
                        Nothing
                        Nothing
                        ( subUnsafe
                            || any ((.freshTargetUnsafe) . snd) contCombo
                            || any (.freshTargetUnsafe) reaims
                            || any (.freshTargetUnsafe) failedReaims
                        )
                  Nothing -> chainAll
                else
                  pure $
                    BrRes
                      (subProgress || contProgress)
                      (subDones <> contDones <> reaimDones <> failedDones)
                      (Left (failedGaps <> reaimGaps <> reachContGaps))
                      (combineLandings endCtx (map snd contRights))
                      Nothing
                      ( subUnsafe
                          || any ((.freshTargetUnsafe) . snd) contCombo
                          || any (.freshTargetUnsafe) reaims
                          || any (.freshTargetUnsafe) failedReaims
                      )
            [] -> do
              -- no continuation resolved anywhere: pin failed branches to
              -- the junction this alternative's continuation gaps get
              -- created from (so the failed member's pair goes through the
              -- same node), else to a landing, else the continuation must
              -- be minted per pair
              let contRootNid gp = case gp.roots of
                    AtRoots rm -> case mapToList rm of
                      ((Pointlike pdp, _) : _)
                        | Specific n <- pdp.anchor -> Just n
                      _ -> Nothing
                    AtStart _ -> Nothing
                  -- only a pair-point candidate counts: a continuation gap
                  -- rooted deeper inside the continuation (past a midpoint)
                  -- is not a node the failed member can land at
                  landingNodes = unions landingSets
                  -- A continuation that made progress may have advanced
                  -- beyond its pair point before it gapped. Its recorded
                  -- source is still the junction the failed prefix must
                  -- meet; choosing an arbitrary sibling landing mixes two
                  -- incompatible pair alternatives.
                  progressJunction =
                    headMay
                      [ n
                      | (ls, r) <- contCombo,
                        r.progress,
                        n <- toList ls
                      ]
                  jnFromCont =
                    headMay
                      [ n
                      | n <- mapMaybe contRootNid contGaps,
                        n `member` landingNodes
                      ]
                  adjust gp
                    | isNothing gp.end = case jnFromCont of
                        Just jn -> retargetTo jn gp
                        Nothing -> case progressJunction of
                          Just jn -> retargetTo jn gp
                          Nothing -> case landingSets of
                            (ls : _) -> retargetTo (minimumEx (toList ls)) gp
                            [] -> extendGap gp
                    | otherwise = gp
              let reaches =
                    [ rs
                    | (_, r) <- contCombo,
                      Just rs <- [resReach r]
                    ]
                  reach = combineLandings endCtx reaches
              case endCtx of
                -- These continuations reused suffixes ending at incompatible
                -- nodes before a required outer pair boundary. Their gaps do
                -- not repair that meet, so retain the connected structure.
                EndPair
                  | not (null reaches), isNothing reach -> chainAll
                _ ->
                  pure $
                    BrRes
                      (subProgress || contProgress)
                      (subDones <> contDones)
                      (Left (map adjust failed <> contGaps))
                      reach
                      Nothing
                      ( subUnsafe
                          || any ((.freshTargetUnsafe) . snd) contCombo
                          || (not (null landed) && not (null failed))
                      )

-- * Layer 1: structural recursion over deterministic paths

-- | One alternative for a whole deterministic path.
data DpAlt = DpAlt
  { progress :: Bool,
    -- | Just landings when the path fully resolved
    landed :: Maybe (Set NID),
    -- | the residual path ending at the dp's target, chainable by an
    -- enclosing structure
    main :: Maybe (DeterministicPath Anchor),
    extras :: [DeterministicPath Anchor],
    -- | the concrete node the target resolved/pinned to, if any
    pin :: Maybe NID,
    -- | a concrete landing the enclosing path may resume from. Unlike
    -- 'pin', this does not specialize an Unanchored target in the residual.
    resume :: Maybe (Set NID),
    -- | whether every branch landing agrees on the landed set; False for
    -- an Unanchored target whose branches landed at disjoint positions
    -- (nested reuse is unsound then: the exploded pairs need a common
    -- continuation point)
    agreed :: Bool
  }
  deriving stock (Eq, Ord)

dpAltMembers :: DpAlt -> [DeterministicPath Anchor]
dpAltMembers a = maybeToList a.main <> a.extras

residualPointlikeAlts ::
  NID -> Graph Text () -> PointlikeDeterministicPath Anchor -> [DpAlt]
residualPointlikeAlts startNid g p = case unifyRootAnchor startNid p.anchor of
  Nothing -> [DpAlt False Nothing Nothing [] Nothing Nothing True]
  Just n
    | null p.loops ->
        if isJust (maybeLookupNode g n)
          then
            [ DpAlt
                True
                (Just (singletonSet n))
                Nothing
                []
                (Just n)
                (Just (singletonSet n))
                True
            ]
          else [DpAlt False Nothing Nothing [] (Just n) Nothing True]
    | otherwise -> ordNub do
        la <- processLoops g n p.loops
        let pdp = PointlikeDeterministicPath (pinAnchor p.anchor n) la.attached
        if la.resolved
          then
            pure
              ( DpAlt
                  True
                  (Just (singletonSet n))
                  Nothing
                  []
                  (Just n)
                  (Just (singletonSet n))
                  True
              )
          else do
            let (main, offMembers) =
                  buildMembers pdp (Just n) (orientGaps n False la.gaps)
                attachedDone =
                  [Pointlike pdp | isNothing main, not (null la.attached)]
            pure
              DpAlt
                { progress = la.progress,
                  landed = Nothing,
                  main,
                  extras = offMembers <> la.dones <> attachedDone,
                  pin = Just n,
                  resume = Nothing,
                  agreed = True
                }

-- | How one root of a rooted path resolved (one alternative).
data RootInfo = RootInfo
  { -- | where branches from this root start (Nothing = unknown)
    frontier :: Maybe (Set NID),
    -- | root key for gaps that resume at the root
    form :: DeterministicPath Anchor,
    -- | emit 'form' standalone if no gap roots at it (nested residuals)
    formMustEmit :: Bool,
    -- | the root pointlike's node, for chaining its loop gaps
    stationNid :: Maybe NID,
    -- | the root pointlike's attached loops (already inside 'form')
    attached :: ABranches,
    loopGaps :: [Gap],
    extraDones :: [DeterministicPath Anchor],
    clean :: Bool,
    progress :: Bool
  }
  deriving stock (Eq, Ord)

resolveRootAlts :: NID -> Graph Text () -> DeterministicPath Anchor -> [RootInfo]
resolveRootAlts startNid g = \case
  Pointlike p -> case unifyRootAnchor startNid p.anchor of
    Nothing -> error "resolveRootAlts: unsatisfiable root escaped dpUnsatisfiable"
    Just n ->
      [ RootInfo
          { frontier = Just (singletonSet n),
            form =
              Pointlike (PointlikeDeterministicPath (pinAnchor p.anchor n) la.attached),
            formMustEmit = False,
            stationNid = Just n,
            attached = la.attached,
            loopGaps = la.gaps,
            extraDones = la.dones,
            clean = la.resolved,
            progress = la.progress
          }
      | la <- processLoops g n p.loops
      ]
  Rooted nested -> ordNub do
    -- Unanchored nested targets deliberately stay Unanchored (§5): the
    -- outer branches resume from the concrete landings that resolution
    -- discovered, without turning those landings into a residual pin.
    sub <- residualRootedAlts startNid g nested
    case sub.landed of
      Just ls
        | sub.agreed ->
            [ RootInfo
                { frontier = Just ls,
                  form = specificDp v,
                  formMustEmit = False,
                  stationNid = Nothing,
                  attached = mempty,
                  loopGaps = [],
                  extraDones = [],
                  clean = True,
                  progress = sub.progress
                }
            | v <- toList (maybe ls singletonSet sub.pin)
            ]
      _ ->
        let exactUnanchored a = do
              guard (nested.target.anchor == Unanchored)
              landing <- toList (fromMaybe mempty a.resume)
              unanchoredRootRepairAt startNid g nested landing

            fallback a =
              let (form, formMustEmit, extraDones, subProgress) =
                    case nested.target.anchor of
                      -- With no concrete target, a partial nested residual
                      -- cannot safely feed the outer branches: instantiating
                      -- it would mint a different target NID. Recreate the
                      -- whole nested prefix instead, leaving its target
                      -- Unanchored (§5).
                      Unanchored -> (Rooted nested, True, [], False)
                      _ -> case (a.main, a.pin) of
                        (Just mn, _) -> (mn, True, a.extras, a.progress)
                        (Nothing, Just pn) ->
                          (specificDp pn, False, a.extras, a.progress)
                        (Nothing, Nothing) ->
                          (Rooted nested, True, [], a.progress)
               in RootInfo
                    { frontier = singletonSet <$> a.pin,
                      form,
                      formMustEmit,
                      stationNid = Nothing,
                      attached = mempty,
                      loopGaps = [],
                      extraDones,
                      clean = False,
                      progress = subProgress
                    }
         in exactUnanchored sub <> [fallback sub]

-- | The exact-repair alternatives for a nested root with an Unanchored
-- target, given a concrete landing the outer structure identified: rerun
-- the nested path toward that landing, then reverse the residual between
-- its concrete seams ('invertResidualAt') so the Unanchored target stays
-- unpinned in the result (§5) while the created edges still land at the
-- recovered seam.
unanchoredRootRepairAt ::
  NID -> Graph Text () -> RootedDeterministicPath Anchor -> NID -> [RootInfo]
unanchoredRootRepairAt startNid g nested landing = do
  aimed <-
    residualRootedAlts
      startNid
      g
      nested {target = nested.target {anchor = Specific landing}}
  guard (isNothing aimed.landed)
  reversed <-
    maybeToList $
      maybe
        (Just [])
        (invertResidualAt startNid landing)
        aimed.main
  let members = reversed <> aimed.extras
  guard (not (null members))
  pure
    RootInfo
      { frontier = Just (singletonSet landing),
        form = specificDp landing,
        formMustEmit = False,
        stationNid = Nothing,
        attached = mempty,
        loopGaps = [],
        extraDones = members,
        clean = False,
        progress = aimed.progress
      }

branchFromFrontier ::
  Graph Text () -> EndCtx -> Maybe (Set NID) -> DPBranch Anchor -> [BrRes]
branchFromFrontier g ctx frontier b = case frontier of
  Just f -> resolveBranch g True True f b ctx
  Nothing ->
    [ brRes False [] (Left [Gap (AtStart (singletonSet b)) e])
    | e <- gapEndAlts ctx
    ]

residualRootedAlts ::
  NID -> Graph Text () -> RootedDeterministicPath Anchor -> [DpAlt]
residualRootedAlts startNid g r = ordNub do
  infos <- combineAlts rootAlts
  let entries = zip infos (map snd rootPairs)
  probe <-
    combineAlts
      [ combineAlts [map (b,) (probeFor info.frontier b) | b <- toList bs]
      | (info, bs) <- entries
      ]
  let landings =
        [ls | brs <- probe, (_, res) <- brs, Right ls <- [res.outcome]]
  (targetPin, results) <- case r.target.anchor of
    Specific t -> pure (Just t, probe)
    Unanchored -> pure (Nothing, probe)
    JoinPoint exc
      | null landings ->
          -- the safe unpinned residual first; then reach-pinned
          -- minimality alternatives (§5.2): when the gapped branches'
          -- resolved suffixes agree on a terminal, or the target's loops
          -- resolve somewhere, pinning there lets the residual reuse that
          -- surviving structure (each pin is individually sound: created
          -- branches land at the pin and the loops resolve there)
          pure (Nothing, map (map demoteGapped) probe)
            <> do
              p <- toList (reachPins exc probe)
              rerun <-
                combineAlts
                  [ combineAlts
                      [map (b,) (rerunOne p info b res) | (b, res) <- brs]
                  | ((info, _), brs) <- zip entries probe
                  ]
              pure (Just p, rerun)
      | otherwise ->
          -- the join pins only on agreed landings; when resolved branches
          -- disagree (or all agreement is excluded) the join is
          -- unsatisfied: the landed branches are treated as unresolved and
          -- nothing pins (§5). A landing that exists only via a self-loop
          -- edge matching a non-loop step doesn't block the meet (§5.1):
          -- when the self-loop-free landings do agree, alternatives pinned
          -- there accompany the safe demoted residual, with the
          -- coincidentally-landed branches re-aimed at the pin.
          let meet = intersectAll landings `difference` exc
              cleanConstraints =
                [ fromMaybe ls res.cleanLanding
                | brs <- probe,
                  (_, res) <- brs,
                  Right ls <- [res.outcome]
                ]
              meetClean =
                case filter (not . null) cleanConstraints of
                  [] -> mempty
                  cs -> intersectAll cs `difference` exc
              -- one branch's landing may be the odd one out (a missing
              -- edge broke exactly it): every leave-one-out subset's meet
              -- is a pin candidate, with the odd branch re-aimed at the
              -- pin like a gapped one
              leaveOneOutMeets =
                unions
                  [ m
                  | (i, _) <- zip [0 :: Int ..] landings,
                    let rest = take i landings <> drop (i + 1) landings,
                    not (null rest),
                    let m = intersectAll rest `difference` exc,
                    not (null m)
                  ]
              disagreePins =
                meetClean
                  `union` leaveOneOutMeets
                  `union` (targetLoopPins `difference` exc)
              pinAt p = do
                rerun <-
                  combineAlts
                    [ combineAlts
                        [map (b,) (rerunOne p info b res) | (b, res) <- brs]
                    | ((info, _), brs) <- zip entries probe
                    ]
                pure (Just p, rerun)
           in if null meet
                then
                  pure (Nothing, map (map (demoteGapped . demote)) probe)
                    <> (toList disagreePins >>= pinAt)
                else
                  (toList meet <> toList (disagreePins `difference` meet))
                    >>= pinAt
  finishRooted g r entries targetPin results
  where
    rootPairs = mapToList r.rootBranches
    targetCtx = targetEndCtx r.target.anchor
    rootAlts =
      [ resolveRootAlts startNid g rk <> recoveredNestedRoots rk bs
      | (rk, bs) <- rootPairs
      ]
    -- A partial nested JoinPoint or Unanchored root may have no frontier of
    -- its own, even though an outer branch's surviving tail identifies the
    -- missing seam. Search only nodes from which the whole outer branch set
    -- already resolves, then rerun the nested root toward that concrete
    -- seam. A recovered JoinPoint target pins there (§5.2); an Unanchored
    -- target stays unpinned via the reversed exact repair (§5).
    recoveredNestedRoots rk bs = case rk of
      Rooted nested
        | Unanchored <- nested.target.anchor -> do
            n <- toList (nidSet g)
            guard (outerBranchesResolve n bs)
            unanchoredRootRepairAt startNid g nested n
        | JoinPoint exc <- nested.target.anchor -> do
            n <- toList (nidSet g `difference` exc)
            guard (outerBranchesResolve n bs)
            aimed <-
              residualRootedAlts
                startNid
                g
                nested
                  { target = nested.target {anchor = Specific n}
                  }
            guard (isNothing aimed.landed)
            let (form, formMustEmit, extraDones) = case (aimed.main, aimed.pin) of
                  (Just mn, _) -> (mn, True, aimed.extras)
                  (Nothing, Just pn) ->
                    (specificDp pn, False, aimed.extras)
                  (Nothing, Nothing) -> (Rooted nested, True, [])
            pure
              RootInfo
                { frontier = Just (singletonSet n),
                  form,
                  formMustEmit,
                  stationNid = Nothing,
                  attached = mempty,
                  loopGaps = [],
                  extraDones,
                  clean = False,
                  progress = aimed.progress
                }
      _ -> []
    outerBranchesResolve n bs =
      any (isJust . combineLandings targetCtx) $
        combineAlts
          [ [ls | res <- branchAt n b, Right ls <- [res.outcome]]
          | b <- toList bs
          ]
    branchAt n b =
      resolveBranch g False True (singletonSet n) b targetCtx
    -- branch resolutions are shared across root-alternative combinations
    -- and pin choices via lazily-evaluated caches: without them the same
    -- branch is re-resolved once per combination, which multiplies
    frontierChoices =
      [ ordNub [info.frontier | info <- alts]
      | alts <- rootAlts
      ]
    probeCache =
      mapFromList @(Map _ _)
        [ ((fr, b), branchFromFrontier g targetCtx fr b)
        | (frs, (_, bs)) <- zip frontierChoices rootPairs,
          fr <- frs,
          b <- toList bs
        ]
    probeFor fr b =
      fromMaybe (branchFromFrontier g targetCtx fr b) (lookup (fr, b) probeCache)
    rerunCache =
      mapFromList @(Map _ _)
        [ ((p, fr, b), branchFromFrontier g (EndAt (singletonSet p)) fr b)
        | (frs, (_, bs)) <- zip frontierChoices rootPairs,
          fr <- frs,
          b <- toList bs,
          p <- toList (nidSet g)
        ]
    -- pin candidates for a join no branch landed at (§5.2): the agreed
    -- terminal of the gapped branches' resolved suffixes, plus the nodes
    -- the target's loops fully resolve at
    targetLoopPins
      | null r.target.loops = mempty
      | otherwise =
          setFromList
            [ n
            | n <- toList (nidSet g),
              any (.resolved) (processLoops g n r.target.loops)
            ]
    reachPins exc probe =
      let reaches =
            [ rs
            | brs <- probe,
              (_, res) <- brs,
              isLeft res.outcome,
              Just rs <- [res.reach]
            ]
          reachAgreed
            | null reaches = mempty
            | otherwise = intersectAll reaches
       in (reachAgreed `union` targetLoopPins) `difference` exc
    demote (b, res) = case res.outcome of
      Right _ ->
        (b, brRes False [] (Left [Gap (AtStart (singletonSet b)) Nothing]))
      Left _ -> (b, res)
    -- When the join stays unpinned, instantiation mints a fresh join node
    -- all residual branches must meet at. A gap ending at that unpinned
    -- join is already sound and can keep its forward progress. A gap that
    -- reuses a suffix (or resolved tail structure) ends at a concrete
    -- existing node instead, so its safe alternative recreates the branch
    -- wholesale. Suffix reuse returns as the reach-pinned minimality
    -- alternative (§5.2), where the pin makes it sound.
    demoteGapped pair@(b, res) = case res.outcome of
      Right _ -> pair
      Left gaps
        | not res.freshTargetUnsafe,
          isNothing res.reach,
          all (isNothing . (.end)) gaps ->
            pair
      Left _ ->
        ( b,
          BrRes
            res.progress
            []
            (Left [Gap (AtStart (singletonSet b)) Nothing])
            res.reach
            Nothing
            False
        )
    -- a resolved branch whose landing excludes the pin is re-aimed like a
    -- gapped one (its landing was a coincidence the pin doesn't use)
    rerunOne p info b res = case res.outcome of
      Right ls
        | p `member` ls -> [res]
      _ ->
        fromMaybe
          (branchFromFrontier g (EndAt (singletonSet p)) info.frontier b)
          (lookup (p, info.frontier, b) rerunCache)

finishRooted ::
  Graph Text () ->
  RootedDeterministicPath Anchor ->
  [(RootInfo, ABranches)] ->
  Maybe NID ->
  [[(DPBranch Anchor, BrRes)]] ->
  [DpAlt]
finishRooted g r entries targetPin results = do
  lrT <- case targetPin of
    Just t | not (null r.target.loops) -> processLoops g t r.target.loops
    _ -> pure (LoopsAlt False (null r.target.loops) r.target.loops [] [])
  let infos = map fst entries
      allRes = concat results
      allBranchLanded = all (isRight . (.outcome) . snd) allRes
      rootsClean = all (.clean) infos
      landings = [ls | (_, res) <- allRes, Right ls <- [res.outcome]]
      progressTotal =
        any (.progress) infos
          || any ((.progress) . snd) allRes
          || lrT.progress
      resolvedAll = allBranchLanded && rootsClean && lrT.resolved
      landingsAgree = case r.target.anchor of
        Unanchored -> not (null landings) && not (null (intersectAll landings))
        _ -> True
      baseLanding = case r.target.anchor of
        Specific t -> singletonSet t
        JoinPoint _ -> maybe mempty singletonSet targetPin
        Unanchored ->
          let m = intersectAll landings
           in if null m then unions landings else m
      finalLanding
        | null r.target.loops = baseLanding
        | otherwise = maybe baseLanding singletonSet targetPin
  if resolvedAll
    then
      pure
        DpAlt
          { progress = True,
            landed = Just finalLanding,
            main = Nothing,
            extras = [],
            pin = targetPin,
            resume = Just finalLanding,
            agreed = landingsAgree
          }
    else do
      let targetAnchor = case r.target.anchor of
            Specific t -> Specific t
            JoinPoint exc -> maybe (JoinPoint exc) Specific targetPin
            Unanchored -> Unanchored
          targetPdp = PointlikeDeterministicPath targetAnchor lrT.attached
          perRoot =
            [ assembleStation
                info.stationNid
                info.form
                info.formMustEmit
                info.attached
                info.loopGaps
                (concat [resGaps res | (_, res) <- brs])
            | ((info, _), brs) <- zip entries results
            ]
          targetGaps = case targetPin of
            Just t -> orientGaps t False lrT.gaps
            Nothing -> lrT.gaps
          allGaps = concatMap fst perRoot <> targetGaps
          allDones =
            concatMap ((.dones) . snd) allRes
              <> concatMap (.extraDones) infos
              <> concatMap snd perRoot
              <> lrT.dones
          (main, offMembers) = buildMembers targetPdp targetPin allGaps
          attachedTargetDone =
            [ Pointlike targetPdp
            | isNothing main,
              not (null lrT.attached),
              not lrT.resolved
            ]
      pure
        DpAlt
          { progress = progressTotal,
            landed = Nothing,
            main,
            extras = offMembers <> allDones <> attachedTargetDone,
            pin = targetPin,
            resume = case r.target.anchor of
              Unanchored ->
                justIfTrue landingsAgree (intersectAll landings)
              _ -> singletonSet <$> targetPin,
            agreed = landingsAgree
          }

spec_pathResidualsUnresolvedBy :: Spec
spec_pathResidualsUnresolvedBy = do
  let -- expected alternatives, in order (meta-union: any one alternative
      -- is a complete repair; see the spec)
      testResidualAlts' :: Graph Text () -> Text -> [Text] -> Spec
      testResidualAlts' graph pathToResidualStr expectedStrs =
        it (unpack testName) do
          pathToResidual <-
            parseForTest "normalized path" pNormalizedPath pathToResidualStr
          expected <-
            traverse
              (parseForTest "normalized path" pNormalizedPath)
              expectedStrs
          expectedAlts <- case expected of
            (e : es) -> pure (e :| es)
            [] -> fail "testResidualAlts': no expected alternatives given"
          pathResidualsUnresolvedBy (smallNID 1) graph pathToResidual
            `shouldBe` expectedAlts
        where
          testName =
            "residuals of "
              <> pathToResidualStr
              <> " in the graph "
              <> compactGraphRepresentation graph
              <> " = "
              <> intercalate " | " expectedStrs
      testResiduals' :: Graph Text () -> Text -> Text -> Spec
      testResiduals' graph pathToResidualStr expectedStr =
        testResidualAlts' graph pathToResidualStr [expectedStr]
      testResiduals edges = testResiduals' (graphWithEdges edges)
      testResidualAlts edges = testResidualAlts' (graphWithEdges edges)

  describe "simple matches" do
    testResiduals [edge' 1 "a" 2, edge' 1 "b" 2] "[a]" "%never"
    testResiduals [edge' 1 "a" 2] "[a]" "%never"
    testResiduals [edge' 1 "a" 2] "[*]" "%never"
    testResiduals [edge' 1 "a" 2] [rq|[regex:"^a*$"]|] "%never"
    testResiduals [edge' 1 "apple" 2] [rq|[regex:"a"]|] "%never"
    testResiduals [edge' 1 "a" 2] "[@<a]" "%never"
    testResiduals [edge' 1 "a" 2] "[@1<a]" "%never"
    testResiduals [edge' 1 "a" 2] "[a]>@" "%never"
    testResiduals [edge' 1 "a" 2] "[a]>@2" "%never"
    testResiduals [edge' 1 "a" 2] "[@1<a]>@2" "%never"
    testResiduals [edge' 2 "a" 1] "[~a]" "%never"

    testResiduals [edge' 1 "a" 1, edge' 1 "b" 1] "@[a]" "%never"
    testResiduals [edge' 1 "a" 1] "@[a]" "%never"
    testResiduals [edge' 1 "a" 1] "@[*]" "%never"
    testResiduals [edge' 1 "a" 1] [rq|@[regex:"^a*$"]|] "%never"
    testResiduals [edge' 1 "apple" 1] [rq|@[regex:"a"]|] "%never"
    testResiduals [edge' 1 "a" 1] "@[a]" "%never"
    testResiduals [edge' 1 "a" 1] "@1[a]" "%never"
    testResiduals [edge' 2 "a" 2] "@2[a]" "%never"

  describe "intersection matches" do
    testResiduals [edge' 1 "a" 2, edge' 1 "b" 2] "[a & b]" "%never"
    testResiduals [edge' 1 "a" 2, edge' 2 "a" 1] "[~a & a]" "%never"
    -- the JoinPoint root @< pins to @1 because a sibling branch resolved,
    -- but the Unanchored target is never pinned
    testResiduals [edge' 1 "a" 2] "[@<a & @<b]" "[@1<b]"
    testResiduals
      [edge' 1 "a" 2, edge' 3 "b" 2]
      "[@<a & @3<b & @<c]>@"
      "[@1<c]>@2"
    -- Unanchored anchors are never pinned in residuals: neither the root nor
    -- the target gets specialized here. Paths like this one, whose
    -- intersection must meet at an Unanchored target, are exempt from the
    -- residual invariant; callers that want join semantics must anchor the
    -- target with >@ (see docs/pathResidualsUnresolvedBy-spec.md)
    testResiduals [edge' 1 "a" 2] "[a & b]" "[b]"
    testResiduals [edge' 1 "a" 2] "[@<a & @<b & @<c]>@" "[@1<b & @1<c]>@2"
    testResiduals [edge' 1 "b" 2] "[@<a & @<b & @<c]>@" "[@1<a & @1<c]>@2"
    testResiduals [edge' 1 "c" 2] "[@<a & @<b & @<c]>@" "[@1<a & @1<b]>@2"
    testResiduals
      [edge' 1 "c" 2, edge' 1 "a" 2]
      "[@<a & @<b & @<c]>@"
      "[@1<b]>@2"

  describe "sequence matches" do
    testResiduals [edge' 1 "a" 2, edge' 2 "b" 3] "[a /| b]" "%never"
    testResiduals [edge' 1 "a" 2] "[a /| b]" "[@2<b]"
    -- A forward gap that already runs to an unpinned join stays partial;
    -- there is no concrete suffix endpoint that would conflict with the
    -- fresh join minted while creating the residual.
    testResiduals [edge' 3 "e" 2] "[@2<~e /| z]>@" "[@3<z]>@"
    -- The first c can match the self-loop, but the self-loop-free reading
    -- recovers @2 as the join-midpoint seam from the surviving ~baz tail.
    testResidualAlts
      [edge' 1 "c" 1, edge' 3 "baz" 2]
      "[@<c /@| ~baz]>@"
      [ "[@1<c /@| ~baz]>@",
        "[@1<c]>@2",
        "[@1<~baz]>@3",
        "[@1<~baz]>@"
      ]
    -- The missing ~foo branch must meet its resolved ~x sibling at @1.
    -- Keeping only the terminal gap with an unpinned target would mint a
    -- different landing and violate the residual invariant.
    testResidualAlts
      [edge' 1 "x" 2, edge' 2 "b" 1]
      "[@<~b /@| (~x & ~foo)]>@"
      ["[@1<~b /@| (~x & ~foo)]>@", "[@2<~foo]>@1"]
    testResidualAlts
      [edge' 1 "baz" 2, edge' 2 "x" 1, edge' 3 "e" 2]
      "[@<(baz & ~x) /| (a & ~e)]>@"
      ["[@1<(baz & ~x) /| (a & ~e)]>@", "[@2<a]>@3"]
    testResidualAlts
      [ edge' 1 "e" 1,
        edge' 1 "e" 3,
        edge' 2 "d" 1,
        edge' 4 "z" 3
      ]
      "[@<(~d /@2| ~d & e) /| ~z]>@"
      ["[@1<(~d /@2| ~d & e) /| ~z]>@", "[@2<~d]>@3"]
    testResidualAlts
      [ edge' 1 "a" 1,
        edge' 1 "x" 1,
        edge' 2 "a" 1,
        edge' 2 "baz" 4
      ]
      "[@<(~a & x) /@| baz]>@4"
      ["[@1<baz]>@4", "[@1<x]>@2"]
    testResidualAlts
      [edge' 1 "foo" 2, edge' 1 "y" 2, edge' 3 "c" 2]
      "[@1<y /| (~foo & ~c)]>@"
      [ "[@1<y /| (~foo & ~c)]>@",
        "[@2<~c]>@1",
        "[@2<~foo]>@3"
      ]
    testResidualAlts
      [edge' 2 "d" 1, edge' 3 "b" 2, edge' 3 "z" 4]
      "[~d /@2| ~b /@| ~z]>@"
      [ "[@3<~z]>@",
        "[~d /@2| ~b /@| ~z]>@",
        "[@3<~z]>@3",
        "[@2<~b]>@4"
      ]
    testResidualAlts
      [ edge' 2 "z" 2,
        edge' 2 "z" 3,
        edge' 2 "bar" 4,
        edge' 2 "bar" 3
      ]
      "[@2<(z & bar) /| x]>@"
      ["[@3<x]>@", "[@2<(z & bar) /| x]>@"]
    testResidualAlts
      [edge' 2 "c" 2, edge' 3 "x" 3, edge' 3 "a" 3]
      "[@2[c]<~c]>@[x & ~a]"
      ["@2[x & ~a]", "[@2<~c]>@3"]
    testResiduals
      [edge' 2 "e" 3, edge' 4 "e" 3, edge' 3 "a" 5]
      "[@2<(e & ~x /| ~a) /| ~e]>@4"
      "[@2<~x]>@5"
    testResidualAlts
      [edge' 1 "z" 2, edge' 2 "baz" 1, edge' 3 "z" 2, edge' 4 "a" 3]
      "[@<z /@2| (baz & ~z) /| ~a]>@4"
      ["[@1<~a]>@4", "[@2<baz]>@3"]
    testResidualAlts
      [edge' 2 "b" 1, edge' 2 "x" 4, edge' 4 "z" 5, edge' 3 "z" 5]
      "[@1<~b /@2| (bar & x) /| z]>@5"
      ["[@2<bar]>@4", "[@2<bar]>@3"]
    -- The baz branch's continuation progresses from @4, not from the
    -- smaller fan-out landing @2. The failed ~y sibling must meet that
    -- actual pair point or the two residual pieces are incompatible.
    testResidualAlts
      [edge' 3 "baz" 2, edge' 3 "baz" 4, edge' 4 "b" 5]
      "[@1<~y /@2| ~baz /@| (baz & ~y) /| b /@| (~b & ~y)]>@"
      [ "[@1<~y /@2| ~baz /@| (baz & ~y) /| b /@| (~b & ~y)]>@",
        "[@1<~y]>@2 + [@3<~y & @5<~y]>@4"
      ]
    -- Per-arrival continuation tracking must map a failed singleton back to
    -- the intersecting branch's larger fan-out set. Equality would miss it
    -- and incorrectly report the whole path as resolved.
    testResidualAlts
      [ edge' 1 "foo" 1,
        edge' 2 "baz" 3,
        edge' 4 "foo" 1,
        edge' 3 "z" 1
      ]
      "[@1<(~foo & ~z) /| ~baz]>@"
      [ "[@1<(~foo & ~z) /| ~baz]>@",
        "[@1<~foo]>@3",
        "[@1<~z /@1| ~baz]>@2",
        "[@1<~z /@4| ~baz]>@2"
      ]
    -- A gapped nested intersection still carries the terminal reached by
    -- its resolved ~d sibling. The enclosing intersection must propagate
    -- that reach so its outer join can pin the c and ~bar repairs there.
    testResidualAlts
      [edge' 1 "baz" 2, edge' 4 "d" 3, edge' 3 "foo" 2]
      "[@1<baz /@2| (~foo /| (~d & c) & ~bar)]>@"
      [ "[@1<baz /@2| (~foo /| (~d & c) & ~bar)]>@",
        "[@3<c & @2<~bar]>@4"
      ]
    -- Re-aiming the failed ~x branch through @1 would make its surviving x
    -- tail land at @5, while the working ~foo sibling's tail lands at @4.
    -- Only @3 is a compatible pair junction for the shared y continuation.
    testResiduals
      [edge' 1 "x" 5, edge' 3 "foo" 2, edge' 3 "x" 4]
      "[~foo /@2| ((~x & ~foo) /| x & baz) /| y]>@6"
      "[~foo]>@2 + [@2<~x]>@3 + [@2<baz /@4| y]>@6"
    -- The unpinned outer join cannot keep only the missing half of the
    -- inner `foo & baz` meet: instantiation would aim `baz` at a fresh
    -- node while the surviving `foo` still lands at @5. Its safe fallback
    -- recreates the whole branch; the pinned alternatives remain minimal.
    testResidualAlts
      [ edge' 1 "d" 1,
        edge' 2 "x" 3,
        edge' 3 "a" 4,
        edge' 4 "foo" 5,
        edge' 5 "baz" 2
      ]
      "[@1<~d & @2<x /@3| (a /| (foo & baz) & a) & @2<~baz]>@"
      [ "[@1<~d & @2<x /@3| (a /| (foo & baz) & a) & @2<~baz]>@",
        "[@4<foo & @4<baz & @3<a & @2<~baz]>@1",
        "[@1<~d & @4<baz & @3<a]>@5"
      ]
    -- we want to keep the resulting expression minimally constrained; so we
    -- don't e.g. specialize "c /| d" to "c /@2| d"
    testResiduals
      [edge' 1 "a" 2]
      "[@<(a /| b) & @<(c /| d)]"
      "[@2<b & @1<(c /| d)]"
    testResiduals
      [edge' 1 "c" 2]
      "[@<(a /| b) & @<(c /| d)]"
      -- root-map keys follow the declaration order of the branches that
      -- produced them (a /| b came first), not consumed-first order
      "[@1<(a /| b) & @2<d]"
    testResiduals
      [edge' 1 "z" 2, edge' 2 "a" 3]
      "[@<z /| (a /| b & c /| d)]"
      "[@3<b & @2<(c /| d)]"
    testResiduals
      [edge' 1 "z" 2, edge' 2 "c" 3]
      "[@<z /| (a /| b & c /| d)]"
      "[@2<(a /| b) & @3<d]"
    testResiduals
      [edge' 1 "z" 2, edge' 2 "c" 3, edge' 3 "d" 4]
      "[@<z /| (a /| b & c /| d)]"
      -- the Unanchored target is not pinned to @4 where c /| d landed
      "[@2<(a /| b)]"
    testResiduals [edge' 1 "a" 2, edge' 2 "a" 1] "@[a /@2| ~a]" "%never"
    testResiduals [edge' 1 "a" 2, edge' 2 "a" 1] "@[a /@| ~a]" "%never"
    testResiduals [edge' 1 "a" 1, edge' 1 "a" 1] "@[a /@| ~a]" "%never"
    testResiduals [edge' 1 "a" 2, edge' 2 "a" 1] "@[a /| ~a]" "%never"
    testResiduals [edge' 1 "a" 1, edge' 1 "a" 1] "@[a /| ~a]" "%never"
    testResiduals [edge' 1 "a" 2] "[a /| (b & c)]" "[@2<(b & c)]"
    testResiduals [edge' 1 "a" 2, edge' 2 "b" 3] "[a /| (b & c)]" "[@2<c]"

    describe "with loops at midpoints" do
      testResiduals [edge' 1 "a" 2, edge' 2 "c" 3] "[a /@[b]| c]" "@2[b]"
      testResiduals [edge' 1 "a" 2, edge' 2 "c" 3] "[a /@2[b]| c]" "@2[b]"
      testResiduals [edge' 1 "a" 2, edge' 2 "b" 2] "[a /@[b]| c]" "[@2<c]"
      testResiduals [edge' 1 "a" 2, edge' 2 "b" 2] "[a /@2[b]| c]" "[@2<c]"
      testResiduals
        [edge' 1 "a" 2, edge' 2 "b" 3, edge' 3 "c" 2]
        "[a /@[b/|c]| d]"
        "[@2<d]"
      testResiduals
        [edge' 1 "a" 2, edge' 2 "b" 2, edge' 2 "c" 2]
        "[a /@[b & c]| d]"
        "[@2<d]"
      testResiduals [edge' 1 "a" 2] "[a /@[b/|c]| d]" "[@2[b /| c]<d]"
      -- Note: loop b/|c at @2 completes via incoming c from @3, so b is inverted to ~b
      testResiduals [edge' 1 "a" 2, edge' 3 "c" 2] "[a /@[b/|c]| d]" "[@3<~b /@2| d]"
      testResiduals [edge' 1 "a" 2] "[a /@2[b]| c]" "[@2[b]<c]"
      testResiduals [edge' 1 "a" 2] "[a /@[b & c]| d]" "[@2[b & c]<d]"

    describe "with explicit join midpoints" do
      testResidualAlts
        [ edge' 2 "a" 3,
          edge' 4 "b" 5,
          edge' 4 "b" 3,
          edge' 4 "e" 1
        ]
        "[@1<~e /@| b /@| (~a & ~b)]>@"
        [ "[@1<~e /@| b /@| (~a & ~b)]>@",
          "[@3<~b]>@2",
          "[@3<~a]>@4",
          "[@5<~a]>@4"
        ]

    describe "with implicit pair-point disagreements" do
      testResidualAlts
        [ edge' 3 "bar" 1,
          edge' 1 "x" 1,
          edge' 4 "bar" 5,
          edge' 5 "e" 1
        ]
        "[@1<((~x & ~e) /| (~b & ~bar) & ~y) /| ~foo]>@2"
        [ "[@3<~foo]>@2 + [@1<~b & @1<~y]>@3 + [@1<~e]>@1",
          "[@1<~x /@5| ~b & @1<~y]>@4 + [@4<~foo]>@2"
        ]

  describe "keeps deconstructed loops as deterministic as possible" do
    testResiduals
      [edge' 1 "a" 2]
      "[@[a/|b]<c]"
      -- we want to keep the resulting paths as connected/deterministic as
      -- possible, e.g. we could produce "[@2<b]>@1 + [@1<c]" which encodes the
      -- same edges, but that requires two deterministic paths instead of just
      -- one
      "[@2<b /@1| c]"
    testResiduals
      [edge' 1 "a" 2, edge' 2 "b" 3]
      "[a /@[b/|c]| d]"
      "[@3<c /@2| d]"
    testResiduals
      [edge' 1 "a" 2, edge' 3 "c" 2]
      "[a /@[b/|c]| d]"
      "[@3<~b /@2| d]"
    testResiduals
      [edge' 1 "a" 2]
      "@[a/|b/|c]"
      "[@2<b /| c]>@1"
    testResiduals
      [edge' 1 "a" 2]
      "[@[a/|b/|c]<d]"
      "[@2<b /| c /@1| d]"
    testResiduals
      [edge' 2 "c" 1]
      "[@[a/|b/|c]<d]"
      -- inverting edges is preferrable to disconnecting DPaths
      "[@2<~b /| ~a /@1| d]"
    testResiduals
      [edge' 2 "c" 1, edge' 1 "d" 3]
      "[@[a/|b/|c & d/|e/|f]<g]"
      "[[@2<~b /| ~a & @3<e /| f]>@1<g]"
    testResiduals
      [edge' 2 "d" 1, edge' 1 "a" 3]
      "[@[a/|b/|c/|d]<e]"
      -- it's possible for it to be necessary to separate out an edge though
      "[@3<b/|c]>@2 + [@1<e]"
    testResiduals
      [edge' 2 "d" 1, edge' 1 "a" 3]
      "[@[a/|~b/|~c/|d]<e]"
      -- this should preference the original orientation of edges
      "[@3<~b/|~c]>@2 + [@1<e]"

  describe "multiple transition matches" do
    testResiduals [edge' 1 "a" 2, edge' 1 "a" 3] "[a]" "%never"
    -- a fan-out of ONE branch's matching yields alternatives (either
    -- continuation alone repairs the path), ascending NID order
    testResidualAlts
      [edge' 1 "a" 2, edge' 1 "a" 3]
      "[a/|b]"
      ["[@2<b]", "[@3<b]"]
    testResidualAlts
      [edge' 1 "a" 2, edge' 2 "b" 3, edge' 2 "b" 4]
      "[a/|b/|c]"
      ["[@3<c]", "[@4<c]"]

  describe "unchanged when unmatched" do
    testResiduals [] "[a]" "[a]"
    testResiduals [edge' 2 "a" 3] "[a]>@3" "[a]>@3"
    -- the backward-recovered seam repair is a tie-split alternative
    testResidualAlts [edge' 3 "b" 4, edge' 1 "a" 2] "[a/|b]>@4" ["[@2<b]>@4", "[a]>@3"]
    testResiduals [edge' 3 "a" 2] "[~a]>@3" "[~a]>@3"
    testResidualAlts [edge' 4 "b" 3, edge' 1 "a" 2] "[a/|~b]>@4" ["[@2<~b]>@4", "[a]>@3"]
    testResiduals [edge' 1 "~a" 2] "[~a]" "[~a]"
    testResiduals [edge' 1 "~a" 1] "@[~a]" "@[~a]"
    testResiduals [edge' 1 "a" 2] "[~a]" "[~a]"
    testResiduals' (disconnectedGraph [1]) "[a]" "[a]"
    testResiduals [] "[@<a & @<b]" "[@<a & @<b]"
    testResiduals [] "[a & b]" "[a & b]"
    testResiduals [] "@" "@"
    testResiduals [] "@[a & b /| c]" "@[a & b /| c]"
    testResiduals [] "@[a]" "@[a]"
    testResiduals [] "@1[a]" "@1[a]"
    testResiduals [edge' 1 "a" 2] "@[a]" "@[a]"
    testResiduals [edge' 1 "a" 2] "@1[a]" "@1[a]"
    testResiduals' (disconnectedGraph [1]) "@[a]" "@[a]"
    testResiduals [edge' 1 "a" 2] "[a /@3| b]" "[a /@3| b]"
    testResiduals [edge' 1 "a" 2] [rq|[regex:"^b*$"]|] [rq|[regex:"^b*$"]|]
    testResiduals [edge' 1 "apple" 2] [rq|[regex:"b"]|] [rq|[regex:"b"]|]

  describe "%never is always %never" do
    testResiduals [edge' 1 "a" 2] "%never" "%never"
    testResiduals [] "%never" "%never"

  describe "unions" do
    testResiduals [edge' 1 "a" 2] "[a] + [b]" "[b]"
    testResiduals [edge' 1 "a" 2] "[@<a & @<b]>@ + [c]" "[@1<b]>@2 + [c]"
    testResiduals [edge' 1 "a" 2] "[@<a]>@ + [@<a & @<b]>@" "[@1<b]>@2"
    testResidualAlts
      [ edge' 1 "a" 2,
        edge' 1 "a" 3,
        edge' 1 "c" 4,
        edge' 1 "c" 5
      ]
      "[a /| b] + [c /| d]"
      [ "[@2<b] + [@4<d]",
        "[@3<b] + [@4<d]",
        "[@2<b] + [@5<d]",
        "[@3<b] + [@5<d]"
      ]

  describe "edge cases" do
    describe "multiple incoming sources" do
      -- Both @2 and @3 have 'c' edges to @1: either seam alone repairs the
      -- path, so they are alternatives (not a union), each expressed
      -- forward from the pinned loop root
      testResidualAlts
        [edge' 2 "c" 1, edge' 3 "c" 1]
        "@[a/|b/|c]"
        ["[@1<a /| b]>@2", "[@1<a /| b]>@3"]

    describe "wildcard matching incoming" do
      -- Wildcard matches the 'x' edge back to @1, but 'a' still needs
      -- resolution; the gap is expressed forward from the pinned loop root
      testResiduals [edge' 2 "x" 1] "@[a/|*]" "[@1<a]>@2"
      -- There still needs to be an edge that is actually a loop for the
      -- wildcard to succeed
      testResiduals [edge' 2 "x" 1] "@[*]" "@[*]"

    describe "loop paths handling from both ends" do
      describe "complete resolution" do
        testResiduals [edge' 1 "a" 2, edge' 1 "b" 2] "@[a/|~b]" "%never"
        testResiduals
          [edge' 1 "a" 2, edge' 1 "b" 2, edge' 2 "c" 2]
          "@[a/|~c/|~b]"
          "%never"
        testResiduals
          [edge' 1 "a" 2, edge' 2 "d" 2, edge' 1 "b" 2, edge' 2 "c" 2]
          "@[a/|d/|~c/|~b]"
          "%never"
      describe "partial resolution from outgoing end only" do
        testResiduals [edge' 1 "a" 2] "@[a /| b]" "[@2<b]>@1"
        testResiduals [edge' 1 "a" 2] "@1[a /| b]" "[@2<b]>@1"
        testResiduals [edge' 1 "a" 2] "[@[a /| b]<c]" "[@2<b/@1|c]"
      describe "partial resolution from incoming end only" do
        -- Multiple incoming edges forming a chain should all be resolved
        -- @2>"c">@1 handles 'c', @3>"b">@2 handles 'b', only 'a' remains;
        -- the gap is expressed forward (original orientation) from the
        -- pinned loop root @1 to the seam @3
        testResiduals [edge' 2 "c" 1, edge' 3 "b" 2] "@[a/|b/|c]" "[@1<a]>@3"
        -- Neither branch has outgoing, both complete via incoming
        testResiduals
          [edge' 2 "c" 1, edge' 3 "f" 1]
          "@[a/|b/|c & d/|e/|f]"
          "[@2<~b /| ~a & @3<~e /| ~d]>@1"
      describe "don't meet up in middle" do
        -- For @[a/|~b]: 'a' goes @1->@2, '~b' looks for incoming 'b' at @2
        -- But @1>"b">@3, not to @2, so ~b fails - residual starts from @2
        testResidualAlts
          [edge' 1 "a" 2, edge' 1 "b" 3]
          "@[a/|~b]"
          ["[@2<~b]>@1", "[@1<a]>@3"]
        testResidualAlts
          [edge' 1 "a" 2, edge' 1 "b" 2, edge' 2 "c" 3]
          "@[a/|~c/|~b]"
          ["[@2<~c]>@2", "[@1<a]>@3"]
      describe "self-loop bias" do
        -- Forward can consume a then d (via the self-loop @2>"d">@2, a
        -- coincidental match of a non-loop step) with backward consuming
        -- ~b: gap [~c] (total 3). Backward can instead consume ~b then ~c
        -- with forward consuming only a: gap [d] (total 3). The tie is
        -- broken against the split that depends on the self-loop match,
        -- so the backward seam wins (see spec §4.1.1/§5.1)
        testResidualAlts
          [edge' 1 "a" 2, edge' 2 "d" 2, edge' 1 "b" 2, edge' 2 "c" 3]
          "@[a/|d/|~c/|~b]"
          ["[@2<d]>@3", "[@2<~c]>@2"]
        testResidualAlts
          [edge' 1 "a" 2, edge' 2 "d" 2, edge' 1 "b" 2, edge' 2 "c" 3]
          "[@[a/|d/|~c/|~b]<e]"
          ["[@2<d]>@3 + [@1<e]", "[@2<~c]>@2 + [@1<e]"]
        testResidualAlts
          [edge' 1 "a" 2, edge' 2 "d" 2, edge' 1 "b" 2, edge' 2 "c" 3]
          "[@[a/|(d/|~c/|~b & f)]<e]"
          ["[@2<d]>@3 + [@2<f/@1|e]", "[@2<~c]>@2 + [@2<f/@1|e]"]
        testResidualAlts
          [edge' 1 "a" 2, edge' 2 "d" 2, edge' 1 "b" 2, edge' 2 "c" 3]
          "[@[a/|(d/|~c/|~b & f & g)]<e]"
          ["[@2<d]>@3 + [@2<(f & g)/@1|e]", "[@2<~c]>@2 + [@2<(f & g)/@1|e]"]
      describe "flexible middle transition" do
        -- if we can resolve from both ends we can/should
        testResiduals
          [edge' 1 "a" 2, edge' 1 "b" 3]
          "@[a/|c/|~b]"
          "[@2<c]>@3"
        testResiduals
          [edge' 1 "a" 2, edge' 1 "b" 3]
          "@[a/|d/|c/|~b]"
          "[@2<d/|c]>@3"
        -- ~b only matches an edge with the orientation that creating the
        -- path would mint (@1>"b">@3); a b-edge pointing the other way
        -- (@3>"b">@1) cannot satisfy the trailing ~b, so ~b stays in the
        -- residual
        testResiduals
          [edge' 1 "a" 2, edge' 3 "b" 1]
          "@[a/|c/|~b]"
          "[@2<c /| ~b]>@1"
        testResiduals
          [edge' 1 "a" 2, edge' 3 "b" 1]
          "@[a/|d/|c/|~b]"
          "[@2<d /| c /| ~b]>@1"
        -- trailing-gap counterpart: forward consumes a and c, ~b remains
        testResiduals
          [edge' 1 "a" 2, edge' 2 "c" 3]
          "@[a/|c/|~b]"
          "[@3<~b]>@1"
    describe "JoinPoint with exclusions" do
      -- Can't reach @2 when @2 is excluded, unchanged
      testResiduals [edge' 1 "a" 2] "[a]>!{@2}" "[a]>!{@2}"
      -- @2 is not excluded, resolves fine
      testResiduals [edge' 1 "a" 2] "[a]>!{@1}" "%never"
      -- A node excluded by the target is not a pin candidate merely
      -- because the target's loop happens to resolve there.
      testResiduals
        [edge' 4 "b" 2, edge' 3 "d" 3]
        "[@2<~b]>!{@3}[~d]"
        "@4[~d]"

    describe "target anchor mismatch" do
      -- Reached @2 but target requires @3, unchanged
      testResiduals [edge' 1 "a" 2] "[a]>@3" "[a]>@3"

    describe "loops at midpoint of sequence" do
      testResiduals
        [edge' 1 "a" 2, edge' 2 "x" 2]
        "[a /@[x]| b]"
        "[@2<b]"
      testResiduals
        [edge' 1 "a" 2, edge' 2 "x" 3]
        "[a /@[x]| b]"
        "[@2[x]<b]"
      testResiduals
        [edge' 1 "a" 2, edge' 2 "x" 3]
        "[a /@[x/|y]| b]"
        "[@3<y /@2| b]"
      testResiduals
        [edge' 1 "a" 2, edge' 2 "x" 3]
        "[a /@[x/|y/|z]| b]"
        "[@3<y /| z /@2| b]"

    describe "absolute root NID" do
      -- Starts from @2 (not @1), 'a' resolves to @3
      testResiduals [edge' 2 "a" 3] "[@2<a]" "%never"
      -- Same but with explicit target matching
      testResiduals [edge' 2 "a" 3] "[@2<a]>@3" "%never"

    describe "nested intersections" do
      -- Intersection in bs1 of a loop: (a & b) must BOTH succeed to reach midpoint
      -- Both a and b exist and reach @2, so loop needs c to return
      testResiduals
        [edge' 1 "a" 2, edge' 1 "b" 2, edge' 2 "c" 1]
        "@[(a & b) /| c]"
        "%never"
      -- Only 'a' exists, 'b' fails; a/c resolves, leaving only b as residual
      testResiduals
        [edge' 1 "a" 2, edge' 2 "c" 1]
        "@[(a & b) /| c]"
        "[@1<b]>@2"
      -- Both exist but reach different nodes; the unanchored midpoint
      -- explodes per branch pair, so c continues from both frontiers.
      testResidualAlts
        [edge' 1 "a" 2, edge' 1 "b" 3]
        "@[(a & b) /| c]"
        [ "[@2<c & @3<c]>@1",
          "[@2<~b & @2<c]>@1",
          "[@3<~a & @3<c]>@1"
        ]

      -- Intersection in bs1 of rooted path (not loop)
      testResiduals
        [edge' 1 "a" 2, edge' 1 "b" 2, edge' 2 "c" 3]
        "[(a & b) /| c]"
        "%never"
      testResiduals
        [edge' 1 "a" 2, edge' 2 "b" 3, edge' 2 "c" 3]
        "[a /| (b & c)]"
        "%never"
      testResiduals
        [edge' 1 "a" 2, edge' 2 "c" 3]
        "[a /| (b & c)]"
        -- the Unanchored target is not pinned to @3 where c landed
        "[@2<b]"
      testResiduals
        [edge' 1 "a" 2, edge' 2 "c" 3]
        "[(a & b) /| c]"
        -- the Unanchored root is not pinned to @1; the @2 target is the
        -- junction where the existing c edge continues
        "[b]>@2"

      -- Nested: intersection in bs1, then sequence in bs2
      -- a & b both reach @2, then c/|d must complete
      testResiduals
        [edge' 1 "a" 2, edge' 1 "b" 2, edge' 2 "c" 3]
        "[(a & b) /| c /| d]"
        "[@3<d]"
      -- a & b both reach @2, c fails
      testResiduals
        [edge' 1 "a" 2, edge' 1 "b" 2]
        "[(a & b) /| c /| d]"
        "[@2<c /| d]"

      -- Double nested: intersection in both bs1 and bs2
      testResiduals
        [edge' 1 "a" 2, edge' 1 "b" 2, edge' 2 "c" 3, edge' 2 "d" 3]
        "[(a & b) /| (c & d)]"
        "%never"
      -- bs1 intersection succeeds, bs2 intersection partially fails;
      -- the Unanchored target is not pinned to @3 where c landed
      testResiduals
        [edge' 1 "a" 2, edge' 1 "b" 2, edge' 2 "c" 3]
        "[(a & b) /| (c & d)]"
        "[@2<d]"

      -- Loop with intersection in bs2 and gap computation
      -- @1 -a-> @2, then (b & c) must return to @1
      -- b completes via @3>"b">@1, c fails entirely
      testResiduals
        [edge' 1 "a" 2, edge' 3 "b" 1]
        "@[a /| (b & c)]"
        "[@2<b & @2<c]>@1"

    describe "complex roots" do
      testResiduals
        [edge' 2 "a" 4, edge' 3 "b" 4]
        "[@2<a & @3<b]>@"
        "%never"
      testResiduals
        [edge' 2 "c" 2, edge' 2 "a" 3, edge' 1 "b" 3]
        "[@2[c]<a & @<b]>@"
        "%never"
      testResiduals
        [edge' 2 "c" 2, edge' 2 "a" 3]
        "[@2[c]<a & @<b]>@"
        -- the JoinPoint root @< pins to @1 once a sibling branch resolves;
        -- the JoinPoint target pins to @3 which @2<a proved
        "[@1<b]>@3"
      testResiduals
        [edge' 1 "a" 3]
        "[@2<a & @<b]>@"
        -- nothing resolves (@2 has no a-edge, @1 has no b-edge), so nothing
        -- pins and the path is returned unchanged
        "[@2<a & @<b]>@"
      testResiduals
        [edge' 2 "c" 2, edge' 1 "b" 3]
        "[@2[c]<a & @<b]>@"
        "[@2<a]>@3"
      testResiduals
        [edge' 2 "a" 3, edge' 1 "b" 3]
        "[@2[c]<a & @1<b]"
        -- Both branches resolve and agree on target @3; only the loop `c` at
        -- @2 is unresolved. Adding just `@2>"c">@2` makes the original path
        -- fully resolvable, so the residual is only the unresolved loop,
        -- which consumed zero steps and therefore stays attached to its
        -- pointlike instead of deconstructing. The resolved `@1<b` branch
        -- is excluded from the residual.
        "@2[c]"

    describe "multi-root residuals" do
      -- The nested JoinPoint has no forward landing after y disappears,
      -- but the surviving outer ~x branch recovers @2 as its seam. Pinning
      -- the nested join there exposes the exact one-edge repair.
      testResidualAlts
        [edge' 3 "x" 2, edge' 1 "d" 3]
        "[[y]>@<~x & @<d]>@3"
        ["[y /@| ~x]>@3", "[y]>@2"]
      -- A partial nested root with an Unanchored target cannot feed an
      -- outer branch by pinning that target. Reverse the missing segment
      -- between its concrete seams instead, so the target stays unpinned
      -- while the surviving outer branch can be reused.
      testResiduals
        [edge' 1 "x" 2, edge' 2 "d" 3]
        "[[e & @<x]<d]>@"
        "[@2<~e]>@1"
      -- A nested Unanchored-target root whose branch is entirely missing:
      -- nothing of the nested path survives, so the seam @2 is identified
      -- only by the surviving outer branch b. The exact repair reruns the
      -- nested root toward that seam and reverses it, keeping the
      -- Unanchored target unpinned (regression: property 2 exactness).
      testResiduals
        [edge' 2 "b" 3]
        "[[a]<b]>@3"
        "[@2<~a]>@1"
      -- one connected multi-root alternative: the nested root's unresolved
      -- branch b chains through the nested target @3 into the outer branch
      -- c, and the other root's branch d stays in the same deterministic
      -- path (spec §6.1)
      testResiduals
        [edge' 1 "a" 3]
        "[[@1<a & @2<b]>@3<c & @4<d]>@5"
        "[@2<b/@3|c & @4<d]>@5"
      -- a root JoinPoint excluding the start NID can never unify, and
      -- resolution is conjunctive across roots, so no edge creation can
      -- help: the whole deterministic path is unsatisfiable and returned
      -- unchanged; its branches are not dropped
      testResiduals [edge' 1 "b" 2] "[!{@1}<a & @<b]" "[!{@1}<a & @<b]"
      testResiduals [] "[!{@1}<a]" "[!{@1}<a]"

  describe "property tests" do
    -- Generated paths are made "covered" constructively instead of being
    -- filtered with suchThat/==>: 'coveredPathFrom' normalizes carve-out
    -- paths (intersections meeting at an Unanchored target, which are
    -- exempt from the residual invariant) and strips the start NID from
    -- JoinPoint exclusion sets (a root excluding the start NID can never
    -- resolve from it), so the generated graph resolves the path by
    -- construction; see docs/pathResidualsUnresolvedBy-spec.md
    let genStartAndPath = do
          startNid <- genNID
          dp <-
            coveredPathFrom startNid
              <$> genDeterministicPathLiteralOnly defaultGenConfig
          pure (startNid, NormalizedPath (singletonSet dp))

        resolvesIn :: Graph Text () -> NID -> NormalizedPath Anchor -> Bool
        resolvesIn graph startNid path =
          resolveNPathExists startNid (leastConstrainedNormalizedPath path)
            & runInMemoryGraphMetadataEditing
            & evalStateLocal graph
            & runPureEff

    -- The remaining `==>`s are guards, not filters: 'coveredPathFrom'
    -- makes the resolvesIn precondition hold by construction (up to
    -- astronomically unlikely collisions with the generator's fresh
    -- uniformly random NIDs), so they essentially never discard. Property
    -- 2 additionally discards edgeless graphs (pointlike paths with no
    -- loops), where there is no edge to remove.

    it "resolving graph produces %never residuals for complex paths" $
      property $ do
        seed <- arbitrary
        let gen = mkStdGen seed
        (startNid, path) <- genStartAndPath
        let graph = graphResolvingPathFrom gen startNid path
            residual = pathResidualsUnresolvedBy startNid graph path
        pure $
          resolvesIn graph startNid path
            ==> counterexample
              ( "path: "
                  <> show path
                  <> "\nstart: "
                  <> show startNid
                  <> "\ngraph: "
                  <> show (graphEdges graph)
              )
              (residual === pure (NormalizedPath mempty))

    it "removing one edge produces residuals that resolve in removed edge graph" $
      property $ do
        seed <- arbitrary
        let gen = mkStdGen seed
        (startNid, path) <- genStartAndPath
        let graph = graphResolvingPathFrom gen startNid path
            edges = graphEdges graph
        pure
          $ (not (null edges) && resolvesIn graph startNid path)
            ==> forAll (elements edges)
          $ \edgeToRemove ->
            let graphMinusEdge = deleteEdge edgeToRemove graph
                residualAlts = pathResidualsUnresolvedBy startNid graphMinusEdge path
                singleEdgeGraph = insertEdges [edgeToRemove] emptyGraph
                -- an alternative is the exact repair iff it fully resolves
                -- in the graph containing only the removed edge
                altResolves alt =
                  pathResidualsUnresolvedBy startNid singleEdgeGraph alt
                    == pure (NormalizedPath mempty)
             in -- Either the path still fully resolves (the edge wasn't
                -- critical) or SOME alternative is the exact repair: the
                -- removed edge's contribution must be among the enumerated
                -- alternatives even when the graph alone can't determine
                -- which one it was
                counterexample
                  ( "path: "
                      <> show path
                      <> "\nstart: "
                      <> show startNid
                      <> "\ngraph: "
                      <> show edges
                      <> "\nresidual alternatives: "
                      <> show residualAlts
                  )
                  $ (residualAlts === pure (NormalizedPath mempty))
                    .||. property (any altResolves (toList residualAlts))

    -- the core invariant, stated directly: EVERY alternative is a complete
    -- repair — creating its edges on top of the partial graph must make
    -- the original path resolvable by resolveNPath
    it "creating the residual's edges makes the path resolve" $
      property $ do
        seed <- arbitrary
        let (graphGen, residualGen) = split (mkStdGen seed)
        (startNid, path) <- genStartAndPath
        let graph = graphResolvingPathFrom graphGen startNid path
        pure $
          resolvesIn graph startNid path
            ==> forAll (sublistOf (graphEdges graph)) \removedEdges ->
              let partialGraph = deleteEdges removedEdges graph
                  residualAlts = pathResidualsUnresolvedBy startNid partialGraph path
                  combinedFor alt =
                    partialGraph
                      <> graphResolvingPathFrom residualGen startNid alt
               in counterexample
                    ( "path: "
                        <> show path
                        <> "\nstart: "
                        <> show startNid
                        <> "\ngraph: "
                        <> show (graphEdges graph)
                        <> "\nresidual alternatives: "
                        <> show residualAlts
                    )
                    -- a single case occasionally blows up combinatorially
                    -- (20+ min); bound each alternative so the offending
                    -- input gets printed instead of hanging the suite
                    $ conjoin
                      [ counterexample ("alternative: " <> show alt) $
                          within 10000000 (resolvesIn (combinedFor alt) startNid path)
                      | alt <- toList residualAlts
                      ]
