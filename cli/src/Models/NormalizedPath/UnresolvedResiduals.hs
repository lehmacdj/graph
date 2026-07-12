module Models.NormalizedPath.UnresolvedResiduals
  ( pathResidualsUnresolvedBy,
    spec_pathResidualsUnresolvedBy,
  )
where

import Graph.GraphMetadataEditing (runInMemoryGraphMetadataEditing)
import Graph.Resolve (resolveNPath)
import Models.Connect (Connect (..), matchConnect)
import Models.Graph
import Models.NID
import Models.Node
import Models.NormalizedPath
import Models.NormalizedPath.Generators
import Models.NormalizedPath.Parse (pNormalizedPath)
import Models.NormalizedPath.TestUtils
import Models.ResolvedPath (ResolvedPath (..))
import MyPrelude
import Polysemy.State (evalState)
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

-- | All ways of picking one alternative from each sub-result, first
-- sub-result outermost so the all-preferred combination comes first.
combineAlts :: [[a]] -> [[a]]
combineAlts = foldr (\xs rest -> [x : r | x <- xs, r <- rest]) [[]]

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
  = -- | frontiers met; the meeting NIDs
    SegResolved (Set NID)
  | -- | fwd consumed count, fwd frontier, gap steps, bwd consumed count,
    -- and the recovered seams paired with the landings their resolved
    -- suffix reaches (for reach pinning, §5.2)
    SegGap Int (Set NID) [DPDirection] Int [(NID, Set NID)]

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
-- the larger k (forward wins). A gap always keeps at least one step
-- (k + j <= length - 1); consuming every step but missing the backward
-- frontier is not progress — the run failed as a whole.
resolveSegment ::
  Graph Text () -> Bool -> Set NID -> [DPDirection] -> EndCtx -> SegResult
resolveSegment g openBwd start steps end
  | kAll == len, Just met <- meets fwdFinal = SegResolved met
  | otherwise =
      let (k, j) = lastEx (sortOn splitKey splits)
          fF = if k == 0 then start else fwds `indexEx` (k - 1)
          seams
            | j == 0 = []
            | otherwise = [(s, reachOf j s) | s <- toList (bwds `indexEx` (j - 1))]
       in SegGap k fF (take (len - k - j) (drop k steps)) j seams
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
    splitKey (k, j) = (k + j, clean (k, j), k)
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
    reach :: Maybe (Set NID)
  }
  deriving stock (Eq, Ord)

-- | A 'BrRes' with no terminal reach information.
brRes :: Bool -> [DeterministicPath Anchor] -> Either [Gap] (Set NID) -> BrRes
brRes p ds o = BrRes p ds o Nothing

-- | The terminal position a branch result got to, resolved or not.
resReach :: BrRes -> Maybe (Set NID)
resReach r = either (const r.reach) Just r.outcome

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
                        (map toRooted (either id (const []) r.outcome))
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
  ordNub (goParts origin0 start (flattenBranch branch))
  where
    boundaryCtx :: [Part] -> EndCtx
    boundaryCtx = \case
      [] -> endCtx
      (PMid m : _) -> anchorCtx m.anchor
      _ -> EndPair

    finalEndAlts :: [Maybe NID]
    finalEndAlts = gapEndAlts endCtx

    goParts :: Bool -> Set NID -> [Part] -> [BrRes]
    goParts origin f = \case
      [] -> [brRes False [] (Right f)]
      (PRun ds : rest) ->
        case resolveSegment g (null rest && openBwd) f ds (boundaryCtx rest) of
          SegResolved f' -> afterUnit True [] f' rest
          SegGap k fF mid j seams -> afterGap origin k fF mid j seams rest
      (PSplit bs : rest) -> goSplit origin f bs rest
      (PMid _ : _) -> error "goParts: branch begins with a midpoint"

    -- continue after a fully-resolved leading unit
    afterUnit ::
      Bool -> [DeterministicPath Anchor] -> Set NID -> [Part] -> [BrRes]
    afterUnit prog ds f' = \case
      [] -> [BrRes prog ds (Right f') Nothing]
      (PMid m : rest') -> map (addProg prog ds) (stationFromArrivals f' m rest')
      rest -> map (addProg prog ds) (goParts False f' rest)

    addProg :: Bool -> [DeterministicPath Anchor] -> BrRes -> BrRes
    addProg prog ds r =
      BrRes (prog || r.progress) (ds <> r.dones) r.outcome r.reach

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
              [ BrRes prog [] (Left [Gap roots e]) reach
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
                      (Left (Gap roots e : either id (const []) st.outcome))
                      (resReach st)
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
      let cands = tailCandidates parts
          chained =
            [ brRes (k > 0) [] (Left [Gap roots e])
            | roots <- gapRootAlts origin k fF (rebuildParts (PRun mid : parts)),
              e <- finalEndAlts
            ]
       in if null cands
            then chained
            else case resolveSegment g True fF mid (EndAt cands) of
              SegResolved met ->
                map (addProg True []) (continueTail met parts)
              SegGap k2 fF2 mid2 j2 seams2 ->
                let bs = singletonSet (runToBranch mid2)
                    seamReach cs =
                      let ls = unions (mapMaybe (tailLandingFrom parts) (toList cs))
                       in justIfTrue (not (null ls)) ls
                 in ordNub
                      [ BrRes True [] (Left [Gap roots (Just s)]) reach
                      | roots <- gapRootAlts origin k2 fF2 bs,
                        (s, reach) <-
                          if j2 > 0
                            then [(s, seamReach r) | (s, r) <- seams2]
                            else [(c, tailLandingFrom parts c) | c <- toList cands]
                      ]

    continueTail :: Set NID -> [Part] -> [BrRes]
    continueTail met = \case
      (PMid m : rest') -> stationFromArrivals met m rest'
      parts -> goParts False met parts

    tailCandidates :: [Part] -> Set NID
    tailCandidates parts =
      setFromList [v | v <- toList (nidSet g), isJust (tailLandingFrom parts v)]

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
    stationAt n m rest = ordNub do
      la <- processLoops g n m.loops
      cr <- goParts True (singletonSet n) rest
      case (la.resolved, cr.outcome) of
        (True, Right f') ->
          pure $
            BrRes (la.progress || cr.progress) (la.dones <> cr.dones) (Right f') Nothing
        _ -> do
          let contGaps = either id (const []) cr.outcome
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
              ]
            [] ->
              let allAlts = concat perArrival
               in ordNub case filter (.progress) allAlts of
                    [] -> allAlts
                    ps -> ps

    goSplit :: Bool -> Set NID -> ABranches -> [Part] -> [BrRes]
    goSplit origin f bs rest = ordNub do
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
      case rest of
        [] ->
          if not anyFailed
            then case combineLandings subEnd (map snd landed) of
              Just ls -> pure (brRes subProgress subDones (Right ls))
              Nothing -> chainAll
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
                    let gaps = concat [either id (const []) r.outcome | r <- reaimed]
                        ds = concatMap (.dones) reaimed
                    pure $
                      BrRes
                        (subProgress || any (.progress) reaimed)
                        (subDones <> ds)
                        (Left gaps)
                        (Just (singletonSet v))
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
                    ( let ms = intersectAll (map snd landed)
                       in justIfTrue (not (null landed) && not (null ms)) ms
                    )
        (PMid m : rest')
          | Just n <- specificMidNid m ->
              if not anyFailed && not (null landed)
                then
                  map (addProg subProgress subDones) (afterUnit False [] (singletonSet n) rest)
                else do
                  st <- stationAt n m rest'
                  if not subProgress && not st.progress
                    then chainAll
                    else
                      pure $
                        BrRes
                          (subProgress || st.progress)
                          (subDones <> st.dones)
                          (Left (failed <> either id (const []) st.outcome))
                          (resReach st)
          | otherwise ->
              joinMidRecover origin f combo landed anyFailed subDones subProgress m rest' chainAll
        _ ->
          let failedReaches =
                [rs | (_, r) <- combo, isLeft r.outcome, Just rs <- [r.reach]]
           in goSplitImplicit origin f landed failed failedReaches subDones subProgress rest chainAll

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
      let meet = intersectAll (map snd landed) `difference` excOf m
       in if not (null landed) && not anyFailed && not (null meet)
            then map (addProg subProgress subDones) (stationFromArrivals meet m rest')
            else
              let cands =
                    [ v
                    | v <- toList (nidSet g),
                      anchorAdmits m.anchor v,
                      isJust (tailLandingFrom (PMid m : rest') v)
                    ]
                  perCand v = do
                    rs <-
                      combineAlts
                        [ resolveBranch g False origin f b (EndAt (singletonSet v))
                        | (b, _) <- combo
                        ]
                    st <- stationAt v m rest'
                    let gaps =
                          concat [either id (const []) r.outcome | r <- rs]
                            <> either id (const []) st.outcome
                        ds = concatMap (.dones) rs <> st.dones
                        prog = any (.progress) rs || st.progress
                    guard prog
                    pure (BrRes True ds (Left gaps) (resReach st))
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
      -- \| terminal positions gapped members' residuals land at: their
      -- pair-continuations must exist from there too
      [Set NID] ->
      [DeterministicPath Anchor] ->
      Bool ->
      [Part] ->
      [BrRes] ->
      [BrRes]
    goSplitImplicit origin f landed failed failedReaches subDones subProgress rest' chainAll = do
      let landingSets = ordNub (map snd landed <> failedReaches)
      contCombo <-
        combineAlts [map (ls,) (goParts False ls rest') | ls <- landingSets]
      let contRights = [(ls, f') | (ls, r) <- contCombo, Right f' <- [r.outcome]]
          contProgress = any ((.progress) . snd) contCombo
          contDones = concatMap ((.dones) . snd) contCombo
          contGaps = concat [either id (const []) r.outcome | (_, r) <- contCombo]
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
      if not subProgress && not contProgress
        then chainAll
        else case contRights of
          ((jls, _) : _) -> do
            -- a junction with a working continuation exists; failed
            -- branches end at it and branches that landed where the
            -- continuation fails are re-aimed at it
            let jn = minimumEx (toList jls)
                failingSets = [ls | (ls, r) <- contCombo, isLeft r.outcome]
            reaims <-
              combineAlts
                [ resolveBranch g False origin f b (EndAt (singletonSet jn))
                | (b, ls) <- landed,
                  ls `elem` failingSets
                ]
            let reaimGaps = concat [either id (const []) r.outcome | r <- reaims]
                reaimDones = concatMap (.dones) reaims
                failedGaps = map (retargetTo jn) failed
            if null failedGaps && null reaimGaps
              then case combineLandings endCtx (map snd contRights) of
                Just ls ->
                  pure $
                    brRes
                      (subProgress || contProgress)
                      (subDones <> contDones <> reaimDones)
                      (Right ls)
                Nothing -> chainAll
              else
                pure $
                  BrRes
                    (subProgress || contProgress)
                    (subDones <> contDones <> reaimDones)
                    (Left (failedGaps <> reaimGaps))
                    (combineLandings endCtx (map snd contRights))
          [] -> do
            -- no continuation resolved anywhere: pin failed branches to a
            -- landing if one exists, otherwise the continuation must be
            -- minted per pair
            let adjust gp
                  | isNothing gp.end = case landingSets of
                      (ls : _) -> retargetTo (minimumEx (toList ls)) gp
                      [] -> extendGap gp
                  | otherwise = gp
            pure $
              brRes
                (subProgress || contProgress)
                (subDones <> contDones)
                (Left (map adjust failed <> contGaps))

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
  Nothing -> [DpAlt False Nothing Nothing [] Nothing True]
  Just n
    | null p.loops ->
        if isJust (maybeLookupNode g n)
          then [DpAlt True (Just (singletonSet n)) Nothing [] (Just n) True]
          else [DpAlt False Nothing Nothing [] (Just n) True]
    | otherwise -> ordNub do
        la <- processLoops g n p.loops
        let pdp = PointlikeDeterministicPath (pinAnchor p.anchor n) la.attached
        if la.resolved
          then pure (DpAlt True (Just (singletonSet n)) Nothing [] (Just n) True)
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
      _
        | nested.target.anchor == Unanchored ->
            -- an Unanchored nested target is never pinned, and a partial
            -- residual would drop the resolved nested branches, whose
            -- exploded pair continuations then follow existing edges to a
            -- different terminal than the recreated ones; the only safe
            -- reuse is none: recreate the nested root wholesale, so
            -- instantiation's one-NID-per-occurrence makes every nested
            -- branch meet before the outer branches continue
            pure
              RootInfo
                { frontier = Nothing,
                  form = Rooted nested,
                  formMustEmit = True,
                  stationNid = Nothing,
                  attached = mempty,
                  loopGaps = [],
                  extraDones = [],
                  clean = False,
                  progress = sub.progress
                }
      _ ->
        let (form, formMustEmit, extraDones) = case (sub.main, sub.pin) of
              (Just mn, _) -> (mn, True, sub.extras)
              (Nothing, Just pn) -> (specificDp pn, False, sub.extras)
              (Nothing, Nothing) -> (Rooted nested, True, [])
         in pure
              RootInfo
                { frontier = singletonSet <$> sub.pin,
                  form,
                  formMustEmit,
                  stationNid = Nothing,
                  attached = mempty,
                  loopGaps = [],
                  extraDones,
                  clean = False,
                  progress = sub.progress
                }

branchOf ::
  Graph Text () -> EndCtx -> RootInfo -> DPBranch Anchor -> [BrRes]
branchOf g ctx info b = case info.frontier of
  Just f -> resolveBranch g True True f b ctx
  Nothing ->
    [ brRes False [] (Left [Gap (AtStart (singletonSet b)) e])
    | e <- gapEndAlts ctx
    ]

residualRootedAlts ::
  NID -> Graph Text () -> RootedDeterministicPath Anchor -> [DpAlt]
residualRootedAlts startNid g r = ordNub do
  let rootPairs = mapToList r.rootBranches
      targetCtx = targetEndCtx r.target.anchor
  infos <- combineAlts [resolveRootAlts startNid g rk | (rk, _) <- rootPairs]
  let entries = zip infos (map snd rootPairs)
  probe <-
    combineAlts
      [ combineAlts [map (b,) (branchOf g targetCtx info b) | b <- toList bs]
      | (info, bs) <- entries
      ]
  let landings =
        [ls | brs <- probe, (_, res) <- brs, Right ls <- [res.outcome]]
  (targetPin, results) <- case r.target.anchor of
    Specific t -> pure (Just t, probe)
    Unanchored -> pure (Nothing, probe)
    JoinPoint exc
      | null landings -> pure (Nothing, map (map demoteGapped) probe)
      | otherwise ->
          -- the join pins only on agreed landings; when resolved branches
          -- disagree (or all agreement is excluded) the join is
          -- unsatisfied: the landed branches are treated as unresolved and
          -- nothing pins (§5)
          let meet = intersectAll landings `difference` exc
           in if null meet
                then pure (Nothing, map (map (demoteGapped . demote)) probe)
                else do
                  p <- toList meet
                  rerun <-
                    combineAlts
                      [ combineAlts
                          [map (b,) (rerunOne p info b res) | (b, res) <- brs]
                      | ((info, _), brs) <- zip entries probe
                      ]
                  pure (Just p, rerun)
  finishRooted g r entries targetPin results
  where
    demote (b, res) = case res.outcome of
      Right _ ->
        (b, brRes False [] (Left [Gap (AtStart (singletonSet b)) Nothing]))
      Left _ -> (b, res)
    -- when the join stays unpinned, instantiation mints a fresh join node
    -- all residual branches must meet at; a partially-resolved branch
    -- residual that reuses its suffix (or resolved tail structure) lands
    -- at a concrete existing node instead, so the safe unpinned residual
    -- recreates every unresolved branch wholesale, running to the join.
    -- Suffix reuse returns as the reach-pinned minimality alternative
    -- (§5.2), where the pin makes it sound.
    demoteGapped (b, res) = case res.outcome of
      Right _ -> (b, res)
      Left _ ->
        (b, BrRes res.progress [] (Left [Gap (AtStart (singletonSet b)) Nothing]) res.reach)
    rerunOne p info b res = case res.outcome of
      Right _ -> [res]
      Left _ -> branchOf g (EndAt (singletonSet p)) info b

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
                (concat [either id (const []) res.outcome | (_, res) <- brs])
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
    testResiduals [edge' 3 "b" 4, edge' 1 "a" 2] "[a/|b]>@4" "[@2<b]>@4"
    testResiduals [edge' 3 "a" 2] "[~a]>@3" "[~a]>@3"
    testResiduals [edge' 4 "b" 3, edge' 1 "a" 2] "[a/|~b]>@4" "[@2<~b]>@4"
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
        testResiduals
          [edge' 1 "a" 2, edge' 1 "b" 3]
          "@[a/|~b]"
          "[@2<~b]>@1"
        testResiduals
          [edge' 1 "a" 2, edge' 1 "b" 2, edge' 2 "c" 3]
          "@[a/|~c/|~b]"
          "[@2<~c]>@2"
      describe "self-loop bias" do
        -- Forward can consume a then d (via the self-loop @2>"d">@2, a
        -- coincidental match of a non-loop step) with backward consuming
        -- ~b: gap [~c] (total 3). Backward can instead consume ~b then ~c
        -- with forward consuming only a: gap [d] (total 3). The tie is
        -- broken against the split that depends on the self-loop match,
        -- so the backward seam wins (see spec §4.1.1/§5.1)
        testResiduals
          [edge' 1 "a" 2, edge' 2 "d" 2, edge' 1 "b" 2, edge' 2 "c" 3]
          "@[a/|d/|~c/|~b]"
          "[@2<d]>@3"
        testResiduals
          [edge' 1 "a" 2, edge' 2 "d" 2, edge' 1 "b" 2, edge' 2 "c" 3]
          "[@[a/|d/|~c/|~b]<e]"
          "[@2<d]>@3 + [@1<e]"
        testResiduals
          [edge' 1 "a" 2, edge' 2 "d" 2, edge' 1 "b" 2, edge' 2 "c" 3]
          "[@[a/|(d/|~c/|~b & f)]<e]"
          "[@2<d]>@3 + [@2<f/@1|e]"
        testResiduals
          [edge' 1 "a" 2, edge' 2 "d" 2, edge' 1 "b" 2, edge' 2 "c" 3]
          "[@[a/|(d/|~c/|~b & f & g)]<e]"
          "[@2<d]>@3 + [@2<(f & g)/@1|e]"
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
      -- Both exist but reach different nodes; c gets split with residuals from both nodes
      testResiduals
        [edge' 1 "a" 2, edge' 1 "b" 3]
        "@[(a & b) /| c]"
        "[@2<c & @3<c]>@1"

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
          resolveNPath startNid (leastConstrainedNormalizedPath path)
            & runInMemoryGraphMetadataEditing
            & evalState graph
            & run
            & \(resolved :: ResolvedPath) -> not (null resolved.path.union)

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
            ==> residual
            === pure (NormalizedPath mempty)

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
