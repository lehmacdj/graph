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
--
-- TODO(stage 3): the implementation still computes the pre-review
-- single-path behavior, wrapped as a singleton alternative; the tests
-- already encode the target behavior. See
-- docs/pathResidualsUnresolvedBy-implementation-notes.md for the worklist.
pathResidualsUnresolvedBy ::
  NID ->
  Graph Text () ->
  NormalizedPath Anchor ->
  NonEmpty (NormalizedPath Anchor)
pathResidualsUnresolvedBy startNid graph (NormalizedPath dps) =
  pure . NormalizedPath . setFromList $
    concatMap (residualOfDp startNid graph) (toList dps)

residualOfDp ::
  NID ->
  Graph Text () ->
  DeterministicPath Anchor ->
  [DeterministicPath Anchor]
residualOfDp startNid g dp = case dp of
  Pointlike p -> case unifyRootAnchor startNid p.anchor of
    Nothing -> [dp]
    Just n
      | null p.loops ->
          if isJust (maybeLookupNode g n) then [] else [dp]
      | otherwise ->
          let lr = processLoops g DeconstructZeroLoops n p.loops
           in if lr.resolved
                then []
                else
                  if not lr.progress
                    then [dp]
                    else
                      let (groups, alones) = groupLoopFrags lr.frags
                       in [mkRootedDp rm (specificPdp e) | (e, rm) <- groups]
                            <> [ mkRootedDp fr.roots (specificPdp fr.end)
                               | fr <- alones
                               ]
                            <> lr.dones
  Rooted r ->
    let res = residualRooted startNid g r
     in if isJust res.landed
          then []
          else
            if not res.progress
              then [dp]
              else maybeToList res.main <> res.dones

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

anchorCtx :: Anchor -> EndCtx
anchorCtx = \case
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
  = -- | unconstrained landing
    EndOpen
  | -- | land anywhere outside the exclusion set (unpinned join point)
    EndJoin (Set NID)
  | -- | land exactly at one of these
    EndAt (Set NID)

endCtxNid :: EndCtx -> Maybe NID
endCtxNid = \case
  EndAt s -> Just (minimumEx (toList s))
  _ -> Nothing

data SegResult
  = -- | frontiers met; the meeting NIDs
    SegResolved (Set NID)
  | -- | fwd consumed, fwd frontier, gap, bwd consumed, bwd frontier
    SegGap Int (Set NID) [DPDirection] Int (Set NID)

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
  Graph Text () ->
  (Graph Text () -> NID -> DPDirection -> Set NID) ->
  Set NID ->
  [DPDirection] ->
  [Set NID]
frontierTrail g step = go
  where
    go f = \case
      [] -> []
      (d : ds) ->
        let f' = unions [step g n d | n <- toList f]
         in if null f' then [] else f' : go f' ds

-- | Bidirectionally match one linear run of directed steps between a forward
-- frontier and the end context.
--
-- When @openBwd@ is set (the run ends the branch), an open/join end still
-- gets a backward pass starting from every node of the graph: matching the
-- run's suffix anywhere recovers a concrete seam node for the residual.
resolveSegment ::
  Graph Text () -> Bool -> Set NID -> [DPDirection] -> EndCtx -> SegResult
resolveSegment g openBwd start steps end
  | kAll == length steps,
    Just met <- meets fwdFinal =
      SegResolved met
  | otherwise =
      -- consuming every step but missing the backward frontier is not
      -- progress; the run failed as a whole
      let kMax = if kAll == length steps then 0 else kAll
          greedy = gapAt kMax
          greedyJ = case fst greedy of
            SegGap _ _ _ j _ -> j
            SegResolved _ -> 0
       in if kMax > 0
            && trailEff start (take kMax fwds) == 0
            && greedyJ == 0
            && snd (gapAt 0) > 0
            then -- every forward match was frontier-preserving (self-loop
            -- coincidences) and incompatible with the backward pass,
            -- which does make real progress: trust the backward seam
              fst (gapAt 0)
            else fst greedy
  where
    fwds = frontierTrail g stepForward start steps
    kAll = length fwds
    fwdFinal = fromMaybe start (lastMay fwds)
    -- how many steps of a frontier trail actually moved the frontier
    trailEff prev0 trail =
      length (filter id (zipWith (/=) trail (prev0 : trail)))
    meets f = case end of
      EndOpen -> Just f
      EndJoin exc ->
        let ok = f `difference` exc
         in justIfTrue (not (null ok)) ok
      EndAt s ->
        let ok = f `intersection` s
         in justIfTrue (not (null ok)) ok
    -- the gap when only the first @k@ steps are consumed forward, paired
    -- with the backward pass's effective progress
    gapAt k =
      let fF = if k == 0 then start else fwds `indexEx` (k - 1)
          remaining = drop k steps
          bwdStart = case end of
            EndAt s -> Just s
            EndJoin exc -> justIfTrue openBwd (nidSet g `difference` exc)
            EndOpen -> justIfTrue openBwd (nidSet g)
       in case bwdStart of
            Nothing -> (SegGap k fF remaining 0 mempty, 0 :: Int)
            Just s ->
              let bwds = frontierTrail g stepBackward s (reverse remaining)
                  -- a backward pass that consumes every remaining step can
                  -- never meet the forward frontier (forward matching would
                  -- already have met), so at least one step must remain to
                  -- bridge the frontiers
                  j = min (length remaining - 1) (length bwds)
                  bFinal = if j == 0 then mempty else bwds `indexEx` (j - 1)
               in ( SegGap k fF (take (length remaining - j) remaining) j bFinal,
                    trailEff s (take j bwds)
                  )

-- * Residual building blocks

type ABranches = OSet (DPBranch Anchor)

type ARootsMap = OMap (DeterministicPath Anchor) ABranches

-- | Where a residual gap resumes from.
data GapRoots
  = -- | nothing was consumed before the gap: it begins at the position the
    -- branch started from, and the enclosing structure decides how to
    -- anchor it (pinned root form, midpoint station, ...)
    AtStart ABranches
  | -- | concrete resume points with the unresolved branches from each
    AtRoots ARootsMap

data Gap = Gap
  { roots :: GapRoots,
    -- | Nothing = the gap runs to the (possibly unpinned) end position
    end :: Maybe NID,
    -- | alternatives of a single multi-NID backward gap must not be merged
    -- with other gaps into one deterministic path
    alone :: Bool
  }

data BrRes = BrRes
  { progress :: Bool,
    dones :: [DeterministicPath Anchor],
    outcome :: Either [Gap] (Set NID),
    -- | even when the outcome is gapped, the branch's terminal position may
    -- have been reached by its fully-resolved suffix; join-point targets
    -- can pin there
    reach :: Maybe (Set NID)
  }

-- | A 'BrRes' with no terminal reach information.
brRes :: Bool -> [DeterministicPath Anchor] -> Either [Gap] (Set NID) -> BrRes
brRes p ds o = BrRes p ds o Nothing

-- | The terminal position a branch result got to, resolved or not.
resReach :: BrRes -> Maybe (Set NID)
resReach r = either (const r.reach) Just r.outcome

gapRootsMap :: Gap -> ARootsMap
gapRootsMap gp = case gp.roots of
  AtRoots rm -> rm
  AtStart _ -> error "gapRootsMap: unanchored gap escaped its position"

-- | Rough size of the unresolved structure, for ranking pin candidates.
branchWeight :: DPBranch Anchor -> Int
branchWeight = \case
  DPSingle _ -> 1
  DPSequence b1 m b2 ->
    branchesWeight b1 + branchesWeight m.loops + branchesWeight b2

branchesWeight :: ABranches -> Int
branchesWeight = sum . map branchWeight . toList

gapWeight :: Gap -> Int
gapWeight gp = case gp.roots of
  AtStart bs -> branchesWeight bs
  AtRoots rm -> sum (map branchesWeight (toList rm))

loopsWeight :: LoopsRes -> Int
loopsWeight lr =
  branchesWeight lr.attached
    + sum [sum (map branchesWeight (toList fr.roots)) | fr <- lr.frags]

isAtStartGap :: Gap -> Bool
isAtStartGap gp = case gp.roots of
  AtStart _ -> True
  AtRoots _ -> False

nidsRoots :: Set NID -> ABranches -> ARootsMap
nidsRoots f bs = unionsWith (<>) [singletonMap (specificDp n) bs | n <- toList f]

mkRootedDp ::
  ARootsMap -> PointlikeDeterministicPath Anchor -> DeterministicPath Anchor
mkRootedDp rm t = Rooted (smartBuildRootedDeterministicPath rm t)

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

-- * Loops

data LoopStyle = AttachZeroLoops | DeconstructZeroLoops
  deriving stock (Eq)

data LoopFrag = LoopFrag
  { roots :: ARootsMap,
    end :: NID,
    alone :: Bool
  }

data LoopsRes = LoopsRes
  { progress :: Bool,
    resolved :: Bool,
    attached :: ABranches,
    frags :: [LoopFrag],
    dones :: [DeterministicPath Anchor]
  }

-- | Resolve the loops of a pointlike at a concrete node. Unresolved loops
-- with no consumption either stay attached to the pointlike (midpoint
-- positions) or are deconstructed into rooted fragments (root positions);
-- partially consumed loops always deconstruct.
processLoops :: Graph Text () -> LoopStyle -> NID -> ABranches -> LoopsRes
processLoops g style n loops = foldl' combine (LoopsRes False True mempty [] []) (toList loops)
  where
    combine acc b =
      let br =
            resolveBranch g True (Just n) (singletonSet n) b (EndAt (singletonSet n))
       in case br.outcome of
            Right _ ->
              LoopsRes True acc.resolved acc.attached acc.frags acc.dones
            Left gaps
              | not br.progress,
                AttachZeroLoops <- style ->
                  LoopsRes
                    acc.progress
                    False
                    (acc.attached <> singletonSet b)
                    acc.frags
                    acc.dones
              | otherwise ->
                  LoopsRes
                    (acc.progress || br.progress)
                    False
                    acc.attached
                    (acc.frags <> map (gapFrag n) gaps)
                    (acc.dones <> br.dones)

gapFrag :: NID -> Gap -> LoopFrag
gapFrag n gp =
  LoopFrag
    ( case gp.roots of
        AtStart bs -> singletonMap (specificDp n) bs
        AtRoots rm -> rm
    )
    (fromMaybe n gp.end)
    gp.alone

-- | Group mergeable fragments by the NID they end at (preserving encounter
-- order); alternatives marked alone stay separate.
groupLoopFrags :: [LoopFrag] -> ([(NID, ARootsMap)], [LoopFrag])
groupLoopFrags fs =
  let (alones, mergeables) = partition (.alone) fs
      grouped =
        foldl'
          (\acc fr -> upsertAssoc (\a b -> unionsWith (<>) [a, b]) fr.end fr.roots acc)
          []
          mergeables
   in (grouped, alones)

-- * Layer 1: structural recursion over a branch

-- | Resolve one branch from a frontier toward an end context, computing the
-- gaps that remain.
resolveBranch ::
  Graph Text () ->
  -- | origin: the frontier is exactly the branch's start position, so gaps
  -- with nothing consumed are reported 'AtStart'
  Bool ->
  -- | the start position as a concrete pinnable node, used as the target of
  -- backward-expressed gaps (Nothing for never-pinned Unanchored roots)
  Maybe NID ->
  Set NID ->
  DPBranch Anchor ->
  EndCtx ->
  BrRes
resolveBranch g origin0 pin0 start branch endCtx =
  goParts origin0 pin0 start (flattenBranch branch)
  where
    finalEnd = endCtxNid endCtx

    boundaryCtx :: [Part] -> EndCtx
    boundaryCtx = \case
      [] -> endCtx
      (PMid m : _) -> anchorCtx m.anchor
      _ -> EndOpen

    goParts :: Bool -> Maybe NID -> Set NID -> [Part] -> BrRes
    goParts origin pin f = \case
      [] -> brRes False [] (Right f)
      (PRun ds : rest) ->
        case resolveSegment g (null rest) f ds (boundaryCtx rest) of
          SegResolved f' -> afterUnit pin True [] f' rest
          SegGap k fFwd mid j bFwd -> afterGap origin pin k fFwd mid j bFwd rest
      (PSplit bs : rest) -> goSplit origin pin f bs rest
      (PMid _ : _) -> error "goParts: branch begins with a midpoint"

    -- continue after a fully-resolved leading unit
    afterUnit ::
      Maybe NID -> Bool -> [DeterministicPath Anchor] -> Set NID -> [Part] -> BrRes
    afterUnit pin prog ds f' = \case
      [] -> brRes prog ds (Right f')
      (PMid m : rest') ->
        let st = stationFromArrivals f' m rest'
         in BrRes (prog || st.progress) (ds <> st.dones) st.outcome st.reach
      rest ->
        let r = goParts False pin f' rest
         in BrRes (prog || r.progress) (ds <> r.dones) r.outcome r.reach

    afterGap ::
      Bool ->
      Maybe NID ->
      Int ->
      Set NID ->
      [DPDirection] ->
      Int ->
      Set NID ->
      [Part] ->
      BrRes
    afterGap origin pin k fFwd mid j bFwd rest =
      let consumed = k > 0 || j > 0
          mkRoots bs
            | k == 0 && origin = AtStart bs
            | otherwise = AtRoots (nidsRoots fFwd bs)
          mkGaps mid' j' bFwd' fallbackEnd
            | k == 0,
              j' > 0,
              origin,
              Just p <- pin =
                -- backward-expressed: inverted steps from the backward
                -- frontier targeting the start position (§6)
                [ Gap
                    ( AtRoots
                        ( singletonMap
                            (specificDp b)
                            (singletonSet (runToBranch (invertedRun mid')))
                        )
                    )
                    (Just p)
                    (length bFwd' > 1)
                | b <- toList bFwd'
                ]
            | j' > 0 =
                [ Gap (mkRoots (singletonSet (runToBranch mid'))) (Just b) (length bFwd' > 1)
                | b <- toList bFwd'
                ]
            | otherwise = [Gap (mkRoots (singletonSet (runToBranch mid'))) fallbackEnd False]
          segEnd = boundaryCtx rest
          gapsHere = mkGaps mid j bFwd (endCtxNid segEnd)
          chainGap parts = Gap (mkRoots (rebuildParts parts)) finalEnd False
          -- the boundary's NID is unknown: look for positions the whole
          -- remaining tail resolves from to recover a concrete seam,
          -- otherwise keep one connected structural gap
          recoverOrChain parts =
            let cands = tailCandidates parts
             in if null cands
                  then brRes consumed [] (Left [chainGap (PRun mid : parts)])
                  else case resolveSegment g True fFwd mid (EndAt cands) of
                    SegResolved met -> afterUnit pin True [] met parts
                    SegGap _ _ mid2 j2 b2 ->
                      BrRes
                        True
                        []
                        (Left (mkGaps mid2 j2 b2 (Just (minimumEx (toList cands)))))
                        (tailLandingFrom parts (minimumEx (toList cands)))
       in case rest of
            [] -> brRes consumed [] (Left gapsHere)
            (PMid m : rest')
              | Just n <- specificMidNid m ->
                  let st = stationAt n m rest'
                   in if j == 0 && not st.progress
                        then
                          -- keep one connected gap through the midpoint
                          brRes consumed [] (Left [chainGap (PRun mid : PMid m : rest')])
                        else
                          BrRes
                            (consumed || st.progress)
                            st.dones
                            (Left (gapsHere <> either id (const []) st.outcome))
                            (resReach st)
              | otherwise -> recoverOrChain (PMid m : rest')
            _ -> recoverOrChain rest

    tailCandidates :: [Part] -> Set NID
    tailCandidates parts =
      setFromList [v | v <- toList (nidSet g), tailResolvesFrom parts v]

    tailResolvesFrom :: [Part] -> NID -> Bool
    tailResolvesFrom parts v = isJust (tailLandingFrom parts v)

    -- \| where the tail lands when fully resolved from @v@
    tailLandingFrom :: [Part] -> NID -> Maybe (Set NID)
    tailLandingFrom parts v = case parts of
      (PMid m : rest')
        | anchorAdmits m.anchor v ->
            either (const Nothing) Just (stationAt v m rest').outcome
        | otherwise -> Nothing
      _ ->
        either
          (const Nothing)
          Just
          (goParts False Nothing (singletonSet v) parts).outcome

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
    stationAt :: NID -> PointlikeDeterministicPath Anchor -> [Part] -> BrRes
    stationAt n m rest =
      let lr = processLoops g AttachZeroLoops n m.loops
          cr = goParts True (Just n) (singletonSet n) rest
       in case (lr.resolved, cr.outcome) of
            (True, Right f') ->
              brRes (lr.progress || cr.progress) (lr.dones <> cr.dones) (Right f')
            _ ->
              let contGaps = either id (const []) cr.outcome
                  (atStartGaps, otherGaps) = partition isAtStartGap contGaps
                  (groups, alones) = groupLoopFrags lr.frags
                  (connectRM, plainGroups)
                    | not (null atStartGaps),
                      Just rm <- lookup n groups =
                        (Just rm, filter ((/= n) . fst) groups)
                    | otherwise = (Nothing, groups)
                  stationAnchor = pinAnchor m.anchor n
                  -- gaps resuming at this midpoint root at the station:
                  -- partially-consumed loops become its (deconstructed)
                  -- root, zero-consumption loops stay attached
                  stationKey = case connectRM of
                    Just rm ->
                      mkRootedDp rm (PointlikeDeterministicPath (Specific n) lr.attached)
                    Nothing ->
                      Pointlike (PointlikeDeterministicPath stationAnchor lr.attached)
                  rooted =
                    [ Gap (AtRoots (singletonMap stationKey bs)) e a
                    | Gap (AtStart bs) e a <- atStartGaps
                    ]
                  fragGaps =
                    [Gap (AtRoots rm) (Just e) False | (e, rm) <- plainGroups]
                      <> [Gap (AtRoots fr.roots) (Just fr.end) True | fr <- alones]
                  attachedDone =
                    [ Pointlike (PointlikeDeterministicPath stationAnchor lr.attached)
                    | null atStartGaps,
                      not (null lr.attached)
                    ]
               in BrRes
                    (lr.progress || cr.progress)
                    (lr.dones <> cr.dones <> attachedDone)
                    (Left (otherGaps <> rooted <> fragGaps))
                    (resReach cr)

    stationFromArrivals ::
      Set NID -> PointlikeDeterministicPath Anchor -> [Part] -> BrRes
    stationFromArrivals arrivals m rest =
      let results = [stationAt n m rest | n <- toList arrivals]
          resolveds = [(r, f') | r <- results, Right f' <- [r.outcome]]
       in case resolveds of
            (_ : _) ->
              brRes
                (any ((.progress) . fst) resolveds)
                (concatMap ((.dones) . fst) resolveds)
                (Right (unions (map snd resolveds)))
            [] -> case filter (.progress) results of
              (r : _) -> r
              [] -> case results of
                (r : _) -> r
                [] -> error "stationFromArrivals: empty arrival frontier"

    goSplit :: Bool -> Maybe NID -> Set NID -> ABranches -> [Part] -> BrRes
    goSplit origin pin f bs rest =
      let subEnd = boundaryCtx rest
          subs = [(b, resolveBranch g origin pin f b subEnd) | b <- toList bs]
          landed = [(b, ls) | (b, r) <- subs, Right ls <- [r.outcome]]
          failed = [gs | (_, r) <- subs, Left gs <- [r.outcome]]
          subDones = concatMap ((.dones) . snd) subs
          subProgress = any ((.progress) . snd) subs
       in case rest of
            [] ->
              if null failed
                then case combineLandings subEnd (map snd landed) of
                  Just ls -> brRes subProgress subDones (Right ls)
                  Nothing -> chainAll
                else
                  BrRes
                    subProgress
                    subDones
                    (Left (concat failed))
                    ( justIfTrue
                        (not (null landed) && not (null (intersectAll (map snd landed))))
                        (intersectAll (map snd landed))
                    )
            (PMid m : rest')
              | Just n <- specificMidNid m ->
                  if null failed && not (null landed)
                    then afterUnit pin subProgress subDones (singletonSet n) rest
                    else
                      let st = stationAt n m rest'
                       in if not subProgress && not st.progress
                            then chainAll
                            else
                              BrRes
                                (subProgress || st.progress)
                                (subDones <> st.dones)
                                (Left (concat failed <> either id (const []) st.outcome))
                                (resReach st)
              | otherwise ->
                  -- an explicit join midpoint: every branch of the
                  -- intersection must agree on it
                  let meet = intersectAll (map snd landed) `difference` excOf m
                   in if not (null landed) && not (null meet) && null failed
                        then afterUnit pin subProgress subDones meet rest
                        else joinMidSearch m rest'
            _ -> goSplitImplicit pin subProgress subDones landed failed rest
      where
        -- search for the node that unifies a join midpoint best: every
        -- branch of the intersection and the continuation must work from
        -- one node, so try each admissible node and keep the one where
        -- the most pieces resolve
        joinMidSearch m rest' =
          let evalAt v =
                let subRes =
                      [ resolveBranch g origin pin f b (EndAt (singletonSet v))
                      | b <- toList bs
                      ]
                    st = stationAt v m rest'
                    score =
                      length [() | r <- subRes, isRight r.outcome]
                        + (if isRight st.outcome then 1 else 0)
                    gaps =
                      concat [either id (const []) r.outcome | r <- subRes]
                        <> either id (const []) st.outcome
                    ds = concatMap (.dones) subRes <> st.dones
                    prog = any (.progress) subRes || st.progress
                 in (score, v, prog, gaps, ds)
              ranked =
                sortOn (\(score, v, _, _, _) -> (negate score, v)) $
                  [ evalAt v
                  | v <- toList (nidSet g),
                    anchorAdmits m.anchor v
                  ]
           in case ranked of
                ((score, _, prog, gaps, ds) : _)
                  | score > 0 && prog ->
                      brRes True ds (Left gaps)
                _ -> chainAll

        mkRootsSplit bs'
          | origin = AtStart bs'
          | otherwise = AtRoots (nidsRoots f bs')

        chainAll =
          brRes False [] . Left $
            [Gap (mkRootsSplit (rebuildParts (PSplit bs : rest))) finalEnd False]

        -- continuation after an implicit (unanchored, exploded per pair)
        -- midpoint: each landing continues independently
        goSplitImplicit pin' subProgress subDones landed failed rest' =
          let landingSets = ordNub (map snd landed)
              conts = [(ls, goParts False pin' ls rest') | ls <- landingSets]
              contRights = [(ls, f') | (ls, r) <- conts, Right f' <- [r.outcome]]
              contProgress = any ((.progress) . snd) conts
              contDones = concatMap ((.dones) . snd) conts
              contGaps = concat [either id (const []) r.outcome | (_, r) <- conts]
              extendGap gp =
                let extendBs bs' =
                      singletonSet $
                        smartBuildSequence bs' unanchored (rebuildParts rest')
                 in Gap
                      ( case gp.roots of
                          AtStart bs' -> AtStart (extendBs bs')
                          AtRoots rm -> AtRoots (map extendBs rm)
                      )
                      finalEnd
                      gp.alone
              retargetTo jn gp
                | isNothing gp.end = Gap gp.roots (Just jn) gp.alone
                | otherwise = gp
           in if not subProgress && not contProgress
                then chainAll
                else case contRights of
                  ((jls, _) : _) ->
                    -- a junction with a working continuation exists; failed
                    -- branches end at it and branches that landed where the
                    -- continuation fails are re-aimed at it
                    let jn = minimumEx (toList jls)
                        failingSets = [ls | (ls, r) <- conts, isLeft r.outcome]
                        reaims =
                          [ resolveBranch g origin pin' f b (EndAt (singletonSet jn))
                          | (b, ls) <- landed,
                            ls `elem` failingSets
                          ]
                        reaimGaps =
                          concat [either id (const []) r.outcome | r <- reaims]
                        reaimDones = concatMap (.dones) reaims
                        failedGaps = map (retargetTo jn) (concat failed)
                     in if null failed && null reaimGaps
                          then case combineLandings endCtx (map snd contRights) of
                            Just ls ->
                              brRes
                                (subProgress || contProgress)
                                (subDones <> contDones <> reaimDones)
                                (Right ls)
                            Nothing -> chainAll
                          else
                            BrRes
                              (subProgress || contProgress)
                              (subDones <> contDones <> reaimDones)
                              (Left (failedGaps <> reaimGaps))
                              (combineLandings endCtx (map snd contRights))
                  [] ->
                    -- no continuation resolved anywhere: pin failed branches
                    -- to a landing if one exists, otherwise the continuation
                    -- must be minted per pair
                    let adjust gp
                          | isNothing gp.end = case landingSets of
                              (ls : _) -> retargetTo (minimumEx (toList ls)) gp
                              [] -> extendGap gp
                          | otherwise = gp
                        failedGaps = map adjust (concat failed)
                     in brRes
                          (subProgress || contProgress)
                          (subDones <> contDones)
                          (Left (failedGaps <> contGaps))

-- * Layer 1: structural recursion over deterministic paths

data RootInfo = RootInfo
  { -- | the root anchor can never unify (a JoinPoint excluding the start
    -- NID): creating edges cannot help, so its branches are dropped
    unreachable :: Bool,
    -- | where branches from this root start (Nothing = unknown)
    frontier :: Maybe (Set NID),
    -- | the start position as a pinnable node, for backward-expressed gaps
    pin :: Maybe NID,
    -- | root form for gaps that resume at the root
    form :: DeterministicPath Anchor,
    -- | the root pointlike's node, for connecting its loop fragments
    stationNid :: Maybe NID,
    frags :: [LoopFrag],
    extraDones :: [DeterministicPath Anchor],
    -- | emit 'form' standalone if no gap roots at it (nested residuals)
    formMustEmit :: Bool,
    clean :: Bool,
    progress :: Bool
  }

resolveRoot :: NID -> Graph Text () -> DeterministicPath Anchor -> RootInfo
resolveRoot startNid g = \case
  Pointlike p -> case unifyRootAnchor startNid p.anchor of
    Nothing ->
      RootInfo True Nothing Nothing (Pointlike p) Nothing [] [] False False False
    Just n ->
      let lr = processLoops g DeconstructZeroLoops n p.loops
          pin = case p.anchor of
            Unanchored -> Nothing
            _ -> Just n
       in RootInfo
            False
            (Just (singletonSet n))
            pin
            (Pointlike (PointlikeDeterministicPath (pinAnchor p.anchor n) mempty))
            (Just n)
            lr.frags
            lr.dones
            False
            lr.resolved
            lr.progress
  Rooted nested ->
    let -- a nested root's unanchored target must unify like a join point:
        -- the §3 carve-out only applies to the top-level target, so the
        -- residual has to make the nested root's branches meet (and may
        -- pin that meet as a concrete resume point)
        nested' = case nested.target.anchor of
          Unanchored ->
            RootedDeterministicPath
              nested.rootBranches
              (PointlikeDeterministicPath (JoinPoint mempty) nested.target.loops)
          _ -> nested
        sub = residualRooted startNid g nested'
     in case sub.landed of
          Just ls ->
            let pin = sub.pin <|> Just (minimumEx (toList ls))
             in RootInfo
                  False
                  (Just ls)
                  pin
                  (maybe (Rooted nested) specificDp pin)
                  Nothing
                  []
                  []
                  False
                  True
                  sub.progress
          Nothing ->
            RootInfo
              False
              (singletonSet <$> sub.pin)
              sub.pin
              (maybe (fromMaybe (Rooted nested) sub.main) specificDp sub.pin)
              Nothing
              []
              (sub.dones <> (if isJust sub.pin then maybeToList sub.main else []))
              (isNothing sub.pin && isJust sub.main)
              False
              sub.progress

data DpRes = DpRes
  { progress :: Bool,
    landed :: Maybe (Set NID),
    main :: Maybe (DeterministicPath Anchor),
    dones :: [DeterministicPath Anchor],
    -- | the concrete node the target resolved/pinned to, if any
    pin :: Maybe NID
  }

residualRooted ::
  NID -> Graph Text () -> RootedDeterministicPath Anchor -> DpRes
residualRooted startNid g r
  | resolvedAll = DpRes True (Just finalLanding) Nothing [] targetPin
  | otherwise = DpRes totalProgress Nothing mainDp allStandalones targetPin
  where
    targetCtx = anchorCtx r.target.anchor

    rootEntries :: [(RootInfo, ABranches)]
    rootEntries =
      [ (resolveRoot startNid g rootDp, bs)
      | (rootDp, bs) <- mapToList r.rootBranches
      ]

    branchOf :: RootInfo -> DPBranch Anchor -> EndCtx -> BrRes
    branchOf info b ctx = case info.frontier of
      Just f -> resolveBranch g True info.pin f b ctx
      Nothing
        | info.unreachable -> brRes False [] (Left [])
        | otherwise ->
            brRes False [] (Left [Gap (AtStart (singletonSet b)) (endCtxNid ctx) False])

    results0 :: [(RootInfo, [(DPBranch Anchor, BrRes)])]
    results0 =
      [ (info, [(b, branchOf info b targetCtx) | b <- toList bs])
      | (info, bs) <- rootEntries
      ]

    landings0 :: [Set NID]
    landings0 =
      [ls | (_, brs) <- results0, (_, res) <- brs, Right ls <- [res.outcome]]

    -- terminal positions reached by branches whose suffix resolved even
    -- though they are gapped; a join target can pin there too
    reaches0 :: [Set NID]
    reaches0 =
      [ ls
      | (_, brs) <- results0,
        (_, res) <- brs,
        Left _ <- [res.outcome],
        Just ls <- [res.reach]
      ]

    joinMeet :: Maybe (Set NID)
    joinMeet = case r.target.anchor of
      JoinPoint _
        | not (null landings0) ->
            let m = intersectAll landings0
             in justIfTrue (not (null m)) m
        | not (null reaches0) ->
            let m = intersectAll reaches0
             in justIfTrue (not (null m)) m
      _ -> Nothing

    targetPin :: Maybe NID
    targetPin = case r.target.anchor of
      Specific n -> Just n
      JoinPoint _ -> case joinMeet of
        Just s -> Just (chooseTargetNid s)
        -- nothing landed: search the graph for the node that unifies the
        -- join best (smallest residual when every branch is aimed at it)
        Nothing
          | null landings0 -> searchPin
          | otherwise -> Nothing
      Unanchored -> Nothing
      where
        loopsOkAt n =
          (processLoops g AttachZeroLoops n r.target.loops).resolved
        -- prefer a meet node whose target loops resolve
        chooseTargetNid s =
          let cands = toList s
           in case filter loopsOkAt cands of
                (n : _) -> n
                [] -> minimumEx cands

    searchPin :: Maybe NID
    searchPin = case r.target.anchor of
      JoinPoint exc ->
        let cands = [v | v <- toList (nidSet g), v `notMember` exc]
            costAt v =
              let brs =
                    [ branchOf info b (EndAt (singletonSet v))
                    | (info, bs) <- rootEntries,
                      b <- toList bs
                    ]
                  lt = processLoops g AttachZeroLoops v r.target.loops
                  gapsW =
                    sum
                      [ gapWeight gp
                      | res <- brs,
                        gp <- either id (const []) res.outcome
                      ]
               in (gapsW + loopsWeight lt, v)
         in case sortOn id (map costAt cands) of
              ((_, v) : _) -> Just v
              [] -> Nothing
      _ -> Nothing

    results :: [(RootInfo, [(DPBranch Anchor, BrRes)])]
    results = case r.target.anchor of
      JoinPoint _
        | not (null landings0) -> case (joinMeet, targetPin) of
            -- resolved branches disagree: the join is unsatisfied, so they
            -- are treated as unresolved and nothing pins
            (Nothing, _) ->
              [(info, map demote brs) | (info, brs) <- results0]
            -- the pinned join becomes a concrete backward frontier for the
            -- still-unresolved branches
            (Just _, Just p) ->
              [ (info, [(b, rerun info p b res) | (b, res) <- brs])
              | (info, brs) <- results0
              ]
            _ -> results0
        | Just p <- targetPin ->
            -- nothing landed, but reaches or target loops pinned the join
            [ (info, [(b, rerun info p b res) | (b, res) <- brs])
            | (info, brs) <- results0
            ]
      _ -> results0
      where
        demote (b, res) = case res.outcome of
          Right _ ->
            (b, brRes False [] (Left [Gap (AtStart (singletonSet b)) Nothing False]))
          Left _ -> (b, res)
        rerun info p b res = case res.outcome of
          Right _ -> res
          Left _ -> branchOf info b (EndAt (singletonSet p))

    allLanded :: Bool
    allLanded =
      all (isRight . (.outcome) . snd) (concatMap snd results)

    rootsClean :: Bool
    rootsClean = all ((.clean) . fst) rootEntries

    -- target loops are evaluated as soon as the target node is known
    lrT :: Maybe LoopsRes
    lrT = targetPin <&> \t -> processLoops g AttachZeroLoops t r.target.loops

    targetResolved :: Bool
    targetResolved = maybe (null r.target.loops) (.resolved) lrT

    resolvedAll :: Bool
    resolvedAll = allLanded && rootsClean && targetResolved && not (null landings0)

    baseLanding :: Set NID
    baseLanding = case r.target.anchor of
      Specific n -> singletonSet n
      JoinPoint _ -> fromMaybe mempty joinMeet
      Unanchored ->
        let m = intersectAll landings0
         in if null m then unions landings0 else m

    finalLanding :: Set NID
    finalLanding
      | null r.target.loops = baseLanding
      | otherwise = maybe baseLanding singletonSet targetPin

    attachedTarget :: ABranches
    attachedTarget = maybe r.target.loops (.attached) lrT

    mainTargetPdp :: PointlikeDeterministicPath Anchor
    mainTargetPdp =
      PointlikeDeterministicPath
        ( case r.target.anchor of
            Specific n -> Specific n
            JoinPoint exc -> maybe (JoinPoint exc) Specific targetPin
            Unanchored -> Unanchored
        )
        attachedTarget

    rootStationGaps ::
      RootInfo ->
      [(DPBranch Anchor, BrRes)] ->
      ([Gap], [DeterministicPath Anchor])
    rootStationGaps info brs =
      let gaps = concat [either id (const []) res.outcome | (_, res) <- brs]
          ds = concat [res.dones | (_, res) <- brs]
          (atStartGaps, others) = partition isAtStartGap gaps
          (groups, alones) = groupLoopFrags info.frags
          (connectRM, plainGroups)
            | not (null atStartGaps),
              Just n <- info.stationNid,
              Just rm <- lookup n groups =
                (Just (n, rm), filter ((/= n) . fst) groups)
            | otherwise = (Nothing, groups)
          rootKey = case connectRM of
            Just (n, rm) -> mkRootedDp rm (specificPdp n)
            Nothing -> info.form
          rooted =
            [ Gap (AtRoots (singletonMap rootKey bs)) e a
            | Gap (AtStart bs) e a <- atStartGaps
            ]
          fragGaps =
            [Gap (AtRoots rm) (Just e) False | (e, rm) <- plainGroups]
              <> [Gap (AtRoots fr.roots) (Just fr.end) True | fr <- alones]
          danglingForm = [info.form | info.formMustEmit, null atStartGaps]
       in (others <> rooted <> fragGaps, ds <> info.extraDones <> danglingForm)

    perRoot :: [([Gap], [DeterministicPath Anchor])]
    perRoot = [rootStationGaps info brs | (info, brs) <- results]

    targetFragGaps :: [Gap]
    targetFragGaps = case lrT of
      Nothing -> []
      Just lt ->
        let (groups, alones) = groupLoopFrags lt.frags
         in [Gap (AtRoots rm) (Just e) False | (e, rm) <- groups]
              <> [Gap (AtRoots fr.roots) (Just fr.end) True | fr <- alones]

    allGaps :: [Gap]
    allGaps = concatMap fst perRoot <> targetFragGaps

    (mainGaps, offGaps) =
      partition (\gp -> not gp.alone && gp.end == targetPin) allGaps

    mainRootsMap :: ARootsMap
    mainRootsMap = unionsWith (<>) (map gapRootsMap mainGaps)

    mainDp :: Maybe (DeterministicPath Anchor)
    mainDp =
      justIfTrue (not (null mainRootsMap)) (mkRootedDp mainRootsMap mainTargetPdp)

    -- when everything landed but the target's own loops did not resolve,
    -- the attached loops still need a home
    attachedTargetDone :: [DeterministicPath Anchor]
    attachedTargetDone =
      [ Pointlike mainTargetPdp
      | isNothing mainDp,
        not (null attachedTarget),
        maybe False (not . (.resolved)) lrT
      ]

    standaloneOf :: Gap -> DeterministicPath Anchor
    standaloneOf gp =
      mkRootedDp (gapRootsMap gp) (maybe mainTargetPdp specificPdp gp.end)

    offStandalones :: [DeterministicPath Anchor]
    offStandalones =
      let (alones, mergeables) = partition (.alone) offGaps
          grouped =
            foldl'
              ( \acc gp ->
                  upsertAssoc (\a b -> unionsWith (<>) [a, b]) gp.end (gapRootsMap gp) acc
              )
              []
              mergeables
       in [mkRootedDp rm (maybe mainTargetPdp specificPdp e) | (e, rm) <- grouped]
            <> map standaloneOf alones

    allStandalones :: [DeterministicPath Anchor]
    allStandalones =
      offStandalones
        <> concatMap snd perRoot
        <> maybe [] (.dones) lrT
        <> attachedTargetDone

    totalProgress :: Bool
    totalProgress =
      any ((.progress) . fst) rootEntries
        || any
          (.progress)
          [res | (_, brs) <- results, (_, res) <- brs]
        || maybe False (.progress) lrT

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
