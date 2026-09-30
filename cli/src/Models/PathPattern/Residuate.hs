-- | Residuation: what to add to the graph so that a path resolves
-- (docs/path-pattern-ir.md §5).
--
-- A residual is the pattern with a total binding: every vertex is an
-- existing node or a fresh one. The patch (nodes and edges to create) is
-- derived from it. Candidates are enumerated by the evidence rule, fresh
-- vertices are identified where that makes two pattern edges one patch edge,
-- and dominated or redundant candidates are dropped.
module Models.PathPattern.Residuate
  ( Binding (..),
    Patch' (..),
    Patch,
    Residual' (..),
    Residual,
    PatchLabel (..),
    Residuation (..),
    ResidualError (..),
    NodeLookup,
    LabelChooser,
    residuate,
    Choice (..),
    chooseRepair,
    residuatePure,
    noLabels,
    patchSize,
    patchSubsumes,
    patchOrphans,
    renderResidual,
    renderResidualWith,
    renderPatchLabel,
    renderMatcher,
    renderBinding,
  )
where

import Data.List.NonEmpty (nonEmpty)
import Data.Map.Strict qualified as Map
import Models.Connect
import Models.Edge
import Models.NID
import Models.Node
import Models.PathPattern
import Models.PathPattern.Match (Lookup, stepEdges)
import MyPrelude

-- | What a vertex becomes: an existing node, or the n-th created node.
data Binding = Existing NID | Fresh Int
  deriving stock (Eq, Ord, Show, Generic)
  deriving anyclass (NFData)

instance CompactNodeShow Binding where
  type Augmentation Binding = Void
  minimumNidLength settings = \case
    Existing n -> minimumNidLength settings n
    Fresh _ -> 1
  nshowSettings settings = \case
    Existing n -> nshowSettings settings n
    Fresh k -> "new" <> tshow (k + 1)

-- | Nodes and edges to create, with labels of type @l@. Created nodes are
-- fresh nodes and fixed nodes that do not exist yet.
data Patch' l = Patch
  { nodes :: Set Binding,
    edges :: Set (Edge' Binding l)
  }
  deriving stock (Eq, Ord, Show, Generic)

type Patch = Patch' Text

data Residual' l = Residual
  { patternPath :: PatternPath,
    sigma :: Map PVertex Binding,
    labels :: Map EdgeId l,
    patch :: Patch' l,
    -- | no binding relies on right evidence and no fresh vertices were
    -- identified from the right
    leftOnly :: Bool
  }
  deriving stock (Eq, Show, Generic)

type Residual = Residual' Text

-- | The label of an edge to create during selection. 'Missing' stands in
-- for a label the chooser did not give, so that selection runs as though
-- it had given one: equal for every edge with the same matcher (a chooser
-- that returns one fresh label per matcher) and equal to no real label.
data PatchLabel = Known Text | Missing Matcher
  deriving stock (Eq, Ord, Show, Generic)

data Residuation
  = -- | some alternative already resolves; nothing needs to change
    AlreadyResolves
  | -- | survivors, preferred first
    Repairs (NonEmpty Residual)
  | -- | no alternative can be made to resolve
    Unsatisfiable
  deriving stock (Eq, Show, Generic)

data ResidualError
  = -- | a surviving repair must create an edge with this matcher, but no
    -- label was given. The step is at this route and hop (from 0) of its
    -- alternative. The survivors are listed preferred first; each one
    -- that needs a label shows it as 'Missing'.
    UnsupportedLabel Matcher Int Int (NonEmpty (Residual' PatchLabel))
  deriving stock (Eq, Show, Generic)

-- | What a construction command does with a residuation.
data Choice
  = NoChange
  | Apply Residual
  | -- | at least two survivors; nothing is changed
    Ambiguous (NonEmpty Residual)
  | Impossible
  deriving stock (Eq, Show, Generic)

chooseRepair :: Residuation -> Choice
chooseRepair = \case
  AlreadyResolves -> NoChange
  Unsatisfiable -> Impossible
  Repairs (r :| []) -> Apply r
  Repairs rs -> Ambiguous rs

type NodeLookup m = NID -> m (Maybe (Node Text ()))

-- | Supplies a label for an edge that must be created but is not a literal.
type LabelChooser m = Matcher -> m (Maybe Text)

noLabels :: (Applicative m) => LabelChooser m
noLabels = const (pure Nothing)

residuatePure ::
  Lookup ->
  (Matcher -> Maybe Text) ->
  NID ->
  PathQuery ->
  Either ResidualError Residuation
residuatePure lookupNode chooser cur q =
  runIdentity (residuate (pure . lookupNode) (pure . chooser) cur q)

-- | One candidate before selection.
data Candidate = Candidate
  { residual :: Residual' PatchLabel,
    -- | variables bound to existing nodes
    bound :: Map VarId NID,
    leftJustified :: Set VarId,
    alternative :: Int,
    -- | steps (route, hop) whose edge must be created but got no label
    -- from the chooser, in route order; such an edge is labelled 'Missing'
    missingLabels :: [((Int, Int), Matcher)]
  }

residuate ::
  forall m.
  (Monad m) =>
  NodeLookup m ->
  LabelChooser m ->
  NID ->
  PathQuery ->
  m (Either ResidualError Residuation)
residuate lookupNode chooser cur q = do
  results <-
    traverse
      (uncurry (candidatesFor lookupNode chooser cur))
      (zip [0 ..] (toList q.alternatives))
  let candidates = concat results
  pure
    if any (isEmptyPatch . (.residual.patch)) candidates
      then Right AlreadyResolves
      else case nonEmpty (select candidates) of
        Nothing -> Right Unsatisfiable
        Just survivors ->
          case [miss | s <- toList survivors, miss : _ <- [s.missingLabels]] of
            -- a survivor needs a label: report it rather than fall back to
            -- the other survivors or to eliminated candidates
            ((ri, hi), m) : _ ->
              Left (UnsupportedLabel m ri hi (fmap (.residual) survivors))
            [] -> Right (Repairs (fmap (knownResidual . (.residual)) survivors))
  where
    isEmptyPatch patch = null patch.nodes && null patch.edges
    known = \case
      Known l -> l
      Missing _ -> error "residuate: a survivor without missing labels has one"
    knownResidual r =
      r
        { labels = map known r.labels,
          patch =
            Patch
              { nodes = r.patch.nodes,
                edges =
                  setFromList
                    [ Edge e.source (known e.transition) e.sink
                    | e <- toList r.patch.edges
                    ]
              }
        }

-- | Drop dominated and redundant candidates and order the survivors.
select :: [Candidate] -> [Candidate]
select candidates =
  dedupe . sortOn key . minimal $ undominated
  where
    undominated = filter (not . dominated) candidates
    dominated c = any (dominates c) candidates
    -- c' extends c with only left-justified extra bindings
    dominates c c' =
      c'.alternative == c.alternative
        && Map.isSubmapOf c.bound c'.bound
        && length c'.bound > length c.bound
        && all
          (`member` c'.leftJustified)
          (keys (Map.difference c'.bound c.bound))
    minimal cs =
      [ c
      | c <- cs,
        not $
          any
            ( \c' ->
                patchSubsumes c'.residual.patch c.residual.patch
                  && not (patchSubsumes c.residual.patch c'.residual.patch)
            )
            cs
      ]
    -- keep the first of patches equal modulo renaming
    dedupe = reverse . foldl' keep []
      where
        keep kept r
          | any (samePatch r) kept = kept
          | otherwise = r : kept
        samePatch a b =
          patchSubsumes a.residual.patch b.residual.patch
            && patchSubsumes b.residual.patch a.residual.patch
    key c =
      let r = c.residual
       in ( not r.leftOnly,
            patchSize r.patch,
            renderResidualWith renderPatchLabel r
          )

patchSize :: (Ord l) => Patch' l -> Int
patchSize p = length p.nodes + length p.edges

-- | Whether the first patch is contained in the second, modulo an injective
-- renaming of fresh nodes. Existing nodes are never renamed.
patchSubsumes :: (Ord l) => Patch' l -> Patch' l -> Bool
patchSubsumes small large = go (freshOf small) mempty
  where
    go :: [Int] -> Map Int Int -> Bool
    freshOf p = [k | Fresh k <- toList p.nodes]
    largeFresh = freshOf large
    go [] renaming =
      let rename = \case
            Fresh k -> Fresh (findWithDefault k k renaming)
            b -> b
          renameEdge e = Edge (rename e.source) e.transition (rename e.sink)
       in all ((`member` large.nodes) . rename) small.nodes
            && all ((`member` large.edges) . renameEdge) small.edges
    go (k : ks) renaming =
      any
        (\k' -> go ks (insertMap k k' renaming))
        [k' | k' <- largeFresh, k' `notElem` toList renaming]

-- | Weakly connected groups of created nodes that touch no existing node.
patchOrphans :: (Ord l) => Patch' l -> [Set Binding]
patchOrphans p =
  filter (all (`member` p.nodes)) (components (toList p.nodes <> endpoints))
  where
    endpoints = concat [[e.source, e.sink] | e <- toList p.edges]
    neighbours b =
      [e.sink | e <- toList p.edges, e.source == b]
        <> [e.source | e <- toList p.edges, e.sink == b]
    components bs = go (ordNub bs) mempty
      where
        go [] _ = []
        go (b : rest) seen
          | b `member` seen = go rest seen
          | otherwise =
              let c = reach [b] (singletonSet b)
               in c : go rest (seen <> c)
        reach [] acc = acc
        reach (x : xs) acc =
          let new = filter (`notMember` acc) (neighbours x)
           in reach (new <> xs) (acc <> setFromList new)

-- * Candidates

candidatesFor ::
  forall m.
  (Monad m) =>
  NodeLookup m ->
  LabelChooser m ->
  NID ->
  Int ->
  PatternPath ->
  m [Candidate]
candidatesFor lookupNode chooser cur altIndex p
  | not boundTestsOk = pure []
  | not (null unreachable) =
      -- only %unanchored could produce these; it is not supported yet
      error $ "residuate: variables without a bound vertex: " <> show unreachable
  | otherwise = do
      sigmas <- closure [mempty] (singletonSet mempty)
      concat <$> traverse candidatesForSigma sigmas
  where
    specs = patternEdges p
    order = edgeOrder p
    vars = patternVars p
    position e =
      fromMaybe (0, 0) $
        headMay
          [ (ri, hi)
          | (ri, rt) <- zip [0 ..] (toList p.routes),
            (hi, h) <- zip [0 ..] rt.hops,
            h.edge == e
          ]
    -- variables connected to no bound vertex through pattern edges
    unreachable = filter (`notMember` reached) vars
      where
        bound :: Set PVertex
        bound = setFromList (filter (isJust . boundValue) (patternVertices p))
        reached :: Set VarId
        reached = setFromList [v | Var v <- toList (grow bound)]
        grow :: Set PVertex -> Set PVertex
        grow acc =
          let new =
                setFromList
                  [ other
                  | spec <- toList specs,
                    (here, other) <- [(spec.source, spec.target), (spec.target, spec.source)],
                    here `member` acc,
                    other `notMember` acc
                  ]
           in if null new then acc else grow (acc <> new)
    boundValue = \case
      Current -> Just cur
      Fixed n -> Just n
      Var _ -> Nothing
    boundTestsOk =
      and
        [ testAccepts (vertexTest p v) n
        | v <- patternVertices p,
          Just n <- [boundValue v]
        ]
    valueIn :: Map VarId NID -> PVertex -> Maybe NID
    valueIn sigma = \case
      Var v -> lookup v sigma
      v -> boundValue v

    stepsFrom :: NID -> EdgeSpec -> m [(Text, NID)]
    stepsFrom x spec = do
      node <- lookupNode x
      pure (stepEdges (const node) x spec)

    -- nodes y such that the edge can be taken from y to x
    stepsInto :: NID -> EdgeSpec -> m [(Text, NID)]
    stepsInto x spec = do
      node <- lookupNode x
      pure case node of
        Nothing -> []
        Just n ->
          [ (c.transition, c.node)
          | c <- toList case spec.direction of
              Out -> n.incoming
              In -> n.outgoing,
            matcherAccepts spec.matcher c.transition
          ]

    -- All evidence-backed bindings, closed under binding one more variable.
    closure :: [Map VarId NID] -> Set (Map VarId NID) -> m [Map VarId NID]
    closure [] seen = pure (toList seen)
    closure (sigma : rest) seen = do
      next <- concat <$> traverse (extendWith sigma) [v | v <- vars, v `notMember` sigma]
      let new = ordNub (filter (`notMember` seen) next)
      closure (new <> rest) (seen <> setFromList new)

    extendWith :: Map VarId NID -> VarId -> m [Map VarId NID]
    extendWith sigma v = do
      found <- fmap concat . for (mapToList specs) $ \(_, spec) ->
        case (spec.source, spec.target) of
          (s, Var t)
            | t == v,
              s /= Var v,
              Just x <- valueIn sigma s ->
                map snd <$> stepsFrom x spec
          (Var s, t)
            | s == v,
              t /= Var v,
              Just x <- valueIn sigma t ->
                map snd <$> stepsInto x spec
          _ -> pure []
      pure
        [ insertMap v n sigma
        | n <- ordNub found,
          testAccepts (vertexTest p (Var v)) n
        ]

    -- which edges exist between bound endpoints, and with which label
    existingLabel :: Map VarId NID -> EdgeSpec -> m (Maybe Text)
    existingLabel sigma spec = case (valueIn sigma spec.source, valueIn sigma spec.target) of
      (Just x, Just y) -> do
        steps <- stepsFrom x spec
        pure (headMay [l | (l, t) <- steps, t == y])
      _ -> pure Nothing

    leftClosure :: Map EdgeId (Maybe Text) -> Map VarId NID -> Set VarId
    leftClosure existing sigma = go mempty
      where
        go acc =
          let justified v = case v of
                Var x -> x `member` acc
                _ -> True
              new =
                setFromList
                  [ t
                  | (e, spec) <- mapToList specs,
                    Var t <- [spec.target],
                    t `member` sigma,
                    t `notMember` acc,
                    justified spec.source,
                    isJust (join (lookup e existing))
                  ]
           in if null new then acc else go (acc <> new)

    candidatesForSigma :: Map VarId NID -> m [Candidate]
    candidatesForSigma sigma = do
      existing <-
        mapFromList
          <$> traverse (\(e, spec) -> (e,) <$> existingLabel sigma spec) (mapToList specs)
      missingNodes <-
        setFromList . catMaybes
          <$> traverse
            ( \n ->
                lookupNode n <&> \case
                  Nothing -> Just n
                  Just _ -> Nothing
            )
            ( ordNub
                ( [n | v <- patternVertices p, Just n <- [boundValue v]]
                    <> toList sigma
                )
            )
      -- labels for edges that must be created; a missing one is a
      -- placeholder, recorded so that a surviving candidate reports it
      created <-
        for (mapToList specs) \(e, spec) -> case join (lookup e existing) of
          Just l -> pure (e, Known l)
          Nothing -> case spec.matcher of
            MLiteral l -> pure (e, Known l)
            m ->
              chooser m <&> \case
                Just l | matcherAccepts m l -> (e, Known l)
                _ -> (e, Missing m)
      let labels = mapFromList created
          missing = sortOn fst [(position e, m) | (e, Missing m) <- created]
          unbound = [v | v <- vars, v `notMember` sigma]
          initial = mapFromList (zip unbound [0 ..]) :: Map VarId Int
          binding classes = \case
            Var v -> case lookup v sigma of
              Just n -> Existing n
              Nothing -> Fresh (findWithDefault 0 v classes)
            v -> Existing (fromJustEx (boundValue v))
          leftVars = leftClosure existing sigma
          -- number created nodes by first appearance
          compact classes =
            let order' = ordNub [k | v <- unbound, Just k <- [lookup v classes]]
                renumber = mapFromList (zip order' [0 ..]) :: Map Int Int
             in fmap (\k -> findWithDefault k k renumber) classes
          states = mergeStates specs labels binding initial
      pure
        [ Candidate
            { residual =
                Residual
                  { patternPath = p,
                    sigma =
                      mapFromList
                        [(v, binding classes v) | v <- patternVertices p],
                    labels,
                    patch = patchOf existing labels (binding classes) missingNodes,
                    leftOnly =
                      not usedRight && all (`member` leftVars) (keys sigma)
                  },
              bound = sigma,
              leftJustified = leftVars,
              alternative = altIndex,
              missingLabels = missing
            }
        | (classes, usedRight) <- map (first compact) states
        ]

    patchOf existing labels bind missingNodes =
      Patch
        { nodes =
            setFromList
              [b | v <- patternVertices p, b@(Fresh _) <- [bind v]]
              <> mapSet Existing missingNodes,
          edges =
            setFromList
              [ storedEdge spec (bind spec.source) l (bind spec.target)
              | e <- order,
                Just spec <- [lookup e specs],
                isNothing (join (lookup e existing)),
                Just l <- [lookup e labels]
              ]
        }

storedEdge :: EdgeSpec -> Binding -> l -> Binding -> Edge' Binding l
storedEdge spec s l t = case spec.direction of
  Out -> Edge s l t
  In -> Edge t l s

-- | Identify fresh vertices. Identifications from the left (the same hop
-- from the same vertex) are always made; identifications from the right
-- (the same hop into the same vertex) are alternatives. Returns every
-- resulting class assignment and whether it used a right identification.
mergeStates ::
  (Eq l) =>
  Map EdgeId EdgeSpec ->
  Map EdgeId l ->
  (Map VarId Int -> PVertex -> Binding) ->
  Map VarId Int ->
  [(Map VarId Int, Bool)]
mergeStates specs labels binding initial = go [(leftFix initial, False)] mempty
  where
    go :: [(Map VarId Int, Bool)] -> Set (Map VarId Int) -> [(Map VarId Int, Bool)]
    specList = mapToList specs
    renameClass old new = map (\c -> if c == old then new else c)
    -- Each hop, in stored orientation, as (which stored end is shared,
    -- that end, the other end, whether the hop is taken from that end).
    ends spec = case spec.direction of
      Out -> [(True, spec.source, spec.target, True), (False, spec.target, spec.source, False)]
      In -> [(True, spec.target, spec.source, False), (False, spec.source, spec.target, True)]
    -- Pairs of fresh classes that identifying would make one patch edge: the
    -- same label and stored orientation from the same end. Such a pair is
    -- from the left when both hops are taken from the shared end.
    justified leftSide classes =
      ordNub
        [ (min a b, max a b)
        | (e1, s1) <- specList,
          (e2, s2) <- specList,
          e1 < e2,
          lookup e1 labels == lookup e2 labels,
          isJust (lookup e1 labels),
          (end1, near1, far1, taken1) <- ends s1,
          (end2, near2, far2, taken2) <- ends s2,
          end1 == end2,
          (taken1 && taken2) == leftSide,
          binding classes near1 == binding classes near2,
          Fresh a <- [binding classes far1],
          Fresh b <- [binding classes far2],
          a /= b
        ]
    leftFix classes = case justified True classes of
      (a, b) : _ -> leftFix (renameClass b a classes)
      [] -> classes
    go [] _ = []
    go ((classes, usedRight) : rest) seen
      | classes `member` seen = go rest seen
      | otherwise =
          let next =
                [ (leftFix (renameClass b a classes), True)
                | (a, b) <- justified False classes
                ]
           in (classes, usedRight) : go (rest <> next) (insertSet classes seen)

-- * Rendering

renderBinding :: Binding -> Text
renderBinding = \case
  Existing n -> tshow n
  Fresh k -> "new" <> tshow (k + 1)

-- | The residual's routes with every vertex resolved, and its patch.
renderResidual :: Residual -> Text
renderResidual = renderResidualWith tshow

-- | A known label quoted; a missing one as its matcher, e.g. @-*->@.
renderPatchLabel :: PatchLabel -> Text
renderPatchLabel = \case
  Known l -> tshow l
  Missing m -> renderMatcher m

renderMatcher :: Matcher -> Text
renderMatcher = \case
  MLiteral t -> tshow t
  MWild -> "*"
  MRegex r -> tshow r

renderResidualWith :: (Ord l) => (l -> Text) -> Residual' l -> Text
renderResidualWith renderLabel r =
  intercalate " & " (map renderRoute (toList r.patternPath.routes))
    <> "\n    creates: "
    <> case toList r.patch.edges of
      [] -> intercalate ", " (map renderBinding (toList r.patch.nodes))
      es -> intercalate ", " (map renderEdge es)
  where
    value v = maybe "?" renderBinding (lookup v r.sigma)
    renderRoute rt =
      value rt.root <> concatMap renderHop rt.hops
    renderHop h =
      let l = maybe "\"?\"" renderLabel (lookup h.edge r.labels)
       in case h.direction of
            Out -> " -" <> l <> "-> " <> value h.target
            In -> " <-" <> l <> "- " <> value h.target
    renderEdge e =
      renderBinding e.source
        <> " -"
        <> renderLabel e.transition
        <> "-> "
        <> renderBinding e.sink
