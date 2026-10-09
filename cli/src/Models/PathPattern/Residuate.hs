-- | Residuation: what to add to the graph so that a path resolves
-- (docs/path-route-ir.md, Residuation).
--
-- A residual is the pattern with a total binding: every vertex is an
-- existing node or a fresh one. The patch (nodes and edges to create) is
-- derived from it. Candidates are enumerated by the evidence rule, fresh
-- vertices are identified where that makes two pattern edges one patch edge,
-- and dominated or redundant candidates are dropped. Enumeration stops with
-- 'TooManyRepairs' once it has made more candidates than the path's limit.
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
    residuateWithin,
    defaultMaxRepairs,
    pathMaxRepairs,
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

    -- * For tests
    Candidate (..),
    candidatesOf,
    candidatesWithin,
    select,
    selectReference,
    sharedPairs,
    sharedPairsReference,
  )
where

import Control.Monad.State.Strict qualified as S
import Control.Monad.Trans.Maybe (MaybeT (..))
import Data.Bits (complement, setBit, (.&.))
import Data.List.NonEmpty (nonEmpty)
import Data.Map.Lazy qualified as LazyMap
import Data.Map.Strict qualified as Map
import Data.Set qualified as Set
import Models.Connect
import Models.Edge
import Models.NID
import Models.Node
import Models.Path.Simple qualified as P
import Models.PathPattern
import Models.PathPattern.Match
  ( Lookup,
    Partial (..),
    Resolution (..),
    loadPattern,
    matchPatternWithin,
    stepEdges,
  )
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
  | -- | the path does not resolve, and has more candidate repairs than
    -- this limit, so they were not all made, let alone compared
    TooManyRepairs Int
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
    -- | variables bound to existing nodes, including pins
    bound :: Map VarId NID,
    -- | variables whose fresh vertex was identified with an existing node
    -- (not evidence; never left-justified)
    pins :: Map VarId NID,
    leftJustified :: Set VarId,
    alternative :: Int,
    -- | steps (route, hop) whose edge must be created but got no label
    -- from the chooser, in route order; such an edge is labelled 'Missing'
    missingLabels :: [((Int, Int), Matcher)]
  }
  deriving stock (Eq, Show, Generic)

-- | How many candidates residuating a path may make, unless an
-- @%override({max-repairs = …}, …)@ says otherwise. This bounds a count,
-- not time: what a candidate costs grows with the pattern
-- (docs/path-route-ir.md, Residuation, Limit on candidates, has measurements).
defaultMaxRepairs :: Int
defaultMaxRepairs = 5000

-- | The largest limit in effect at a step or node of the path: the one of
-- the innermost @max-repairs@ override around it, or the default. The
-- candidates are those of the whole path and cannot be attributed to a
-- part of it, so the most generous limit asked for applies to all of it.
pathMaxRepairs :: P.Path -> P.RepairsLimit
pathMaxRepairs = go (P.AtMostRepairs defaultMaxRepairs)
  where
    go l = \case
      P.WithLimits _ l' p -> go (fromMaybe l l'.maxRepairs) p
      P.Union _ p q -> go l p `max` go l q
      p P.:/ q -> go l p `max` go l q
      p P.:& q -> go l p `max` go l q
      P.Backwards p -> go l p
      _ -> l

-- | 'residuateWithin' the default limit.
residuate ::
  (Monad m) =>
  NodeLookup m ->
  LabelChooser m ->
  NID ->
  PathQuery ->
  m (Either ResidualError Residuation)
residuate = residuateWithin (P.AtMostRepairs defaultMaxRepairs)

-- | Residuate, making at most the given number of candidates. A candidate
-- is a binding with one identification of its fresh vertices, in any
-- alternative, whether or not selection keeps it. Beyond the limit the
-- result is 'TooManyRepairs', unless the path resolves, which is checked
-- by matching then: 'AlreadyResolves' does not depend on the limit.
residuateWithin ::
  forall m.
  (Monad m) =>
  P.RepairsLimit ->
  NodeLookup m ->
  LabelChooser m ->
  NID ->
  PathQuery ->
  m (Either ResidualError Residuation)
residuateWithin limit lookupNode chooser cur q = case limit of
  P.UnlimitedRepairs -> choose <$> candidatesOf lookupNode chooser cur q
  P.AtMostRepairs n ->
    candidatesWithin (Just n) lookupNode chooser cur q >>= \case
      Just candidates -> pure (choose candidates)
      -- The candidate with an empty patch may not have been made yet, so
      -- whether the path resolves is found by matching instead.
      Nothing -> do
        resolves <-
          anyM (alternativeResolves lookupNode cur) (toList q.alternatives)
        pure
          if resolves
            then Right AlreadyResolves
            else Left (TooManyRepairs n)
  where
    choose candidates =
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

anyM :: (Monad m) => (a -> m Bool) -> [a] -> m Bool
anyM f =
  foldr (\x rest -> f x >>= \ok -> if ok then pure True else rest) (pure False)

allM :: (Monad m) => (a -> m Bool) -> [a] -> m Bool
allM f = fmap not . anyM (fmap not . f)

-- | Whether an alternative resolves as it is: it matches, and every node
-- of the match exists. This is when some candidate has an empty patch,
-- since a candidate's binding is total and creates nothing exactly when it
-- is a match, and the evidence rule reaches every match.
alternativeResolves ::
  (Monad m) => NodeLookup m -> NID -> PatternPath -> m Bool
alternativeResolves lookupNode cur p =
  loadPattern lookupNode cur p >>= \case
    Nothing -> pure False
    Just (within, nodes) ->
      case matchPatternWithin within (`lookup` nodes) (Just cur) p of
        Nothing -> pure False
        Just r ->
          allM (anyM (allM exists . toList . (.nodes)) . snd) r.components
  where
    exists n = isJust <$> lookupNode n

-- | A candidate with what selection compares of it.
data Scored = Scored
  { candidate :: Candidate,
    size :: Int,
    shape :: ~(PatchShape PatchLabel),
    -- | the order of the survivors
    key :: ~(Bool, Int, Text)
  }

-- | What every renaming of a patch's fresh nodes has in common, to tell
-- cheaply that one patch is not contained in, or not equal to, another.
data PatchShape l = PatchShape
  { -- | the created nodes that are not fresh
    fixedNodes :: Set Binding,
    -- | how many edges there are of each label between each two ends, a
    -- fresh end being 'Nothing'
    edgeKinds :: Map (Maybe NID, l, Maybe NID) Int,
    -- | the edges at each fresh node (whether it is their source, their
    -- label and their other end), in order
    freshKinds :: [[(Bool, l, Maybe NID)]]
  }
  deriving stock (Eq, Ord, Show, Generic)

patchShape :: forall l. (Ord l) => Patch' l -> PatchShape l
patchShape p =
  PatchShape
    { fixedNodes = Set.filter (isNothing . fresh) p.nodes,
      edgeKinds =
        Map.fromListWith
          (+)
          [ ((existing e.source, e.transition, existing e.sink), 1)
          | e <- toList p.edges
          ],
      freshKinds =
        sort . toList $
          map sort $
            Map.unionWith (<>) (mapFromList [(k, []) | k <- nodes]) edgesAt
    }
  where
    fresh = \case
      Fresh k -> Just k
      Existing _ -> Nothing
    existing = \case
      Existing n -> Just n
      Fresh _ -> Nothing
    nodes = mapMaybe fresh (toList p.nodes)
    edgesAt :: Map Int [(Bool, l, Maybe NID)]
    edgesAt =
      Map.fromListWith
        (<>)
        [ (k, [(isSource, e.transition, existing other)])
        | e <- toList p.edges,
          (isSource, Fresh k, other) <-
            [(True, e.source, e.sink), (False, e.sink, e.source)]
        ]

-- | Something a patch has if a patch it contains has it: a kind of edge, or
-- two edges at one fresh node.
data PatchFeature l
  = EdgeKind (Maybe NID, l, Maybe NID)
  | EdgesAtNode (Bool, l, Maybe NID) (Bool, l, Maybe NID)
  deriving stock (Eq, Ord, Show, Generic)

patchFeatures :: (Ord l) => PatchShape l -> Set (PatchFeature l)
patchFeatures shape =
  Set.map EdgeKind (Map.keysSet shape.edgeKinds)
    <> setFromList
      [ EdgesAtNode e1 e2
      | edges <- shape.freshKinds,
        e1 : later <- tails edges,
        e2 <- later
      ]

-- | Whether the first patch can be contained in the second
-- ('patchSubsumes'): a renaming keeps the nodes that are not fresh, and
-- the label and such ends of every edge.
shapeWithin :: (Ord l) => PatchShape l -> PatchShape l -> Bool
shapeWithin small large =
  small.fixedNodes `Set.isSubsetOf` large.fixedNodes
    && Map.isSubmapOfBy (<=) small.edgeKinds large.edgeKinds

-- | Drop dominated and redundant candidates and order the survivors
-- (docs/path-route-ir.md, Residuation, Selection). 'selectReference' states the
-- rule; this gives the same survivors in the same order, without comparing
-- every pair of candidates:
--
-- * Dominance. @c'@ dominates @c@ iff they are of one alternative and
--   @c@'s binding is that of @c'@ without one or more variables that are
--   left-justified in @c'@. The bindings dominated by leaving out one
--   variable are put in a set; for more variables, the candidates are
--   grouped by the variables they bind.
-- * Minimality. A patch contained in another modulo renaming, and not the
--   other way round, has fewer nodes or edges: were they as many, the
--   renaming would be onto, and its inverse would contain the other. So a
--   patch is compared with the strictly smaller ones only, and
--   containment is one search, not two. The search is left out where a
--   'PatchFeature' of the smaller patch is missing from the larger.
-- * Deduplication. Patches equal modulo renaming have one 'PatchShape',
--   and one contains the other; so a patch is compared with the kept ones
--   of its shape only, by one search. A shape is not a canonical form:
--   patches of one shape may differ, and the search tells.
select :: [Candidate] -> [Candidate]
select candidates =
  map (.candidate) . sortOn (.key) . dedupe . sortOn fewerPins . minimal $
    map scored (filter (not . dominated) candidates)
  where
    -- Most dominated candidates are dominated by one that binds a single
    -- variable more, which is a lookup. For the rest, the candidates that
    -- bind two or more variables more are gone through, by the variables
    -- they bind: those of each set of variables at once.
    dominated c =
      (c.alternative, c.bound) `member` dominatedByOneMore
        || any
          (c.bound `member`)
          ( LazyMap.findWithDefault
              []
              (c.alternative, Map.keysSet c.bound)
              dominatedByMore
          )
    -- each alternative, binding and its left-justified variables
    dominators :: [(Int, Map VarId NID, Set VarId)]
    dominators =
      ordNub
        [ ( c.alternative,
            c.bound,
            c.leftJustified `intersection` Map.keysSet c.bound
          )
        | c <- candidates
        ]
    dominatedByOneMore :: Set (Int, Map VarId NID)
    dominatedByOneMore =
      setFromList
        [ (alternative, deleteMap v larger)
        | (alternative, larger, removable) <- dominators,
          v <- toList removable
        ]
    byVars :: Map (Int, Set VarId) [(Map VarId NID, Set VarId)]
    byVars =
      Map.fromListWith
        (<>)
        [ ((alternative, Map.keysSet larger), [(larger, removable)])
        | (alternative, larger, removable) <- dominators
        ]
    -- For an alternative and a set of variables: the bindings of just
    -- those variables that are dominated by a candidate binding two or
    -- more variables more, for each set of variables such candidates bind.
    dominatedByMore :: LazyMap.Map (Int, Set VarId) [Set (Map VarId NID)]
    dominatedByMore =
      LazyMap.mapWithKey
        ( \(alternative, vars) _ ->
            [ setFromList
                [ Map.restrictKeys larger vars
                | (larger, removable) <- largers,
                  more `Set.isSubsetOf` removable
                ]
            | ((alternative', vars'), largers) <- mapToList byVars,
              alternative' == alternative,
              vars `Set.isProperSubsetOf` vars',
              let more = vars' `difference` vars,
              Set.size more >= 2
            ]
        )
        byVars
    scored c =
      let r = c.residual
          size = patchSize r.patch
       in Scored
            { candidate = c,
              size,
              shape = patchShape r.patch,
              key =
                ( not r.leftOnly,
                  size,
                  renderResidualWith renderPatchLabel r
                )
            }
    patchOf s = s.candidate.residual.patch
    -- Identifying a fresh vertex with an existing node usually shrinks the
    -- patch to a subset, but a repair without that identification is
    -- always offered: a candidate is only dropped for one with no pin it
    -- lacks. So a candidate is compared with the strictly smaller patches
    -- of candidates whose pins are among its own. Most are told apart by
    -- their features, as bits; the rest are searched once for each two
    -- patches, whichever candidates have them.
    minimal cs = filter (not . redundant) cs
      where
        featureBit :: Map (PatchFeature PatchLabel) Int
        featureBit =
          mapFromList . flip zip [0 ..] . toList $
            foldMap (patchFeatures . (.shape)) cs
        -- the features of each patch: a patch has those of every patch it
        -- contains
        featuresOf :: Map (Patch' PatchLabel) Integer
        featuresOf =
          mapFromList
            [ ( patchOf s,
                foldl' setBit 0 . mapMaybe (`lookup` featureBit) . toList $
                  patchFeatures s.shape
              )
            | s <- cs
            ]
        lacksNone small large = small .&. complement large == (0 :: Integer)
        -- the distinct patches of the candidates with each pins, by size
        byPins ::
          Map (Map VarId NID) (Map Int (Map (Patch' PatchLabel) Integer))
        byPins =
          Map.fromListWith
            (Map.unionWith (<>))
            [ ( s.candidate.pins,
                singletonMap
                  s.size
                  ( singletonMap
                      (patchOf s)
                      (findWithDefault 0 (patchOf s) featuresOf)
                  )
              )
            | s <- cs
            ]
        -- the patches of candidates whose pins are among the given ones:
        -- by looking up each part of the pins or, if those are more,
        -- by going through the pins there are
        pinnedWithin ::
          Map VarId NID -> [Map Int (Map (Patch' PatchLabel) Integer)]
        pinnedWithin pins
          | length pins < 20 && (2 :: Int) ^ length pins <= length byPins =
              [ patches
              | kept <- subsequences (keys pins),
                Just patches <-
                  [lookup (Map.restrictKeys pins (setFromList kept)) byPins]
              ]
          | otherwise =
              [ patches
              | (pins', patches) <- mapToList byPins,
                pins' `Map.isSubmapOf` pins
              ]
        redundant s =
          or
            [ maybe False (LazyMap.findWithDefault False small) $
                lookup size contained
            | patches <- pinnedWithin s.candidate.pins,
              (size, smaller) <-
                mapToList (Map.takeWhileAntitone (< s.size) patches),
              (small, smallFeatures) <- mapToList smaller,
              lacksNone smallFeatures features
            ]
          where
            features = findWithDefault 0 (patchOf s) featuresOf
            contained = LazyMap.findWithDefault mempty (patchOf s) containedIn
        -- the distinct patches by size, each with its shape
        bySize ::
          Map Int (LazyMap.Map (Patch' PatchLabel) (PatchShape PatchLabel))
        bySize =
          Map.fromListWith
            (<>)
            [(s.size, LazyMap.singleton (patchOf s) s.shape) | s <- cs]
        -- for each patch, whether it contains each strictly smaller patch
        containedIn ::
          LazyMap.Map
            (Patch' PatchLabel)
            (Map Int (LazyMap.Map (Patch' PatchLabel) Bool))
        containedIn =
          LazyMap.fromList
            [ ( patch,
                map
                  ( LazyMap.mapWithKey \small smallShape ->
                      shapeWithin smallShape shape && patchSubsumes small patch
                  )
                  (Map.takeWhileAntitone (< size) bySize)
              )
            | (size, patches) <- mapToList bySize,
              (patch, shape) <- mapToList patches
            ]
    -- of equal patches, keep one bound by evidence over one bound by pins
    fewerPins s = (length s.candidate.pins, s.key)
    -- keep the first of patches equal modulo renaming
    dedupe = reverse . snd . foldl' keep (mempty, [])
      where
        keep ::
          (Map (PatchShape PatchLabel) [Patch' PatchLabel], [Scored]) ->
          Scored ->
          (Map (PatchShape PatchLabel) [Patch' PatchLabel], [Scored])
        keep (seen, kept) s
          | any (samePatch (patchOf s)) (findWithDefault [] s.shape seen) =
              (seen, kept)
          | otherwise =
              (Map.insertWith (<>) s.shape [patchOf s] seen, s : kept)
        -- of one shape, so of as many nodes and edges
        samePatch a b = a == b || patchSubsumes a b

-- | Selection as its rule is stated, comparing every pair of candidates:
-- the reference 'select' is tested against. It takes time quadratic in
-- the candidates, with a search for each pair of patches.
selectReference :: [Candidate] -> [Candidate]
selectReference candidates =
  sortOn key . dedupe . sortOn fewerPins . minimal $ undominated
  where
    undominated = filter (not . dominated) candidates
    dominated c = any (dominates c) candidates
    -- c' extends c with only left-justified extra bindings (a pin is never
    -- left-justified)
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
                -- identifying a fresh vertex with an existing node usually
                -- shrinks the patch to a subset, but a repair without that
                -- identification is always offered
                Map.isSubmapOf c'.pins c.pins
                  && patchSubsumes c'.residual.patch c.residual.patch
                  && not (patchSubsumes c.residual.patch c'.residual.patch)
            )
            cs
      ]
    samePatch a b =
      patchSubsumes a.residual.patch b.residual.patch
        && patchSubsumes b.residual.patch a.residual.patch
    -- of equal patches, keep one bound by evidence over one bound by pins
    fewerPins c = (length c.pins, key c)
    -- keep the first of patches equal modulo renaming
    dedupe = reverse . foldl' keep []
      where
        keep kept r
          | any (samePatch r) kept = kept
          | otherwise = r : kept
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
--
-- Cheap necessary conditions come first. The search then assigns fresh
-- nodes along edges, taking images from the second patch's matching edges,
-- and checks each edge as soon as both its ends are assigned.
patchSubsumes :: (Ord l) => Patch' l -> Patch' l -> Bool
patchSubsumes small large =
  length small.nodes <= length large.nodes
    && length small.edges <= length large.edges
    && all (`member` large.nodes) [b | b@(Existing _) <- toList small.nodes]
    && all (`member` large.edges) [e | e <- smallEdges, not (touchesFresh e)]
    && Map.isSubmapOfBy (<=) (labelCounts small) (labelCounts large)
    && go mempty
  where
    smallEdges = toList small.edges
    largeEdges = toList large.edges
    isFresh = \case
      Fresh _ -> True
      Existing _ -> False
    touchesFresh e = isFresh e.source || isFresh e.sink
    labelCounts p =
      Map.fromListWith (+) [(e.transition, 1 :: Int) | e <- toList p.edges]
    smallFresh = [k | Fresh k <- toList small.nodes]
    largeFresh = [k | Fresh k <- toList large.nodes]
    edgesAt k = [e | e <- smallEdges, Fresh k `elem` [e.source, e.sink]]
    rename :: Map Int Int -> Binding -> Maybe Binding
    rename r = \case
      Fresh k -> Fresh <$> lookup k r
      b -> Just b
    edgeOk r e = case (rename r e.source, rename r e.sink) of
      (Just s, Just t) -> Edge s e.transition t `member` large.edges
      _ -> True
    -- images for k from an edge whose other end is already assigned
    anchored r k =
      headMay
        [ ordNub
            [ k'
            | e' <- largeEdges,
              e'.transition == e.transition,
              Fresh k' <- [if kIsSource then e'.source else e'.sink],
              (if kIsSource then e'.sink else e'.source) == other
            ]
        | e <- edgesAt k,
          (kIsSource, otherEnd) <-
            [(True, e.sink) | e.source == Fresh k]
              <> [(False, e.source) | e.sink == Fresh k],
          otherEnd /= Fresh k,
          Just other <- [rename r otherEnd]
        ]
    go :: Map Int Int -> Bool
    go r = case filter (`notMember` r) smallFresh of
      [] -> True
      unassigned ->
        let (k, images) =
              case [(x, c) | x <- unassigned, Just c <- [anchored r x]] of
                xc : _ -> xc
                [] -> (headEx unassigned, largeFresh)
            used = setFromList (toList r) :: Set Int
         in any
              ( \k' ->
                  let r' = insertMap k k' r
                   in all (edgeOk r') (edgesAt k) && go r'
              )
              [k' | k' <- images, k' `notMember` used]

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

-- | Every candidate of every alternative, with no limit.
candidatesOf ::
  (Monad m) =>
  NodeLookup m ->
  LabelChooser m ->
  NID ->
  PathQuery ->
  m [Candidate]
candidatesOf lookupNode chooser cur q =
  fromMaybe (error "candidatesOf: stopped without a limit")
    <$> candidatesWithin Nothing lookupNode chooser cur q

-- | The candidates of every alternative, or 'Nothing' if there are more
-- than the limit. What is counted is every binding with every
-- identification of its fresh vertices that enumeration makes
-- ('mergeStates'), before any is dropped, so that it can stop as soon as
-- there are too many.
candidatesWithin ::
  forall m.
  (Monad m) =>
  Maybe Int ->
  NodeLookup m ->
  LabelChooser m ->
  NID ->
  PathQuery ->
  m (Maybe [Candidate])
candidatesWithin limit lookupNode chooser cur q =
  runMaybeT . flip S.evalStateT limit $
    concat
      <$> traverse
        ( uncurry $
            candidatesFor
              budget
              (lift . lift . lookupNode)
              (lift . lift . chooser)
              cur
        )
        (zip [0 ..] (toList q.alternatives))
  where
    budget :: Budget (S.StateT (Maybe Int) (MaybeT m))
    budget =
      Budget
        { left = S.get,
          spend = \k ->
            S.get >>= \case
              Nothing -> pure ()
              Just n
                | k > n -> lift (MaybeT (pure Nothing))
                | otherwise -> S.put (Just (n - k))
        }

-- | How enumeration counts the candidates it makes.
data Budget m = Budget
  { -- | how many more may be made, if that is limited
    left :: m (Maybe Int),
    -- | count this many more; with fewer left, enumeration stops here
    spend :: Int -> m ()
  }

candidatesFor ::
  forall m.
  (Monad m) =>
  Budget m ->
  NodeLookup m ->
  LabelChooser m ->
  NID ->
  Int ->
  PatternPath ->
  m [Candidate]
candidatesFor budget lookupNode chooser cur altIndex p
  | not boundTestsOk = pure []
  | not (null unreachable) =
      -- only %unanchored could produce these; it is not supported yet
      error $ "residuate: variables without a bound vertex: " <> show unreachable
  | otherwise = explore [start] (singletonSet start) mempty
  where
    start = (mempty, mempty)
    -- Each seed is a binding and the pins in it. Its evidence closure gives
    -- candidates; identifying a fresh vertex with an existing node gives a
    -- new seed, whose closure continues from the pinned node.
    explore ::
      [(Map VarId NID, Map VarId NID)] ->
      -- seeds queued so far
      Set (Map VarId NID, Map VarId NID) ->
      -- bindings whose candidates are made
      Set (Map VarId NID, Map VarId NID) ->
      m [Candidate]
    explore [] _ _ = pure []
    explore ((sigma0, pins) : rest) queued done = do
      sigmas <- closure (Set.size done) [sigma0] (singletonSet sigma0)
      let todo = ordNub [s | s <- sigmas, (s, pins) `notMember` done]
          done' = done <> setFromList [(s, pins) | s <- todo]
      results <- traverse (candidatesForSigma pins) todo
      let seeds =
            ordNub
              [ seed
              | seed <- concatMap snd results,
                seed `notMember` queued,
                seed `notMember` done'
              ]
      (concatMap fst results <>)
        <$> explore (rest <> seeds) (queued <> setFromList seeds) done'
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
    -- Each binding that has no candidates yet (all but at most the given
    -- number) gets at least one, so with more of them than candidates are
    -- left, there are too many already.
    closure ::
      Int -> [Map VarId NID] -> Set (Map VarId NID) -> m [Map VarId NID]
    closure _ [] seen = pure (toList seen)
    closure made (sigma : rest) seen = do
      next <- concat <$> traverse (extendWith sigma) [v | v <- vars, v `notMember` sigma]
      let new = ordNub (filter (`notMember` seen) next)
          seen' = seen <> setFromList new
          fresh = Set.size seen' - made
      remaining <- budget.left
      when (maybe False (fresh >) remaining) (budget.spend fresh)
      closure made (new <> rest) seen'

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

    -- A pin is not left-justified, but what an existing edge reaches from it
    -- is, relative to the pin.
    leftClosure ::
      Map EdgeId (Maybe Text) -> Map VarId NID -> Map VarId NID -> Set VarId
    leftClosure existing sigma pins = go mempty
      where
        go acc =
          let justified v = case v of
                Var x -> x `member` acc || x `member` pins
                _ -> True
              new =
                setFromList
                  [ t
                  | (e, spec) <- mapToList specs,
                    Var t <- [spec.target],
                    t `member` sigma,
                    t `notMember` acc,
                    t `notMember` pins,
                    justified spec.source,
                    isJust (join (lookup e existing))
                  ]
           in if null new then acc else go (acc <> new)

    -- The candidates for a binding, and the seeds that pin one of its fresh
    -- vertices.
    candidatesForSigma ::
      Map VarId NID ->
      Map VarId NID ->
      m ([Candidate], [(Map VarId NID, Map VarId NID)])
    candidatesForSigma pins sigma = do
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
          leftVars = leftClosure existing sigma pins
          createdEdges = setFromList [e | (e, Nothing) <- mapToList existing]
          accepts v = testAccepts (vertexTest p (Var v))
          -- number created nodes by first appearance
          compact classes =
            let order' = ordNub [k | v <- unbound, Just k <- [lookup v classes]]
                renumber = mapFromList (zip order' [0 ..]) :: Map Int Int
             in fmap (\k -> findWithDefault k k renumber) classes
          (states, pinned) =
            mergeStates specs labels createdEdges binding accepts initial
      -- count the states, making no more of them than are left and one
      remaining <- budget.left
      budget.spend (length (maybe id (take . (+ 1)) remaining states))
      let seeds =
            [ (sigma <> new, pins <> new)
            | (vs, nid) <- pinned,
              let new = mapFromList [(v, nid) | v <- toList vs]
            ]
          -- Every pin must still make a created hop the same patch edge as
          -- one at its node: directly, or through pins justified already.
          -- The seed was rebuilt without the identifications that led to
          -- the pin, so a state may have lost them.
          pinsJustified classes = justify (keysSet pins)
            where
              bind = binding classes
              links =
                [ (v, w)
                | (_, far1, far2) <-
                    sharedPairs specs labels createdEdges bind,
                  (Var v, w) <- [(far1, far2), (far2, far1)],
                  v `member` pins,
                  bind w == bind (Var v)
                ]
              justify unjustified
                | null unjustified = True
                | null ok = False
                | otherwise = justify (unjustified `difference` ok)
                where
                  pending = \case
                    Var x -> x `member` unjustified
                    _ -> False
                  ok =
                    setFromList
                      [ v
                      | (v, w) <- links,
                        v `member` unjustified,
                        not (pending w)
                      ] ::
                      Set VarId
      pure . (,seeds) $
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
              pins,
              leftJustified = leftVars,
              alternative = altIndex,
              missingLabels = missing
            }
        | (classes, usedRight) <- map (first compact) states,
          pinsJustified classes
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

-- | The other ends of pairs of created hops with the same label and stored
-- orientation from the same end (which binds equally), and whether the pair
-- is from the left (both hops taken from the shared end). Identifying the
-- other ends makes the two hops one patch edge.
--
-- The pairs are in the order of 'sharedPairsReference', which goes through
-- every two hops; this groups the hops by label, end and what the end
-- binds to, and pairs them within a group.
sharedPairs ::
  forall l.
  (Ord l) =>
  Map EdgeId EdgeSpec ->
  Map EdgeId l ->
  -- | edges to create
  Set EdgeId ->
  (PVertex -> Binding) ->
  [(Bool, PVertex, PVertex)]
sharedPairs specs labels created bind =
  map snd . sortOn fst $
    [ ((e1, e2, i1, i2), (taken1 && taken2, far1, far2))
    | hops <- toList groups,
      (e1, i1, far1, taken1) : later <- tails hops,
      (e2, i2, far2, taken2) <- later
    ]
  where
    -- A hop is in two groups, one for each end, and the hops of a group
    -- are in the order of their edges.
    groups :: Map (l, Bool, Binding) [(EdgeId, Int, PVertex, Bool)]
    groups =
      Map.fromListWith
        (flip (<>))
        [ ((l, end, bind near), [(e, i, far, taken)])
        | (e, spec) <- mapToList specs,
          e `member` created,
          Just l <- [lookup e labels],
          (i, (end, near, far, taken)) <- zip [0 :: Int ..] (hopEnds spec)
        ]

-- | Each hop, in stored orientation, as (which stored end is shared, that
-- end, the other end, whether the hop is taken from that end).
hopEnds :: EdgeSpec -> [(Bool, PVertex, PVertex, Bool)]
hopEnds spec = case spec.direction of
  Out ->
    [ (True, spec.source, spec.target, True),
      (False, spec.target, spec.source, False)
    ]
  In ->
    [ (True, spec.target, spec.source, False),
      (False, spec.source, spec.target, True)
    ]

-- | 'sharedPairs' as it is defined, going through every two hops.
sharedPairsReference ::
  (Eq l) =>
  Map EdgeId EdgeSpec ->
  Map EdgeId l ->
  -- | edges to create
  Set EdgeId ->
  (PVertex -> Binding) ->
  [(Bool, PVertex, PVertex)]
sharedPairsReference specs labels created bind =
  [ (taken1 && taken2, far1, far2)
  | (e1, s1) <- specList,
    (e2, s2) <- specList,
    e1 < e2,
    e1 `member` created,
    e2 `member` created,
    lookup e1 labels == lookup e2 labels,
    isJust (lookup e1 labels),
    (end1, near1, far1, taken1) <- hopEnds s1,
    (end2, near2, far2, taken2) <- hopEnds s2,
    end1 == end2,
    bind near1 == bind near2
  ]
  where
    specList = mapToList specs

-- | Identify fresh vertices where that makes two created pattern edges one
-- patch edge. Identifying two fresh vertices from the left (the same hop
-- from the same vertex) is always done; from the right (the same hop into
-- the same vertex) it is an alternative. Returns every resulting class
-- assignment and whether it used a right identification.
--
-- Identifying a fresh vertex with an existing node (a pin), from either
-- side, is also an alternative, allowed only when the node passes the test
-- of every variable in the fresh vertex's class. Pins are returned as the
-- class's variables and the node, to be explored as a new binding.
mergeStates ::
  (Ord l) =>
  Map EdgeId EdgeSpec ->
  Map EdgeId l ->
  -- | edges to create
  Set EdgeId ->
  (Map VarId Int -> PVertex -> Binding) ->
  -- | whether a variable's test accepts a node
  (VarId -> NID -> Bool) ->
  Map VarId Int ->
  ([(Map VarId Int, Bool)], [(Set VarId, NID)])
mergeStates specs labels created binding accepts initial =
  second ordNub (go [(leftFix initial, False)] mempty)
  where
    renameClass old new = map (\c -> if c == old then new else c)
    classVars classes a = [v | (v, k) <- mapToList classes, k == a]
    pairs classes =
      [ (fromLeftSide, binding classes far1, binding classes far2)
      | (fromLeftSide, far1, far2) <-
          sharedPairs specs labels created (binding classes)
      ]
    merges leftSide classes =
      ordNub
        [ (min a b, max a b)
        | (leftPair, Fresh a, Fresh b) <- pairs classes,
          leftPair == leftSide,
          a /= b
        ]
    pinsOf classes =
      ordNub
        [ (setFromList vs, nid)
        | (_, far1, far2) <- pairs classes,
          (Fresh a, Existing nid) <- [(far1, far2), (far2, far1)],
          let vs = classVars classes a,
          all (`accepts` nid) vs
        ]
    leftFix classes = case merges True classes of
      (a, b) : _ -> leftFix (renameClass b a classes)
      [] -> classes
    go ::
      [(Map VarId Int, Bool)] ->
      Set (Map VarId Int) ->
      ([(Map VarId Int, Bool)], [(Set VarId, NID)])
    go [] _ = ([], [])
    go ((classes, usedRight) : rest) seen
      | classes `member` seen = go rest seen
      | otherwise =
          let next =
                [ (leftFix (renameClass b a classes), True)
                | (a, b) <- merges False classes
                ]
              (states, pinned) = go (rest <> next) (insertSet classes seen)
           in ((classes, usedRight) : states, pinsOf classes <> pinned)

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
