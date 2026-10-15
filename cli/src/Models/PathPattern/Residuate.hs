-- | What @mk@ creates so that a path resolves (docs/path-route-ir.md section 5).
--
-- A path whose shape "Models.PathPattern.Admit" refuses is not built,
-- whatever the graph holds ('Refused'). For every other path, each
-- alternative is built on its own, ONCE, in the order of its plan
-- ('planOf'): an operation places an unnamed node, by all the steps into
-- it, or connects two known nodes. Standing on a node, a step follows an
-- edge that exists, or one that an operation before it created by a step
-- that is no further from the start of its route (SAME PACE); with none it
-- creates a new node and the edge to it; with edges to several nodes every
-- one is tried, and each gives a walk of its own. No step is taken twice,
-- and nothing waits. What a walk creates is a possibility. A possibility
-- that creates everything another creates and more is dropped ('select');
-- one left is what @mk@ creates, and several are listed. Building stops
-- with 'TooManyRepairs' once there are more walks than the limit (the
-- option @--max-repairs@ of @mk@ and @tag@).
-- A node that the path writes as @\@N@ is never created: an alternative
-- that needs one that does not exist is not built ('MissingNodes').
module Models.PathPattern.Residuate
  ( Binding (..),
    Patch' (..),
    Patch,
    Residual' (..),
    Residual,
    Branch (..),
    PatchLabel (..),
    Residuation (..),
    ResidualError (..),
    NodeLookup,
    LabelChooser,
    Refusal (..),
    Request (..),
    Step (..),
    LeftOut (..),
    residuate,
    residuateWithin,
    residuateNoting,
    defaultMaxRepairs,
    Choice (..),
    chooseRepair,
    residuatePure,
    noLabels,
    patchSize,
    patchSubsumes,
    renderResidual,
    renderResidualWith,
    renderPatchLabel,
    renderMatcher,
    renderBinding,

    -- * For tests
    Candidate (..),
    Walked (..),
    WalkCost (..),
    walkedWithin,
    candidatesWithin,
    residuationOf,
    select,
    selectReference,
  )
where

import Data.Bits (complement, setBit, (.&.))
import Data.List.NonEmpty (nonEmpty)
import Data.Map.Strict qualified as Map
import Data.Set qualified as Set
import Models.Edge
import Models.NID
import Models.Node
import Models.Path.Simple qualified as P
import Models.PathPattern
import Models.PathPattern.Admit
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

-- | Nodes and edges to create, with labels of type @l@. The created nodes
-- are all 'Fresh': a node with a written id is never created.
data Patch' l = Patch
  { nodes :: Set Binding,
    edges :: Set (Edge' Binding l)
  }
  deriving stock (Eq, Ord, Show, Generic)

type Patch = Patch' Text

-- | One possibility: the pattern with every vertex at a node and every
-- step along an edge, as one walk left them, and what it creates.
data Residual' l = Residual
  { patternPath :: PatternPath,
    sigma :: Map PVertex Binding,
    labels :: Map EdgeId l,
    patch :: Patch' l,
    -- | the places where the walk had several nodes to go to, in the
    -- order of the plan
    branches :: [Branch]
  }
  deriving stock (Eq, Show, Generic)

type Residual = Residual' Text

-- | A vertex whose steps had edges to several nodes to follow: the node
-- this walk took, and the others.
data Branch = Branch
  { vertex :: PVertex,
    taken :: Binding,
    others :: [Binding]
  }
  deriving stock (Eq, Ord, Show, Generic)

-- | The label of an edge to create. 'Missing' stands in for a label the
-- chooser did not give, so that the walk goes on as though it had given
-- one: equal for every edge with the same matcher (a chooser that returns
-- one fresh label per matcher) and equal to no real label.
data PatchLabel = Known Text | Missing Matcher
  deriving stock (Eq, Ord, Show, Generic)

data Residuation
  = -- | some alternative already resolves; nothing needs to change
    AlreadyResolves
  | -- | the possibilities, the one that creates least first
    Repairs (NonEmpty Residual)
  | -- | no alternative can be made to resolve
    Unsatisfiable
  deriving stock (Eq, Show, Generic)

data ResidualError
  = -- | a possibility has to create an edge with this matcher, but no
    -- label was given. The step is at this route and hop (from 0) of its
    -- alternative. The possibilities are listed, the one that creates
    -- least first; each one that needs a label shows it as 'Missing'.
    UnsupportedLabel Matcher Int Int (NonEmpty (Residual' PatchLabel))
  | -- | the path does not resolve, and has more walks than this limit, so
    -- they were not all made, let alone compared
    TooManyRepairs Int
  | -- | the path is not built, whatever the graph holds: its shape is
    -- refused. This comes before everything else: also a path that
    -- resolves already is refused.
    Refused Refusal
  | -- | every way of creating the path needs one of these nodes, which the
    -- path writes as @\@N@ and which do not exist; none is ever created
    MissingNodes (NonEmpty NID)
  deriving stock (Eq, Show, Generic)

-- | What a construction command does with a residuation.
data Choice
  = NoChange
  | Apply Residual
  | -- | at least two possibilities; nothing is changed
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

-- | What one walk creates, before selection.
data Candidate = Candidate
  { residual :: Residual' PatchLabel,
    -- | steps (route, hop) that create an edge but got no label from the
    -- chooser, in route order; such an edge is labelled 'Missing'
    missingLabels :: [((Int, Int), Matcher)]
  }
  deriving stock (Eq, Show, Generic)

-- | An alternative that is not built because it needs these nodes, which
-- the path writes as @\@N@ and which do not exist.
data LeftOut = LeftOut {alternative :: PatternPath, missing :: NonEmpty NID}
  deriving stock (Eq, Show, Generic)

-- | The walks of a path, the alternatives that were not built for a
-- written node that does not exist, and what building took.
data Walked = Walked
  { candidates :: [Candidate],
    leftOut :: [LeftOut],
    cost :: WalkCost
  }
  deriving stock (Eq, Show, Generic)

-- | What building took, in counts: the walks (what the limit counts), and
-- the batches of operations done, over all walks. A walk goes through the
-- batches of its alternative's plan once.
data WalkCost = WalkCost {walks :: Int, batches :: Int}
  deriving stock (Eq, Show, Generic)

instance Semigroup WalkCost where
  a <> b = WalkCost (a.walks + b.walks) (a.batches + b.batches)

instance Monoid WalkCost where
  mempty = WalkCost 0 0

-- | How many walks a path may have, unless the command's option
-- @--max-repairs@ says otherwise. This bounds a count, not time: what a
-- walk costs grows with the pattern.
defaultMaxRepairs :: Int
defaultMaxRepairs = 5000

-- | 'residuateWithin' the default limit.
residuate ::
  (Monad m) =>
  NodeLookup m ->
  LabelChooser m ->
  NID ->
  PathQuery ->
  m (Either ResidualError Residuation)
residuate = residuateWithin (P.AtMostRepairs defaultMaxRepairs)

-- | What @mk@ creates for a path, making at most the given number of
-- walks: a walk does every operation of one alternative, with one node
-- chosen wherever steps had several to go to. Every walk of the command
-- counts, over all its alternatives. With more walks the result is
-- 'TooManyRepairs', unless the path resolves, which is checked by
-- matching then: 'AlreadyResolves' does not depend on the limit. A path
-- of a refused shape is 'Refused' before any of this.
residuateWithin ::
  forall m.
  (Monad m) =>
  P.RepairsLimit ->
  NodeLookup m ->
  LabelChooser m ->
  NID ->
  PathQuery ->
  m (Either ResidualError Residuation)
residuateWithin limit lookupNode chooser cur q =
  snd <$> residuateNoting limit lookupNode chooser cur q

-- | 'residuateWithin', and the alternatives that were not built for a
-- written node that does not exist (none when the limit stopped the
-- building: they are not known then).
residuateNoting ::
  forall m.
  (Monad m) =>
  P.RepairsLimit ->
  NodeLookup m ->
  LabelChooser m ->
  NID ->
  PathQuery ->
  m ([LeftOut], Either ResidualError Residuation)
residuateNoting limit lookupNode chooser cur q =
  walkedWithin bound lookupNode chooser cur q >>= \case
    Right walked ->
      pure (walked.leftOut, residuationOf (absentOf walked) walked.candidates)
    -- The walk that creates nothing may not have been made yet, so
    -- whether the path resolves is found by matching instead.
    Left stopped@(TooManyRepairs _) -> do
      resolves <-
        anyM (alternativeResolves lookupNode cur) (toList q.alternatives)
      pure ([], if resolves then Right AlreadyResolves else Left stopped)
    Left refused -> pure ([], Left refused)
  where
    bound = case limit of
      P.UnlimitedRepairs -> Nothing
      P.AtMostRepairs n -> Just n
    absentOf walked =
      setFromList (concatMap (toList . (.missing)) walked.leftOut) :: Set NID

-- | The result for the walks of a path. If some walk creates nothing the
-- path resolves and nothing is done. Otherwise the walks that 'select'
-- keeps are the possibilities; if one of them needs a label that was not
-- given, that is the error, rather than falling back to the others. With
-- no possibility, the written nodes that do not exist are the error if
-- there are any: an alternative that needs one is not a possibility.
residuationOf :: Set NID -> [Candidate] -> Either ResidualError Residuation
residuationOf absent candidates
  | any (isEmptyPatch . (.residual.patch)) candidates = Right AlreadyResolves
  | otherwise = case nonEmpty (select candidates) of
      Nothing -> maybe (Right Unsatisfiable) (Left . MissingNodes) (nonEmpty (toList absent))
      Just kept ->
        case [miss | s <- toList kept, miss : _ <- [s.missingLabels]] of
          ((ri, hi), m) : _ ->
            Left (UnsupportedLabel m ri hi (fmap (.residual) kept))
          [] -> Right (Repairs (fmap (knownResidual . (.residual)) kept))
  where
    isEmptyPatch patch = null patch.nodes && null patch.edges
    known = \case
      Known l -> l
      Missing _ -> error "mk: a possibility without missing labels has one"
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
-- of the match exists. This is when some walk creates nothing: a walk
-- follows every edge that exists, so every match is a walk, and a walk
-- that creates nothing is a match.
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

-- * Selection

-- | A candidate with what selection compares of it.
data Scored = Scored
  { candidate :: Candidate,
    size :: Int,
    shape :: ~(PatchShape PatchLabel),
    -- | the order of the possibilities
    key :: ~(Int, Text)
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

-- | The possibilities among the walks, the one that creates least first
-- (docs/path-route-ir.md section 5, Selection). 'selectReference' states the
-- rule; this gives the same possibilities in the same order, without a
-- search for every two walks:
--
-- * Walks that create exactly the same are one from the start.
-- * A patch contained in another modulo renaming, and not the other way
--   round, has fewer nodes or edges: were they as many, the renaming would
--   be onto, and its inverse would contain the other. So a patch is
--   compared with the strictly smaller ones only, and containment is one
--   search, not two. The search is left out where a 'PatchFeature' of the
--   smaller patch is missing from the larger.
-- * Patches equal modulo renaming have one 'PatchShape', and one contains
--   the other; so a patch is compared with the kept ones of its shape
--   only, by one search. A shape is not a canonical form: patches of one
--   shape may differ, and the search tells.
select :: [Candidate] -> [Candidate]
select candidates =
  map (.candidate) . dedupe . sortOn (.key) . minimal . toList $
    Map.fromListWith first' [(patchOf s, s) | s <- map scored candidates]
  where
    first' a b = if a.key <= b.key then a else b
    scored c =
      let r = c.residual
          size = patchSize r.patch
       in Scored
            { candidate = c,
              size,
              shape = patchShape r.patch,
              key = (size, renderResidualWith renderPatchLabel r)
            }
    patchOf s = s.candidate.residual.patch
    -- Most patches are told apart by their features, as bits; the rest
    -- are searched.
    minimal cs = filter (not . redundant) cs
      where
        featureBit :: Map (PatchFeature PatchLabel) Int
        featureBit =
          mapFromList . flip zip [0 ..] . toList $
            foldMap (patchFeatures . (.shape)) cs
        -- a patch has the features of every patch it contains
        featuresOf :: Scored -> Integer
        featuresOf s =
          foldl' setBit 0 . mapMaybe (`lookup` featureBit) . toList $
            patchFeatures s.shape
        lacksNone small large = small .&. complement large == (0 :: Integer)
        -- the patches by size, each with its features
        bySize :: Map Int [(Scored, Integer)]
        bySize = Map.fromListWith (<>) [(s.size, [(s, featuresOf s)]) | s <- cs]
        redundant s =
          or
            [ shapeWithin small.shape s.shape
                && patchSubsumes (patchOf small) (patchOf s)
            | let features = featuresOf s,
              smaller <- toList (Map.takeWhileAntitone (< s.size) bySize),
              (small, smallFeatures) <- smaller,
              lacksNone smallFeatures features
            ]
    -- keep the first of patches equal modulo renaming
    dedupe = reverse . snd . foldl' keep (mempty, [])
      where
        keep ::
          (Map (PatchShape PatchLabel) [Patch' PatchLabel], [Scored]) ->
          Scored ->
          (Map (PatchShape PatchLabel) [Patch' PatchLabel], [Scored])
        keep (seen, kept) s
          | any (patchSubsumes (patchOf s)) (findWithDefault [] s.shape seen) =
              (seen, kept)
          | otherwise =
              (Map.insertWith (<>) s.shape [patchOf s] seen, s : kept)

-- | Selection as its rule is stated, comparing every two walks: a walk
-- that creates everything another creates and more (modulo renaming of
-- fresh nodes) is dropped, of walks that create the same one is kept, and
-- the rest are ordered by how much they create, then by their text. The
-- reference 'select' is tested against.
selectReference :: [Candidate] -> [Candidate]
selectReference candidates = dedupe (sortOn key (minimal candidates))
  where
    within a b = patchSubsumes a.residual.patch b.residual.patch
    minimal cs =
      [c | c <- cs, not (any (\c' -> c' `within` c && not (c `within` c')) cs)]
    dedupe = reverse . foldl' keep []
      where
        keep kept c
          | any (\k -> k `within` c && c `within` k) kept = kept
          | otherwise = c : kept
    key c =
      let r = c.residual
       in (patchSize r.patch, renderResidualWith renderPatchLabel r)

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

-- * The construction

-- | The walks of every alternative; 'TooManyRepairs' if there are more
-- than the limit, and 'Refused' for a path of a refused shape.
candidatesWithin ::
  (Monad m) =>
  Maybe Int ->
  NodeLookup m ->
  LabelChooser m ->
  NID ->
  PathQuery ->
  m (Either ResidualError [Candidate])
candidatesWithin limit lookupNode chooser cur q =
  fmap (.candidates) <$> walkedWithin limit lookupNode chooser cur q

-- | The walks of every alternative with what they took. The limit is on
-- the walks of the whole command: with more, 'TooManyRepairs'. A path of
-- a refused shape is 'Refused', before the graph is looked at.
walkedWithin ::
  forall m.
  (Monad m) =>
  Maybe Int ->
  NodeLookup m ->
  LabelChooser m ->
  NID ->
  PathQuery ->
  m (Either ResidualError Walked)
walkedWithin limit lookupNode chooser cur q = case refusals cur q of
  refusal : _ -> pure (Left (Refused refusal))
  [] -> go (Walked [] [] mempty) (toList q.alternatives)
  where
    go done [] = pure (Right done)
    go done (p : rest) =
      absentOf p >>= \case
        -- NO NODE WITH A WRITTEN ID IS CREATED: an alternative that needs
        -- one that does not exist has no walk.
        n : ns -> go (done {leftOut = done.leftOut <> [LeftOut p (n :| ns)]}) rest
        [] -> do
          -- the label of the edge each step creates if it has to
          own <- for (patternEdges p) \spec -> case spec.matcher of
            MLiteral l -> pure (Known l)
            m ->
              chooser m <&> \case
                Just l | matcherAccepts m l -> Known l
                _ -> Missing m
          walksOf (subtract done.cost.walks <$> limit) lookupNode cur p own >>= \case
            Nothing -> pure (Left (TooManyRepairs (fromMaybe maxBound limit)))
            Just (walks, cost) ->
              go
                done
                  { candidates = done.candidates <> map (candidateOf cur p) walks,
                    cost = done.cost <> cost
                  }
                rest
    -- the nodes an alternative stands on by name (the current node and
    -- every @\@N@; not the nodes of a @!{…}@) that do not exist
    absentOf p =
      filterM
        (fmap isNothing . lookupNode)
        (ordNub ([cur | Current <- patternVertices p] <> [n | Fixed n <- patternVertices p]))

-- | A node during a walk: one of the graph (or one written as @\@N@), or
-- one the walk creates, numbered as the walk comes to create them.
data Term = Ex NID | New Int
  deriving stock (Eq, Ord, Show)

-- | Where a node is created: from this node, by an edge with this label,
-- in this direction. Operations of one batch that create from the same
-- site create the same node.
data Site = Site {from :: Term, label :: PatchLabel, direction :: Direction}
  deriving stock (Eq, Ord, Show)

-- | A created edge, in stored orientation.
type CreatedEdge = (Term, PatchLabel, Term)

-- | The steps that created an edge: those that found it missing in the
-- same batch. None: an edge of the graph. A step that goes along an edge
-- another batch created is not one of its creators.
type Creators = Set EdgeId

-- | A step about to be taken: the node it stands on and the edges it can
-- go along from there (of the graph, and created by the batches before),
-- each with its label, its far end and its creators.
data Ready = Ready
  { edge :: EdgeId,
    source :: Term,
    spec :: EdgeSpec,
    edges :: [(PatchLabel, Term, Creators)]
  }

data Walk = Walk
  { -- | where the variables placed so far are
    sigma :: Map VarId Term,
    -- | the created edges at each node as a step in each direction sees
    -- them: by label, the far ends
    created :: Map (Term, Direction) (Map PatchLabel (Set Term)),
    creators :: Map CreatedEdge Creators,
    -- | the steps taken, each with the label of the edge it went along
    went :: Map EdgeId PatchLabel,
    -- | the variables that had several nodes to go to, the last first:
    -- the node taken, and all of them
    choices :: [(VarId, Term, [Term])],
    -- | how many nodes the walk has created
    fresh :: Int
  }

acceptsLabel :: Matcher -> PatchLabel -> Bool
acceptsLabel m = \case
  Known l -> matcherAccepts m l
  -- a label still to be given: the same matcher will accept it, and `*`
  -- accepts any label
  Missing m' -> m == m' || m == MWild

-- | A new node passes every test but that of being a given node.
acceptsTerm :: NodeTest -> Term -> Bool
acceptsTerm t = \case
  Ex n -> testAccepts t n
  New _ -> case t of
    Except _ -> True
    Exactly _ -> False

-- | Every walk of one alternative of an admitted path, given the label of
-- the edge each step creates if it has to, and what building took;
-- 'Nothing' with more walks than the limit.
--
-- __The order.__ The operations are done in the batches of the plan
-- ('planOf'), each batch once for each walk. An operation is in a later
-- batch than every operation that could create an edge it might follow,
-- so when its batch comes, every such edge is there or never will be.
--
-- __A batch__ ('batch'). All its operations look at the same state: the
-- graph, and what the batches before created. An operation that places a
-- node has, as choices, the nodes any of its steps could follow an edge
-- to; each choice gives a walk of its own, and the choices of the
-- operations of a batch multiply. With no edge to follow it creates a
-- node, the same node as the other operations of the batch that create
-- from a common 'Site'. Then every step with a literal label creates its
-- edge, unless that edge was there when the batch began; steps of one
-- batch that create the same edge all count as its creators. Last, a
-- step with @*@ or a regex goes along an edge between its two nodes if
-- one counts for it, also one this batch created, and creates one
-- otherwise.
--
-- __Which edges count__ ('counts'). An edge of the graph always does. An
-- edge this command created counts for an operation if a step that
-- created it is no further from the start of its route than the
-- operation is (SAME PACE).
walksOf ::
  forall m.
  (Monad m) =>
  Maybe Int ->
  NodeLookup m ->
  NID ->
  PatternPath ->
  Map EdgeId PatchLabel ->
  m (Maybe ([Walk], WalkCost))
walksOf limit lookupNode cur p own
  | not startsPass = pure (Just ([], mempty))
  | maybe False (< 1) limit = pure Nothing
  | otherwise = go [(initial, plan.batches)] 1 [] 0
  where
    specs = patternEdges p
    plan = planOf cur p
    specOf e = fromMaybe (error "mk: no such step") (lookup e specs)
    ownOf e = fromMaybe (error "mk: no such step") (lookup e own)
    written = \case
      Current -> Just cur
      Fixed n -> Just n
      Var _ -> Nothing
    startsPass =
      and
        [ testAccepts (vertexTest p v) n
        | v <- patternVertices p,
          Just n <- [written v]
        ]
    -- where a vertex is: the plan puts an operation after the placing of
    -- every node its steps start from
    valueIn :: Walk -> PVertex -> Term
    valueIn w = \case
      Var v -> fromMaybe (error "mk: a step from a node that is not placed") (lookup v w.sigma)
      Current -> Ex cur
      Fixed n -> Ex n

    initial :: Walk
    initial = Walk mempty mempty mempty mempty [] 0

    -- The walks under way, each with the batches it still has to go
    -- through, depth first; how many walks there are, under way and
    -- finished; the finished ones; and the batches done so far.
    go ::
      [(Walk, [[Operation]])] -> Int -> [Walk] -> Int -> m (Maybe ([Walk], WalkCost))
    go [] _ finished batches =
      pure (Just (finished, WalkCost (length finished) batches))
    go ((w, []) : rest) count finished batches =
      go rest count (w : finished) batches
    go ((w, ops : later) : rest) count finished batches = do
      next <- map (,later) <$> batch ops w
      case limit of
        Nothing -> go (next <> rest) count finished (batches + 1)
        Just most ->
          -- the choices of a batch multiply: no more of them are made
          -- than there is room for
          let free = most - (count - 1)
              n = length (take (free + 1) next)
           in if n > free
                then pure Nothing
                else go (next <> rest) (count - 1 + n) finished (batches + 1)

    -- SAME PACE: whether an edge counts for an operation, given the steps
    -- that created it (none: an edge of the graph).
    counts :: Operation -> Creators -> Bool
    counts op cs =
      null cs || any (\c -> findWithDefault 0 c plan.distances <= op.distance) cs

    stored :: EdgeSpec -> Term -> PatchLabel -> Term -> CreatedEdge
    stored spec x l y = case spec.direction of
      Out -> (x, l, y)
      In -> (y, l, x)

    addEdge :: CreatedEdge -> EdgeId -> Walk -> Walk
    addEdge key@(s, l, t) e w =
      w
        { created =
            Map.insertWith (Map.unionWith (<>)) (s, Out) (singletonMap l (singletonSet t))
              . Map.insertWith (Map.unionWith (<>)) (t, In) (singletonMap l (singletonSet s))
              $ w.created,
          creators = Map.insertWith (<>) key (singletonSet e) w.creators
        }

    -- the created edges a step can go along from a node
    createdAt :: Walk -> Term -> EdgeSpec -> [(PatchLabel, Term, Creators)]
    createdAt w x spec =
      [ (l, y, findWithDefault mempty (stored spec x l y) w.creators)
      | (l, ys) <- labelled (findWithDefault mempty (x, spec.direction) w.created),
        y <- toList ys
      ]
      where
        labelled byLabel = case spec.matcher of
          MLiteral l -> [(Known l, ys) | Just ys <- [lookup (Known l) byLabel]]
          m -> filter (acceptsLabel m . fst) (mapToList byLabel)

    ready :: Walk -> EdgeId -> m Ready
    ready w e = do
      let spec = specOf e
          x = valueIn w spec.source
      existing <- case x of
        Ex n ->
          lookupNode n <&> \node ->
            [(Known l, Ex t, mempty) | (l, t) <- stepEdges (const node) n spec]
        New _ -> pure []
      pure (Ready e x spec (existing <> createdAt w x spec))

    -- One batch for one walk: every walk it becomes.
    batch :: [Operation] -> Walk -> m [Walk]
    batch ops w = do
      looked <- for ops \op -> (op,) <$> traverse (ready w) op.steps
      let -- the operations that place a node, each with the nodes its
          -- steps could follow an edge to
          placings :: [(VarId, [Ready], [Term])]
          placings =
            [ ( v,
                rs,
                ordNub . sort $
                  [ y
                  | r <- rs,
                    (_, y, cs) <- r.edges,
                    counts op cs,
                    acceptsTerm (vertexTest p op.target) y
                  ]
              )
            | (op, rs) <- looked,
              not op.connects,
              Var v <- [op.target]
            ]
          -- Those with no edge to follow create a node each; the same
          -- node for those that create from a common site, directly or
          -- through other sites.
          creating :: [(VarId, [Site])]
          creating =
            [ (v, ordNub [Site r.source (ownOf r.edge) r.spec.direction | r <- rs])
            | (v, rs, []) <- placings
            ]
          -- the class of each site, and how many classes there are
          classes :: (Map Site Int, Int)
          classes = foldl' share (mempty, 0) (map snd creating)
          share (known, count) sites =
            case ordNub (mapMaybe (`lookup` known) sites) of
              [] -> (foldl' (\m site -> insertMap site count m) known sites, count + 1)
              c : others ->
                ( foldl'
                    (\m site -> insertMap site c m)
                    (if null others then known else map (\c' -> if c' `elem` others then c else c') known)
                    sites,
                  count
                )
          createdNodes :: Map VarId Term
          createdNodes =
            mapFromList
              [ (v, New (w.fresh + c))
              | (v, site : _) <- creating,
                Just c <- [lookup site (fst classes)]
              ]
          -- Those with edges to follow go to each node they could
          -- follow one to: every combination is a walk of its own.
          following = [(v, options) | (v, _, options@(_ : _)) <- placings]
          placed :: [Map VarId Term]
          placed =
            foldl'
              (\ms (v, options) -> [insertMap v y m | m <- ms, y <- options])
              [createdNodes]
              following
          finish :: Map VarId Term -> Walk
          finish here =
            let w1 =
                  w
                    { sigma = here <> w.sigma,
                      fresh = w.fresh + snd classes,
                      choices =
                        reverse
                          [ (v, y, options)
                          | (v, options@(_ : _ : _)) <- following,
                            Just y <- [lookup v here]
                          ]
                          <> w.choices
                    }
                steps = [(op, r) | (op, rs) <- looked, r <- rs]
                isLiteral r = case r.spec.matcher of
                  MLiteral _ -> True
                  _ -> False
                -- a literal step creates its edge unless the edge was
                -- there when the batch began
                literalStep :: Walk -> (Operation, Ready) -> Walk
                literalStep acc (_, r) =
                  let y = valueIn w1 r.spec.target
                      l = ownOf r.edge
                      taken = acc {went = insertMap r.edge l acc.went}
                   in if any (\(_, y', _) -> y' == y) r.edges
                        then taken
                        else addEdge (stored r.spec r.source l y) r.edge taken
                w2 = foldl' literalStep w1 (filter (isLiteral . snd) steps)
                -- a step that is no literal label goes along an edge
                -- that counts for it, also one of this batch, or creates
                -- its own
                otherStep :: Walk -> (Operation, Ready) -> Walk
                otherStep acc (op, r) =
                  let y = valueIn w1 r.spec.target
                      along =
                        [ l
                        | (l, y', cs) <-
                            [old | old@(_, _, cs) <- r.edges, null cs]
                              <> createdAt w2 r.source r.spec,
                          y' == y,
                          counts op cs
                        ]
                      mine = ownOf r.edge
                   in case along of
                        [] ->
                          addEdge
                            (stored r.spec r.source mine y)
                            r.edge
                            (acc {went = insertMap r.edge mine acc.went})
                        ls -> acc {went = insertMap r.edge (minimumEx ls) acc.went}
             in foldl' otherStep w2 (filter (not . isLiteral . snd) steps)
      pure (map finish placed)

-- | What a finished walk creates.
candidateOf :: NID -> PatternPath -> Walk -> Candidate
candidateOf cur p w =
  Candidate
    { residual =
        Residual
          { patternPath = p,
            sigma = mapFromList [(v, binding (value v)) | v <- vertices],
            labels = w.went,
            patch =
              Patch
                { nodes = setFromList (map Fresh (toList numbering)),
                  edges =
                    setFromList
                      [ Edge (binding s) l (binding t)
                      | (s, l, t) <- keys w.creators
                      ]
                },
            branches =
              [ Branch (Var v) (binding taken) (map binding (filter (/= taken) options))
              | (v, taken, options) <- reverse w.choices
              ]
          },
      missingLabels =
        sortOn
          fst
          [ (findWithDefault (0, 0) e positions, m)
          | (e, Missing m) <- mapToList w.went
          ]
    }
  where
    value = \case
      Var v -> fromMaybe (error "mk: a vertex that was not placed") (lookup v w.sigma)
      Current -> Ex cur
      Fixed n -> Ex n
    vertices = patternVertices p
    -- created nodes are numbered in order of first appearance
    numbering :: Map Term Int
    numbering =
      mapFromList (zip (ordNub [t | v <- vertices, t@(New _) <- [value v]]) [0 ..])
    binding = \case
      Ex n -> Existing n
      t -> Fresh (fromMaybe (error "mk: a created node at no vertex") (lookup t numbering))
    -- the first place (route, hop) of each step
    positions :: Map EdgeId (Int, Int)
    positions =
      Map.fromListWith
        min
        [ (h.edge, (ri, hi))
        | (ri, rt) <- zip [0 ..] (toList p.routes),
          (hi, h) <- zip [0 ..] rt.hops
        ]

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
