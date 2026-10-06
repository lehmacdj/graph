{-# LANGUAGE MultiWayIf #-}

-- | Pure matching of route patterns (docs/path-pattern-ir.md §4).
--
-- Matching is a homomorphism: distinct variables may bind the same node. The
-- pattern is split at bound vertices ('Current' and 'Fixed') into
-- independent components; the witnesses of a pattern are the product of the
-- per-component solutions, which is stored rather than expanded.
--
-- 'loadPattern' is the one part that reads nodes: it finds the nodes a
-- match can use, with any way of reading them.
module Models.PathPattern.Match
  ( Component (..),
    Partial (..),
    Resolution (..),
    Witness (..),
    ConcreteHop (..),
    ConcreteRoute (..),
    Lookup,
    lookupIn,
    patternComponents,
    matchPattern,
    matchPatternWithin,
    loadPattern,
    witnesses,
    vertexValue,
    concreteRoutes,
    targets,
    selectedRoutes,
    firstSelectedRoute,
    routeLastEdges,
    firstWitnessKey,
    concreteEdge,
    stepEdges,
  )
where

import Data.Map.Strict qualified as Map
import Models.Connect
import Models.Edge
import Models.Graph (Graph' (..))
import Models.NID
import Models.Node
import Models.PathPattern
import MyPrelude

-- | A connected group of variables and the edges touching them. An edge
-- between two bound vertices is a component without variables.
data Component = Component
  { vars :: Set VarId,
    -- | in order of first appearance along the routes
    edges :: [EdgeId]
  }
  deriving stock (Eq, Ord, Show)

-- | One way of satisfying one component.
data Partial = Partial
  { nodes :: Map VarId NID,
    labels :: Map EdgeId Text
  }
  deriving stock (Eq, Ord, Show)

-- | The matches of one alternative. The witnesses are the cartesian product
-- of the component lists, none of which is empty.
data Resolution = Resolution
  { patternPath :: PatternPath,
    current :: Maybe NID,
    components :: [(Component, [Partial])]
  }
  deriving stock (Eq, Show)

-- | A complete match.
data Witness = Witness
  { nodes :: Map VarId NID,
    labels :: Map EdgeId Text
  }
  deriving stock (Eq, Ord, Show)

-- | A traversed edge: its direction of traversal, label and the node reached.
data ConcreteHop = ConcreteHop
  { direction :: Direction,
    label :: Text,
    target :: NID
  }
  deriving stock (Eq, Ord, Show)

data ConcreteRoute = ConcreteRoute {root :: NID, hops :: [ConcreteHop]}
  deriving stock (Eq, Ord, Show)

-- | Access to nodes of the graph being matched against.
type Lookup = NID -> Maybe (Node Text ())

lookupIn :: forall a. Graph' NID Text a -> Lookup
lookupIn g n =
  void <$> Map.lookup n (g.nodeMap :: Map NID (Node' NID Text Text a))

-- * Components

patternComponents :: PatternPath -> [Component]
patternComponents p =
  sortOn (\c -> headMay c.edges >>= (`lookup` edgeIndex)) $
    map finish (toList grouped)
  where
    specs = patternEdges p
    order = edgeOrder p
    edgeIndex :: Map EdgeId Int
    edgeIndex = mapFromList (zip order [0 :: Int ..])
    -- union-find over variables linked by an edge between two variables
    parents :: Map VarId VarId
    parents = foldl' linkVars mempty (toList specs)
    linkVars ps spec = case (spec.source, spec.target) of
      (Var a, Var b) ->
        let ra = findRoot ps a
            rb = findRoot ps b
         in if ra == rb then ps else insertMap (max ra rb) (min ra rb) ps
      _ -> ps
    findRoot ps v = maybe v (findRoot ps) (lookup v ps)
    keyOf e spec = case (spec.source, spec.target) of
      (Var a, _) -> Left (findRoot parents a)
      (_, Var b) -> Left (findRoot parents b)
      _ -> Right e
    grouped :: Map (Either VarId EdgeId) [EdgeId]
    grouped =
      Map.fromListWith
        (flip (++))
        [(keyOf e spec, [e]) | e <- order, Just spec <- [lookup e specs]]
    finish es =
      Component
        { vars =
            setFromList
              [ v
              | e <- es,
                Just spec <- [lookup e specs],
                Var v <- [spec.source, spec.target]
              ],
          edges = es
        }

-- * Matching

-- | Match a pattern. 'Nothing' if it has no match.
matchPattern :: Lookup -> Maybe NID -> PatternPath -> Maybe Resolution
matchPattern = matchPatternWithin mempty

-- | Like 'matchPattern', restricting some variables to candidate sets
-- (typically computed while loading).
matchPatternWithin ::
  Map VarId (Set NID) ->
  Lookup ->
  Maybe NID ->
  PatternPath ->
  Maybe Resolution
matchPatternWithin candidates lookupNode cur p = do
  let bound = [v | v <- patternVertices p, not (isVar v)]
  boundValues <- traverse (\v -> (v,) <$> boundValue v) bound
  let values = mapFromList boundValues :: Map PVertex NID
  guard $ all (\(v, n) -> testAccepts (vertexTest p v) n) boundValues
  guard $ all (isJust . lookupNode . snd) boundValues
  let solve c = (c, solveComponent values c)
      comps = map solve (patternComponents p)
  guard $ all (not . null . snd) comps
  pure Resolution {patternPath = p, current = cur, components = comps}
  where
    isVar = \case
      Var _ -> True
      _ -> False
    boundValue = \case
      Current -> cur
      Fixed n -> Just n
      Var _ -> Nothing
    specs = patternEdges p
    solveComponent values c = go c.edges (Partial mempty mempty)
      where
        valueOf partial = \case
          Var v -> lookup v partial.nodes
          v -> lookup v values
        go [] partial = [partial]
        go (e : es) partial = case lookup e specs of
          Nothing -> error "matchPattern: unknown edge"
          Just spec -> do
            source <- case valueOf partial spec.source of
              Just s -> [s]
              Nothing -> error "matchPattern: edge source is unbound"
            (label, reached) <- stepEdges lookupNode source spec
            partial' <- case spec.target of
              Var v -> case lookup v partial.nodes of
                Just existing -> [partial | existing == reached]
                Nothing -> do
                  guard (testAccepts (vertexTest p spec.target) reached)
                  guard (maybe True (member reached) (lookup v candidates))
                  [Partial (insertMap v reached partial.nodes) partial.labels]
              bound -> [partial | lookup bound values == Just reached]
            go es (Partial partial'.nodes (insertMap e label partial'.labels))

-- | Propagate candidate sets along the pattern edges until a fixpoint,
-- reading every node that is a candidate source of an edge. 'Nothing' if
-- some vertex has no candidate left.
loadPattern ::
  (Monad m) =>
  (NID -> m (Maybe (Node Text ()))) ->
  NID ->
  PatternPath ->
  m (Maybe (Map VarId (Set NID), Map NID (Node Text ())))
loadPattern getNode cur p = do
  let initial :: Map PVertex (Set NID)
      initial =
        mapFromList
          [(v, singletonSet n) | v <- patternVertices p, Just n <- [bound v]]
  nodes0 <- foldlM load mempty (concatMap toList initial)
  let loop cands nodes = do
        (cands', nodes') <- forward cands nodes
        let cands'' = backward nodes' cands'
        if
          | any null cands'' -> pure Nothing
          | cands'' == cands -> pure (Just (cands'', nodes'))
          | otherwise -> loop cands'' nodes'
  loop initial nodes0 <&> fmap \(cands, nodes) ->
    (mapFromList [(v, s) | (Var v, s) <- mapToList cands], nodes)
  where
    bound = \case
      Current -> Just cur
      Fixed n -> Just n
      Var _ -> Nothing
    ordered = [s | e <- edgeOrder p, Just s <- [lookup e (patternEdges p)]]
    load nodes n
      | n `member` nodes = pure nodes
      | otherwise =
          getNode n <&> \case
            Nothing -> nodes
            Just node -> insertMap n node nodes
    forward cands0 nodes0 =
      foldlM
        ( \(cands, nodes) spec -> do
            let sources = findWithDefault mempty spec.source cands
            nodes' <- foldlM load nodes (toList sources)
            let reached =
                  setFromList
                    [ t
                    | s <- toList sources,
                      (_, t) <- stepEdges (`lookup` nodes') s spec,
                      testAccepts (vertexTest p spec.target) t
                    ]
                narrowed = case lookup spec.target cands of
                  Nothing -> reached
                  Just existing -> intersection existing reached
            pure (insertMap spec.target narrowed cands, nodes')
        )
        (cands0, nodes0)
        ordered
    backward nodes cands0 =
      foldl'
        ( \cands spec ->
            let targetsOk = findWithDefault mempty spec.target cands
                keep s =
                  any
                    ((`member` targetsOk) . snd)
                    (stepEdges (`lookup` nodes) s spec)
             in adjustMap (filterSet keep) spec.source cands
        )
        cands0
        (reverse ordered)

-- | The edges a pattern edge can take from a node, in label/node order.
stepEdges :: Lookup -> NID -> EdgeSpec -> [(Text, NID)]
stepEdges lookupNode source spec = case lookupNode source of
  Nothing -> []
  Just n ->
    [ (c.transition, c.node)
    | c <- toList case spec.direction of
        Out -> n.outgoing
        In -> n.incoming,
      matcherAccepts spec.matcher c.transition
    ]

-- * Witnesses and projections

-- | All complete matches, component-major. The first is the
-- lexicographically smallest by concrete routes.
witnesses :: Resolution -> [Witness]
witnesses r = combine (map snd r.components)

combine :: [[Partial]] -> [Witness]
combine = foldr step [Witness mempty mempty]
  where
    step partials rest =
      [ Witness (p.nodes <> w.nodes) (p.labels <> w.labels)
      | p <- partials,
        w <- rest
      ]

vertexValue :: Resolution -> Witness -> PVertex -> NID
vertexValue r w = \case
  Current -> fromMaybe (error "vertexValue: no current node") r.current
  Fixed n -> n
  Var v -> fromMaybe (error "vertexValue: unbound variable") (lookup v w.nodes)

concreteRoute :: Resolution -> Witness -> Route -> ConcreteRoute
concreteRoute r w rt =
  ConcreteRoute
    (vertexValue r w rt.root)
    [ ConcreteHop h.direction (label h.edge) (vertexValue r w h.target)
    | h <- rt.hops
    ]
  where
    label e = fromMaybe (error "concreteRoute: unlabelled edge") (lookup e w.labels)

concreteRoutes :: Resolution -> Witness -> [ConcreteRoute]
concreteRoutes r w = map (concreteRoute r w) (toList r.patternPath.routes)

-- | The witnesses of only the components the given edges belong to.
witnessesFor :: [EdgeId] -> Resolution -> [Witness]
witnessesFor es r =
  combine [ps | (c, ps) <- r.components, any (`elem` c.edges) es]

routeEndValues :: Resolution -> Route -> Set NID
routeEndValues r rt = case routeEnd rt of
  Var v ->
    setFromList
      [ n
      | (c, ps) <- r.components,
        v `member` c.vars,
        p <- ps,
        Just n <- [lookup v p.nodes]
      ]
  bound -> singletonSet (vertexValue r (Witness mempty mempty) bound)

-- | Every node an output can be.
targets :: Resolution -> Set NID
targets r = foldMap (routeEndValues r) r.patternPath.routes

-- | Every concrete version of route 0, in witness order.
selectedRoutes :: Resolution -> [ConcreteRoute]
selectedRoutes r =
  ordNub [concreteRoute r w selected | w <- witnessesFor edges r]
  where
    selected :| _ = r.patternPath.routes
    edges = map (.edge) selected.hops

firstSelectedRoute :: Resolution -> ConcreteRoute
firstSelectedRoute r = case selectedRoutes r of
  selected : _ -> selected
  [] -> error "firstSelectedRoute: a resolution always has a witness"

-- | The possible last edges of each route with at least one hop, in stored
-- orientation.
routeLastEdges :: Resolution -> Map Int (Set (Edge Text))
routeLastEdges r =
  mapFromList
    [ (i, setFromList [lastEdge w | w <- witnessesFor [h.edge] r])
    | (i, rt) <- zip [0 ..] (toList r.patternPath.routes),
      Just h <- [lastMay rt.hops],
      let lastEdge w = case lastMay (concreteRoute r w rt).hops of
            Just ch -> concreteEdge (penultimate r w rt) ch
            Nothing -> error "routeLastEdges: unreachable"
    ]
  where
    penultimate res w rt = case reverse rt.hops of
      _ : before : _ -> vertexValue res w before.target
      _ -> vertexValue res w rt.root

-- | The stored edge for a hop taken from a node.
concreteEdge :: NID -> ConcreteHop -> Edge Text
concreteEdge origin h = case h.direction of
  Out -> Edge origin h.label h.target
  In -> Edge h.target h.label origin

-- | Orders alternatives by their smallest witness.
firstWitnessKey :: Resolution -> [ConcreteRoute]
firstWitnessKey r = case witnesses r of
  w : _ -> concreteRoutes r w
  [] -> []
