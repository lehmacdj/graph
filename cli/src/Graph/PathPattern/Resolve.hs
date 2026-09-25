{-# LANGUAGE MultiWayIf #-}

-- | Resolve route patterns against the graph (docs/path-pattern-ir.md §4).
--
-- Resolution has an effectful load phase, which propagates candidate sets
-- from the bound vertices and reads each node it needs once, and a pure
-- matching phase over the loaded nodes.
module Graph.PathPattern.Resolve
  ( ResolvedPath (..),
    resolveQuery,
    resolveQueryWith,
    resolvePath,
    loadPattern,
  )
where

import Effectful.State.Dynamic
import Graph.GraphMetadataEditing
import Models.Augmentation.IsThin
import Models.Graph (Graph, emptyGraph)
import Models.NID
import Models.Node
import Models.Path.Simple (Path)
import Models.PathPattern
import Models.PathPattern.Compile
import Models.PathPattern.Match
import MyPrelude

data ResolvedPath = ResolvedPath
  { -- | one per matching alternative, ordered by smallest witness
    resolutions :: [Resolution],
    graph :: Graph Text IsThin,
    nonexistentNodes :: Set NID
  }
  deriving stock (Eq, Show, Generic)

resolvePath ::
  (GraphMetadataReading :> r, HasCallStack) =>
  NID ->
  Path ->
  Eff r ResolvedPath
resolvePath cur = resolveQuery cur . compilePath

resolveQuery ::
  (GraphMetadataReading :> r, HasCallStack) =>
  NID ->
  PathQuery ->
  Eff r ResolvedPath
resolveQuery cur q =
  resolveQueryWith getNodeMetadata cur q
    & cachingReadingInState
    & runStateLocal emptyGraph
    & runStateLocal mempty
    & fmap \((resolutions, graph), nonexistentNodes) ->
      ResolvedPath {resolutions, graph, nonexistentNodes}

-- | Resolve with any way of reading nodes. Resolutions are ordered by their
-- smallest witness.
resolveQueryWith ::
  (Monad m) =>
  (NID -> m (Maybe (Node Text ()))) ->
  NID ->
  PathQuery ->
  m [Resolution]
resolveQueryWith lookupNode cur q =
  sortOn firstWitnessKey . catMaybes
    <$> traverse resolveOne (toList q.alternatives)
  where
    resolveOne p = do
      loaded <- loadPattern lookupNode cur p
      pure do
        (candidates, nodes) <- loaded
        matchPatternWithin candidates (`lookup` nodes) (Just cur) p

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
