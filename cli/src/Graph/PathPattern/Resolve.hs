-- | Resolve route patterns against the graph (docs/path-pattern-ir.md §4).
--
-- Resolution has an effectful load phase, which propagates candidate sets
-- from the bound vertices and reads each node it needs once, and a pure
-- matching phase over the loaded nodes.
module Graph.PathPattern.Resolve
  ( ResolvedPath (..),
    resolveQuery,
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
  traverse resolveOne (toList q.alternatives)
    & cachingReadingInState
    & runStateLocal emptyGraph
    & runStateLocal mempty
    & fmap \((rs, graph), nonexistentNodes) ->
      ResolvedPath
        { resolutions = sortOn firstWitnessKey (catMaybes rs),
          graph,
          nonexistentNodes
        }
  where
    resolveOne p = do
      loaded <- loadPattern cur p
      pure do
        (candidates, nodes) <- loaded
        matchPatternWithin candidates (`lookup` nodes) (Just cur) p

-- | Propagate candidate sets along the pattern edges until a fixpoint,
-- reading every node that is a candidate source of an edge. 'Nothing' if
-- some vertex has no candidate left.
loadPattern ::
  (GraphMetadataReading :> r) =>
  NID ->
  PatternPath ->
  Eff r (Maybe (Map VarId (Set NID), Map NID (Node Text ())))
loadPattern cur p = withEarlyReturn do
  let initial :: Map PVertex (Set NID)
      initial =
        mapFromList
          [(v, singletonSet n) | v <- patternVertices p, Just n <- [bound v]]
  nodes0 <- foldlM load mempty (concatMap toList initial)
  let loop cands nodes = do
        (cands', nodes') <- forward cands nodes
        let cands'' = backward nodes' cands'
        when (any null cands'') $ returnEarly Nothing
        if cands'' == cands then pure (cands'', nodes') else loop cands'' nodes'
  (cands, nodes) <- loop initial nodes0
  pure $ Just ([(v, s) | (Var v, s) <- mapToList cands] & mapFromList, nodes)
  where
    bound = \case
      Current -> Just cur
      Fixed n -> Just n
      Var _ -> Nothing
    ordered = [s | e <- edgeOrder p, Just s <- [lookup e (patternEdges p)]]
    load nodes n
      | n `member` nodes = pure nodes
      | otherwise =
          getNodeMetadata n <&> \case
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
