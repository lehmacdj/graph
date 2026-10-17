-- | Resolve route patterns against the graph (docs/path-route-ir.md, Resolution).
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
    & cachingReadingInState @Text
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
