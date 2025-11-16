module Graph.Complete where

import Graph.GraphMetadataEditing
import Models.Connect (Connect (..), matchConnect)
import Models.Graph (Graph, emptyGraph)
import Models.NID
import Models.Node
import Models.NormalizedPath
import Models.Path.Simple
import Models.ResolvedPath
import MyPrelude hiding ((\\))
import Polysemy.State

-- | Traverse a path, creating any transitions/nodes that are missing.
--
-- This overapproximates the transitions to create. For example given a path
-- like `(a & b)/(c & d)` in a graph that already contains the paths
-- `a/@1/c/@3` and `b/@2/d/@3`. It would be fine to not create any new nodes
-- since based on the semantics of unanchored paths this already satisfies the
-- path completely. However, we use `leastNodesNormalizedPath` which treats
-- unanchored paths as join points, meaning we would create new edges `b/@1`,
-- `a/@2`, `@1/d/@3`, and `@2/c/@3`.
--
-- We do this since this is typically better behavior when creating brand new
-- paths that don't already match any part of the graph.
--
-- If the overapproximation is undesireable you can call `resolveNPath` using
-- what ever interpretation of unanchored nodes you want.
--
-- TODO: I may want to change this to a slightly better default where we
-- effectively use leastConstrainedNormalizedPath for unanchored nodes when
-- possible to resolve within the graph but fall back to
-- leastNodesNormalizedPath when we need to create new nodes. This requires a
-- function that converts unanchored nodes based on the current state of the
-- graph though.
completePath ::
  forall r.
  ( Members '[GraphMetadataReading] r,
    HasCallStack
  ) =>
  NID ->
  Path ->
  Sem r ResolvedPath
completePath nid path = undefined
