-- | Tests for 'Graph.Resolve.resolveNPath'. These live in a separate module
-- (rather than in-file in "Graph.Resolve") because importing "Utils.Testing"
-- there would pull the Objective-C bindings into the dependency closure of
-- downstream TemplateHaskell-using modules, breaking TH linking on macOS.
module Graph.ResolveSpec where

import Graph.GraphMetadataEditing (runInMemoryGraphMetadataEditing)
import Graph.Resolve (resolveNPath)
import Models.Edge (Edge)
import Models.Graph (Graph, compactGraphRepresentation)
import Models.NormalizedPath
import Models.NormalizedPath.Parse (pNormalizedPath)
import Models.ResolvedPath (ResolvedPath (..))
import MyPrelude
import Polysemy.State (evalState)
import Utils.Testing

spec_resolveNPath :: Spec
spec_resolveNPath = do
  let resolvesIn :: Graph Text () -> NormalizedPath Anchor -> Bool
      resolvesIn graph path =
        resolveNPath (smallNID 1) (leastConstrainedNormalizedPath path)
          & runInMemoryGraphMetadataEditing
          & evalState graph
          & run
          & \(resolved :: ResolvedPath) -> not (null resolved.path.union)

      checkResolves :: Bool -> [Edge Text] -> Text -> Spec
      checkResolves expected edges pathStr =
        it (unpack testName) do
          path <- parseForTest "normalized path" pNormalizedPath pathStr
          resolvesIn (graphWithEdges edges) path `shouldBe` expected
        where
          testName =
            pathStr
              <> (if expected then " resolves in " else " doesn't resolve in ")
              <> compactGraphRepresentation (graphWithEdges edges)

  describe "intersections with multiple roots" do
    -- single root sanity checks
    checkResolves True [edge' 2 "a" 3] "[@2<a]>@"
    checkResolves True [edge' 2 "a" 3] "[@2<a]>@3"
    -- every root must resolve: here root @4 has no `d` edge at all, so the
    -- path must not resolve even though root @2 resolves on its own
    checkResolves False [edge' 2 "a" 3] "[@2<a & @4<d]>@"
    -- both roots resolve, but their landings (@3 vs @5) disagree at the
    -- JoinPoint target
    checkResolves False [edge' 2 "a" 3, edge' 4 "d" 5] "[@2<a & @4<d]>@"
    -- both roots resolve and agree on landing @3
    checkResolves True [edge' 2 "a" 3, edge' 4 "d" 3] "[@2<a & @4<d]>@"
    -- same with a Specific target
    checkResolves False [edge' 2 "a" 3] "[@2<a & @4<d]>@3"
    checkResolves False [edge' 2 "a" 3, edge' 4 "d" 5] "[@2<a & @4<d]>@3"
    checkResolves True [edge' 2 "a" 3, edge' 4 "d" 3] "[@2<a & @4<d]>@3"
