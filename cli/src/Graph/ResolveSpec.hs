-- | Tests for 'Graph.Resolve.resolveNPath'. These live in a separate module
-- (rather than in-file in "Graph.Resolve") because importing "Utils.Testing"
-- there would pull the Objective-C bindings into the dependency closure of
-- downstream TemplateHaskell-using modules, breaking TH linking on macOS.
module Graph.ResolveSpec where

import Graph.GraphMetadataEditing (runInMemoryGraphMetadataEditing)
import Graph.Resolve (resolveNPath, resolveNPathExists)
import Models.Edge (Edge)
import Models.Graph (Graph, compactGraphRepresentation, deleteEdges)
import Models.NormalizedPath
import Models.NormalizedPath.Generators
  ( genDeterministicPathLiteralOnly,
    shallowGenConfig,
  )
import Models.NormalizedPath.Parse (pNormalizedPath)
import Models.NormalizedPath.TestUtils
  ( coveredPathFrom,
    graphEdges,
    graphResolvingPathFrom,
  )
import Models.ResolvedPath (ResolvedPath (..))
import MyPrelude
import Polysemy.State (evalState)
import System.Random (mkStdGen)
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

      resolvesInFast :: Graph Text () -> NormalizedPath Anchor -> Bool
      resolvesInFast graph path =
        resolveNPathExists
          (smallNID 1)
          (leastConstrainedNormalizedPath path)
          & runInMemoryGraphMetadataEditing
          & evalState graph
          & run

      checkResolves :: Bool -> [Edge Text] -> Text -> Spec
      checkResolves expected edges pathStr =
        it (unpack testName) do
          path <- parseForTest "normalized path" pNormalizedPath pathStr
          let graph = graphWithEdges edges
          resolvesIn graph path `shouldBe` expected
          resolvesInFast graph path `shouldBe` expected
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

  describe "existence-only resolution" do
    checkResolves True [edge' 1 "a" 2, edge' 2 "b" 1] "@[a /| b]"
    checkResolves False [edge' 1 "a" 2, edge' 2 "b" 3] "@[a /| b]"
    checkResolves
      True
      [edge' 1 "a" 2, edge' 1 "b" 2, edge' 2 "c" 3]
      "[(a & b) /| c]>@3"
    checkResolves
      False
      [edge' 1 "a" 2, edge' 1 "b" 3, edge' 2 "c" 4, edge' 3 "c" 5]
      "[(a & b) /| c]>@"
    checkResolves True [edge' 1 "a" 2] "[missing] + [a]"

    it "matches resolveNPath on generated shallow paths" $
      property do
        seed <- arbitrary
        let start = smallNID 1
        dp <-
          coveredPathFrom start
            <$> genDeterministicPathLiteralOnly shallowGenConfig
        let path = NormalizedPath (singletonSet dp)
            full = graphResolvingPathFrom (mkStdGen seed) start path
        pure $
          forAll (sublistOf (graphEdges full)) \removedEdges ->
            let partial = deleteEdges removedEdges full
             in counterexample
                  ( "path: "
                      <> show path
                      <> "\ngraph: "
                      <> show (graphEdges partial)
                  )
                  (resolvesInFast partial path === resolvesIn partial path)
