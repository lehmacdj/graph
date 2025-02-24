module Graph.ResolvePathSpec where

import Graph.ResolvePath
import Models.Graph
import Models.Node
import Models.NID
import TestPrelude
import Graph.GraphMetadataEditing (GraphMetadataEditing, runInMemoryGraphMetadataEditing)
import Polysemy.State
import Models.Path

-- | Graph that has the following structure:
-- @0:
-- - a, e, f -> @1
--  - b, c -> @2
--  - b -> @0
-- - d, e -> @2
-- - e, f -> @0
testGraph :: Graph Text ()
testGraph =
  insertEdge (edge' 1 "one-to-two" 2)
    . insertEdge (edge' 1 "one-to-others" 2)
    . insertEdge (edge' 1 "one-to-others" 0)
    . insertEdge (edge' 0 "zero-to-one" 1)
    . insertEdge (edge' 0 "zero-to-two" 2)
    . insertEdge (edge' 0 "zero-to-all" 0)
    . insertEdge (edge' 0 "zero-to-all" 1)
    . insertEdge (edge' 0 "zero-to-all" 2)
    . insertEdge (edge' 0 "zero-to-zero" 0)
    . insertNode (emptyNode (smallNID 2))
    . insertNode (emptyNode (smallNID 1))
    . insertNode (emptyNode (smallNID 0))
    $ emptyGraph

runWithTestGraph :: Sem '[GraphMetadataEditing Text] a -> a
runWithTestGraph =
  run . evalState testGraph . runInMemoryGraphMetadataEditing . raiseUnder

spec_materializePathAsGraph :: Spec
spec_materializePathAsGraph = do
  let materializesPath ::
        NID -> Path Text -> Graph Text (MaterializedPathInfo Text) -> Spec
      materializesPath from p expected =
        it ("materialize " ++ show p ++ " from " ++ show from) do
         runWithTestGraph (materializePathAsGraph from p) `shouldBe` Just expected
  let materializesTo :: Path Text -> [(Int, Bool)] -> Spec
      materializesTo p (asMap . mapFromList . over (traverse . _1) smallNID -> m) =
        materializesPath (smallNID 0) p g where
          f n = mempty @(MaterializedPathInfo Text) &
            #isTarget .~ (m ^. at n.nid . non False)
          g = traceShowId . mapGraph f $
            additiveFilterGraph (\n -> has (ix n.nid) m) testGraph
  One `materializesTo` [(0, True)]
  Zero `materializesTo` [(0, False)]
  Literal "zero-to-one" `materializesTo` [(0, False), (1, True)]
  (Literal "zero-to-one" :/ Literal "one-to-others")
    `materializesTo` [(0, True), (1, False), (2, True)]
  (Literal "zero-to-one" :/ Literal "one-to-two")
    `materializesTo` [(0, False), (1, False), (2, True)]
  (Literal "zero-to-two" :+ Literal "zero-to-one")
    `materializesTo` [(0, False), (1, True), (2, True)]
  (Literal "zero-to-all" :& (Literal "zero-to-zero" :+ Literal "zero-to-one"))
    `materializesTo` [(0, True), (1, True), (2, False)]
