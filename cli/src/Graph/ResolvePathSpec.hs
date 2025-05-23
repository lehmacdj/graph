{-# LANGUAGE CPP #-}

module Graph.ResolvePathSpec where

import Graph.GraphMetadataEditing (GraphMetadataEditing, runInMemoryGraphMetadataEditing)
import Graph.ResolvePath
import Models.Graph
import Models.NID
import Models.Node
import Models.Path
import Polysemy.State
import TestPrelude

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

data ResultExpectation = Target | Thin | Leftover (Path Text)
  deriving (Show, Eq, Ord, Generic)

shouldBeTarget :: Maybe [ResultExpectation] -> Bool
shouldBeTarget = maybe False (elem Target)

shouldBeThin :: Maybe [ResultExpectation] -> Bool
shouldBeThin = maybe True (elem Thin)

leftovers :: Maybe [ResultExpectation] -> Maybe (Path Text)
leftovers Nothing = Nothing
leftovers (Just es) =
  es
    & filter (has #_Leftover)
    & assertMaxOne
    & fmap \case
      Leftover p -> p
      _ -> error "impossible"

spec_materializePathAsGraph :: Spec
spec_materializePathAsGraph = do
  let materializesPath ::
        NID -> Path Text -> Graph Text (MaterializedPathInfo Text) -> Spec
      materializesPath from p expected =
        it ("materialize " ++ show p ++ " from " ++ show from) do
          runWithTestGraph (materializePathAsGraph from p) `shouldBe` expected
  let restrictToExpected ::
        Graph Text () ->
        [(Int, [ResultExpectation])] ->
        Graph Text (MaterializedPathInfo Text)
      restrictToExpected graph expectedTemplate =
        mapGraph expectedInfoEx
          $ (<> concat thinNodes)
          $ additiveFilterGraph (\n -> has (ix n.nid . filtered (notElem Thin)) infoMap) graph
        where
          infoMap = asMap . mapFromList . over (traverse . _1) smallNID $ expectedTemplate
          expectedInfoEx n =
            let info = infoMap ^. at n.nid
             in mempty @(MaterializedPathInfo Text)
                  & (#isTarget .~ shouldBeTarget info)
                  & (#isThin .~ shouldBeThin info)
                  & (#leftoverPath .~ leftovers info)
          thinNodes =
            (infoMap ^@.. (ifolded . filtered (elem Thin)))
              & map (singletonGraph . emptyNode . fst)
  let materializesTo :: Path Text -> [(Int, [ResultExpectation])] -> Spec
      materializesTo p expectedTemplate =
        materializesPath
          (smallNID 0)
          p
          (testGraph `restrictToExpected` expectedTemplate)
  One `materializesTo` [(0, [Target, Thin])]
  Zero `materializesTo` []
  Literal "zero-to-one" `materializesTo` [(0, []), (1, [Target, Thin])]
  (Literal "zero-to-one" :/ Literal "one-to-others")
    `materializesTo` [(0, [Target]), (1, []), (2, [Target, Thin])]
  (Literal "zero-to-one" :/ Literal "one-to-two")
    `materializesTo` [(0, []), (1, []), (2, [Target, Thin])]
  (Literal "zero-to-two" :+ Literal "zero-to-one")
    `materializesTo` [(0, []), (1, [Target, Thin]), (2, [Target, Thin])]
  (Literal "zero-to-all" :& (Literal "zero-to-zero" :+ Literal "zero-to-one"))
    `materializesTo` [(0, [Target]), (1, [Target, Thin]), (2, [Thin])]
  Wild `materializesTo` [(0, [Target]), (1, [Target, Thin]), (2, [Target, Thin])]
  Literal "non-existent" `materializesTo` [(0, [Leftover (Literal "non-existent")])]
  (Literal "zero-to-one" :/ Literal "non-existent")
    `materializesTo` [(0, []), (1, [Leftover (Literal "non-existent")])]
  (Literal "zero-to-one" :/ (Literal "one-to-two" :/ Literal "non-existent"))
    `materializesTo` [(0, []), (1, []), (2, [Leftover (Literal "non-existent")])]
  (Literal "non-existent-1" :+ Literal "non-existent-2")
    `materializesTo` [(0, [Leftover (Literal "non-existent-1" :+ Literal "non-existent-2")])]
  (Literal "non-existent-1" :/ Literal "non-existent-2")
    `materializesTo` [(0, [Leftover (Literal "non-existent-1" :/ Literal "non-existent-2")])]
