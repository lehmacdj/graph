module GraphSpec where

import qualified Data.ByteString.Lazy.Char8 as BS
import Graph'
import Graph.Node'
import Graph.Types
import Graph.Types.New
import TestPrelude

graphIsNodes :: Graph' -> [Node'] -> Assertion
graphIsNodes g ns = nodeMap' g @?= mapFromList [(nidOf n, n) | n <- ns]

-- | short function for emptyNode
ne :: NID -> Node'
ne = emptyNode

-- | short function for node with no data
nnd :: NID -> [Connect NID] -> [Connect NID] -> [UnlabledEdge] -> Node'
nnd nid i o r = Node' nid (setFromList i) (setFromList o) (setFromList r) Nothing

-- | short function for node with data
nd :: NID -> [Connect NID] -> [Connect NID] -> [UnlabledEdge] -> LByteString -> Node'
nd nid i o r d = Node' nid (setFromList i) (setFromList o) (setFromList r) (Just d)

fourNodesNoEdges :: (String, Graph')
fourNodesNoEdges = ("fourNodesNoEdges", graph)
  where
    graph = Graph' . mapFromList $ (\x -> (x, ne x)) <$> [0 .. 3]

twoNodesNoEdges :: (String, Graph')
twoNodesNoEdges = ("twoNodesNoEdges", graph)
  where
    graph = Graph' . mapFromList $ (\x -> (x, ne x)) <$> [0 .. 1]

test_insertNode :: TestTree
test_insertNode =
  testGroup
    "insertNode"
    [ test (ne 0) fourNodesNoEdges (ne <$> [0 .. 3]),
      test (ne 4) fourNodesNoEdges (ne <$> [0 .. 4]),
      -- inserting new incoming edge in node works
      test
        (nnd 2 [Connect 0 1] [] [])
        twoNodesNoEdges
        [ nnd 2 [Connect 0 1] [] [],
          nnd 1 [] [Connect 0 2] [],
          nnd 0 [] [] [UnlabledEdge 1 2]
        ],
      -- inserting new outgoing edge in node works
      test
        (nnd 2 [] [Connect 0 1] [])
        twoNodesNoEdges
        [ nnd 2 [] [Connect 0 1] [],
          nnd 1 [Connect 0 2] [] [],
          nnd 0 [] [] [UnlabledEdge 2 1]
        ],
      -- inserting new unlabled edge in node works
      test
        (nnd 2 [] [] [UnlabledEdge 0 1])
        twoNodesNoEdges
        [ nnd 2 [] [] [UnlabledEdge 0 1],
          nnd 1 [Connect 2 0] [] [],
          nnd 0 [] [Connect 2 1] []
        ]
    ]
  where
    test nodeToInsert (graphName, graph) result =
      testCase name $
        insertNode nodeToInsert graph `graphIsNodes` result
      where
        name = show nodeToInsert ++ " into " ++ graphName

unit_insertEdge_simple_into_fourNodesNoEdges :: Assertion
unit_insertEdge_simple_into_fourNodesNoEdges =
  insertEdge (Edge 0 1 2) (snd fourNodesNoEdges)
    `graphIsNodes` [ nnd 0 [] [Connect 1 2] [],
                     nnd 1 [] [] [UnlabledEdge 0 2],
                     nnd 2 [Connect 1 0] [] [],
                     nnd 3 [] [] []
                   ]

-- | graph where everything is connected to everything else via every thing
-- else including self edges of every kind
kn :: Int -> (String, Graph')
kn n = ("K" ++ show n, graph)
  where
    allConnects = [Connect y z | y <- [0 .. n - 1], z <- [0 .. n - 1]]
    graph =
      Graph' . mapFromList $
        [ ( x,
            nnd
              x
              allConnects
              allConnects
              [UnlabledEdge y z | y <- [0 .. n - 1], z <- [0 .. n - 1]]
          )
          | x <- [0 .. n - 1]
        ]

test_delNode' :: TestTree
test_delNode' =
  testGroup
    "delNode'"
    [ test 2 (kn 3) (nodesOf (snd (kn 2))),
      test 1 (kn 2) [nnd 0 [Connect 0 0] [Connect 0 0] [UnlabledEdge 0 0]]
    ]
  where
    test nidToDelete (graphName, graph) result =
      testCase name $
        delNode' nidToDelete graph `graphIsNodes` result
      where
        name = show nidToDelete ++ " from " ++ graphName

unit_delEdge_onlyEdge_from_K1 :: Assertion
unit_delEdge_onlyEdge_from_K1 =
  delEdge (Edge 0 0 0) (snd (kn 1))
    `graphIsNodes` [ne 0]

unit_setData'_toJust :: Assertion
unit_setData'_toJust =
  setData' (Just (BS.pack "asdf")) 0 (snd twoNodesNoEdges)
    `graphIsNodes` [nd 0 [] [] [] (BS.pack "asdf"), ne 1]

-- TODO: write these tests as well:
--
-- unit_filterGraph :: Assertion
-- unit_filterGraph = undefined
--
-- unit_mapGraph :: Assertion
-- unit_mapGraph = undefined
--
-- unit_dualizeGraph :: Assertion
-- unit_dualizeGraph = undefined
--
-- unit_nextFreeNodeId :: Assertion
-- unit_nextFreeNodeId = undefined
