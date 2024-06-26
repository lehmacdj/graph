module GraphSpec where

import Control.Lens (filtered)
import Graph'
import Graph.Node'
import Graph.Types
import TestPrelude

graphOfNodes :: [Node'] -> Graph'
graphOfNodes ns = Graph' $ mapFromList [(nidOf n, n) | n <- ns]

graphIsNodes :: Graph' -> [Node'] -> Assertion
graphIsNodes g ns = g @?= graphOfNodes ns

-- | short function for emptyNode
ne :: Int -> Node'
ne = emptyNode . smallNID

-- | short function for node with no data
nnd :: Int -> [Connect NID] -> [Connect NID] -> [UnlabledEdge] -> Node'
nnd nid i o r = Node' (smallNID nid) (setFromList i) (setFromList o) (setFromList r) Nothing

-- | short function for node with data
nd :: Int -> [Connect NID] -> [Connect NID] -> [UnlabledEdge] -> ByteString -> Node'
nd nid i o r d = Node' (smallNID nid) (setFromList i) (setFromList o) (setFromList r) (Just d)

fourNodesNoEdges :: (String, Graph')
fourNodesNoEdges = ("fourNodesNoEdges", graph)
  where
    graph = Graph' . mapFromList $ (\x -> (smallNID x, ne x)) <$> [0 .. 3]

twoNodesNoEdges :: (String, Graph')
twoNodesNoEdges = ("twoNodesNoEdges", graph)
  where
    graph = Graph' . mapFromList $ (\x -> (smallNID x, ne x)) <$> [0 .. 1]

-- | graph where everything is connected to everything else via every thing
-- else including self edges of every kind
kn :: Int -> (String, Graph')
kn n = ("K" ++ show n, graph)
  where
    allConnects = [connect y z | y <- [0 .. n - 1], z <- [0 .. n - 1]]
    graph =
      Graph' . mapFromList $
        [ ( smallNID x,
            nnd
              x
              allConnects
              allConnects
              [unlabledEdge y z | y <- [0 .. n - 1], z <- [0 .. n - 1]]
          )
          | x <- [0 .. n - 1]
        ]

helloWorldGraph :: (String, Graph')
helloWorldGraph = ("helloWorldGraph", graph)
  where
    graph =
      graphOfNodes
        [ nnd 0 [] [connect 1 2] [],
          nd 1 [] [] [unlabledEdge 0 2] "Hello",
          nnd 2 [connect 1 0] [connect 3 4] [],
          nd 3 [] [] [unlabledEdge 2 4] "world!",
          nnd 4 [connect 3 2] [] []
        ]

test_insertNode :: TestTree
test_insertNode =
  testGroup
    "insertNode"
    [ test (ne 0) fourNodesNoEdges (ne <$> [0 .. 3]),
      test (ne 4) fourNodesNoEdges (ne <$> [0 .. 4]),
      -- inserting new incoming edge in node works
      test
        (nnd 2 [connect 0 1] [] [])
        twoNodesNoEdges
        [ nnd 2 [connect 0 1] [] [],
          nnd 1 [] [connect 0 2] [],
          nnd 0 [] [] [unlabledEdge 1 2]
        ],
      -- inserting new outgoing edge in node works
      test
        (nnd 2 [] [connect 0 1] [])
        twoNodesNoEdges
        [ nnd 2 [] [connect 0 1] [],
          nnd 1 [connect 0 2] [] [],
          nnd 0 [] [] [unlabledEdge 2 1]
        ],
      -- inserting new unlabled edge in node works
      test
        (nnd 2 [] [] [unlabledEdge 0 1])
        twoNodesNoEdges
        [ nnd 2 [] [] [unlabledEdge 0 1],
          nnd 1 [connect 2 0] [] [],
          nnd 0 [] [connect 2 1] []
        ],
      -- inserting a existing node with no edges into a graph with edges should
      -- do nothing
      test
        (ne 0)
        (kn 4)
        (nodesOf (snd (kn 4))),
      -- not having data can't override existing data
      test
        (ne 1)
        helloWorldGraph
        (nodesOf (snd helloWorldGraph)),
      -- having data overrides lack of data
      test
        (nd 0 [] [] [] "foobar")
        helloWorldGraph
        ( set
            (traverse . filtered ((== smallNID 0) . nidOf) . #associatedData')
            (Just "foobar")
            $ nodesOf (snd helloWorldGraph)
        ),
      -- inserting a node with updated data is the same as just updating that
      -- data
      test
        (nd 1 [] [] [] "HELLO")
        helloWorldGraph
        ( set
            (traverse . filtered ((== smallNID 1) . nidOf) . #associatedData')
            (Just "HELLO")
            $ nodesOf (snd helloWorldGraph)
        ),
      -- not updating data does nothing
      test
        (ne 1)
        helloWorldGraph
        (nodesOf (snd helloWorldGraph)),
      -- an new incoming edge updates other nodes properly
      test
        (nnd 0 [connect 1 2] [] [])
        fourNodesNoEdges
        [ nnd 0 [connect 1 2] [] [],
          nnd 1 [] [] [unlabledEdge 2 0],
          nnd 2 [] [connect 1 0] [],
          ne 3
        ],
      -- an new outgoing edge updates other nodes properly
      test
        (nnd 0 [] [connect 1 2] [])
        fourNodesNoEdges
        [ nnd 0 [] [connect 1 2] [],
          nnd 1 [] [] [unlabledEdge 0 2],
          nnd 2 [connect 1 0] [] [],
          ne 3
        ],
      -- an new referent edge updates other nodes properly
      test
        (nnd 0 [] [] [unlabledEdge 1 2])
        fourNodesNoEdges
        [ nnd 0 [] [] [unlabledEdge 1 2],
          nnd 1 [] [connect 0 2] [],
          nnd 2 [connect 0 1] [] [],
          ne 3
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
  insertEdge (edge 0 1 2) (snd fourNodesNoEdges)
    `graphIsNodes` [ nnd 0 [] [connect 1 2] [],
                     nnd 1 [] [] [unlabledEdge 0 2],
                     nnd 2 [connect 1 0] [] [],
                     nnd 3 [] [] []
                   ]

unit_insertEdge_dangling :: Assertion
unit_insertEdge_dangling =
  insertEdge (edge 0 1 2) emptyGraph
    `graphIsNodes` [ nnd 0 [] [connect 1 2] [],
                     nnd 1 [] [] [unlabledEdge 0 2],
                     nnd 2 [connect 1 0] [] []
                   ]

test_delNode :: TestTree
test_delNode =
  testGroup
    "delNode"
    [ test 2 (kn 3) (nodesOf (snd (kn 2))),
      test 1 (kn 2) [nnd 0 [connect 0 0] [connect 0 0] [unlabledEdge 0 0]]
    ]
  where
    test nidToDelete (graphName, graph) result =
      testCase name $
        delNode (smallNID nidToDelete) graph `graphIsNodes` result
      where
        name = show nidToDelete ++ " from " ++ graphName

unit_delEdge_onlyEdge_from_K1 :: Assertion
unit_delEdge_onlyEdge_from_K1 =
  delEdge (edge 0 0 0) (snd (kn 1))
    `graphIsNodes` [ne 0]

unit_setData_toJust :: Assertion
unit_setData_toJust =
  setData (Just "asdf") (smallNID 0) (snd twoNodesNoEdges)
    `graphIsNodes` [nd 0 [] [] [] "asdf", ne 1]

unit_setData_notInGraph :: Assertion
unit_setData_notInGraph =
  setData Nothing (smallNID 2) (snd twoNodesNoEdges)
    `graphIsNodes` nodesOf (snd twoNodesNoEdges)

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
