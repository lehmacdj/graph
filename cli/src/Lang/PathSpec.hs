module Lang.PathSpec where

import qualified Data.Set.Ordered as OSet
import Lang.Path
import Models.Graph
import Models.NID
import Models.Node
import TestPrelude

testGraph :: Graph String ()
testGraph =
  insertEdge (edge' 1 "c" 2)
    . insertEdge (edge' 1 "b" 2)
    . insertEdge (edge' 1 "b" 0)
    . insertEdge (edge' 0 "a" 1)
    . insertEdge (edge' 0 "d" 2)
    . insertEdge (edge' 0 "e" 0)
    . insertEdge (edge' 0 "e" 1)
    . insertEdge (edge' 0 "e" 2)
    . insertEdge (edge' 0 "f" 0)
    . insertNode (emptyNode (smallNID 2))
    . insertNode (emptyNode (smallNID 1))
    . insertNode (emptyNode (smallNID 0))
    $ emptyGraph

fromVia :: Int -> a -> DPathComponent a
fromVia i = FromVia (smallNID i)

test_path :: TestTree
test_path =
  testGroup
    "path"
    [ testCase "pathLit" pathLit,
      testCase "pathLitNondet" pathLitNondet,
      testCase "pathSum" pathSum,
      testCase "pathIntersectSum" pathIntersectSum
    ]
  where
    resolvePath = unprimed . resolvePath'
    pathLit =
      OSet.fromList
        [ DPath (smallNID 0) [fromVia 0 "a", fromVia 1 "b"] (smallNID 0) [],
          DPath (smallNID 0) [fromVia 0 "a", fromVia 1 "b"] (smallNID 2) []
        ]
        @=? resolvePath (Literal "a" :/ Literal "b") (smallNID 0) testGraph
    pathLitNondet =
      OSet.fromList [DPath (smallNID 0) [fromVia 0 "a", fromVia 1 "c"] (smallNID 2) []]
        @=? resolvePath (Literal "a" :/ Literal "c") (smallNID 0) testGraph
    pathSum =
      OSet.fromList
        [ DPath (smallNID 0) [fromVia 0 "d"] (smallNID 2) [],
          DPath (smallNID 0) [fromVia 0 "a"] (smallNID 1) []
        ]
        @=? resolvePath (Literal "d" :+ Literal "a") (smallNID 0) testGraph
    pathIntersectSum =
      OSet.fromList
        [ DPath (smallNID 0) [fromVia 0 "e"] (smallNID 0) [],
          DPath (smallNID 0) [fromVia 0 "e"] (smallNID 1) []
        ]
        @=? resolvePath
          (Literal "e" :& (Literal "f" :+ Literal "a"))
          (smallNID 0)
          testGraph
