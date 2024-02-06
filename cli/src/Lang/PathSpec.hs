module Lang.PathSpec where

import qualified Data.Set as Set
import Graph
import Lang.Path
import TestPrelude

testGraph :: Graph String
testGraph =
  insertEdge (edge' 1 "c" 2)
    . insertEdge (edge' 1 "b" 2)
    . insertEdge (edge' 1 "b" 0)
    . insertEdge (edge' 0 "a" 1)
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
      testCase "pathLitNondet" pathLitNondet
    ]
  where
    resolvePath' = primed . resolvePath
    pathLit =
      Set.fromList
        [ DPath (smallNID 0) [fromVia 0 "a", fromVia 1 "b"] (smallNID 0) [],
          DPath (smallNID 0) [fromVia 0 "a", fromVia 1 "b"] (smallNID 2) []
        ]
        @=? resolvePath' (Literal "a" :/ Literal "b") (smallNID 0) testGraph
    pathLitNondet =
      Set.fromList [DPath (smallNID 0) [fromVia 0 "a", fromVia 1 "c"] (smallNID 2) []]
        @=? resolvePath' (Literal "a" :/ Literal "c") (smallNID 0) testGraph
