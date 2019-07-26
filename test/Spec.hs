import Test.Tasty
import Test.Tasty.HUnit

import qualified Data.Set as Set

import Graph
import Lang.Path

main :: IO ()
main = defaultMain allTests

testGraph :: Graph String
testGraph =
  insertEdge (Edge 1 "c" 2)
  . insertEdge (Edge 1 "b" 2)
  . insertEdge (Edge 1 "b" 0)
  . insertEdge (Edge 0 "a" 1)
  . insertNode (emptyNode 2)
  . insertNode (emptyNode 1)
  . insertNode (emptyNode 0)
  $ emptyGraph

allTests :: TestTree
allTests = testGroup "path tests"
  [ assert pathLit
  , assert pathLitNondet
  ] where
    resolvePath' = primed . resolvePath
    pathLit =
      Set.fromList
        [ DPath [FromVia 0 "a", FromVia 1 "b"] 0 []
        , DPath [FromVia 0 "a", FromVia 1 "b"] 2 []
        ] @=?
      resolvePath' (Literal "a" :/ Literal "b") 0 testGraph
    pathLitNondet =
      [DPath [FromVia 0 "a", FromVia 1 "c"] 2 []]
      @=?
      resolvePath' (Literal "a" :/ Literal "c") 0 testGraph
