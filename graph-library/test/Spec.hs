import qualified Data.Set as Set
import Graph
import Lang.Path
import Lang.Path.Partial
import Test.Tasty
import Test.Tasty.HUnit

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
allTests =
  testGroup
    "graph"
    [ pathTests,
      completionTests
    ]

pathTests :: TestTree
pathTests =
  testGroup
    "path"
    [ testCase "pathLit" pathLit,
      testCase "pathLitNondet" pathLitNondet
    ]
  where
    resolvePath' = primed . resolvePath
    pathLit =
      Set.fromList
        [ DPath [FromVia 0 "a", FromVia 1 "b"] 0 [],
          DPath [FromVia 0 "a", FromVia 1 "b"] 2 []
        ]
        @=? resolvePath' (Literal "a" :/ Literal "b") 0 testGraph
    pathLitNondet =
      Set.fromList [DPath [FromVia 0 "a", FromVia 1 "c"] 2 []]
        @=? resolvePath' (Literal "a" :/ Literal "c") 0 testGraph

completionTests :: TestTree
completionTests = testGroup "completion" [takeRelevantFromEndTests]

takeRelevantFromEndTests :: TestTree
takeRelevantFromEndTests =
  testGroup
    "takeRelevantFromEnd"
    [ takeRelevantFromEnd' "a/(b + c" "a/ c",
      takeRelevantFromEnd' "a/(b + c/d" "a/ c/d",
      takeRelevantFromEnd' "(a + b)/e + (c + d)" " (c + d)",
      takeRelevantFromEnd' "(a + b)/(e + (c + d)" "(a + b)/ (c + d)",
      takeRelevantFromEnd' "a & b/" " b/",
      takeRelevantFromEnd' "a & b/(a + b & c/(d + e" " b/ c/ e"
    ]
  where
    takeRelevantFromEnd' i e = testCase i $ e @=? (takeRelevantFromEnd . reverse $ i)
