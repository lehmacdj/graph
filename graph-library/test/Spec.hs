module Main (main) where

import qualified Data.Set as Set
import Graph
import qualified Graph.Types.NID as BigNID
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
      completionTests,
      nidTests
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

nidTests :: TestTree
nidTests = testGroup "NID" [readShowNidTest]

readShowNidTest :: TestTree
readShowNidTest =
  testGroup
    "readShowNid"
    [ rs "zVjDcoKxrkuqtG1_VE0JF8_3zAIDpbm9",
      rs "00jD-oKxr-uqtG10VE0JF8_3zAKDkbm9",
      rs "00000000000000000000000000000000",
      rs "10000000000000000000000000000000",
      rs "00000000000000000000000000000001",
      rs "0000000000000000000000000000000g",
      rs "g0000000000000000000000000000000"
    ]
  where
    rs x = testCase x $ x @=? show (read x :: BigNID.NID)
