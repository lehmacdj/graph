module Graph.Types.NIDSpec where

import Graph.Types.NID
import TestPrelude

test_readShowNid :: TestTree
test_readShowNid =
  testGroup
    "readShowNid"
    [ rs "xzLrHWAnSX6ABWmV1LYesTg8xuoC5KMj",
      rs "R7lAAD962ORUUmtS91Gx0AAyIwQolv54",
      rs "00lAAD962ORUUmtS91Gx0AAyIwQolv54",
      rs "00000000000000000000000000000000",
      rs "10000000000000000000000000000000",
      rs "00000000000000000000000000000001",
      rs "0000000000000000000000000000000g",
      rs "g0000000000000000000000000000000"
    ]
  where
    rs x = testCase x $ Just x @=? (show <$> (readMay x :: Maybe NID))
