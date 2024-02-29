module Graph.Types.NIDSpec where

import Graph.Types.NID
import TestPrelude

test_readShowNid :: TestTree
test_readShowNid =
  testGroup
    "readShowNid"
    [ rs "sTg8xuoC5KMj",
      rs "0AAyIwQolv54",
      rs "0AAyIwQolv54",
      rs "000000000000",
      rs "100000000000",
      rs "000000000001",
      rs "00000000000g",
      rs "g00000000000"
    ]
  where
    rs x = testCase x $ Just x @=? (show <$> (readMay x :: Maybe NID))
