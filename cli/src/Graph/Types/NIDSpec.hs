module Graph.Types.NIDSpec where

import Graph.Types.NID
import TestPrelude
import Text.Read

test_readShowNid :: TestTree
test_readShowNid =
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
    rs x = testCase x $ x @=? show (read x :: NID)
