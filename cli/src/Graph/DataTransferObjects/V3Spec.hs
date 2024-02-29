module Graph.DataTransferObjects.V3Spec where

import Graph.DataTransferObjects.V3
import Graph.Types
import TestPrelude

test_nodeDTO_backwardsCompat :: TestTree
test_nodeDTO_backwardsCompat =
  NodeDTO (smallNID 0) (setFromList []) (setFromList []) (setFromList [])
    `representedByJson` "{\"id\":\"000000000000\",\"incoming\":[],\"outgoing\":[],\"referents\":[]}"

test_connectDTO_backwardsCompat :: TestTree
test_connectDTO_backwardsCompat =
  UnlabledEdgeDTO (smallNID 0) (smallNID 1)
    `representedByJson` "{\"s\":\"000000000000\",\"t\":\"000000000001\"}"
