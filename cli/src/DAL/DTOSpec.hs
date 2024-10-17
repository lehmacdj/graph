module DAL.DTOSpec where

import DAL.DTO
import Graph.Types
import TestPrelude

test_nodeDTO_backwardsCompat :: TestTree
test_nodeDTO_backwardsCompat =
  (NodeDTO (smallNID 0) (setFromList []) (setFromList []) :: NodeDTO String)
    `representedByJson` "{\"id\":\"000000000000\",\"incoming\":[],\"outgoing\":[]}"

test_connectDTO_backwardsCompat :: TestTree
test_connectDTO_backwardsCompat =
  ConnectDTO ("foo" :: String) (smallNID 0)
    `representedByJson` "{\"t\":\"foo\",\"n\":\"000000000000\"}"
