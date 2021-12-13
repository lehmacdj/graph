module Graph.DataTransferObjectsSpec where

import Data.Aeson
import Graph.DataTransferObjects
import Graph.Types
import TestPrelude

representedByJson ::
  (ToJSON a, FromJSON a, Eq a, Show a) => a -> LByteString -> TestTree
representedByJson x j =
  testGroup
    (show x ++ " has correct json representation")
    [ testCase "serializes" $ encode x `shouldBe` j,
      testCase "deserializes" $ eitherDecode j `shouldBe` Right x
    ]

test_nodeDTO_backwardsCompat :: TestTree
test_nodeDTO_backwardsCompat =
  (NodeDTO (smallNID 0) (setFromList []) (setFromList []) :: NodeDTO String)
    `representedByJson` "{\"id\":0,\"incoming\":[],\"outgoing\":[]}"

test_connectDTO_backwardsCompat :: TestTree
test_connectDTO_backwardsCompat =
  ConnectDTO ("foo" :: String) (smallNID 0)
    `representedByJson` "{\"t\":\"foo\",\"n\":0}"
