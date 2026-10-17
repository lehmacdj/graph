module Graph.GraphMetadataEditingSpec where

import Graph.GraphMetadataEditing
import Models.Connect
import Models.NID
import Models.Node
import MyPrelude
import Utils.Testing

-- | The cache of something that only reads the graph keeps each node as its
-- own file says, like the one of a transaction.
spec_cachingReadingInMemory :: Spec
spec_cachingReadingInMemory = do
  let n0 = smallNID 0
      n1 = smallNID 1
      -- only the node 1 records the edge 0 -b-> 1
      files :: Map NID (Node Text ())
      files =
        mapFromList
          [ (n0, emptyNode n0),
            (n1, emptyNode n1 & #incoming .~ singletonSet (Connect "b" n0))
          ]
      fromFiles :: Eff (GraphMetadataEditing : r) a -> Eff r a
      fromFiles = interpret \_ -> \case
        GetNodeMetadata nid -> pure (lookup nid files)
        _ -> error "the test only reads nodes"
      reading :: [NID] -> [Maybe (Node Text ())]
      reading order =
        runPureEff . fromFiles . cachingReadingInMemory @Text $
          traverse getNodeMetadata order
  it "answers as the files say after reading the other node of a damaged edge" do
    -- storing the node 0 used to take the edge out of what had been read
    -- of the node 1
    reading [n1, n0, n1] `shouldBe` map (`lookup` files) [n1, n0, n1]
    reading [n0, n1, n0] `shouldBe` map (`lookup` files) [n0, n1, n0]
  it "answers that a node without a file is missing, every time" $
    reading [smallNID 2, n1, smallNID 2]
      `shouldBe` [Nothing, lookup n1 files, Nothing]
