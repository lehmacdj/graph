module Graph.SystemNodes.InitSpec where

import Effectful.State.Dynamic
import Error.UserError
import Graph.GraphMetadataEditing
import Graph.GraphMetadataEditingSpec
import Graph.SystemNodes
import Graph.SystemNodes.Init
import Models.Connect
import Models.Edge
import Models.NID
import Models.Node
import MyPrelude
import Utils.Testing

spec_createSystemNodes :: Spec
spec_createSystemNodes = do
  let -- what opening a graph with these node files says it added, and the
      -- node files it leaves
      opening :: NodeFiles -> Either String (SystemParts, NodeFiles)
      opening files =
        first show
          . runPureEff
          . runError @UserError
          . runStateLocal files
          $ transactionStoringIn ownRecords createSystemNodes
      new :: NodeFiles
      new = singletonMap nilNID (emptyNode nilNID)
      opened :: NodeFiles
      opened = either (error . unpack) snd (opening new)
      tags = Edge systemNodeNID "tags" tagsNID
      origin = Edge systemNodeNID "origin" nilNID
      withoutOutgoing, withoutIncoming :: Edge Text -> NodeFiles -> NodeFiles
      withoutOutgoing e = ix e.source . #outgoing %~ deleteSet (outConnect e)
      withoutIncoming e = ix e.sink . #incoming %~ deleteSet (inConnect e)
      nothing = SystemParts {nodes = [], edges = []}
  it "makes the system nodes of a new graph, and has nothing to say about it" do
    fst <$> opening new `shouldBe` Right nothing
    consistent opened `shouldBe` True
    lookup tagsNID opened
      `shouldBe` Just
        ( emptyNode tagsNID
            & #incoming .~ singletonSet (Connect "tags" systemNodeNID)
        )
  it "has nothing to say about a directory without any node file either" $
    fst <$> opening mempty `shouldBe` Right nothing
  it "says that it created them all in a graph that had nodes but not them" do
    -- a graph that an old ge made, or that lost them: the user sees that a
    -- command that perhaps only reads wrote ten files
    let n1 = smallNID 1
        old :: NodeFiles
        old =
          mapFromList
            [ (nilNID, emptyNode nilNID & #outgoing .~ singletonSet (Connect "a" n1)),
              (n1, emptyNode n1 & #incoming .~ singletonSet (Connect "a" nilNID))
            ]
    fst
      <$> opening old
        `shouldBe` Right
          nothing
            { nodes =
                [ systemNodeNID,
                  tagsNID,
                  importDatesNID,
                  fileHashesNID,
                  importUrlsNID,
                  historicalSystemNodesNID,
                  sequenceIDsNID,
                  mimeTypesNID,
                  fileExtensionsNID
                ]
            }
    -- also if the origin is the only node it has no file for
    fst
      <$> opening (deleteMap nilNID old)
        `shouldSatisfy` either (const False) ((nilNID `elem`) . (.nodes))
  it "changes nothing in a graph that has them" $
    opening opened `shouldBe` Right (nothing, opened)
  it "takes an edge that only one of its nodes records for there" do
    -- adding it would be an edit where the graph is damaged, which is
    -- refused: no command but fsck and fix could run
    let onlyOutgoing = withoutIncoming tags opened
        onlyIncoming = withoutOutgoing tags opened
    opening onlyOutgoing `shouldBe` Right (nothing, onlyOutgoing)
    opening onlyIncoming `shouldBe` Right (nothing, onlyIncoming)
  it "says which edge it added between two nodes that were there" do
    let unrecorded = withoutOutgoing origin (withoutIncoming origin opened)
    opening unrecorded
      `shouldBe` Right (nothing {edges = [origin]}, opened)
  it "says which node it created, which says that it added the edge to it" do
    let lost = deleteMap tagsNID (withoutOutgoing tags opened)
    opening lost `shouldBe` Right (nothing {nodes = [tagsNID]}, opened)
