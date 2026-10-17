module Graph.DualizeableSpec where

import Effectful.State.Dynamic
import Graph.Dualizeable
import Graph.GraphMetadataEditing
import Models.Connect
import Models.Edge
import Models.Graph (Graph, emptyGraph)
import Models.NID
import Models.Node
import MyPrelude
import Utils.Testing

n1, n2 :: NID
n1 = smallNID 1
n2 = smallNID 2

spec_dualizingGraphMetadataEditing :: Spec
spec_dualizingGraphMetadataEditing = do
  let runDualized :: IsDual -> Eff [GraphMetadataEditing, Dualizeable] a -> Graph Text ()
      runDualized dual =
        runPureEff
          . evalStateLocal dual
          . execStateLocal emptyGraph
          . runInMemoryGraphMetadataEditing @Text
          . inject
          . dualizingGraphMetadataEditing @Text
  it "flips inserted edges while dualized" do
    let g = runDualized (IsDual True) $ insertEdge @Text (Edge n1 "x" n2)
    g ^? ix n2 . #outgoing `shouldBe` Just (singletonSet (Connect "x" n1))
  it "leaves edges alone while not dualized" do
    let g = runDualized (IsDual False) $ insertEdge @Text (Edge n1 "x" n2)
    g ^? ix n1 . #outgoing `shouldBe` Just (singletonSet (Connect "x" n2))
  it "reads nodes with their edges swapped while dualized" do
    let read1 = runPureEff
          . evalStateLocal (IsDual False)
          . evalStateLocal @(Graph Text ()) emptyGraph
          . runInMemoryGraphMetadataEditing @Text
          $ do
            insertEdge @Text (Edge n1 "x" n2)
            dualize
            dualizingGraphMetadataEditing @Text $ getNodeMetadata @Text n1
    fmap (.incoming) read1 `shouldBe` Just (singletonSet (Connect "x" n2))
