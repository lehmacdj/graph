module Graph.GraphEditingSpec where

import DAL.DirectoryFormat (legacyNodeDataFile)
import DAL.FileSystemOperations.Data
import DAL.FileSystemOperations.Metadata
import DAL.FileSystemOperations.MetadataWriteDiff
import DAL.RawGraph
import Effectful.State.Dynamic
import Error.UserError
import Error.Warn
import Graph.Dualizeable
import Graph.GraphDataEditing
import Graph.GraphEditing
import Graph.GraphMetadataEditing
import Models.Connect
import Models.Edge
import Models.Graph (Graph, emptyGraph)
import Models.NID
import Models.Node
import MyPrelude
import System.Directory (doesFileExist)
import Utils.Testing

n1, n2 :: NID
n1 = smallNID 1
n2 = smallNID 2

type OnDisk =
  [ GraphMetadataEditing,
    GraphDataEditing,
    GraphMetadataFilesystemOperations,
    GraphMetadataFilesystemOperationsWriteDiff,
    GraphDataFilesystemOperations,
    RawGraph,
    Warn UserError,
    Error UserError,
    IOE
  ]

-- | Run an action as one transaction against a graph on disk.
runOnDiskEither :: FilePath -> Eff OnDisk a -> IO (Either UserError a)
runOnDiskEither dir =
  runEff
    . runErrorNoCallStack @UserError
    . ignoreWarnings @UserError
    . runRawGraphWithPath dir
    . runGraphDataFilesystemOperationsIO
    . runGraphMetadataFilesystemOperationsWriteDiffIO
    . runGraphMetadataFilesystemOperationsIO True
    . runGraphEditingTransactionally

runOnDisk :: FilePath -> Eff OnDisk a -> IO a
runOnDisk dir = runOnDiskEither dir >=> either (fail . show) pure

spec_runGraphEditingTransactionally :: Spec
spec_runGraphEditingTransactionally = do
  it "writes metadata and data when the action finishes" do
    withEmptyTempGraph \dir -> do
      _ <- runOnDisk dir do
        insertEdge (Edge n1 "x" n2)
        writeRawData n2 "hello"
      result <- runOnDisk dir $ (,) <$> getNodeMetadata n1 <*> getRawData n2
      fmap (.outgoing) (fst result)
        `shouldBe` Just (singletonSet (Connect "x" n2))
      snd result `shouldBe` Just "hello"
  it "reads its own writes before they are written" do
    withEmptyTempGraph \dir -> do
      result <- runOnDisk dir do
        writeRawData n1 "pending"
        touchNode n2
        (,) <$> getRawData n1 <*> listNodes
      result `shouldBe` (Just "pending", setFromList [nilNID, n2])
  it "deletes a node's data with the node" do
    withEmptyTempGraph \dir -> do
      _ <- runOnDisk dir do
        touchNode n1
        writeRawData n1 "data"
      _ <- runOnDisk dir $ deleteNode n1
      doesFileExist (legacyNodeDataFile dir n1) `shouldReturn` False
      runOnDisk dir listNodes `shouldReturn` singletonSet nilNID
  it "writes nothing if the action fails" do
    withEmptyTempGraph \dir -> do
      _ <- runOnDiskEither dir do
        touchNode n1
        writeRawData n1 "data"
        throwText "failed"
      doesFileExist (legacyNodeDataFile dir n1) `shouldReturn` False
      runOnDisk dir listNodes `shouldReturn` singletonSet nilNID

spec_dualizingGraphMetadataEditing :: Spec
spec_dualizingGraphMetadataEditing = do
  let runDualized :: IsDual -> Eff [GraphMetadataEditing, Dualizeable] a -> Graph Text ()
      runDualized dual =
        runPureEff
          . evalStateLocal dual
          . execStateLocal emptyGraph
          . runInMemoryGraphMetadataEditing
          . inject
          . dualizingGraphMetadataEditing @Text
  it "flips inserted edges while dualized" do
    let g = runDualized (IsDual True) $ insertEdge (Edge n1 "x" n2)
    g ^? ix n2 . #outgoing `shouldBe` Just (singletonSet (Connect "x" n1))
  it "leaves edges alone while not dualized" do
    let g = runDualized (IsDual False) $ insertEdge (Edge n1 "x" n2)
    g ^? ix n1 . #outgoing `shouldBe` Just (singletonSet (Connect "x" n2))
  it "reads nodes with their edges swapped while dualized" do
    let read1 = runPureEff
          . evalStateLocal (IsDual False)
          . evalStateLocal emptyGraph
          . runInMemoryGraphMetadataEditing @Text
          $ do
            insertEdge (Edge n1 "x" n2)
            dualize
            dualizingGraphMetadataEditing @Text $ getNodeMetadata n1
    fmap (.incoming) read1 `shouldBe` Just (singletonSet (Connect "x" n2))
