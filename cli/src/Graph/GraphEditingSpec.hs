module Graph.GraphEditingSpec where

import DAL.DirectoryFormat (legacyNodeDataFile, metadataFile)
import DAL.FileSystemOperations.Data
import DAL.FileSystemOperations.Metadata
import DAL.FileSystemOperations.MetadataWriteDiff
import DAL.RawGraph
import DAL.Serialization (serializeNodeEx)
import Data.ByteString qualified as B
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
import System.Directory (doesFileExist, listDirectory)
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

-- | Like 'runOnDiskEither', but as @--dry-run@ does it.
runDryEither :: FilePath -> Eff OnDisk a -> IO (Either UserError a)
runDryEither dir =
  runEff
    . runErrorNoCallStack @UserError
    . ignoreWarnings @UserError
    . runRawGraphWithPath dir
    . runGraphDataFilesystemOperationsDryRun
    . runGraphMetadataFilesystemOperationsWriteDiffDryRun
    . runGraphMetadataFilesystemOperationsDryRun True
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
  it "removes a deleted node's edges from its neighbors" do
    withEmptyTempGraph \dir -> do
      runOnDisk dir $ insertEdge (Edge n1 "x" n2)
      runOnDisk dir $ deleteNode n2
      fmap (.outgoing)
        <$> runOnDisk dir (getNodeMetadata n1)
          `shouldReturn` Just mempty
  it "creates a node again after deleting it" do
    withEmptyTempGraph \dir -> do
      runOnDisk dir $ insertEdge (Edge n1 "x" n2)
      runOnDisk dir do
        deleteNode n1
        insertEdge (Edge n2 "y" n1)
      node1 <- runOnDisk dir $ getNodeMetadata n1
      fmap (.incoming) node1 `shouldBe` Just (singletonSet (Connect "y" n2))
      fmap (.outgoing) node1 `shouldBe` Just mempty
  it "deletes an edge between nodes it hasn't read" do
    withEmptyTempGraph \dir -> do
      runOnDisk dir $ insertEdge (Edge n1 "x" n2)
      runOnDisk dir $ deleteEdge (Edge n1 "x" n2)
      result <- runOnDisk dir $ traverse getNodeMetadata [n1, n2]
      map (fmap (\n -> (n.incoming, n.outgoing))) result
        `shouldBe` [Just (mempty, mempty), Just (mempty, mempty)]
  it "doesn't create a node by deleting one of its edges" do
    withEmptyTempGraph \dir -> do
      result <- runOnDisk dir do
        deleteEdge (Edge n1 "x" n2)
        (,) <$> listNodes <*> getNodeMetadata n1
      result `shouldBe` (singletonSet nilNID, Nothing)
      runOnDisk dir listNodes `shouldReturn` singletonSet nilNID
  it "deletes an edge of a node that it read and found missing" do
    withEmptyTempGraph \dir -> do
      result <- runOnDisk dir do
        missing <- getNodeMetadata n1
        deleteEdge (Edge n1 "x" n2)
        (missing,) <$> getNodeMetadata n1
      result `shouldBe` (Nothing, Nothing)
      runOnDisk dir listNodes `shouldReturn` singletonSet nilNID
  it "removes the record of an edge to a node that it found missing" do
    withEmptyTempGraph \dir -> do
      -- the origin records an edge to n1, which has no file
      serializeNodeEx
        ( (emptyNode nilNID :: Node Text ())
            & #outgoing .~ setFromList [Connect "b" n1]
            & ($> Nothing)
        )
        dir
      runOnDisk dir do
        _ <- getNodeMetadata nilNID
        _ <- getNodeMetadata n1
        deleteEdge (Edge nilNID "b" n1)
      fmap (.outgoing)
        <$> runOnDisk dir (getNodeMetadata nilNID)
          `shouldReturn` Just mempty
      doesFileExist (metadataFile dir n1) `shouldReturn` False
  it "writes nothing if the action fails" do
    withEmptyTempGraph \dir -> do
      _ <- runOnDiskEither dir do
        touchNode n1
        writeRawData n1 "data"
        throwText "failed"
      doesFileExist (legacyNodeDataFile dir n1) `shouldReturn` False
      runOnDisk dir listNodes `shouldReturn` singletonSet nilNID

-- | Every file of a directory with its content
directoryContents :: FilePath -> IO (Map FilePath ByteString)
directoryContents dir = do
  files <- listDirectory dir
  mapFromList <$> for files \file -> (file,) <$> B.readFile (dir </> file)

spec_transactionOnDamagedGraph :: Spec
spec_transactionOnDamagedGraph = do
  -- the origin -a-> n1, and an edge the origin -b-> n1 that only n1 records
  let withOneSidedEdge :: (FilePath -> IO ()) -> IO ()
      withOneSidedEdge test = withEmptyTempGraph \dir -> do
        let write :: Node Text () -> IO ()
            write n = serializeNodeEx (n $> Nothing) dir
        write $
          emptyNode nilNID & #outgoing .~ setFromList [Connect "a" n1]
        write $
          emptyNode n1
            & #incoming .~ setFromList [Connect "a" nilNID, Connect "b" nilNID]
        test dir
      editNearDamage :: Eff OnDisk ()
      editNearDamage = do
        _ <- getNodeMetadata n1
        _ <- getNodeMetadata nilNID
        insertEdge (Edge n2 "x" nilNID)
        insertEdge (Edge n1 "y" n2)
        writeRawData n2 "data"
  -- reading the origin after n1 used to make the transaction forget the edge
  -- b that only n1 records, and the change was then refused because the
  -- edits to n1 didn't give what the transaction expected
  it "keeps the record that only one node has of an edge" do
    withOneSidedEdge \dir -> do
      runOnDisk dir editNearDamage
      fmap (.incoming)
        <$> runOnDisk dir (getNodeMetadata n1)
          `shouldReturn` Just
            (setFromList [Connect "a" nilNID, Connect "b" nilNID])
      fmap (.outgoing)
        <$> runOnDisk dir (getNodeMetadata nilNID)
          `shouldReturn` Just (singletonSet (Connect "a" n1))
  it "still writes a change that doesn't touch the contradicting node" do
    withOneSidedEdge \dir -> do
      runOnDisk dir do
        _ <- getNodeMetadata n1
        _ <- getNodeMetadata nilNID
        insertEdge (Edge nilNID "x" n2)
      fmap (.incoming)
        <$> runOnDisk dir (getNodeMetadata n2)
          `shouldReturn` Just (singletonSet (Connect "x" nilNID))
  it "says which record contradicts the change and that it was not written" do
    let message =
          show . InconsistentNodeRecords $
            RecordNotComputed n1 IncomingRecord "b" nilNID
              :| [FileOfOtherNode n1 n2]
    lines message
      `shouldBe` [ "this change was not written: the graph is damaged where it"
                     <> " applies (inconsistent-node-records)",
                   "- the file of @000000000001 has an incoming edge \"b\" from"
                     <> " @000000000000, but the other records say there is no"
                     <> " such edge",
                   "- the file of @000000000001 says it is the node"
                     <> " @000000000002"
                 ]

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
