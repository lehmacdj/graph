module Graph.GraphEditingSpec where

import DAL.DTO (nodeToDTO)
import DAL.DirectoryFormat (legacyNodeDataFile, metadataFile)
import DAL.FileSystemOperations.Data
import DAL.FileSystemOperations.Metadata
import DAL.FileSystemOperations.MetadataWriteDiff
import DAL.RawGraph
import DAL.Serialization (doesNodeExist, serializeNodeEx)
import Data.Aeson qualified as Aeson
import Data.ByteString qualified as B
import Effectful.State.Dynamic
import Error.UserError
import Error.Warn
import Graph.Dualizeable
import Graph.GraphDataEditing
import Graph.GraphEditing
import Graph.GraphMetadataEditing
import Models.Connect
import Models.Damage
import Models.Edge
import Models.Graph (Graph, emptyGraph)
import Models.NID
import Models.Node
import MyPrelude
import System.Directory
  ( createDirectory,
    createFileLink,
    doesDirectoryExist,
    doesFileExist,
    doesPathExist,
    emptyPermissions,
    getPermissions,
    listDirectory,
    pathIsSymbolicLink,
    removeFile,
    renameFile,
    setPermissions,
  )
import Utils.Testing

n1, n2 :: NID
n1 = smallNID 1
n2 = smallNID 2

-- | What a transaction on a graph on disk runs in
type OnDiskFiles =
  [ GraphMetadataFilesystemOperations,
    GraphMetadataFilesystemOperationsWriteDiff,
    GraphDataFilesystemOperations,
    RawGraph,
    State DamageReadPast,
    Warn UserError,
    Error UserError,
    IOE
  ]

type OnDisk = GraphMetadataEditing : GraphDataEditing : OnDiskFiles

-- | Run something against a graph on disk as one command line does: its
-- result and the warnings it gave. With @dry@, as @--dry-run@ does it.
runCommandLine ::
  Bool -> FilePath -> Eff OnDiskFiles a -> IO (Either UserError a, [String])
runCommandLine dry dir action = do
  warnings <- newIORef []
  let collectWarnings = interpret \_ -> \case
        Warn e -> liftIO $ modifyIORef warnings (show (e :: UserError) :)
      files
        | dry =
            runGraphDataFilesystemOperationsDryRun
              . runGraphMetadataFilesystemOperationsWriteDiffDryRun
              . runGraphMetadataFilesystemOperationsDryRun True
        | otherwise =
            runGraphDataFilesystemOperationsIO
              . runGraphMetadataFilesystemOperationsWriteDiffIO
              . runGraphMetadataFilesystemOperationsIO True
  result <-
    runEff
      . runErrorNoCallStack @UserError
      . collectWarnings
      . evalStateLocal noDamageReadPast
      . runRawGraphWithPath dir
      . files
      $ warningOfDamageReadPast action
  (result,) . reverse <$> readIORef warnings

-- | Run an action as one transaction against a graph on disk.
runOnDiskEither :: FilePath -> Eff OnDisk a -> IO (Either UserError a)
runOnDiskEither dir =
  fmap fst . runCommandLine False dir . runGraphEditingTransactionally

-- | Like 'runOnDiskEither', but as @--dry-run@ does it.
runDryEither :: FilePath -> Eff OnDisk a -> IO (Either UserError a)
runDryEither dir =
  fmap fst . runCommandLine True dir . runGraphEditingTransactionally

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
  it "doesn't take a node file that it can't read for a missing node" do
    withEmptyTempGraph \dir -> do
      runOnDisk dir $ insertEdge (Edge nilNID "x" n1)
      let file = metadataFile dir n1
      readable <- getPermissions file
      before <- B.readFile file
      let unreadable :: IO a -> IO a
          unreadable action =
            bracket_
              (setPermissions file emptyPermissions)
              (setPermissions file readable)
              action
          refusedForIt = \case
            Left (DamagedGraph (UnparsableNodeFile nid reason :| [])) ->
              nid == n1 && "there is no permission to read it (" `isPrefixOf` reason
            _ -> False
      -- it is read as a node that is there, of which no edge is known
      unreadable (runOnDisk dir (getNodeMetadata n1))
        `shouldReturn` Just (emptyNode n1)
      -- a change that read it is refused
      unreadable (runOnDiskEither dir (getNodeMetadata n1 >> touchNode n2))
        >>= (`shouldSatisfy` refusedForIt)
      -- and so is a change that writes it without having read it
      unreadable (runOnDiskEither dir (insertEdge (Edge n2 "y" n1)))
        >>= (`shouldSatisfy` refusedForIt)
      B.readFile file `shouldReturn` before
      doesFileExist (metadataFile dir n2) `shouldReturn` False
  it "doesn't take a symbolic link that leads nowhere for a missing node" do
    withEmptyTempGraph \dir -> do
      let file = metadataFile dir n1
          target = dir </> "elsewhere"
          refusedForIt = \case
            Left (DamagedGraph (UnparsableNodeFile nid reason :| [])) ->
              nid == n1 && "symbolic link" `isInfixOf` reason
            _ -> False
      createFileLink target file
      doesNodeExist dir n1 `shouldReturn` True
      -- it is read as a node that is there, of which no edge is known
      runOnDisk dir (getNodeMetadata n1) `shouldReturn` Just (emptyNode n1)
      -- nothing is written through it: neither by a change that read it
      runOnDiskEither dir (getNodeMetadata n1 >> touchNode n1)
        >>= (`shouldSatisfy` refusedForIt)
      -- nor by one that writes it without having read it
      runOnDiskEither dir (touchNode n1) >>= (`shouldSatisfy` refusedForIt)
      runOnDiskEither dir (insertEdge (Edge nilNID "y" n1))
        >>= (`shouldSatisfy` refusedForIt)
      doesPathExist target `shouldReturn` False
      pathIsSymbolicLink file `shouldReturn` True
      runOnDisk dir (getNodeMetadata nilNID)
        `shouldReturn` Just (emptyNode nilNID)
  it "uses a symbolic link to a node file as that node file" do
    withEmptyTempGraph \dir -> do
      let target = dir </> "elsewhere"
      runOnDisk dir $ insertEdge (Edge nilNID "x" n1)
      renameFile (metadataFile dir n1) target
      createFileLink target (metadataFile dir n1)
      runOnDisk dir $ insertEdge (Edge n1 "y" n2)
      node1 <- runOnDisk dir (getNodeMetadata n1)
      fmap (.outgoing) node1 `shouldBe` Just (singletonSet (Connect "y" n2))
      fmap (.incoming) node1 `shouldBe` Just (singletonSet (Connect "x" nilNID))
      pathIsSymbolicLink (metadataFile dir n1) `shouldReturn` True
  it "doesn't take a directory named like a node file for a missing node" do
    withEmptyTempGraph \dir -> do
      createDirectory (metadataFile dir n1)
      doesNodeExist dir n1 `shouldReturn` True
      doesNodeExist dir n2 `shouldReturn` False
      runOnDisk dir (getNodeMetadata n1) `shouldReturn` Just (emptyNode n1)
      runOnDiskEither dir (touchNode n1) >>= \case
        Left (DamagedGraph (UnparsableNodeFile nid reason :| [])) -> do
          nid `shouldBe` n1
          reason `shouldStartWith` "it is not a file but a directory ("
        _ -> expectationFailure "the change was not refused"
      doesDirectoryExist (metadataFile dir n1) `shouldReturn` True
  describe "the files that fix kept of a node's file" do
    let kept dir nid suffix = metadataFile dir nid <> ".unparsable" <> suffix
        hub dir = do
          runOnDisk dir do
            insertEdge (Edge nilNID "x" n1)
            insertEdge (Edge n1 "y" n2)
            writeRawData n1 "data"
    it "are deleted with the node, and no file that is only named like one" $
      withEmptyTempGraph \dir -> do
        hub dir
        for_ ["", "-2", "-3", "-5", ".txt", "-old"] \suffix ->
          B.writeFile (kept dir n1 suffix) "{not json"
        createDirectory (kept dir n1 "-4")
        (result, warnings) <-
          runCommandLine False dir (runGraphEditingTransactionally (deleteNode n1))
        result `shouldSatisfy` isRight
        warnings `shouldBe` []
        -- their names are tried in order until one is no file, so the ones
        -- after that are left over (fsck reports those)
        (sort . filter ("000000000001." `isPrefixOf`) <$> listDirectory dir)
          `shouldReturn` [ "000000000001.json.unparsable-4",
                           "000000000001.json.unparsable-5",
                           "000000000001.json.unparsable-old",
                           "000000000001.json.unparsable.txt"
                         ]
    it "stay if the change is refused" $
      withEmptyTempGraph \dir -> do
        hub dir
        B.writeFile (kept dir n1 "") "{not json"
        B.writeFile (metadataFile dir n2) "{not json"
        runOnDiskEither dir (deleteNode n1) >>= (`shouldSatisfy` isLeft)
        doesFileExist (kept dir n1 "") `shouldReturn` True
        doesFileExist (metadataFile dir n1) `shouldReturn` True
    it "don't undo the change if one of them can't be removed: it warns" $
      withEmptyTempGraph \dir -> do
        hub dir
        B.writeFile (kept dir n1 "") "{not json"
        B.writeFile (kept dir n1 "-2") "{nor this"
        warnings <- newIORef []
        let collectWarnings = interpret \_ -> \case
              Warn e -> liftIO $ modifyIORef warnings (show (e :: UserError) :)
            -- the first one can't be removed
            removing file
              | ".unparsable" `isSuffixOf` file =
                  liftIO . ioError $
                    mkIOError permissionErrorType "removeLink" Nothing (Just file)
              | otherwise = liftIO (removeFile file)
        result <-
          runEff
            . runErrorNoCallStack @UserError
            . collectWarnings
            . evalStateLocal noDamageReadPast
            . runRawGraphWithPath dir
            . runGraphDataFilesystemOperationsIO
            . writingGraphDiffsRemovingKeptWith removing
            . runGraphMetadataFilesystemOperationsIO True
            $ runGraphEditingTransactionally (deleteNode n1)
        result `shouldSatisfy` isRight
        -- the node, its data and its edges are gone
        (sort <$> listDirectory dir)
          `shouldReturn` [ "000000000000.json",
                           "000000000001.json.unparsable",
                           "000000000002.json"
                         ]
        runOnDisk dir (traverse getNodeMetadata [nilNID, n2])
          `shouldReturn` [Just (emptyNode nilNID), Just (emptyNode n2)]
        readIORef warnings
          `shouldReturn` [ "warning: the node was deleted, but"
                             <> " 000000000001.json.unparsable, which fix had"
                             <> " kept of its file, could not be removed: there"
                             <> " is no permission to remove it ("
                             <> kept dir n1 ""
                             <> ": removeLink: permission denied). It is left"
                             <> " over, and fsck will report it."
                         ]
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
  let write :: FilePath -> Node Text () -> IO ()
      write dir n = serializeNodeEx (n $> Nothing) dir
      -- the origin -a-> n1, and an edge the origin -b-> n1 that only n1
      -- records
      withOneSidedEdge :: (FilePath -> IO ()) -> IO ()
      withOneSidedEdge test = withEmptyTempGraph \dir -> do
        write dir $
          emptyNode nilNID & #outgoing .~ setFromList [Connect "a" n1]
        write dir $
          emptyNode n1
            & #incoming .~ setFromList [Connect "a" nilNID, Connect "b" nilNID]
        test dir
      oneSided = OneSidedEdge IncomingRecord (Edge nilNID "b" n1)
      -- the origin -a-> n1 and the origin -c-> n2, and no file for n2
      withMissingFile :: (FilePath -> IO ()) -> IO ()
      withMissingFile test = withEmptyTempGraph \dir -> do
        write dir $
          emptyNode nilNID
            & #outgoing .~ setFromList [Connect "a" n1, Connect "c" n2]
        write dir $ emptyNode n1 & #incoming .~ setFromList [Connect "a" nilNID]
        test dir
      toMissing = EdgeToMissingNode OutgoingRecord (Edge nilNID "c" n2)
      -- the origin -a-> n1, whose file says it is n2
      withOtherId :: (FilePath -> IO ()) -> IO ()
      withOtherId test = withEmptyTempGraph \dir -> do
        write dir $
          emptyNode nilNID & #outgoing .~ setFromList [Connect "a" n1]
        let saysN2 :: Node Text ()
            saysN2 = emptyNode n2 & #incoming .~ setFromList [Connect "a" nilNID]
        B.writeFile (metadataFile dir n1) . toStrict . Aeson.encode $
          nodeToDTO saysN2
        test dir
      refusedWithoutWriting ::
        ((FilePath -> IO ()) -> IO ()) ->
        [Damage] ->
        Eff OnDisk () ->
        (FilePath -> Eff OnDisk () -> IO (Either UserError ())) ->
        IO ()
      refusedWithoutWriting withGraph damage change run = withGraph \dir -> do
        before <- directoryContents dir
        result <- run dir change
        case result of
          Left (DamagedGraph found) -> toList found `shouldBe` damage
          Left e -> expectationFailure $ "failed with another error: " <> show e
          Right () -> expectationFailure "the transaction didn't fail"
        directoryContents dir `shouldReturn` before
      editAfterReadingBoth :: Eff OnDisk ()
      editAfterReadingBoth = do
        _ <- getNodeMetadata n1
        _ <- getNodeMetadata nilNID
        insertEdge (Edge n2 "x" (smallNID 3))
        writeRawData n2 "data"
      reading :: [NID] -> Eff OnDisk ()
      reading = traverse_ getNodeMetadata
  describe "a change" do
    it "is refused when two nodes it read disagree about an edge" $
      refusedWithoutWriting
        withOneSidedEdge
        [oneSided]
        editAfterReadingBoth
        runOnDiskEither
    it "is refused in the same way in a dry run" $
      refusedWithoutWriting
        withOneSidedEdge
        [oneSided]
        editAfterReadingBoth
        runDryEither
    it "is refused whichever of the two nodes it read first" $
      refusedWithoutWriting
        withOneSidedEdge
        [oneSided]
        (reading [nilNID, n1] >> touchNode n2)
        runOnDiskEither
    it "is refused when the nodes it writes without reading them disagree" $
      refusedWithoutWriting
        withOneSidedEdge
        [oneSided]
        (insertEdge (Edge nilNID "x" n1))
        runOnDiskEither
    it "is refused when a node it read has an edge to a node without a file" $
      refusedWithoutWriting
        withMissingFile
        [toMissing]
        (reading [nilNID, n2] >> touchNode (smallNID 3))
        runOnDiskEither
    it "is refused when it removes the record of an edge to a node without a file" $
      refusedWithoutWriting
        withMissingFile
        [toMissing]
        (reading [nilNID, n2] >> deleteEdge (Edge nilNID "c" n2))
        runOnDiskEither
    it "is refused when it would create a node that a node it read refers to" $
      refusedWithoutWriting
        withMissingFile
        [toMissing]
        (reading [nilNID] >> insertEdge (Edge n2 "x" (smallNID 3)))
        runOnDiskEither
    it "is refused when a file it read says it is another node" $
      refusedWithoutWriting
        withOtherId
        [FileSaysOtherId n1 n2]
        (reading [n1] >> touchNode (smallNID 3))
        runOnDiskEither
    it "is refused when it only deletes the data of a node without a file" do
      -- as rmnf does through an edge to such a node
      let deleteMissing = reading [nilNID] >> deleteNode n2
          withItsData test = withMissingFile \dir ->
            B.writeFile (legacyNodeDataFile dir n2) "kept" >> test dir
      refusedWithoutWriting withItsData [toMissing] deleteMissing runOnDiskEither
      refusedWithoutWriting withItsData [toMissing] deleteMissing runDryEither
    it "is refused when it only writes data next to damage that it read" $
      refusedWithoutWriting
        withOneSidedEdge
        [oneSided]
        (reading [nilNID, n1] >> writeRawData n1 "new")
        runOnDiskEither
    it "is refused naming every file it can't read as a node, in id order" do
      -- the two nodes of the new edge are written without having been read,
      -- and their files are read together
      let withTwoUnparsable test = withOneSidedEdge \dir -> do
            for_ [smallNID 3, n2] \nid ->
              B.writeFile (metadataFile dir nid) "{not json"
            test dir
      replicateM_ 5 $ withTwoUnparsable \dir -> do
        before <- directoryContents dir
        result <- runOnDiskEither dir (insertEdge (Edge (smallNID 3) "x" n2))
        case result of
          Left (DamagedGraph found) ->
            [nid | UnparsableNodeFile nid _ <- toList found]
              `shouldBe` [n2, smallNID 3]
          _ -> expectationFailure "the change was not refused for the damage"
        directoryContents dir `shouldReturn` before
    it "is refused when it read a file that can't be read as a node" do
      let withUnparsable test = withMissingFile \dir ->
            B.writeFile (metadataFile dir n2) "{not json" >> test dir
          change = reading [nilNID, n2] >> touchNode (smallNID 3)
      for_ [runOnDiskEither, runDryEither] \run ->
        withUnparsable \dir -> do
          before <- directoryContents dir
          run dir change >>= \case
            Left (DamagedGraph (UnparsableNodeFile nid _ :| [])) ->
              nid `shouldBe` n2
            other -> expectationFailure ("ended with " <> show other)
          directoryContents dir `shouldReturn` before
    it "is refused when its data is about to be edited outside, as vi does" do
      -- nothing is written for such an edit, which the editor makes
      let outside nid = reading [nilNID, n1] >> editingDataOutside nid
      refusedWithoutWriting withOneSidedEdge [oneSided] (outside n1) runOnDiskEither
      refusedWithoutWriting withOneSidedEdge [oneSided] (outside n1) runDryEither
    it "writes nothing for data that is about to be edited outside" $
      withEmptyTempGraph \dir -> do
        runOnDisk dir (touchNode n1)
        before <- directoryContents dir
        runOnDisk dir do
          _ <- getNodeMetadata n1
          editingDataOutside n1
        directoryContents dir `shouldReturn` before
    it "is written when the other node of a damaged edge was not looked at" do
      withOneSidedEdge \dir -> do
        runOnDisk dir do
          _ <- getNodeMetadata n1
          insertEdge (Edge n1 "y" n2)
        fmap (.incoming)
          <$> runOnDisk dir (getNodeMetadata n1)
            `shouldReturn` Just
              (setFromList [Connect "a" nilNID, Connect "b" nilNID])
  describe "a transaction that changes nothing" do
    let warning damage = show (DamagedGraphWarning (damage :| []))
    it "warns of the damage it read and answers by each node's own file" do
      withOneSidedEdge \dir -> do
        (result, warnings) <-
          runCommandLine False dir . runGraphEditingTransactionally $
            traverse getNodeMetadata [nilNID, n1]
        fmap (map (fmap (\n -> (n.incoming, n.outgoing)))) result
          `shouldSatisfy` \case
            Right [Just (_, out0), Just (in1, _)] ->
              out0 == setFromList [Connect "a" n1]
                && in1 == setFromList [Connect "a" nilNID, Connect "b" nilNID]
            _ -> False
        warnings `shouldBe` [warning oneSided]
    it "warns once in a command line, however often the damage is read" do
      withOneSidedEdge \dir -> do
        (result, warnings) <- runCommandLine False dir do
          runGraphEditingTransactionally $ reading [nilNID, n1]
          runGraphEditingTransactionally $ reading [n1, nilNID]
        void result `shouldSatisfy` isRight
        warnings `shouldBe` [warning oneSided]
    it "doesn't warn when it read only one of the two nodes" do
      withOneSidedEdge \dir -> do
        (_, warnings) <-
          runCommandLine False dir . runGraphEditingTransactionally $
            reading [n1]
        warnings `shouldBe` []
    it "warns of an edge to a node without a file" do
      withMissingFile \dir -> do
        (_, warnings) <-
          runCommandLine False dir . runGraphEditingTransactionally $
            reading [nilNID, n2]
        warnings `shouldBe` [warning toMissing]
    it "reads a file that says it is another node as the node it is named for" do
      withOtherId \dir -> do
        (result, warnings) <-
          runCommandLine False dir . runGraphEditingTransactionally $
            getNodeMetadata n1
        fmap (fmap (.nid)) result `shouldSatisfy` (== Right (Just n1)) . first (const ())
        warnings `shouldBe` [warning (FileSaysOtherId n1 n2)]
    it "reads a file that can't be read as a node as a node without edges" do
      -- and says nothing of the edges that other nodes record with it
      withMissingFile \dir -> do
        B.writeFile (metadataFile dir n2) "{not json"
        (result, warnings) <-
          runCommandLine False dir . runGraphEditingTransactionally $
            traverse getNodeMetadata [nilNID, n2, n1]
        fmap (map (fmap (\n -> (n.nid, n.incoming, n.outgoing)))) result
          `shouldSatisfy` \case
            Right [Just (_, _, out0), Just unusable, Just (_, in1, _)] ->
              out0 == setFromList [Connect "a" n1, Connect "c" n2]
                && unusable == (n2, mempty, mempty)
                && in1 == setFromList [Connect "a" nilNID]
            _ -> False
        case warnings of
          [message] ->
            lines message
              `shouldBe` [ "warning: the graph is damaged where this command"
                             <> " read it (damaged-graph)",
                           "- 000000000002.json cannot be read as a node: Error"
                             <> " in $: object key > 34: Failed reading: satisfy",
                           "fsck lists all such damage; fix repairs it, or says"
                             <> " why it can't"
                         ]
          _ -> expectationFailure ("warned " <> show warnings)
    it "reads on past a file that there is no permission to read, and warns" do
      withMissingFile \dir -> do
        let file = metadataFile dir n2
        B.writeFile file "{}"
        readable <- getPermissions file
        (result, warnings) <-
          bracket_
            (setPermissions file emptyPermissions)
            (setPermissions file readable)
            ( runCommandLine False dir . runGraphEditingTransactionally $
                traverse getNodeMetadata [nilNID, n2, n1]
            )
        -- the nodes around it are read as they are, and it as a node that
        -- is there and of which no edge is known
        fmap (map (fmap (\n -> (n.nid, n.incoming, n.outgoing)))) result
          `shouldSatisfy` \case
            Right [Just (_, _, out0), Just unusable, Just (_, in1, _)] ->
              out0 == setFromList [Connect "a" n1, Connect "c" n2]
                && unusable == (n2, mempty, mempty)
                && in1 == setFromList [Connect "a" nilNID]
            _ -> False
        case map lines warnings of
          [[heading, finding, _]] -> do
            heading
              `shouldBe` "warning: the graph is damaged where this command"
              <> " read it (damaged-graph)"
            finding
              `shouldStartWith` "- 000000000002.json cannot be read as a node:"
              <> " there is no permission to read it ("
          _ -> expectationFailure ("warned " <> show warnings)
    it "warns of the damage it read when it fails for another reason" do
      withOneSidedEdge \dir -> do
        (result, warnings) <-
          runCommandLine False dir . runGraphEditingTransactionally $ do
            reading [nilNID, n1]
            touchNode n2
            throwText "(expected test output) failed"
        void result `shouldSatisfy` isLeft
        warnings `shouldBe` [warning oneSided]
    it "warns when the command line is done, and of all the damage at once" do
      let withBoth test = withOneSidedEdge \dir -> do
            let saysN3 :: Node Text ()
                saysN3 = emptyNode (smallNID 3)
            B.writeFile (metadataFile dir n2) . toStrict . Aeson.encode $
              nodeToDTO saysN3
            test dir
      withBoth \dir -> do
        seenInBetween <- newIORef []
        (_, warnings) <- runCommandLine False dir do
          runGraphEditingTransactionally $ reading [nilNID, n1]
          runGraphEditingTransactionally $ reading [n2]
          get >>= liftIO . writeIORef seenInBetween . pure
        warnings
          `shouldBe` [ show . DamagedGraphWarning $
                         oneSided :| [FileSaysOtherId n2 (smallNID 3)]
                     ]
        readIORef seenInBetween
          `shouldReturn` [ DamageReadPast
                             (setFromList [oneSided, FileSaysOtherId n2 (smallNID 3)])
                             Nothing
                         ]
    it "doesn't warn of the damage that the command line is refused for" do
      withOneSidedEdge \dir -> do
        (result, warnings) <- runCommandLine False dir do
          runGraphEditingTransactionally $ reading [nilNID, n1]
          runGraphEditingTransactionally $ reading [nilNID, n1] >> touchNode n2
        case result of
          Left (DamagedGraph found) -> toList found `shouldBe` [oneSided]
          _ -> expectationFailure "the change was not refused"
        warnings `shouldBe` []
    it "warns of the damage it read however the command line ends" do
      -- with an exception that is no error of ge's own, as that of an editor
      -- that failed or of an interrupt
      withOneSidedEdge \dir -> do
        warnings <- newIORef []
        let collectWarnings = interpret \_ -> \case
              Warn e -> liftIO $ modifyIORef warnings (show (e :: UserError) :)
            line :: Eff OnDiskFiles ()
            line = do
              runGraphEditingTransactionally $ reading [nilNID, n1]
              throwIO (userError "(expected test output) the editor failed")
        ( runEff
            . runErrorNoCallStack @UserError
            . collectWarnings
            . evalStateLocal noDamageReadPast
            . runRawGraphWithPath dir
            . runGraphDataFilesystemOperationsIO
            . runGraphMetadataFilesystemOperationsWriteDiffIO
            . runGraphMetadataFilesystemOperationsIO True
            $ warningOfDamageReadPast line
          )
          `shouldThrow` \e -> isUserError (e :: IOError)
        readIORef warnings `shouldReturn` [warning oneSided]
    it "still warns of damage that a refusal doesn't name" do
      withOneSidedEdge \dir -> do
        (result, warnings) <- runCommandLine False dir do
          runGraphEditingTransactionally $ reading [nilNID, n1]
          throwText "(expected test output) failed" :: Eff OnDiskFiles ()
        void result `shouldSatisfy` isLeft
        warnings `shouldBe` [warning oneSided]
  describe "a command, which is one decision" do
    let n3 = smallNID 3
        warning damage = show (DamagedGraphWarning (damage :| []))
        -- as a command that resolves its arguments, or the targets of at,
        -- and then changes something somewhere else
        readThenChange :: [NID] -> Eff OnDiskFiles ()
        readThenChange nids = do
          runGraphEditingTransactionally $ reading nids
          runGraphEditingTransactionally $ touchNode n3
        refusedWithoutWarning withGraph damage dry line = withGraph \dir -> do
          before <- directoryContents dir
          (result, warnings) <- runCommandLine dry dir line
          case result of
            Left (DamagedGraph found) -> toList found `shouldBe` damage
            Left e -> expectationFailure $ "failed with another error: " <> show e
            Right () -> expectationFailure "the change was not refused"
          -- the refusal names the damage, so no warning does as well
          warnings `shouldBe` []
          directoryContents dir `shouldReturn` before
        withUnusableFile test = withMissingFile \dir ->
          B.writeFile (metadataFile dir n2) "{not json" >> test dir
        isUnusable = \case
          Left (DamagedGraph (UnparsableNodeFile nid _ :| [])) -> nid == n2
          _ -> False
    it "writes nothing once one of its steps has read past a lone record" $
      refusedWithoutWarning withOneSidedEdge [oneSided] False $
        asOneDecision (readThenChange [nilNID, n1])
    it "writes nothing in a dry run either" $
      refusedWithoutWarning withOneSidedEdge [oneSided] True $
        asOneDecision (readThenChange [nilNID, n1])
    it "writes nothing once it has read past a node without a file" $
      refusedWithoutWarning withMissingFile [toMissing] False $
        asOneDecision (readThenChange [nilNID, n2])
    it "writes nothing once it has read past a file that can't be used" $
      withUnusableFile \dir -> do
        before <- directoryContents dir
        (result, warnings) <-
          runCommandLine False dir (asOneDecision (readThenChange [n2]))
        result `shouldSatisfy` isUnusable
        warnings `shouldBe` []
        directoryContents dir `shouldReturn` before
    it "takes what runs inside of it for part of its decision" $
      -- as the command that at runs at each of its targets
      refusedWithoutWarning withOneSidedEdge [oneSided] False . asOneDecision $ do
        asOneDecision . runGraphEditingTransactionally $ reading [nilNID, n1]
        asOneDecision . runGraphEditingTransactionally $ touchNode n3
    it "decides nothing for the command after it" $
      -- as cd, which reads, and then a command that edits where it stands
      withOneSidedEdge \dir -> do
        (result, warnings) <- runCommandLine False dir do
          asOneDecision . runGraphEditingTransactionally $ reading [nilNID, n1]
          asOneDecision . runGraphEditingTransactionally $ touchNode n3
        result `shouldSatisfy` isRight
        warnings `shouldBe` [warning oneSided]
        doesFileExist (metadataFile dir n3) `shouldReturn` True
    it "is not bound by what was read outside of every command" $
      -- as opening the graph, which reads the system nodes
      withOneSidedEdge \dir -> do
        (result, warnings) <- runCommandLine False dir do
          runGraphEditingTransactionally $ reading [nilNID, n1]
          asOneDecision (readThenChange [])
        result `shouldSatisfy` isRight
        warnings `shouldBe` [warning oneSided]
        doesFileExist (metadataFile dir n3) `shouldReturn` True
    it "is over when it fails, and binds no later command" $
      withOneSidedEdge \dir -> do
        (result, _) <- runCommandLine False dir do
          _ <- tryError @UserError (asOneDecision (readThenChange [nilNID, n1]))
          asOneDecision (readThenChange [])
        result `shouldSatisfy` isRight
        doesFileExist (metadataFile dir n3) `shouldReturn` True
    describe "compares the node files that its steps read with each other" do
      -- each transaction is a step of its own, as resolving the targets of at
      -- is one and the command that runs at a target another
      let steps :: [Eff OnDisk ()] -> Eff OnDiskFiles ()
          steps = traverse_ runGraphEditingTransactionally
      it "and writes nothing once two steps have read the files of a lone record" $
        for_ [False, True] \dry ->
          refusedWithoutWarning withOneSidedEdge [oneSided] dry . asOneDecision $
            steps [reading [nilNID], reading [n1] >> touchNode n3]
      it "nor if writing is what reads the other file" $
        -- the origin is changed without having been read
        refusedWithoutWarning withOneSidedEdge [oneSided] False . asOneDecision $
          steps [reading [n1], insertEdge (Edge nilNID "y" n3)]
      it "nor if writing is what read the first of them, in a dry run either" $
        -- The first step changes the origin without reading it, so only
        -- writing reads its file, which has no record of the edge that the
        -- second step finds at n1. A dry run writes nothing and has read
        -- that file all the same.
        for_ [False, True] \dry -> withOneSidedEdge \dir -> do
          (result, warnings) <-
            runCommandLine dry dir . asOneDecision . steps $
              [insertEdge (Edge nilNID "y" n3), reading [n1] >> touchNode n2]
          case result of
            Left (DamagedGraph found) -> toList found `shouldBe` [oneSided]
            _ -> expectationFailure "the change was not refused"
          warnings `shouldBe` []
          -- what the first step did is written, unless it is a dry run
          doesFileExist (metadataFile dir n3) `shouldReturn` not dry
          doesFileExist (metadataFile dir n2) `shouldReturn` False
      it "and warns of it once if it only reads" $
        withOneSidedEdge \dir -> do
          (result, warnings) <-
            runCommandLine False dir . asOneDecision . steps $
              map reading [[nilNID], [n1], [nilNID], [n1]]
          result `shouldSatisfy` isRight
          warnings `shouldBe` [warning oneSided]
      it "and warns of it if writing read the first of them, in a dry run too" $
        for_ [False, True] \dry -> withOneSidedEdge \dir -> do
          (result, warnings) <-
            runCommandLine dry dir . asOneDecision . steps $
              [insertEdge (Edge nilNID "y" n3), reading [n1]]
          result `shouldSatisfy` isRight
          warnings `shouldBe` [warning oneSided]
      it "and keeps none of them for the command after it" $
        withOneSidedEdge \dir -> do
          (result, warnings) <- runCommandLine False dir do
            asOneDecision (steps [reading [nilNID]])
            asOneDecision (steps [reading [n1] >> touchNode n3])
          result `shouldSatisfy` isRight
          warnings `shouldBe` []
          doesFileExist (metadataFile dir n3) `shouldReturn` True
      it "nor any that were read outside of every command" $
        withOneSidedEdge \dir -> do
          (result, warnings) <- runCommandLine False dir do
            steps [reading [nilNID]]
            asOneDecision (steps [reading [n1] >> touchNode n3])
          result `shouldSatisfy` isRight
          warnings `shouldBe` []
          doesFileExist (metadataFile dir n3) `shouldReturn` True
      -- What is kept of a file is what the command last read or wrote of it,
      -- so its own changes never look like damage: each of these steps reads
      -- one node of an edge that an earlier step changed at both of them.
      let n4 = smallNID 4
          writing =
            [ reading [nilNID, n1],
              -- changes both files without reading them
              insertEdge (Edge nilNID "a" n1),
              reading [n1] >> insertEdge (Edge n1 "b" n2),
              reading [nilNID],
              -- reads n1, and changes the origin and n2 without reading them
              deleteNode n1,
              reading [nilNID] >> touchNode n3,
              reading [n2] >> insertEdge (Edge n2 "c" n1),
              reading [n1, nilNID, n2, n3] >> touchNode n4
            ]
          allNodes = [nilNID, n1, n2, n3, n4]
      it "and is not refused for what its own steps wrote" $
        withEmptyTempGraph \dir -> do
          (result, warnings) <-
            runCommandLine False dir (asOneDecision (steps writing))
          result `shouldSatisfy` isRight
          warnings `shouldBe` []
          (final, damage) <-
            runCommandLine False dir . runGraphEditingTransactionally $
              traverse getNodeMetadata allNodes
          damage `shouldBe` []
          fmap (map (fmap (\n -> (toList n.incoming, toList n.outgoing)))) final
            `shouldSatisfy` \case
              Right nodes ->
                nodes
                  == [ Just ([], []),
                       Just ([Connect "c" n2], []),
                       Just ([], [Connect "c" n1]),
                       Just ([], []),
                       Just ([], [])
                     ]
              Left _ -> False
      it "nor in a dry run, which leaves every file as the steps read it" $
        withEmptyTempGraph \dir -> do
          runOnDisk dir $ insertEdge (Edge nilNID "a" n1)
          before <- directoryContents dir
          (result, warnings) <-
            runCommandLine True dir . asOneDecision . steps $
              [ reading [nilNID, n1],
                deleteNode n1,
                -- the origin still has the edge, and n1 is still there
                reading [nilNID] >> touchNode n3,
                insertEdge (Edge n1 "b" n2),
                reading [n1] >> touchNode n3,
                reading [n2] >> touchNode n4
              ]
          result `shouldSatisfy` isRight
          warnings `shouldBe` []
          directoryContents dir `shouldReturn` before
      it "takes what another program wrote between two of its steps for damage" $
        -- The price of reading no file again: the origin is kept as the
        -- first step read it, without the edge that the second step finds
        -- at n1. Nothing is written, and the command goes through when it
        -- is run again.
        withEmptyTempGraph \dir -> do
          let added = Edge nilNID "z" n1
              command =
                asOneDecision $ steps [reading [nilNID], reading [n1] >> touchNode n3]
          (result, warnings) <- runCommandLine False dir . asOneDecision $ do
            steps [reading [nilNID]]
            liftIO . runOnDisk dir $ insertEdge added
            steps [reading [n1] >> touchNode n3]
          case result of
            Left (DamagedGraph found) ->
              toList found `shouldBe` [OneSidedEdge IncomingRecord added]
            _ -> expectationFailure "the change was not refused"
          warnings `shouldBe` []
          doesFileExist (metadataFile dir n3) `shouldReturn` False
          (again, _) <- runCommandLine False dir command
          again `shouldSatisfy` isRight
          doesFileExist (metadataFile dir n3) `shouldReturn` True
      it "and takes a file that it reads again and finds changed for newly read" $
        -- Since the first step read both files, another program has left a
        -- record at the origin that n1 doesn't have. The second step reads
        -- the origin again and not n1: the command has read both files of
        -- that edge, the origin's as it is now.
        withEmptyTempGraph \dir -> do
          runOnDisk dir $ insertEdge (Edge nilNID "a" n1)
          let lone = Edge nilNID "z" n1
          (result, warnings) <- runCommandLine False dir . asOneDecision $ do
            steps [reading [nilNID, n1]]
            liftIO . write dir $
              emptyNode nilNID
                & #outgoing .~ setFromList [Connect "a" n1, Connect "z" n1]
            steps [reading [nilNID] >> touchNode n3]
          case result of
            Left (DamagedGraph found) ->
              toList found `shouldBe` [OneSidedEdge OutgoingRecord lone]
            _ -> expectationFailure "the change was not refused"
          warnings `shouldBe` []
          doesFileExist (metadataFile dir n3) `shouldReturn` False
  it "says which files are damaged, how, and that the change was not written" do
    lines (show (DamagedGraph (oneSided :| [toMissing, FileSaysOtherId n1 n2])))
      `shouldBe` [ "this change was not written: the graph is damaged where it"
                     <> " applies (damaged-graph)",
                   "- the edge @000000000000 -\"b\"-> @000000000001 is among"
                     <> " the incoming edges in 000000000001.json but not among"
                     <> " the outgoing edges in 000000000000.json",
                   "- the edge @000000000000 -\"c\"-> @000000000002 is among"
                     <> " the outgoing edges in 000000000000.json but there is"
                     <> " no 000000000002.json",
                   "- 000000000001.json says it is the node @000000000002",
                   "fsck lists all such damage; fix repairs it, or says why it can't"
                 ]
  it "warns of damage in the same words" do
    lines (show (DamagedGraphWarning (oneSided :| [toMissing])))
      `shouldBe` [ "warning: the graph is damaged where this command read it"
                     <> " (damaged-graph)",
                   "- the edge @000000000000 -\"b\"-> @000000000001 is among"
                     <> " the incoming edges in 000000000001.json but not among"
                     <> " the outgoing edges in 000000000000.json",
                   "- the edge @000000000000 -\"c\"-> @000000000002 is among"
                     <> " the outgoing edges in 000000000000.json but there is"
                     <> " no 000000000002.json",
                   "fsck lists all such damage; fix repairs it, or says why it can't"
                 ]
  it "says which record contradicts a change that was miscomputed" do
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
