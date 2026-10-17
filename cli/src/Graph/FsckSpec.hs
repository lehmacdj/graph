module Graph.FsckSpec where

import DAL.DTO (nodeToDTO)
import DAL.DirectoryFormat (metadataFile)
import DAL.Interpreters.GraphMetadataEditingSpec (genConsistentFiles, genNID)
import DAL.RawGraph
import Data.Aeson qualified as Aeson
import Data.ByteString qualified as B
import Data.Map.Strict qualified as Map
import Data.Time (addUTCTime, getCurrentTime)
import Effect.IOWrapper.Echo
import Error.UserError
import Graph.Fsck
import Models.Connect
import Models.Damage
import Models.Edge
import Models.NID
import Models.Node
import MyPrelude
import System.Directory
  ( createDirectory,
    createFileLink,
    removeFile,
    setModificationTime,
  )
import Utils.Testing
import Utils.Testing.TempGraph

-- | The files of an undamaged graph, in which the node 5 is left out so that
-- there is a node without a node file that a data file can be for
genUndamaged :: Gen GraphFiles
genUndamaged = do
  nodes <- map (without (smallNID 5)) . deleteMap (smallNID 5) <$> genConsistentFiles
  pure
    GraphFiles
      { nodes = map Right (Map.insertWith (\_ old -> old) nilNID (emptyNode nilNID) nodes),
        unread = mempty,
        unreadable = mempty,
        others = mempty
      }
  where
    without :: NID -> Node Text () -> Node Text ()
    without nid n =
      n
        & #incoming %~ filterSet ((/= nid) . (.node))
        & #outgoing %~ filterSet ((/= nid) . (.node))

-- | One piece of damage done to the files of an undamaged graph
data Plant
  = -- | Take the record of an edge out of one of its two nodes
    DropRecord RecordSide (Edge Text)
  | RemoveFile NID
  | -- | Make the file of the first node say it is the second
    SayOtherId NID NID
  | -- | Make the file of the node unreadable as a node
    Garble NID
  | -- | Add a data file for a node that has no file: this is no damage
    StrayData NID
  | -- | Add a file that a fix kept of the node file of a node that has none
    KeptFile NID
  deriving stock (Eq, Show)

garbled :: String
garbled = "it is garbled"

dataFileName :: NID -> FilePath
dataFileName nid = unpack (nidRepresentation nid) <> ".data"

-- | The name of the first copy that fix keeps of the node's file
keptName :: NID -> FilePath
keptName nid = nodeFileName nid <> ".unparsable"

plant :: Plant -> GraphFiles -> GraphFiles
plant = \case
  DropRecord OutgoingRecord e ->
    #nodes . ix e.source . _Right . #outgoing %~ deleteSet (outConnect e)
  DropRecord IncomingRecord e ->
    #nodes . ix e.sink . _Right . #incoming %~ deleteSet (inConnect e)
  RemoveFile nid -> #nodes %~ deleteMap nid
  SayOtherId nid claimed -> #nodes . ix nid . _Right . #nid .~ claimed
  Garble nid -> #nodes . ix nid .~ Left garbled
  StrayData nid -> #others . at nid %~ Just . insertSet (dataFileName nid) . fold
  KeptFile nid -> #others . at nid %~ Just . insertSet (keptName nid) . fold

-- | Every record of an undamaged graph
allRecords :: GraphFiles -> [(NID, RecordSide, Edge Text)]
allRecords files =
  [ (nid, side, e)
  | (nid, n) <- mapToList (readableNodes files),
    (side, e) <- recordedEdges nid n
  ]

-- | The damage that fsck must report for a plant in an undamaged graph
expectedDamage :: GraphFiles -> Plant -> Set Damage
expectedDamage files = \case
  DropRecord side e -> singletonSet (OneSidedEdge (otherSide side) e)
  RemoveFile removed ->
    setFromList
      ( [ EdgeToMissingNode side e
        | (nid, side, e) <- allRecords files,
          nid /= removed,
          snd (recordingNode side e) == removed
        ]
          <> [MissingOriginFile | removed == nilNID]
      )
  SayOtherId nid claimed -> singletonSet (FileSaysOtherId nid claimed)
  Garble nid -> singletonSet (UnparsableNodeFile nid garbled)
  StrayData _ -> mempty
  KeptFile nid -> singletonSet (KeptFileWithoutNode nid [keptName nid])

-- | A piece of damage that can be done to these files (which may be damaged
-- already)
genPlant :: GraphFiles -> Gen Plant
genPlant files =
  oneof $
    [ DropRecord <$> elements [IncomingRecord, OutgoingRecord] <*> elements edges
    | not (null edges)
    ]
      <> concat
        [ [ RemoveFile <$> elements nids,
            do
              nid <- elements nids
              SayOtherId nid <$> genNID `suchThat` (/= nid),
            Garble <$> elements nids
          ]
        | not (null nids)
        ]
      <> [pure (StrayData (smallNID 5)), pure (KeptFile (smallNID 5))]
  where
    nids = keys files.nodes
    edges = ordNub [e | (_, _, e) <- allRecords files]

test_scanDamage :: TestTree
test_scanDamage =
  testGroup
    "scanDamage"
    [ testProperty "finds nothing in an undamaged graph" $
        forAll genUndamaged \files -> scanDamage files === mempty,
      testProperty "reports exactly the damage that was done" . withMaxSuccess 2000 $
        forAll genUndamaged \files ->
          forAll (genPlant files) \planted ->
            scanDamage (plant planted files) === expectedDamage files planted
    ]

-- | What fsck printed and how it ended
runFsck :: FilePath -> IO ([String], Either UserError ())
runFsck dir = runFsckReading dir (pure ())

-- | 'runFsck', with something done to the files each time before fsck reads
-- the files of its findings again
runFsckReading :: FilePath -> IO () -> IO ([String], Either UserError ())
runFsckReading dir = runFsckReadings dir . const

-- | 'runFsckReading', given the number of the reading that is about to be
-- made, from 1
runFsckReadings :: FilePath -> (Int -> IO ()) -> IO ([String], Either UserError ())
runFsckReadings dir beforeReading = do
  printed <- newIORef []
  result <-
    runEff
      . runErrorNoCallStack @UserError
      . runRawGraphWithPath dir
      . interpret (\_ (Echo line) -> liftIO (modifyIORef printed (line :)))
      $ fsckRereading (liftIO . beforeReading)
  (,result) . reverse <$> readIORef printed

spec_fsck :: Spec
spec_fsck = do
  let n1 = smallNID 1
      n2 = smallNID 2
      n3 = smallNID 3
      n4 = smallNID 4
      n5 = smallNID 5
      write :: FilePath -> NID -> Node Text () -> IO ()
      write dir name n =
        B.writeFile (metadataFile dir name) . toStrict . Aeson.encode $ nodeToDTO n
      node' :: NID -> [Connect Text] -> [Connect Text] -> Node Text ()
      node' nid incoming outgoing =
        Node nid (setFromList incoming) (setFromList outgoing) ()
      -- the origin -a-> 1, and every kind of damage
      withEveryKind :: (FilePath -> IO ()) -> IO ()
      withEveryKind test = withEmptyTempGraph \dir -> do
        write dir nilNID $
          node'
            nilNID
            []
            [Connect "a" n1, Connect "b" n1, Connect "gone" n2, Connect "bad" n3]
        write dir n1 $ node' n1 [Connect "a" nilNID, Connect "c" nilNID] []
        B.writeFile (metadataFile dir n3) "{not json"
        write dir n4 $ node' n5 [] []
        -- the data of a node that has no node file: nothing to report
        B.writeFile (dir </> "000000000006.data") "stray"
        B.writeFile (dir </> "000000000001.data") "the data of a node"
        B.writeFile (dir </> "notes.txt") "not named after a node"
        B.writeFile (dir </> "000000000001 2.json") "a copy left by a sync"
        test dir
      everyKind =
        [ OneSidedEdge IncomingRecord (Edge nilNID "c" n1),
          OneSidedEdge OutgoingRecord (Edge nilNID "b" n1),
          EdgeToMissingNode OutgoingRecord (Edge nilNID "gone" n2),
          FileSaysOtherId n4 n5,
          UnparsableNodeFile
            n3
            "Unexpected \"not json\", expecting record key literal or }"
        ]
  it "says that an undamaged graph is undamaged" $
    withEmptyTempGraph \dir -> do
      write dir nilNID $ node' nilNID [] [Connect "a" n1]
      write dir n1 $ node' n1 [Connect "a" nilNID] []
      runFsck dir
        >>= ( `shouldSatisfy`
                \case
                  (["no damage found in 2 node files"], Right ()) -> True
                  _ -> False
            )
  it "reads every node file as it is and goes on past the ones it can't read" $
    withEveryKind \dir -> do
      scan <-
        either (fail . show) pure
          =<< runEff
            (runErrorNoCallStack @UserError (runRawGraphWithPath dir scanGraph))
      keys scan.files.nodes `shouldBe` [nilNID, n1, n3, n4]
      keys scan.contents `shouldBe` [nilNID, n1, n3, n4]
      scan.files.others
        `shouldBe` mapFromList
          [ (n1, singletonSet "000000000001.data"),
            (smallNID 6, singletonSet "000000000006.data")
          ]
      toList (scanDamage scan.files) `shouldBe` everyKind
  it "lists every piece of damage and fails" $
    withEveryKind \dir -> do
      (printed, result) <- runFsck dir
      printed `shouldBe` map describeDamage everyKind
      case result of
        Left (DamageFound 5) -> pure ()
        other -> expectationFailure ("ended with " <> show other)
  describe "when files change while it reads them" do
    let inGraph :: FilePath -> Eff [RawGraph, Error UserError, IOE] a -> IO a
        inGraph dir =
          either (fail . show) pure
            <=< runEff
            . runErrorNoCallStack @UserError
            . runRawGraphWithPath dir
        -- what is found once the files of a scan's findings are read again
        settled dir scan = do
          (latest, changed) <- inGraph dir (settle (const (pure ())) scan)
          pure (toList (scanDamage latest.files), changed)
        a = Edge nilNID "a" n1
        changedLine =
          "node files changed while fsck was reading them; what follows is"
            <> " what its latest reading of them shows"
        -- fsck on a graph that is changed once, right after the first reading
        fsckChangingOnce dir change = do
          done <- newIORef False
          runFsckReading dir do
            unlessM (readIORef done) (change >> writeIORef done True)
    it "drops a finding if the file that was missing has been written" $
      withEmptyTempGraph \dir -> do
        write dir nilNID $ node' nilNID [] [Connect "a" n1]
        scan <- inGraph dir scanGraph
        toList (scanDamage scan.files)
          `shouldBe` [EdgeToMissingNode OutgoingRecord a]
        write dir n1 $ node' n1 [Connect "a" nilNID] []
        settled dir scan `shouldReturn` ([], True)
    it "drops a finding if the other record of the edge has been written" $
      withEmptyTempGraph \dir -> do
        write dir nilNID $ node' nilNID [] [Connect "a" n1]
        write dir n1 $ node' n1 [] []
        scan <- inGraph dir scanGraph
        toList (scanDamage scan.files) `shouldBe` [OneSidedEdge OutgoingRecord a]
        write dir n1 $ node' n1 [Connect "a" nilNID] []
        settled dir scan `shouldReturn` ([], True)
    it "drops a finding if the lone record has been removed" $
      withEmptyTempGraph \dir -> do
        write dir nilNID $ node' nilNID [] [Connect "a" n1]
        write dir n1 $ node' n1 [] []
        scan <- inGraph dir scanGraph
        write dir nilNID $ node' nilNID [] []
        settled dir scan `shouldReturn` ([], True)
    it "drops a finding if the file that couldn't be read as a node now can" $
      withEmptyTempGraph \dir -> do
        write dir nilNID $ node' nilNID [] []
        B.writeFile (metadataFile dir n1) "{\"id\":"
        scan <- inGraph dir scanGraph
        length (scanDamage scan.files) `shouldBe` 1
        write dir n1 $ node' n1 [] []
        settled dir scan `shouldReturn` ([], True)
    it "keeps a finding that holds, and has what only the latest reading finds" $
      withEmptyTempGraph \dir -> do
        write dir nilNID $ node' nilNID [] [Connect "a" n1]
        write dir n1 $ node' n1 [] []
        scan <- inGraph dir scanGraph
        -- still without the record of a, and with a new lone record
        write dir n1 $ node' n1 [Connect "c" nilNID] []
        settled dir scan
          `shouldReturn` ( [ OneSidedEdge IncomingRecord (Edge nilNID "c" n1),
                             OneSidedEdge OutgoingRecord a
                           ],
                           True
                         )
    it "says that nothing changed if its findings hold as they are" $
      withEmptyTempGraph \dir -> do
        write dir nilNID $ node' nilNID [] [Connect "a" n1]
        write dir n1 $ node' n1 [] []
        scan <- inGraph dir scanGraph
        settled dir scan `shouldReturn` ([OneSidedEdge OutgoingRecord a], False)
    it "reads the files of a new finding again, which may be out of date" $
      -- 2.json is read before the edge b is added to it and never again unless
      -- the finding that the reading of 1.json gives makes fsck do so
      withEmptyTempGraph \dir -> do
        write dir nilNID $ node' nilNID [] [Connect "a" n1]
        write dir n1 $ node' n1 [] []
        write dir n2 $ node' n2 [] []
        (printed, result) <- fsckChangingOnce dir do
          write dir n1 $ node' n1 [Connect "a" nilNID] [Connect "b" n2]
          write dir n2 $ node' n2 [Connect "b" n1] []
        printed `shouldBe` [changedLine, "no damage found in 3 node files"]
        first show result `shouldBe` Right ()
    it "checks a finding that the files read again give in the same reading" $
      -- The scan reads 2.json before the edge c is recorded in it. The
      -- reading that then finds c in the origin's file reads 2.json again
      -- itself: it doesn't report the edge, nor leave it to a later reading,
      -- which comes after a pause.
      withEmptyTempGraph \dir -> do
        write dir nilNID $ node' nilNID [] [Connect "a" n1]
        write dir n1 $ node' n1 [] []
        write dir n2 $ node' n2 [] []
        readings <- newIORef []
        (printed, result) <- runFsckReadings dir \reading -> do
          modifyIORef readings (reading :)
          when (reading == 1) do
            write dir nilNID $ node' nilNID [] [Connect "c" n2]
            write dir n2 $ node' n2 [Connect "c" nilNID] []
        readIORef readings `shouldReturn` [1]
        printed `shouldBe` [changedLine, "no damage found in 3 node files"]
        first show result `shouldBe` Right ()
    it "says that a graph that was only being written is undamaged" $
      withEmptyTempGraph \dir -> do
        write dir nilNID $ node' nilNID [] [Connect "a" n1]
        (printed, result) <-
          fsckChangingOnce dir (write dir n1 $ node' n1 [Connect "a" nilNID] [])
        printed `shouldBe` [changedLine, "no damage found in 2 node files"]
        first show result `shouldBe` Right ()
    -- damage that is there before, during and after the scan
    it "reports a lone record whose other file is deleted meanwhile" $
      withEmptyTempGraph \dir -> do
        write dir nilNID $ node' nilNID [] [Connect "a" n1]
        write dir n1 $ node' n1 [] []
        (printed, result) <-
          fsckChangingOnce dir (removeFile (metadataFile dir n1))
        printed
          `shouldBe` [ changedLine,
                       describeDamage (EdgeToMissingNode OutgoingRecord a)
                     ]
        void (first show result) `shouldSatisfy` isLeft
    it "reports an edge to a missing file that is written without the record" $
      withEmptyTempGraph \dir -> do
        write dir nilNID $ node' nilNID [] [Connect "a" n1]
        (printed, result) <-
          fsckChangingOnce dir (write dir n1 $ node' n1 [] [])
        printed
          `shouldBe` [changedLine, describeDamage (OneSidedEdge OutgoingRecord a)]
        void (first show result) `shouldSatisfy` isLeft
    it "reports a file that can't be parsed and is rewritten as another such" $
      withEmptyTempGraph \dir -> do
        write dir nilNID $ node' nilNID [] []
        B.writeFile (metadataFile dir n1) "{not json"
        (printed, result) <-
          fsckChangingOnce dir (B.writeFile (metadataFile dir n1) "")
        printed
          `shouldBe` [ changedLine,
                       describeDamage
                         ( UnparsableNodeFile
                             n1
                             "Unexpected end-of-input, expecting JSON value"
                         )
                     ]
        void (first show result) `shouldSatisfy` isLeft
    it "reports what its last reading shows if the files never hold still" $
      withEmptyTempGraph \dir -> do
        write dir nilNID $ node' nilNID [] []
        write dir n1 $ node' n1 [Connect "0" nilNID] []
        readings <- newIORef (0 :: Int)
        -- another lone record each time
        (printed, result) <- runFsckReading dir do
          count <- atomicModifyIORef' readings (\c -> (c + 1, c + 1))
          write dir n1 $ node' n1 [Connect (tshow count) nilNID] []
        readIORef readings `shouldReturn` maxReadings
        printed
          `shouldBe` [ changedLine,
                       describeDamage . OneSidedEdge IncomingRecord $
                         Edge nilNID (tshow maxReadings) n1
                     ]
        void (first show result) `shouldSatisfy` isLeft
    it "counts a finding once the next reading finds the same, files or not" $
      -- the file changes every time, the damage that it shows doesn't
      withEmptyTempGraph \dir -> do
        write dir nilNID $ node' nilNID [] []
        write dir n1 $ node' n1 [Connect "c" nilNID] []
        readings <- newIORef (0 :: Int)
        (printed, _) <- runFsckReading dir do
          count <- atomicModifyIORef' readings (\c -> (c + 1, c + 1))
          write dir n1 $ node' n1 [Connect "c" nilNID] [Connect (tshow count) n1]
          write dir n1 $
            node' n1 [Connect "c" nilNID, Connect "l" n1] [Connect "l" n1]
        readIORef readings `shouldReturn` 1
        printed
          `shouldBe` [ changedLine,
                       describeDamage
                         (OneSidedEdge IncomingRecord (Edge nilNID "c" n1))
                     ]
    it "doesn't take a node that was created since the scan for a missing one" $
      -- the node 3 is created after the directory was listed; the file of the
      -- node 1, read again for another finding, refers to it by then. Its
      -- file is there and was not read: nothing is said about it
      withEmptyTempGraph \dir -> do
        write dir nilNID $ node' nilNID [] [Connect "a" n1]
        write dir n1 $ node' n1 [] []
        (printed, result) <- fsckChangingOnce dir do
          write dir n1 $ node' n1 [Connect "a" nilNID] [Connect "new" n3]
          write dir n3 $ node' n3 [Connect "new" n1] [Connect "on" n4]
          write dir n4 $ node' n4 [Connect "on" n3] []
        printed `shouldBe` [changedLine, "no damage found in 2 node files"]
        first show result `shouldBe` Right ()
    it "does take a node for missing that a file read again refers to in vain" $
      withEmptyTempGraph \dir -> do
        write dir nilNID $ node' nilNID [] [Connect "a" n1]
        write dir n1 $ node' n1 [] []
        (printed, _) <- fsckChangingOnce dir do
          write dir n1 $ node' n1 [Connect "a" nilNID] [Connect "new" n3]
        printed
          `shouldBe` [ changedLine,
                       describeDamage
                         (EdgeToMissingNode OutgoingRecord (Edge n1 "new" n3))
                     ]
  it "waits before each reading that follows one that found other damage" $
    -- and not before the first one
    map pauseBefore [1 .. maxReadings]
      `shouldBe` [0, 100000, 200000, 400000, 800000]
  describe "on a damaged graph that holds still" do
    -- with a lone record: given what is done to the graph before fsck runs
    -- and while it runs (between its two readings), the readings that it
    -- made after the scan, and what it printed
    let holdingStill before meanwhile = withEmptyTempGraph \dir -> do
          write dir nilNID $ node' nilNID [] [Connect "a" n1]
          write dir n1 $ node' n1 [] []
          before dir
          readings <- newIORef []
          (printed, _) <- runFsckReadings dir \reading -> do
            modifyIORef readings (reading :)
            meanwhile dir
          -- one more reading, which finds the files as they were: none after
          -- it, so no pause, and nothing is said to have changed
          readIORef readings `shouldReturn` [1]
          printed
            `shouldBe` [ describeDamage
                           (OneSidedEdge OutgoingRecord (Edge nilNID "a" n1))
                       ]
        nothing _ = pure ()
        inTheFuture path = do
          now <- getCurrentTime
          setModificationTime path (addUTCTime (10 * 365 * 86400) now)
    it "reads the files of its findings once more and doesn't wait" $
      holdingStill nothing nothing
    it "does the same if a node file is dated in the future" $
      holdingStill (\dir -> inTheFuture (metadataFile dir n1)) nothing
    it "does the same if the directory is dated in the future" $
      holdingStill inTheFuture nothing
    it "does the same if another file comes and goes in the directory" $
      holdingStill nothing \dir -> do
        B.writeFile (dir </> "unrelated.tmp") "x"
        removeFile (dir </> "unrelated.tmp")
    it "does the same if a file of a node of the finding that is no node file comes" $
      -- its data, or a file that is only named like its node file
      holdingStill nothing \dir -> do
        B.writeFile (dir </> "000000000001.data") "x"
        B.writeFile (dir </> "000000000001.json.unparsable.txt") "x"
    it "does the same if such a file goes" $
      holdingStill
        (\dir -> B.writeFile (dir </> "000000000001.data") "x")
        (\dir -> removeFile (dir </> "000000000001.data"))
  describe "on a graph that holds still" do
    it "reports an edge to an id that only differs in case from a file's name" $
      -- where the volume takes the two names for one, opening the name of the
      -- node without a file would open the other node's file
      withEmptyTempGraph \dir -> do
        let upper = unsafeNID "AbCdEfGhIjKl"
            lower = unsafeNID "aBcDeFgHiJkL"
        write dir nilNID $ node' nilNID [] [Connect "k" upper, Connect "z" lower]
        write dir upper $ node' upper [Connect "k" nilNID] []
        (printed, result) <- runFsck dir
        printed
          `shouldBe` [ describeDamage
                         (EdgeToMissingNode OutgoingRecord (Edge nilNID "z" lower))
                     ]
        void (first show result) `shouldSatisfy` isLeft
    it "reports a node file that is a symbolic link to a file that isn't there" $
      withEmptyTempGraph \dir -> do
        write dir nilNID $ node' nilNID [] [Connect "b" n2]
        createFileLink (dir </> "nowhere.json") (metadataFile dir n2)
        (printed, result) <- runFsck dir
        case printed of
          [damage] ->
            damage
              `shouldStartWith` ( "000000000002.json cannot be read as a node:"
                                    <> " the directory lists it but it can't be"
                                    <> " opened, as a symbolic link to a file"
                                    <> " that isn't there can't ("
                                )
          _ -> expectationFailure ("printed " <> show printed)
        void (first show result) `shouldSatisfy` isLeft
  it "reports what a fix kept of a node that has no file any more as left over" $
    withEmptyTempGraph \dir -> do
      write dir nilNID $ node' nilNID [] []
      B.writeFile (dir </> "000000000004.json.unparsable") "{not json"
      B.writeFile (dir </> "000000000004.json.unparsable-3") "{nor this"
      -- no file that a fix kept, whatever its name starts with
      B.writeFile (dir </> "000000000004.json.unparsable.txt") "notes"
      B.writeFile (dir </> "000000000004.json.unparsable-old") "notes"
      B.writeFile (dir </> "000000000004.json.unparsable-") "notes"
      -- nor one with a number that fix doesn't give: it counts from 2
      B.writeFile (dir </> "000000000004.json.unparsable-0") "notes"
      B.writeFile (dir </> "000000000004.json.unparsable-1") "notes"
      B.writeFile (dir </> "000000000004.json.unparsable-03") "notes"
      -- nor what is no file, whatever it is named: fix keeps a copy as a file
      createDirectory (dir </> "000000000004.json.unparsable-2")
      createFileLink (dir </> "nowhere") (dir </> "000000000004.json.unparsable-4")
      createFileLink
        (dir </> "000000000004.json.unparsable.txt")
        (dir </> "000000000004.json.unparsable-5")
      (printed, result) <- runFsck dir
      printed
        `shouldBe` [ "000000000004.json.unparsable,"
                       <> " 000000000004.json.unparsable-3 are left over from a"
                       <> " deleted node: there is no 000000000004.json any"
                       <> " more, of which fix had kept them when it couldn't"
                       <> " read that file as a node"
                   ]
      void (first show result) `shouldSatisfy` isLeft
  it "says nothing of a kept file whose node has a file" $
    withEmptyTempGraph \dir -> do
      write dir nilNID $ node' nilNID [] []
      B.writeFile (dir </> "000000000000.json.unparsable") "{not json"
      (printed, _) <- runFsck dir
      printed `shouldBe` ["no damage found in 1 node files"]
  it "calls nothing left over in a directory that is no graph" $
    -- no file in it is a node file that says the id it is named after
    withSystemTempDirectory "not-a-graph" \dir -> do
      B.writeFile (dir </> "00000000000A.json.unparsable") "{my notes"
      (printed, result) <- runFsck dir
      printed
        `shouldBe` ["there is no 000000000000.json, the file of the origin node"]
      void (first show result) `shouldSatisfy` isLeft
  it "reports that the origin has no file" $
    withEmptyTempGraph \dir -> do
      write dir n1 $ node' n1 [] []
      removeFile (metadataFile dir nilNID)
      (printed, _) <- runFsck dir
      printed
        `shouldBe` ["there is no 000000000000.json, the file of the origin node"]
  it "says how much it found" do
    show (DamageFound 1)
      `shouldBe` "the graph is damaged (damage-found): fsck found 1 problem,"
      <> " listed above; fix repairs what it can and says what it can't"
    show (DamageFound 3) `shouldContain` "fsck found 3 problems,"
