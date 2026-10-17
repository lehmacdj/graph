module Graph.FixSpec where

import Control.Concurrent (myThreadId)
import Control.Exception qualified as E
import DAL.DTO (nodeToDTO)
import DAL.DirectoryFormat (metadataFile)
import DAL.FileSystemOperations.Metadata
import DAL.Interpreters.GraphEditingSpec (directoryContents, runOnDisk)
import DAL.RawGraph
import Data.Aeson qualified as Aeson
import Data.ByteString qualified as B
import Data.Map.Strict qualified as Map
import Effect.IOWrapper.Echo
import Error.UserError
import Error.Warn
import Graph.Fix
import Graph.Fsck
import Graph.FsckSpec (allRecords, genPlant, genUndamaged, plant)
import Graph.GraphMetadataEditing (deleteNode)
import Models.Command (FixMode (..))
import Models.Connect
import Models.Damage
import Models.Edge
import Models.NID
import Models.Node
import MyPrelude
import System.Directory
  ( createDirectory,
    createFileLink,
    doesDirectoryExist,
    doesFileExist,
    emptyPermissions,
    getPermissions,
    listDirectory,
    pathIsSymbolicLink,
    removeFile,
    setOwnerWritable,
    setPermissions,
  )
import Utils.Testing
import Utils.Testing.TempGraph

-- | The files of a graph with up to four pieces of damage of any kind, one
-- done after the other
genDamaged :: Gen GraphFiles
genDamaged = do
  undamaged <- genUndamaged
  count <- chooseInt (0, 4)
  foldM (\files _ -> (`plant` files) <$> genPlant files) undamaged [1 .. count]

test_planFix :: TestTree
test_planFix =
  testGroup
    "planFix"
    [ testProperty "leaves an undamaged graph alone" $
        forAll genUndamaged \files ->
          let plan = planFix files
           in (plan.repairs, plan.writes, plan.setAside, plan.notRepaired)
                === ([], mempty, mempty, []),
      damaged "leaves only damage that it says it doesn't repair" \files plan ->
        scanDamage (applyPlan plan files)
          === setFromList (map fst plan.notRepaired),
      damaged
        "repairs every edge and node file unless a file's id is in doubt"
        \files plan ->
          let doubted = filesInDoubt files
              inDoubt = keysSet doubted <> setFromList (toList doubted)
              unexplained =
                [ d
                | (d, _) <- plan.notRepaired,
                  not (any (`member` inDoubt) (damagedNodes d))
                ]
           in counterexample ("in doubt: " <> show doubted)
                . classify (not (null doubted)) "a file's id in doubt"
                . classify (null plan.repairs) "nothing to repair"
                . classify (not (null plan.setAside)) "a file set aside"
                $ unexplained === [],
      damaged "gives a reason for all that it doesn't repair" \_ plan ->
        filter ((== "fix has no repair for it") . snd) plan.notRepaired === [],
      damaged "keeps every record that the damaged graph had" \files plan ->
        let repaired = readableNodes (applyPlan plan files)
            lost =
              [ record
              | record@(nid, side, e) <- allRecords files,
                not (maybe False (records side e) (lookup nid repaired))
              ]
         in lost === [],
      damaged "deletes no file and keeps aside each one it replaces unread" \files plan ->
        let unreadable = keysSet (Map.filter isLeft files.nodes)
         in (keysSet files.nodes `difference` keysSet (applyPlan plan files).nodes)
              === mempty
              .&&. (keysSet plan.writes `intersection` unreadable)
              === keysSet plan.setAside,
      damaged "has nothing left to do once it is done" \files plan ->
        (planFix (applyPlan plan files)).repairs === [],
      damaged "writes only files that it changes" \files plan ->
        Map.filterWithKey
          (\nid n -> lookup nid files.nodes == Just (Right n))
          plan.writes
          === mempty
    ]
  where
    damaged name holds =
      testProperty name . withMaxSuccess 1000 $
        forAll genDamaged \files ->
          let plan = planFix files
           in counterexample (show plan) (holds files plan)

spec_planFix :: Spec
spec_planFix = do
  let n1 = smallNID 1
      n2 = smallNID 2
      n3 = smallNID 3
      node' :: NID -> [Connect Text] -> [Connect Text] -> Node Text ()
      node' nid incoming outgoing =
        Node nid (setFromList incoming) (setFromList outgoing) ()
      graphFiles nodes =
        GraphFiles
          { nodes = mapFromList nodes,
            unread = mempty,
            unreadable = mempty,
            others = mempty
          }
  it "adds the record that an edge lacks, whichever one it is" do
    let files =
          graphFiles
            [ (nilNID, Right (node' nilNID [] [Connect "b" n1])),
              (n1, Right (node' n1 [Connect "c" nilNID] []))
            ]
    (planFix files).repairs
      `shouldBe` [ AddRecord IncomingRecord (Edge nilNID "b" n1),
                   AddRecord OutgoingRecord (Edge nilNID "c" n1)
                 ]
  it "rebuilds a node that has no file from every record of it" do
    let files =
          graphFiles
            [ (nilNID, Right (node' nilNID [Connect "up" n2] [Connect "b" n2])),
              (n1, Right (node' n1 [Connect "d" n2] []))
            ]
        plan = planFix files
    plan.repairs `shouldBe` [Rebuild n2 3]
    lookup n2 plan.writes
      `shouldBe` Just
        ( node'
            n2
            [Connect "b" nilNID]
            [Connect "up" nilNID, Connect "d" n1]
        )
  it "keeps a file that it can't read and rebuilds the node" do
    let files =
          (graphFiles [(nilNID, Right (node' nilNID [] [Connect "b" n1])), (n1, Left "garbled")])
            { others = singletonMap n1 (setFromList ["000000000001.json.unparsable", "000000000001.data"])
            }
        plan = planFix files
    plan.repairs
      `shouldBe` [SetAside n1 "000000000001.json.unparsable-2", Rebuild n1 1]
    describeRepair files (Rebuild n1 1)
      `shouldBe` "rebuild 000000000001.json from the 1 edge that other nodes"
      <> " record with @000000000001 (any other edge it had is only in the"
      <> " file that is kept; its data in 000000000001.data is kept)"
    describeRepair (graphFiles []) (Rebuild n1 0)
      `shouldBe` "rebuild 000000000001.json without edges, since no other node"
      <> " records an edge with @000000000001 (any other edge it had is lost,"
      <> " and so is its data if it had any)"
  it "rebuilds an unreadable node that no node records an edge with as empty" do
    let files = graphFiles [(nilNID, Right (node' nilNID [] [])), (n1, Left "garbled")]
    lookup n1 (planFix files).writes `shouldBe` Just (node' n1 [] [])
  it "rebuilds a missing origin" do
    let files = graphFiles [(n1, Right (node' n1 [Connect "a" nilNID] []))]
    (planFix files).repairs `shouldBe` [Rebuild nilNID 1]
    (planFix (graphFiles [(n1, Right (node' n1 [] []))])).repairs
      `shouldBe` [Rebuild nilNID 0]
  it "corrects the id of a file whose edges are recorded for its name" do
    let files =
          graphFiles
            [ (nilNID, Right (node' nilNID [] [Connect "b" n2])),
              (n2, Right (node' n3 [Connect "b" nilNID] []))
            ]
        plan = planFix files
    plan.repairs `shouldBe` [CorrectId n2 n3]
    plan.writes `shouldBe` singletonMap n2 (node' n2 [Connect "b" nilNID] [])
    plan.notRepaired `shouldBe` []
    -- the correction is right only as long as 0.json records the edge for
    -- the node 2
    plan.derivedFrom `shouldBe` singletonSet nilNID
  it "leaves a file alone whose edges are recorded for the node it says it is" do
    -- 2.json is a copy of 3.json
    let files =
          graphFiles
            [ (nilNID, Right (node' nilNID [] [Connect "c" n3])),
              (n2, Right (node' n3 [Connect "c" nilNID] [])),
              (n3, Right (node' n3 [Connect "c" nilNID] []))
            ]
        plan = planFix files
    plan.repairs `shouldBe` []
    plan.writes `shouldBe` mempty
    map fst plan.notRepaired
      `shouldBe` [ OneSidedEdge IncomingRecord (Edge nilNID "c" n2),
                   FileSaysOtherId n2 n3
                 ]
    map snd plan.notRepaired
      `shouldBe` [ "it involves a node that 000000000002.json may or may not be"
                     <> " (see there)",
                   "other nodes record its edges for @000000000003, so it may"
                     <> " be a copy of 000000000003.json or a misnamed file:"
                     <> " rename it or correct the id in it by hand"
                 ]
  it "doesn't rebuild the node that a file in doubt says it is" do
    -- 2.json may be the lost 3.json under a wrong name
    let files =
          graphFiles
            [ (nilNID, Right (node' nilNID [] [Connect "c" n3, Connect "a" n1])),
              (n1, Right (node' n1 [] [])),
              (n2, Right (node' n3 [Connect "c" nilNID] []))
            ]
        plan = planFix files
    plan.repairs `shouldBe` [AddRecord IncomingRecord (Edge nilNID "a" n1)]
    map fst plan.notRepaired
      `shouldBe` [ OneSidedEdge IncomingRecord (Edge nilNID "c" n2),
                   EdgeToMissingNode OutgoingRecord (Edge nilNID "c" n3),
                   FileSaysOtherId n2 n3
                 ]
  it "leaves a file in doubt whose own edge leads to the node it says it is" do
    -- 2.json says it is the node 3 and records an edge to the node 3: as the
    -- node 3 that would be a loop, which the file may well be a copy of
    let files =
          graphFiles
            [ (nilNID, Right (node' nilNID [] [])),
              (n2, Right (node' n3 [] [Connect "x" n3])),
              (n3, Right (node' n3 [Connect "x" n2] []))
            ]
        plan = planFix files
    filesInDoubt files `shouldBe` singletonMap n2 n3
    plan.repairs `shouldBe` []
    map fst plan.notRepaired `shouldBe` [FileSaysOtherId n2 n3]
  it "does nothing about the data of a node that has no node file" do
    let files =
          (graphFiles [(nilNID, Right (node' nilNID [] []))])
            { others = singletonMap n1 (singletonSet "000000000001.data")
            }
        plan = planFix files
    (plan.repairs, plan.writes, plan.notRepaired) `shouldBe` ([], mempty, [])
  it "removes what a fix kept of a node that has no file, and makes no node of it" do
    let kept = ["000000000001.json.unparsable", "000000000001.json.unparsable-3"]
        files =
          (graphFiles [(nilNID, Right (node' nilNID [] []))])
            { others = singletonMap n1 (setFromList ("000000000001.data" : kept))
            }
        plan = planFix files
    plan.repairs `shouldBe` [RemoveLeftOver n1 kept]
    plan.removals `shouldBe` singletonMap n1 kept
    (plan.setAside, plan.writes, plan.notRepaired) `shouldBe` (mempty, mempty, [])
    -- the data of the node is none of fix's business
    (applyPlan plan files).others
      `shouldBe` singletonMap n1 (singletonSet "000000000001.data")
    describeRepair files (RemoveLeftOver n1 kept)
      `shouldBe` "remove 000000000001.json.unparsable,"
      <> " 000000000001.json.unparsable-3, left over from a deleted node: there"
      <> " is no 000000000001.json any more, of which fix had kept them"
  it "takes no file for a kept one whose name only starts like one" do
    let files =
          (graphFiles [(nilNID, Right (node' nilNID [] []))])
            { others =
                singletonMap n1 . setFromList $
                  [ "000000000001.json.unparsable.txt",
                    "000000000001.json.unparsable-old",
                    "000000000001.json.unparsable-",
                    -- fix numbers its copies from 2, without leading zeros
                    "000000000001.json.unparsable-0",
                    "000000000001.json.unparsable-1",
                    "000000000001.json.unparsable-02"
                  ]
            }
        plan = planFix files
    (plan.repairs, plan.removals, plan.notRepaired) `shouldBe` ([], mempty, [])
    toList (scanDamage files) `shouldBe` []
  it "leaves what was kept of a node that it rebuilds from other nodes' records" do
    let files =
          (graphFiles [(nilNID, Right (node' nilNID [] [Connect "b" n1]))])
            { others = singletonMap n1 (singletonSet "000000000001.json.unparsable")
            }
        plan = planFix files
    plan.repairs `shouldBe` [Rebuild n1 1]
    plan.removals `shouldBe` mempty
    toList (scanDamage (applyPlan plan files)) `shouldBe` []
  describe "while a node file can't be read" do
    -- such a file may hold any id and any record, so no repair is known to
    -- be right without it
    let unreadableReason = "there is no permission to read it"
        withUnreadable unreadable nodes =
          (graphFiles ((unreadable, Left unreadableReason) : nodes))
            { unreadable = singletonSet unreadable
            }
        nothingWhile name =
          "nothing is repaired while " <> name <> " can't be read"
        repairsNothing files = do
          let plan = planFix files
          (plan.repairs, plan.writes, plan.setAside, plan.derivedFrom)
            `shouldBe` ([], mempty, mempty, mempty)
          map fst plan.notRepaired `shouldBe` toList (scanDamage files)
          ordNub (map snd plan.notRepaired)
            `shouldBe` [nothingWhile "000000000005.json"]
    it "doesn't rebuild a node that the unreadable file may be" do
      -- 5.json may be the file of the node 2 under another name: readable,
      -- it says the id 2 and its id is in doubt
      let n5 = smallNID 5
          nodes =
            [ (nilNID, Right (node' nilNID [] [Connect "b" n2, Connect "a" n1])),
              (n1, Right (node' n1 [Connect "a" nilNID, Connect "d" n2] []))
            ]
          misnamed = node' n2 [Connect "b" nilNID] [Connect "d" n1]
      repairsNothing (withUnreadable n5 nodes)
      (planFix (graphFiles ((n5, Right misnamed) : nodes))).repairs `shouldBe` []
    it "doesn't add a record that the unreadable file may have a say in" do
      -- 5.json may be a copy of 2.json that records the edge x
      let n5 = smallNID 5
          nodes =
            [ (nilNID, Right (node' nilNID [] [Connect "b" n2, Connect "x" n2])),
              (n2, Right (node' n2 [Connect "b" nilNID] []))
            ]
          copy = node' n2 [Connect "b" nilNID, Connect "x" nilNID] []
      repairsNothing (withUnreadable n5 nodes)
      (planFix (graphFiles ((n5, Right copy) : nodes))).repairs `shouldBe` []
    it "makes no repair either that the file seems to have no part in" do
      let n5 = smallNID 5
      repairsNothing . withUnreadable n5 $
        [ (nilNID, Right (node' nilNID [] [Connect "b" n2])),
          (n2, Right (node' n2 [] [])),
          (n3, Left "garbled")
        ]
  it "tells a graph from a directory with a file that is named like a node" do
    looksLikeGraph (graphFiles [(n1, Right (node' n1 [] []))]) `shouldBe` True
    looksLikeGraph (graphFiles [(n1, Left "garbled"), (n2, Right (node' n3 [] []))])
      `shouldBe` False
    looksLikeGraph (graphFiles []) `shouldBe` False
  it "doesn't take a directory for a graph for a file named like one that a fix kept" do
    -- anyone can name a file so, and fix would remove it
    let kept nid = singletonMap nid (singletonSet (nodeFileName nid <> ".unparsable"))
        onlyKept = (graphFiles []) {others = kept n1}
    looksLikeGraph ((graphFiles [(n1, Left "garbled")]) {others = kept nilNID})
      `shouldBe` False
    looksLikeGraph onlyKept `shouldBe` False
    -- and nothing is called left over from a node there
    toList (scanDamage onlyKept) `shouldBe` [MissingOriginFile]
  it "numbers the copies that it keeps from 2, and takes no other name for one" do
    let named suffix = isKeptNodeFileName "000000000001.json" ("000000000001.json.unparsable" <> suffix)
    filter named ["", "-2", "-3", "-10", "-123"] `shouldBe` ["", "-2", "-3", "-10", "-123"]
    filter named ["-", "-0", "-1", "-01", "-02", "-2x", "-old", ".txt", "2", "--2"]
      `shouldBe` []
    take 3 (keptNodeFileNames "000000000001.json")
      `shouldBe` [ "000000000001.json.unparsable",
                   "000000000001.json.unparsable-2",
                   "000000000001.json.unparsable-3"
                 ]

-- | The files of a directory with their content, but for one that can't be
-- read at the moment
directoryContentsBut :: FilePath -> FilePath -> IO (Map FilePath ByteString)
directoryContentsBut unreadable dir = do
  files <- filter (/= unreadable) <$> listDirectory dir
  mapFromList <$> for files \file -> (file,) <$> B.readFile (dir </> file)

-- | What fix printed and how it ended. With @dry@, as @ge --dry-run@.
runFix :: Bool -> FixMode -> FilePath -> IO ([String], Either UserError ())
runFix dry mode dir = do
  printed <- newIORef []
  result <-
    runEff
      . runErrorNoCallStack @UserError
      . runRawGraphWithPath dir
      . interpret (\_ (Echo line) -> liftIO (modifyIORef printed (line :)))
      . ignoreWarnings @UserError
      . ( if dry
            then runGraphMetadataFilesystemOperationsDryRun True
            else runGraphMetadataFilesystemOperationsIO True
        )
      $ fix mode
  (,result) . reverse <$> readIORef printed

spec_fix :: Spec
spec_fix = do
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
        B.writeFile (dir </> "000000000003.data") "the data of 3"
        write dir n4 $ node' n5 [] []
        -- the data of a node that has no node file: none of fix's business
        B.writeFile (dir </> "000000000006.data") "stray"
        test dir
      scanned dir =
        either (fail . show) pure
          =<< runEff
            (runErrorNoCallStack @UserError (runRawGraphWithPath dir scanGraph))
      listing =
        [ "- keep 000000000003.json, which cannot be read as a node, as"
            <> " 000000000003.json.unparsable",
          "- correct the id in 000000000004.json from @000000000005 to"
            <> " @000000000004",
          "- rebuild 000000000002.json from the 1 edge that other nodes"
            <> " record with @000000000002 (any other edge it had is lost, and"
            <> " so is its data if it had any)",
          "- rebuild 000000000003.json from the 1 edge that other nodes"
            <> " record with @000000000003 (any other edge it had is only in"
            <> " the file that is kept; its data in 000000000003.data is kept)",
          "- add the edge @000000000000 -\"b\"-> @000000000001 to the incoming"
            <> " edges in 000000000001.json",
          "- add the edge @000000000000 -\"c\"-> @000000000001 to the outgoing"
            <> " edges in 000000000000.json"
        ]
      succeeds = \case
        Right () -> pure ()
        other -> expectationFailure ("ended with " <> show other)
      diskFull = mkIOError fullErrorType "write" Nothing (Just "/g/file")
      -- fix, where writing the file of one node fails ('Right'), or keeping
      -- the copy of it ('Left')
      fixFailingToWrite :: Either NID NID -> FilePath -> IO (Either UserError ())
      fixFailingToWrite bad dir =
        runEff
          . runErrorNoCallStack @UserError
          . runRawGraphWithPath dir
          . interpret (\_ (Echo _) -> pure ())
          . ignoreWarnings @UserError
          . runGraphMetadataFilesystemOperationsIO True
          . interpose @GraphMetadataFilesystemOperations
            ( \_ -> \case
                WriteNodeMetadata n
                  | Right n.nid == bad -> throwError (IOFail diskFull)
                  | otherwise -> writeNodeMetadata n
                ReadNodeMetadata nid -> readNodeMetadata nid
                DeleteNodeMetadata nid -> deleteNodeMetadata nid
                KeepNodeMetadataCopy nid name
                  | Left nid == bad -> throwError (IOFail diskFull)
                  | otherwise -> keepNodeMetadataCopy nid name
                RemoveKeptNodeMetadata name -> removeKeptNodeMetadata name
                ListNodeMetadata -> listNodeMetadata
                OnlyDescribesWrites -> onlyDescribesWrites
            )
          $ fix ApplyRepairs
      -- fix, where an interrupt arrives in place of the write of one node's
      -- file: what it warned of, and whether the interrupt went on
      fixInterruptedWriting :: NID -> FilePath -> IO ([String], Bool)
      fixInterruptedWriting interruptedAt dir = do
        warned <- newIORef []
        outcome <-
          E.try @E.AsyncException
            . runEff
            . runErrorNoCallStack @UserError
            . runRawGraphWithPath dir
            . interpret (\_ (Echo _) -> pure ())
            . interpret @(Warn UserError) (\_ (Warn e) -> modifyIORef warned (show @UserError e :))
            . runGraphMetadataFilesystemOperationsIO True
            . interpose @GraphMetadataFilesystemOperations
              ( \_ -> \case
                  WriteNodeMetadata n
                    -- as an interrupt arrives: thrown at the thread
                    | n.nid == interruptedAt ->
                        liftIO (myThreadId >>= (`E.throwTo` E.UserInterrupt))
                    | otherwise -> writeNodeMetadata n
                  ReadNodeMetadata nid -> readNodeMetadata nid
                  DeleteNodeMetadata nid -> deleteNodeMetadata nid
                  KeepNodeMetadataCopy nid name -> keepNodeMetadataCopy nid name
                  RemoveKeptNodeMetadata name -> removeKeptNodeMetadata name
                  ListNodeMetadata -> listNodeMetadata
                  OnlyDescribesWrites -> onlyDescribesWrites
              )
            $ fix ApplyRepairs
        warnings <- reverse <$> readIORef warned
        pure (warnings, either (== E.UserInterrupt) (const False) outcome)
      stopped what after result =
        first show result
          `shouldBe` Left
            ( "fix stopped part way: "
                <> what
                <> ": there is no space left for it (/g/file: write: resource"
                <> " exhausted)"
                <> after
                <> ". What was written before it stays; run fix again for the"
                <> " rest."
            )
  it "lists its repairs, makes them, and says what is left" $
    withEveryKind \dir -> do
      before <- directoryContents dir
      (printed, result) <- runFix False ApplyRepairs dir
      printed `shouldBe` "repairs:" : listing
      succeeds result
      scan <- scanned dir
      toList (scanDamage scan.files) `shouldBe` []
      readableNodes scan.files
        `shouldBe` mapFromList
          [ ( nilNID,
              node'
                nilNID
                []
                [ Connect "a" n1,
                  Connect "b" n1,
                  Connect "c" n1,
                  Connect "gone" n2,
                  Connect "bad" n3
                ]
            ),
            ( n1,
              node'
                n1
                [Connect "a" nilNID, Connect "b" nilNID, Connect "c" nilNID]
                []
            ),
            (n2, node' n2 [Connect "gone" nilNID] []),
            (n3, node' n3 [Connect "bad" nilNID] []),
            (n4, node' n4 [] [])
          ]
      -- nothing is lost: the file that couldn't be read is kept, and so are
      -- the data files
      now <- directoryContents dir
      lookup "000000000003.json.unparsable" now `shouldBe` Just "{not json"
      lookup "000000000003.data" now `shouldBe` Just "the data of 3"
      lookup "000000000006.data" now `shouldBe` Just "stray"
      (keysSet before `difference` keysSet now) `shouldBe` mempty
  it "finds nothing to repair after that" $
    withEveryKind \dir -> do
      _ <- runFix False ApplyRepairs dir
      (printed, result) <- runFix False ApplyRepairs dir
      printed `shouldBe` ["nothing to repair"]
      succeeds result
  it "only lists its repairs when asked to" $
    withEveryKind \dir -> do
      before <- directoryContents dir
      (printed, result) <- runFix False ListRepairs dir
      printed `shouldBe` "repairs (only listed, nothing is written):" : listing
      succeeds result
      directoryContents dir `shouldReturn` before
  it "writes nothing in a dry run of ge" $
    withEveryKind \dir -> do
      before <- directoryContents dir
      (printed, _) <- runFix True ApplyRepairs dir
      printed `shouldBe` "repairs (only listed, nothing is written):" : listing
      directoryContents dir `shouldReturn` before
  it "says what it would do, not what it did, in a dry run of ge" $
    withEveryKind \dir -> do
      let readOnly = metadataFile dir n1
      permissions <- getPermissions readOnly
      setPermissions readOnly (setOwnerWritable False permissions)
      (_, result) <- runFix True ApplyRepairs dir
      setPermissions readOnly permissions
      first show result
        `shouldBe` Left
          ( "fix would write nothing: there is no permission to write"
              <> " 000000000001.json in "
              <> dir
          )
  it "says so when there is nothing to repair" $
    withEmptyTempGraph \dir ->
      runFix False ApplyRepairs dir
        >>= ( `shouldSatisfy`
                \case
                  (["nothing to repair"], Right ()) -> True
                  _ -> False
            )
  it "writes nothing if a file changed since it read the graph" $
    withEveryKind \dir -> do
      scan <- scanned dir
      write dir n1 $ node' n1 [Connect "a" nilNID] [Connect "new" n4]
      before <- directoryContents dir
      result <-
        runEff
          . runErrorNoCallStack @UserError
          . runRawGraphWithPath dir
          . interpret (\_ (Echo _) -> pure ())
          . ignoreWarnings @UserError
          . runGraphMetadataFilesystemOperationsIO True
          $ repair ApplyRepairs scan
      first show result
        `shouldBe` Left
          ( "fix wrote nothing: 000000000001.json changed while fix was"
              <> " looking at the graph; run fix again"
          )
      directoryContents dir `shouldReturn` before
  it "writes nothing if a file that it took a new record from changed" $
    withEmptyTempGraph \dir -> do
      -- only the origin records the edge to the node 1
      write dir nilNID $ node' nilNID [] [Connect "x" n1]
      write dir n1 $ node' n1 [] []
      scan <- scanned dir
      (planFix scan.files).repairs
        `shouldBe` [AddRecord IncomingRecord (Edge nilNID "x" n1)]
      -- the edge is removed meanwhile; 1.json, the file to write, is as it was
      write dir nilNID $ node' nilNID [] []
      before <- directoryContents dir
      result <-
        runEff
          . runErrorNoCallStack @UserError
          . runRawGraphWithPath dir
          . interpret (\_ (Echo _) -> pure ())
          . ignoreWarnings @UserError
          . runGraphMetadataFilesystemOperationsIO True
          $ repair ApplyRepairs scan
      first show result
        `shouldBe` Left
          ( "fix wrote nothing: 000000000000.json changed while fix was"
              <> " looking at the graph; run fix again"
          )
      directoryContents dir `shouldReturn` before
  it "writes nothing at all if one of the files can't be written" $
    withEveryKind \dir -> do
      let readOnly = metadataFile dir n1
      permissions <- getPermissions readOnly
      setPermissions readOnly (setOwnerWritable False permissions)
      before <- directoryContents dir
      (_, result) <- runFix False ApplyRepairs dir
      setPermissions readOnly permissions
      first show result
        `shouldBe` Left
          ( "fix wrote nothing: there is no permission to write"
              <> " 000000000001.json in "
              <> dir
          )
      -- in particular 000000000003.json is not kept aside
      directoryContents dir `shouldReturn` before
  it "keeps a copy of a file only when it gets to replace the file" $
    withEveryKind \dir -> do
      -- 0, 1 and 2 are written before 3, whose file can't be read as a node
      fixFailingToWrite (Right n2) dir
        >>= stopped "000000000002.json could not be written" ""
      now <- directoryContents dir
      lookup "000000000003.json" now `shouldBe` Just "{not json"
      lookup "000000000003.json.unparsable" now `shouldBe` Nothing
      (_, result) <- runFix False ApplyRepairs dir
      succeeds result
      scan <- scanned dir
      toList (scanDamage scan.files) `shouldBe` []
  it "says that it stopped part way when an interrupt stops it there" $
    withEveryKind \dir -> do
      -- 0 and 1 are written before 2
      fixInterruptedWriting n2 dir
        `shouldReturn` ( [ "fix stopped part way: it was interrupted. What was"
                             <> " written before it stays; run fix again for the"
                             <> " rest."
                         ],
                         True
                       )
      (_, result) <- runFix False ApplyRepairs dir
      succeeds result
      scan <- scanned dir
      toList (scanDamage scan.files) `shouldBe` []
  it "says nothing when an interrupt stops it before it has written anything" $
    withEveryKind \dir -> do
      before <- directoryContents dir
      -- the file of the origin is the first that it writes
      fixInterruptedWriting nilNID dir `shouldReturn` ([], True)
      directoryContents dir `shouldReturn` before
  it "leaves a file that it can't read as it is if the copy of it can't be written" $
    withEveryKind \dir -> do
      fixFailingToWrite (Left n3) dir
        >>= stopped
          ( "000000000003.json.unparsable, the copy to keep of"
              <> " 000000000003.json, could not be written"
          )
          "; 000000000003.json is as it was"
      now <- directoryContents dir
      lookup "000000000003.json" now `shouldBe` Just "{not json"
      lookup "000000000003.json.unparsable" now `shouldBe` Nothing
  it "never leaves a node without its file, also if the new one can't be written" $
    withEveryKind \dir -> do
      fixFailingToWrite (Right n3) dir
        >>= stopped
          "000000000003.json could not be written"
          "; what it held is kept in 000000000003.json.unparsable"
      now <- directoryContents dir
      lookup "000000000003.json" now `shouldBe` Just "{not json"
      lookup "000000000003.json.unparsable" now `shouldBe` Just "{not json"
      -- the node file is what it was, so the next fix does what this one
      -- would have done (the file after it was not reached either), and
      -- keeps another copy
      scan <- scanned dir
      toList (scanDamage scan.files) `shouldSatisfy` \case
        [FileSaysOtherId _ _, UnparsableNodeFile nid _] -> nid == n3
        _ -> False
      (printed, result) <- runFix False ApplyRepairs dir
      succeeds result
      printed
        `shouldBe` [ "repairs:",
                     "- keep 000000000003.json, which cannot be read as a node,"
                       <> " as 000000000003.json.unparsable-2",
                     "- correct the id in 000000000004.json from @000000000005"
                       <> " to @000000000004",
                     "- rebuild 000000000003.json from the 1 edge that other"
                       <> " nodes record with @000000000003 (any other edge it"
                       <> " had is only in the file that is kept; its data in"
                       <> " 000000000003.data is kept)"
                   ]
      after <- scanned dir
      toList (scanDamage after.files) `shouldBe` []
      lookup n3 (readableNodes after.files)
        `shouldBe` Just (node' n3 [Connect "bad" nilNID] [])
  it "does nothing if a node file can't be read, and says once which and why" $
    withEveryKind \dir -> do
      let unreadable = metadataFile dir n1
      permissions <- getPermissions unreadable
      setPermissions unreadable emptyPermissions
      before <- keysSet <$> directoryContentsBut "000000000001.json" dir
      (printed, result) <- runFix False ApplyRepairs dir
      now <- keysSet <$> directoryContentsBut "000000000001.json" dir
      setPermissions unreadable permissions
      printed `shouldBe` []
      now `shouldBe` before
      case first (lines . show) result of
        Left [header, file] -> do
          header
            `shouldBe` "fix did nothing: it can't read every node file, and a"
            <> " file that it can't read may hold any id and any edge, so no"
            <> " repair is known to be right without it (unreadable-node-file)"
          file
            `shouldStartWith` ( "- 000000000001.json: there is no permission to"
                                  <> " read it ("
                                  <> unreadable
                                  <> ": "
                              )
          file `shouldEndWith` "). Make it readable and run fix again."
          -- the path and what the system said are there once
          length (filter (unreadable `isPrefixOf`) (tails file)) `shouldBe` 1
        other -> expectationFailure ("ended with " <> show other)
      -- every repair is made once the file can be read
      (_, again) <- runFix False ApplyRepairs dir
      succeeds again
  it "says of a directory that is named like a node file what to do with it" $
    withEveryKind \dir -> do
      createDirectory (dir </> "000000000009.json")
      (printed, result) <- runFix False ApplyRepairs dir
      printed `shouldBe` []
      case first (lines . show) result of
        Left [_, file] -> do
          file
            `shouldStartWith` "- 000000000009.json: it is not a file but a directory ("
          file
            `shouldEndWith` "). Move it out of the graph's directory and run fix again."
        other -> expectationFailure ("ended with " <> show other)
  it "says of a link to a file that isn't there what to do with it" $
    withEmptyTempGraph \dir -> do
      write dir nilNID $ node' nilNID [] [Connect "b" n2]
      createFileLink (dir </> "nowhere.json") (metadataFile dir n2)
      before <- keysSet <$> directoryContentsBut "000000000002.json" dir
      results <- for [1, 2 :: Int] \_ -> runFix False ApplyRepairs dir
      for_ results \(printed, result) -> do
        printed `shouldBe` []
        case first (lines . show) result of
          Left [_, file] -> do
            file
              `shouldStartWith` ( "- 000000000002.json: the directory lists it but"
                                    <> " it can't be opened, as a symbolic link to"
                                    <> " a file that isn't there can't ("
                                )
            file
              `shouldEndWith` ( "). Remove the link, or bring back the file that"
                                  <> " it points to, and run fix again."
                              )
          other -> expectationFailure ("ended with " <> show other)
      (keysSet <$> directoryContentsBut "000000000002.json" dir) `shouldReturn` before
      -- without the link the node is rebuilt
      removeFile (metadataFile dir n2)
      (_, result) <- runFix False ApplyRepairs dir
      succeeds result
  it "says that a file changed if it was deleted while the graph was read" $
    withEmptyTempGraph \dir -> do
      write dir nilNID $ node' nilNID [] [Connect "b" n2]
      -- as a scan finds a file that is listed and then gone
      createFileLink (dir </> "nowhere.json") (metadataFile dir n2)
      scan <- scanned dir
      removeFile (metadataFile dir n2)
      result <-
        runEff
          . runErrorNoCallStack @UserError
          . runRawGraphWithPath dir
          . interpret (\_ (Echo _) -> pure ())
          . ignoreWarnings @UserError
          . runGraphMetadataFilesystemOperationsIO True
          $ repair ApplyRepairs scan
      first show result
        `shouldBe` Left
          ( "fix did nothing: 000000000002.json changed while fix was looking"
              <> " at the graph; run fix again"
          )
  it "doesn't write a file over one whose name differs only in letter case" $
    -- where the volume takes the two names for one name; elsewhere the node
    -- that has no file is simply rebuilt
    withEmptyTempGraph \dir -> do
      let upper = unsafeNID "AbCdEfGhIjKl"
          lower = unsafeNID "aBcDeFgHiJkL"
      write dir nilNID $ node' nilNID [] [Connect "k" upper, Connect "z" lower]
      write dir upper $ node' upper [Connect "k" nilNID] []
      oneName <- doesFileExist (metadataFile dir lower)
      before <- directoryContents dir
      results <- for [1, 2 :: Int] \_ -> runFix False ApplyRepairs dir
      if oneName
        then do
          for_ results \(printed, result) -> do
            printed `shouldBe` []
            first show result
              `shouldBe` Left
                ( "fix did nothing: the directory lists no aBcDeFgHiJkL.json,"
                    <> " but a file opens under that name: AbCdEfGhIjKl.json,"
                    <> " whose name differs only in letter case, which this"
                    <> " volume doesn't tell apart. Writing aBcDeFgHiJkL.json"
                    <> " would replace that file, so fix can't rebuild"
                    <> " @aBcDeFgHiJkL on this volume, and ge has no other"
                    <> " repair for it: rm and rmnf of the edges that other"
                    <> " nodes record with @aBcDeFgHiJkL are refused, like"
                    <> " every edit next to damage. What a person can do: run"
                    <> " fix on a copy of the graph on a volume that tells such"
                    <> " names apart, or take those edges out of the node files"
                    <> " that record them by hand (fsck lists them)"
                )
          directoryContents dir `shouldReturn` before
        else for_ results (succeeds . snd)
  it "takes a file that was created since the scan for a file that changed" $
    -- not for one that opens under the name of another file
    withEmptyTempGraph \dir -> do
      write dir nilNID $ node' nilNID [] [Connect "b" n2]
      scan <- scanned dir
      write dir n2 $ node' n2 [Connect "b" nilNID] []
      let repairing mode =
            runEff
              . runErrorNoCallStack @UserError
              . runRawGraphWithPath dir
              . interpret (\_ (Echo _) -> pure ())
              . ignoreWarnings @UserError
              . runGraphMetadataFilesystemOperationsIO True
              $ repair mode scan
      listed <- repairing ListRepairs
      first show listed `shouldBe` Right ()
      applied <- repairing ApplyRepairs
      first show applied
        `shouldBe` Left
          ( "fix wrote nothing: 000000000002.json changed while fix was"
              <> " looking at the graph; run fix again"
          )
  it "writes nothing if a file that an id correction goes by changed" $
    withEmptyTempGraph \dir -> do
      -- 2.json says it is the node 3; the origin records its edge for the
      -- node 2, so the id is corrected
      write dir nilNID $ node' nilNID [] [Connect "b" n2]
      write dir n2 $ node' n3 [Connect "b" nilNID] []
      scan <- scanned dir
      (planFix scan.files).repairs `shouldBe` [CorrectId n2 n3]
      -- meanwhile the origin records the edge for the node 3: the file may
      -- be the one of that node now
      write dir nilNID $ node' nilNID [] [Connect "b" n3]
      before <- directoryContents dir
      result <-
        runEff
          . runErrorNoCallStack @UserError
          . runRawGraphWithPath dir
          . interpret (\_ (Echo _) -> pure ())
          . ignoreWarnings @UserError
          . runGraphMetadataFilesystemOperationsIO True
          $ repair ApplyRepairs scan
      first show result
        `shouldBe` Left
          ( "fix wrote nothing: 000000000000.json changed while fix was"
              <> " looking at the graph; run fix again"
          )
      directoryContents dir `shouldReturn` before
  it "does nothing in a directory where a file is only named like one that it kept" $
    -- with or without a file that is named like a node file and is none
    for_ [[], [("000000000001.json", "")]] \more ->
      withSystemTempDirectory "not-a-graph" \dir -> do
        B.writeFile (dir </> "00000000000A.json.unparsable") "{my notes"
        for_ more \(name, content) -> B.writeFile (dir </> name) content
        before <- directoryContents dir
        let runs =
              [ runFix False ApplyRepairs,
                runFix False ListRepairs,
                runFix True ApplyRepairs
              ]
        for_ runs \run -> do
          (printed, result) <- run dir
          printed `shouldBe` []
          first show result
            `shouldBe` Left
              ( "fix did nothing: no file in "
                  <> dir
                  <> " is a node file that says the id it is named after, so it"
                  <> " doesn't look like a graph"
              )
          directoryContents dir `shouldReturn` before
  it "has the files that it kept of a node deleted with the node" $
    withEveryKind \dir -> do
      -- twice the same node file that can't be read as a node
      (_, first') <- runFix False ApplyRepairs dir
      succeeds first'
      B.writeFile (metadataFile dir n3) "{nor this"
      (_, second) <- runFix False ApplyRepairs dir
      succeeds second
      kept <- filter ("000000000003.json." `isPrefixOf`) . keys <$> directoryContents dir
      kept
        `shouldBe` ["000000000003.json.unparsable", "000000000003.json.unparsable-2"]
      runOnDisk dir (deleteNode @Text n3)
      now <- directoryContents dir
      filter ("000000000003.json" `isPrefixOf`) (keys now) `shouldBe` []
      (printed, third) <- runFix False ApplyRepairs dir
      printed `shouldBe` ["nothing to repair"]
      succeeds third
  it "removes what is left over of a deleted node, and brings no node back" $
    withEmptyTempGraph \dir -> do
      write dir nilNID $ node' nilNID [] [Connect "b" n4]
      write dir n4 $ node' n4 [Connect "b" nilNID] []
      -- deleting a node tries the names of its kept files in order until one
      -- is missing, so this one is not found
      B.writeFile (dir </> "000000000004.json.unparsable-3") "{not json"
      -- and these are no kept files
      B.writeFile (dir </> "000000000004.json.unparsable.txt") "notes"
      createDirectory (dir </> "000000000004.json.unparsable-old")
      runOnDisk dir (deleteNode @Text n4)
      let leftOver =
            "000000000004.json.unparsable-3 is left over from a deleted node:"
              <> " there is no 000000000004.json any more, of which fix had"
              <> " kept it when it couldn't read that file as a node"
      scan <- scanned dir
      map describeDamage (toList (scanDamage scan.files)) `shouldBe` [leftOver]
      (printed, result) <- runFix False ApplyRepairs dir
      printed
        `shouldBe` [ "repairs:",
                     "- remove 000000000004.json.unparsable-3, left over from a"
                       <> " deleted node: there is no 000000000004.json any"
                       <> " more, of which fix had kept it"
                   ]
      succeeds result
      (sort <$> listDirectory dir)
        `shouldReturn` [ "000000000000.json",
                         "000000000004.json.unparsable-old",
                         "000000000004.json.unparsable.txt"
                       ]
      after <- scanned dir
      toList (scanDamage after.files) `shouldBe` []
  it "takes only a file for a copy that it kept: no directory, no symbolic link" $
    withEmptyTempGraph \dir -> do
      let kept suffix = dir </> ("000000000004.json.unparsable" <> suffix)
      write dir nilNID $ node' nilNID [] [Connect "b" n4]
      write dir n4 $ node' n4 [Connect "b" nilNID] []
      B.writeFile (kept "") "{not json"
      createDirectory (kept "-2")
      -- to a file that isn't there, and to a file of the user's
      createFileLink (dir </> "nowhere") (kept "-3")
      B.writeFile (dir </> "notes.txt") "of the user"
      createFileLink (dir </> "notes.txt") (kept "-4")
      B.writeFile (kept "-5") "{nor this"
      -- nor a file with a number that fix doesn't give
      B.writeFile (kept "-1") "of the user"
      -- deleting the node removes its copies up to what is none
      runOnDisk dir (deleteNode @Text n4)
      doesFileExist (kept "") `shouldReturn` False
      scan <- scanned dir
      toList (scanDamage scan.files)
        `shouldBe` [KeptFileWithoutNode n4 ["000000000004.json.unparsable-5"]]
      (printed, result) <- runFix False ApplyRepairs dir
      printed
        `shouldBe` [ "repairs:",
                     "- remove 000000000004.json.unparsable-5, left over from a"
                       <> " deleted node: there is no 000000000004.json any"
                       <> " more, of which fix had kept it"
                   ]
      succeeds result
      (sort <$> listDirectory dir)
        `shouldReturn` [ "000000000000.json",
                         "000000000004.json.unparsable-1",
                         "000000000004.json.unparsable-2",
                         "000000000004.json.unparsable-3",
                         "000000000004.json.unparsable-4",
                         "notes.txt"
                       ]
      traverse (pathIsSymbolicLink . kept) ["-3", "-4"] `shouldReturn` [True, True]
      B.readFile (dir </> "notes.txt") `shouldReturn` "of the user"
      after <- scanned dir
      toList (scanDamage after.files) `shouldBe` []
      (again, _) <- runFix False ApplyRepairs dir
      again `shouldBe` ["nothing to repair"]
  it "doesn't remove what has come to be under the name of a left-over copy" $
    -- between listing its repairs and making them
    withEmptyTempGraph \dir -> do
      let name = "000000000004.json.unparsable"
      write dir nilNID $ node' nilNID [] []
      createDirectory (dir </> name)
      result <-
        runEff
          . runErrorNoCallStack @UserError
          . runRawGraphWithPath dir
          . runGraphMetadataFilesystemOperationsIO True
          $ removeKeptNodeMetadata name
      first show result `shouldBe` Right ()
      doesDirectoryExist (dir </> name) `shouldReturn` True
  it "keeps a copy under the next name if something that is no file of the node has the name" $
    withEmptyTempGraph \dir -> do
      let kept suffix = dir </> ("000000000003.json.unparsable" <> suffix)
      write dir nilNID $ node' nilNID [] [Connect "bad" n3]
      B.writeFile (metadataFile dir n3) "{not json"
      createDirectory (kept "")
      createFileLink (dir </> "nowhere") (kept "-2")
      -- Where the volume takes names that differ only in letter case for one
      -- name, this file has the next name too, which the directory doesn't
      -- list
      B.writeFile (dir </> "000000000003.JSON.UNPARSABLE-3") "of the user"
      oneName <- doesFileExist (kept "-3")
      let copy = "000000000003.json.unparsable" <> bool "-3" "-4" oneName
      for_ [runFix False ListRepairs, runFix True ApplyRepairs, runFix False ApplyRepairs] \run -> do
        (printed, result) <- run dir
        take 1 (drop 1 printed)
          `shouldBe` [ "- keep 000000000003.json, which cannot be read as a node,"
                         <> " as "
                         <> copy
                     ]
        succeeds result
      B.readFile (dir </> copy) `shouldReturn` "{not json"
      B.readFile (dir </> "000000000003.JSON.UNPARSABLE-3") `shouldReturn` "of the user"
      doesDirectoryExist (kept "") `shouldReturn` True
      pathIsSymbolicLink (kept "-2") `shouldReturn` True
      after <- scanned dir
      toList (scanDamage after.files) `shouldBe` []
      (again, _) <- runFix False ApplyRepairs dir
      again `shouldBe` ["nothing to repair"]
  it "only lists what is left over when asked to, and in a dry run of ge" $
    withEmptyTempGraph \dir -> do
      write dir nilNID $ node' nilNID [] []
      B.writeFile (dir </> "000000000004.json.unparsable") "{not json"
      for_ [runFix False ListRepairs, runFix True ApplyRepairs] \run -> do
        (printed, result) <- run dir
        printed
          `shouldBe` [ "repairs (only listed, nothing is written):",
                       "- remove 000000000004.json.unparsable, left over from a"
                         <> " deleted node: there is no 000000000004.json any"
                         <> " more, of which fix had kept it"
                     ]
        succeeds result
        doesFileExist (dir </> "000000000004.json.unparsable") `shouldReturn` True
  it "does nothing in a directory without node files" $
    withSystemTempDirectory "not-a-graph" \dir -> do
      B.writeFile (dir </> "notes.txt") "not a graph"
      (printed, result) <- runFix False ApplyRepairs dir
      printed `shouldBe` []
      first show result `shouldSatisfy` \case
        Left message -> "so it doesn't look like a graph" `isSuffixOf` message
        Right () -> False
      directoryContents dir `shouldReturn` singletonMap "notes.txt" "not a graph"
  it "does nothing in a directory where a file is only named like a node file" $
    withSystemTempDirectory "not-a-graph" \dir -> do
      B.writeFile (dir </> "tsconfigbase.json") "{\"compilerOptions\": {}}"
      B.writeFile (dir </> "notes.txt") "not a graph"
      before <- directoryContents dir
      (printed, result) <- runFix False ApplyRepairs dir
      printed `shouldBe` []
      first show result
        `shouldBe` Left
          ( "fix did nothing: no file in "
              <> dir
              <> " is a node file that says the id it is named after, so it"
              <> " doesn't look like a graph"
          )
      directoryContents dir `shouldReturn` before
