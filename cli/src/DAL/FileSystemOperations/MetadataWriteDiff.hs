{-# LANGUAGE TemplateHaskell #-}

module DAL.FileSystemOperations.MetadataWriteDiff where

import DAL.DirectoryFormat
import DAL.FileSystemOperations.Metadata
import DAL.Interpreters
import DAL.RawGraph
import Data.List.NonEmpty qualified as NonEmpty
import Data.Map.Strict qualified as Map
import Error.UserError
import Error.Warn
import Models.Augmentation.NodeEdits
import Models.Connect
import Models.Damage (damageAmong, damageNear, fileFailure, filesNewTo)
import Models.NID
import Models.Node
import MyPrelude
import System.Directory (removeFile)
import System.FileCoordination
import System.FilePath (takeFileName)

data GraphMetadataFilesystemOperationsWriteDiff :: Effect where
  WriteGraphDiff ::
    -- | Metadata as it was when the graph was loaded; we need to check that
    -- it is still the same before writing any changes
    Map NID (Maybe (Node Text ())) ->
    -- | Changes that need to be written to the underlying filesystem, if a
    -- node is Nothing it should be deleted
    Map NID (NodeEdits Text) ->
    -- | The node that the transaction computed for each changed node that it
    -- had read (Nothing if it deletes the node). Applying the edits to the
    -- node's file must give this node, or none of the change is written. A
    -- node that is changed without having been read has no entry: it is
    -- written as its file with the edits applied, whatever that gives.
    Map NID (Maybe (Node Text ())) ->
    -- | Node files that whoever asks for the change has read besides (in
    -- earlier transactions of the same command), as it last read or wrote
    -- them. They are not read again, and they are compared with the files
    -- that are read here like those are with each other: the change is
    -- refused if they are damaged.
    Map NID (Maybe (Node Text ())) ->
    -- | The file of each changed node as the change leaves it ('Nothing':
    -- no file): as it was written, or as it was read for the change where
    -- nothing is written (a dry run)
    GraphMetadataFilesystemOperationsWriteDiff m (Map NID (Maybe (Node Text ())))

makeEffect ''GraphMetadataFilesystemOperationsWriteDiff

-- | Returns the file of each changed node as it was read here, and as it is
-- to be once the change is written; and the files that were kept of the
-- deleted nodes' files (see 'keptNodeFilesOf') and could not be removed, with
-- the reason.
writeGraphDiff_ ::
  (Members [RawGraph, IOE, Error UserError] r) =>
  (NID -> FilePath -> Eff RawGraphUserErrorIO (Maybe (Node Text ()))) ->
  (FilePath -> Node Text () -> Eff RawGraphUserErrorIO ()) ->
  (FilePath -> Eff RawGraphUserErrorIO ()) ->
  -- | remove a file that was kept of a deleted node's file
  (FilePath -> Eff RawGraphUserErrorIO ()) ->
  Map NID (Maybe (Node Text ())) ->
  Map NID (NodeEdits Text) ->
  Map NID (Maybe (Node Text ())) ->
  Map NID (Maybe (Node Text ())) ->
  Eff
    r
    ( Map NID (Maybe (Node Text ()), Maybe (Node Text ())),
      [(FilePath, IOError)]
    )
writeGraphDiff_
  readNodeMetadata'
  writeNodeMetadata'
  deleteNodeMetadata'
  removeKeptFile'
  (force -> asLoaded)
  (force -> changes)
  changedNodes
  others = do
    writingPaths <- ifor changes \nid _ -> getMetadataFile nid <&> (,False)
    -- we need to read every node we write to, plus any node that was loaded
    -- to check that nothing changed since we started the transaction
    let nodesToRead = mapFromSet $ keysSet changes <> keysSet asLoaded
    readingPaths <- ifor nodesToRead \nid _ -> getMetadataFile nid <&> (,False)
    let blanketReadingOptions = defaultReadingOptions
        blanketWritingOptions = defaultWritingOptions
    writes <- getGraphWrites
    withUnliftIORawGraphUserError \(UnliftIO unliftIO) ->
      coordinateAccessing FilesToCoordinate {..} \wrappedReaders wrappedWriters -> do
        let withReading ::
              forall a. (HasCallStack) => NID -> ReadingOptions -> (FilePath -> IO a) -> IO a
            withReading nid =
              unwrappingReader (wrappedReaders ^. at nid . to (unwrapEx "missing reader"))
        let withWriting ::
              forall a. (HasCallStack) => NID -> WritingOptions -> (FilePath -> IO a) -> IO a
            withWriting nid =
              unwrappingWriter (wrappedWriters ^. at nid . to (unwrapEx "missing writer"))
        -- the files are read concurrently, so a failure to read one is kept
        -- rather than thrown: which of several is thrown first would differ
        -- from run to run
        readNodes <- ipooledForConcurrentlyN 300 readingPaths \nid _ ->
          withReading nid defaultReadingOptions $ \path ->
            unliftIO (tryError @UserError (readNodeMetadata' nid path)) >>= \case
              Left (_, e) -> pure (Left e)
              Right upToDateNode -> Right <$> evaluateDeep upToDateNode
        let unusable =
              [ damage
              | (_, Left e) <- mapToList readNodes,
                damage <- toList (damageOfReadError e)
              ]
            failures =
              [ e
              | (_, Left e) <- mapToList readNodes,
                isNothing (damageOfReadError e)
              ]
            -- if the node was loaded, we require that it is the same now as it was
            -- when loading, if it wasn't the node is up to date inherently
            upToDateNodes' = flip Map.mapMaybeWithKey readNodes \nid -> \case
              Left _ -> Nothing
              Right upToDateNode -> Just case asLoaded ^. at nid of
                Just asLoadedNode
                  | upToDateNode == asLoadedNode -> Just upToDateNode
                  | otherwise -> Nothing
                Nothing -> Just upToDateNode
        -- the one of the node with the least id
        for_ (headMay failures) (unliftIO . throwError)
        let notUpToDateNids = keysSet $ filterMap (== Nothing) upToDateNodes'
        unless (null notUpToDateNids) $
          unliftIO . throwText $
            "The following nodes were changed before trying to write the diff: "
              ++ tshow notUpToDateNids
        let upToDateNodes = map (unwrapEx "node exists due to above check") upToDateNodes'
        -- All of the checks come before the first write: once a file is
        -- written there is no going back, and a change written only in part
        -- damages the graph (further).
        --
        -- A change is refused if the files it was computed from or will be
        -- written to are damaged: what it should do there is anyone's guess.
        -- Every such file is named, the ones that can't be used as a node
        -- among them (in the order of their ids). The files that the
        -- command read before count as well, as it read them: the ones that
        -- were read here are the later reading. (What those files show
        -- among themselves, whoever read them has found: only the files
        -- that are new to the command are compared with them.)
        let damage =
              setFromList unusable
                <> damageAmong upToDateNodes
                <> damageNear
                  (filesNewTo others upToDateNodes)
                  (upToDateNodes <> (others `Map.difference` readNodes))
        for_ (NonEmpty.nonEmpty (toList damage)) $
          unliftIO . throwError . DamagedGraph
        -- every node to write as it is on disk and as it will be once its
        -- edits are applied
        let plannedWrites :: Map NID (Maybe (Node Text ()), Maybe (Node Text ()))
            plannedWrites = flip imap changes \nid nodeEdits ->
              let upToDateNode =
                    unwrapEx "upToDateNode exists for nodes we are writing" $
                      upToDateNodes ^. at nid
               in ( upToDateNode,
                    applyEditsCreatingNonExistent nodeEdits.edits nid upToDateNode
                  )
        -- with undamaged files this cannot fail unless the transaction
        -- miscomputed its nodes
        let mismatches =
              concat
                [ recordMismatches nid changedNode expectedNode
                | (nid, (_, changedNode)) <- mapToList plannedWrites,
                  expectedNode <- toList (changedNodes ^. at nid)
                ]
        for_ (NonEmpty.nonEmpty mismatches) $
          unliftIO . throwError . InconsistentNodeRecords
        -- The files of the change are written as one: the program does not
        -- end between two of them ('noMoreGraphChanges').
        changingGraphFiles writes $
          ipooledForConcurrentlyN_ 300 writingPaths \nid _ ->
            case unwrapEx "a write is planned for nodes we are writing" $
              plannedWrites ^. at nid of
              (Just _, Nothing) ->
                withWriting nid defaultWritingOptions {forDeleting = True} $
                  unliftIO . deleteNodeMetadata'
              (Nothing, Just node) ->
                withWriting nid defaultWritingOptions $ \path ->
                  unliftIO $ writeNodeMetadata' path node
              (Just upToDateNode, Just changedNode)
                | upToDateNode /= changedNode ->
                    withWriting nid defaultWritingOptions \path ->
                      unliftIO $ writeNodeMetadata' path changedNode
                | otherwise ->
                    -- node didn't change
                    pure ()
              (Nothing, Nothing) ->
                -- node never existed and still doesn't exist
                pure ()
        -- What fix kept of the file of a node that is deleted goes with the
        -- node. This comes after the change is written, and doesn't undo it:
        -- a kept file that can't be removed is left over, which whoever
        -- wrote the change says (fsck reports such a file, and fix removes
        -- it).
        let deleted =
              [ path
              | (nid, (path, _)) <- mapToList writingPaths,
                Just (Just _, Nothing) <- [plannedWrites ^. at nid]
              ]
        fmap ((plannedWrites,) . concat) . for deleted $ \path -> do
          kept <- keptNodeFilesOf path
          fmap catMaybes . for kept $ \file ->
            either (Just . (file,)) (const Nothing)
              <$> try @IO @IOError (unliftIO (removeKeptFile' file))

-- | How the node that applying a transaction's edits to the file of @nid@
-- gives differs from the node the transaction computed. The two are the same
-- unless the records of the graph contradict each other: the transaction
-- works with a graph in which both nodes of an edge agree about it.
recordMismatches ::
  NID ->
  -- | the node on disk with the edits applied
  Maybe (Node Text ()) ->
  -- | the node the transaction computed
  Maybe (Node Text ()) ->
  [RecordMismatch]
recordMismatches nid = curry \case
  (Nothing, Nothing) -> []
  (Just _, Nothing) -> [NodeNotDeleted nid]
  (Nothing, Just _) -> [NodeNotCreated nid]
  (Just onDisk, Just computed) ->
    [FileOfOtherNode nid onDisk.nid | onDisk.nid /= computed.nid]
      <> records RecordNotComputed onDisk computed
      <> records RecordNotInFile computed onDisk
  where
    records mismatch present absent =
      [ mismatch nid OutgoingRecord c.transition c.node
      | c <- toList (present.outgoing `difference` absent.outgoing)
      ]
        <> [ mismatch nid IncomingRecord c.transition c.node
           | c <- toList (present.incoming `difference` absent.incoming)
           ]

-- | Warns of each file kept of a deleted node's file that could not be
-- removed: the change is written by then, and stays.
runGraphMetadataFilesystemOperationsWriteDiffIO ::
  (Members [RawGraph, IOE, Error UserError, Warn UserError] effs) =>
  Eff (GraphMetadataFilesystemOperationsWriteDiff ': effs) a ->
  Eff effs a
runGraphMetadataFilesystemOperationsWriteDiffIO =
  writingGraphDiffsRemovingKeptWith (liftIO . removeFile)

-- | 'runGraphMetadataFilesystemOperationsWriteDiffIO', given how a file that
-- was kept of a deleted node's file is removed (which the tests have fail).
writingGraphDiffsRemovingKeptWith ::
  (Members [RawGraph, IOE, Error UserError, Warn UserError] effs) =>
  (FilePath -> Eff RawGraphUserErrorIO ()) ->
  Eff (GraphMetadataFilesystemOperationsWriteDiff ': effs) a ->
  Eff effs a
writingGraphDiffsRemovingKeptWith removeKeptFile' = interpret \_ -> \case
  WriteGraphDiff asLoaded changes changedNodes others -> do
    -- Each file that is changed is put on record here: what changes it is
    -- run with a record of its own ('withUnliftIORawGraphUserError'), and
    -- the files of the change are written as one.
    writes <- getGraphWrites
    (written, leftOver) <-
      writeGraphDiff_
        (readNodeMetadata_ False)
        (\path -> countingGraphFile writes . writeNodeMetadata_ False path)
        (countingGraphFile writes . deleteNodeMetadata_ False)
        (countingGraphFile writes . removeKeptFile')
        asLoaded
        changes
        changedNodes
        others
    traverse_ (warnString . uncurry keptFileLeftOver) leftOver
    pure (snd <$> written)

-- | What is said of a file that was kept of a deleted node's file and could
-- not be removed with the node.
keptFileLeftOver :: FilePath -> IOError -> String
keptFileLeftOver file e =
  "warning: the node was deleted, but "
    <> takeFileName file
    <> ", which fix had kept of its file, could not be removed: "
    <> fileFailure "remove" e
    <> ". It is left over, and fsck will report it."

-- | What a dry run says that a change would do to a file. A change's lines
-- are printed in the order of what they would do, the file's path, and the
-- line: the files that the change would delete and then the files that it
-- would write, each in the order of their paths (for the node files of a
-- graph that is the order of the nodes' ids; what fix kept of a node's file
-- comes after that file).
data WouldDo = WouldDelete | WouldWrite
  deriving (Eq, Ord)

-- | Says what the change would delete and write instead of doing it: one
-- line for each file, in the order given with 'WouldDo', so the same change
-- to the same graph is described by the same text every time.
--
-- 'writeGraphDiff_' writes many files at once, so it asks for the lines from
-- many threads, in an order that differs from run to run, and for several
-- at the same moment. So they are collected, and printed by one thread when
-- the change has been gone through: in the order above, and each one whole.
runGraphMetadataFilesystemOperationsWriteDiffDryRun ::
  (Members [RawGraph, IOE, Error UserError] effs) =>
  Eff (GraphMetadataFilesystemOperationsWriteDiff ': effs) a ->
  Eff effs a
runGraphMetadataFilesystemOperationsWriteDiffDryRun = interpret \_ -> \case
  WriteGraphDiff asLoaded changes changedNodes others -> do
    collected <- newIORef []
    -- A line is worked out in full where it is asked for, not when it is
    -- printed: the path that a callback is given is good only while the
    -- callback runs. (It is read, when it is first looked at, from memory
    -- that file coordination frees afterwards: 'filePathFromNSURL'.)
    let collect :: (MonadIO m) => WouldDo -> FilePath -> Text -> m ()
        collect wouldDo path line = liftIO do
          said <- (wouldDo,,) <$> evaluate (force path) <*> evaluate (force line)
          atomicModifyIORef' collected \sofar -> (said : sofar, ())
        wouldDelete path =
          collect WouldDelete path $ "would delete at path: " <> tshow path
        wouldWrite path node =
          collect WouldWrite path $
            "would write: " <> tshow node <> " -> " <> tshow path
        sayCollected =
          readIORef collected >>= traverse_ (\(_, _, line) -> say line) . sort
    -- Nothing is written, so each of these files stays as it was read for
    -- the change. The command has read it then, as it has in the real run:
    -- what a later step of it reads is compared with a file that only
    -- writing read (that of a neighbour of a deleted node, say) as well.
    --
    -- A change that is given up part way still says what it got to.
    fmap fst . fst
      <$> writeGraphDiff_
        (readNodeMetadata_ False)
        wouldWrite
        wouldDelete
        wouldDelete
        asLoaded
        changes
        changedNodes
        others
        `finally` sayCollected
