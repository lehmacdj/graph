{-# LANGUAGE TemplateHaskell #-}

module DAL.FileSystemOperations.MetadataWriteDiff where

import DAL.DirectoryFormat
import DAL.FileSystemOperations.Metadata
import DAL.Interpreters
import DAL.RawGraph
import Data.List.NonEmpty qualified as NonEmpty
import Error.UserError
import Models.Augmentation.NodeEdits
import Models.Connect
import Models.NID
import Models.Node
import MyPrelude
import System.FileCoordination

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
    GraphMetadataFilesystemOperationsWriteDiff m ()

makeEffect ''GraphMetadataFilesystemOperationsWriteDiff

writeGraphDiff_ ::
  (Members [RawGraph, IOE, Error UserError] r) =>
  (NID -> FilePath -> Eff RawGraphUserErrorIO (Maybe (Node Text ()))) ->
  (FilePath -> Node Text () -> Eff RawGraphUserErrorIO ()) ->
  (FilePath -> Eff RawGraphUserErrorIO ()) ->
  Map NID (Maybe (Node Text ())) ->
  Map NID (NodeEdits Text) ->
  Map NID (Maybe (Node Text ())) ->
  Eff r ()
writeGraphDiff_
  readNodeMetadata'
  writeNodeMetadata'
  deleteNodeMetadata'
  (force -> asLoaded)
  (force -> changes)
  changedNodes = do
    writingPaths <- ifor changes \nid _ -> getMetadataFile nid <&> (,False)
    -- we need to read every node we write to, plus any node that was loaded
    -- to check that nothing changed since we started the transaction
    let nodesToRead = mapFromSet $ keysSet changes <> keysSet asLoaded
    readingPaths <- ifor nodesToRead \nid _ -> getMetadataFile nid <&> (,False)
    let blanketReadingOptions = defaultReadingOptions
        blanketWritingOptions = defaultWritingOptions
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
        upToDateNodes' <- ipooledForConcurrentlyN 300 readingPaths \nid _ ->
          withReading nid defaultReadingOptions $ \path -> do
            upToDateNode <- unliftIO $ readNodeMetadata' nid path
            _ <- evaluateDeep upToDateNode
            -- if the node was loaded, we require that it is the same now as it was
            -- when loading, if it wasn't the node is up to date inherently
            pure $! case asLoaded ^. at nid of
              Just asLoadedNode
                | upToDateNode == asLoadedNode -> Just upToDateNode
                | otherwise -> Nothing
              Nothing -> Just upToDateNode
        let notUpToDateNids = keysSet $ filterMap (== Nothing) upToDateNodes'
        unless (null notUpToDateNids) $
          unliftIO . throwText $
            "The following nodes were changed before trying to write the diff: "
              ++ tshow notUpToDateNids
        let upToDateNodes = map (unwrapEx "node exists due to above check") upToDateNodes'
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
        -- All of the checks come before the first write: once a file is
        -- written there is no going back, and a change written only in part
        -- damages the graph (further).
        let mismatches =
              concat
                [ recordMismatches nid changedNode expectedNode
                | (nid, (_, changedNode)) <- mapToList plannedWrites,
                  expectedNode <- toList (changedNodes ^. at nid)
                ]
        for_ (NonEmpty.nonEmpty mismatches) $
          unliftIO . throwError . InconsistentNodeRecords
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

runGraphMetadataFilesystemOperationsWriteDiffIO ::
  (Members [RawGraph, IOE, Error UserError] effs) =>
  Eff (GraphMetadataFilesystemOperationsWriteDiff ': effs) a ->
  Eff effs a
runGraphMetadataFilesystemOperationsWriteDiffIO = interpret \_ -> \case
  WriteGraphDiff asLoaded changes changedNodes ->
    writeGraphDiff_
      (readNodeMetadata_ False)
      (writeNodeMetadata_ False)
      (deleteNodeMetadata_ False)
      asLoaded
      changes
      changedNodes

runGraphMetadataFilesystemOperationsWriteDiffDryRun ::
  (Members [RawGraph, IOE, Error UserError] effs) =>
  Eff (GraphMetadataFilesystemOperationsWriteDiff ': effs) a ->
  Eff effs a
runGraphMetadataFilesystemOperationsWriteDiffDryRun = interpret \_ -> \case
  WriteGraphDiff asLoaded changes changedNodes -> do
    writeGraphDiff_
      (readNodeMetadata_ False)
      (\path node -> say $ "would write: " <> tshow node <> " -> " <> tshow path)
      (\path -> say $ "would delete at path: " <> tshow path)
      asLoaded
      changes
      changedNodes
