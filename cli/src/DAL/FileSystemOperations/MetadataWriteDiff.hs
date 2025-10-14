{-# LANGUAGE TemplateHaskell #-}

module DAL.FileSystemOperations.MetadataWriteDiff where

import DAL.DirectoryFormat
import DAL.FileSystemOperations.Metadata
import DAL.Interpreters
import DAL.RawGraph
import Error.Missing
import Error.UserError
import Models.Augmentation.NodeEdits
import Models.NID
import Models.Node
import MyPrelude
import System.MacOS.NSFileCoordinator

data GraphMetadataFilesystemOperationsWriteDiff m a where
  WriteGraphDiff ::
    -- | Metadata as it was when the graph was loaded; we need to check that
    -- it is still the same before writing any changes
    Map NID (Maybe (Node Text ())) ->
    -- | Changes that need to be written to the underlying filesystem, if a
    -- node is Nothing it should be deleted
    Map NID (NodeEdits Text) ->
    -- | Debug only parameter that indicates the data we expect to be written
    -- Since we read the node from the filesystem and then apply edits when
    -- writing the diff the two should be identical.
    -- Nothing indicates that the node should be deleted
    Map NID (Maybe (Node Text ())) ->
    GraphMetadataFilesystemOperationsWriteDiff m ()

makeSem ''GraphMetadataFilesystemOperationsWriteDiff

writeGraphDiff_ ::
  (Members [RawGraph, Embed IO, Error UserError] r) =>
  (NID -> FilePath -> Sem RawGraphUserErrorIO (Maybe (Node Text ()))) ->
  (FilePath -> Node Text () -> Sem RawGraphUserErrorIO ()) ->
  (FilePath -> Sem RawGraphUserErrorIO ()) ->
  Map NID (Maybe (Node Text ())) ->
  Map NID (NodeEdits Text) ->
  Map NID (Maybe (Node Text ())) ->
  Sem r ()
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
        ipooledForConcurrentlyN_ 300 writingPaths \nid _ -> do
          let upToDateNode :: Maybe (Node Text ())
              upToDateNode =
                unwrapEx "upToDateNode exists for nodes we are writing" $
                  upToDateNodes ^. at nid
          let nodeChanges :: Seq (NodeEdit Text)
              nodeChanges =
                unwrapEx "changes exists for nodes we are writing" $
                  changes ^? ix nid . #edits
          let changedNode =
                applyEditsCreatingNonExistent nodeChanges nid upToDateNode
              expectedChangedNode = changedNodes ^. at nid
          case (upToDateNode, changedNode, expectedChangedNode) of
            (_, _, Just expectedChangedNode')
              | changedNode /= expectedChangedNode' ->
                  let nshowIncoming :: (CompactNodeShow n) => n -> String
                      nshowIncoming =
                        unpack . nshowWith (\s -> s {showIncoming = True})
                   in error . unlines $
                        [ "applying changes to node from disk "
                            <> "did not yield expected result!",
                          "expected:\n" <> nshowIncoming expectedChangedNode,
                          "changes to apply:\n" <> show nodeChanges,
                          "read from disk:\n" <> nshowIncoming upToDateNode,
                          "after applying changes:\n" <> nshowIncoming changedNode
                        ]
            (Just _, Nothing, _) ->
              withWriting nid defaultWritingOptions {forDeleting = True} $
                unliftIO . deleteNodeMetadata'
            (Nothing, Just node, _) ->
              withWriting nid defaultWritingOptions $ \path ->
                unliftIO $ writeNodeMetadata' path node
            (Just upToDateNode', Just changedNode', _)
              | upToDateNode' /= changedNode' ->
                  withWriting nid defaultWritingOptions \path ->
                    unliftIO $ writeNodeMetadata' path changedNode'
              | otherwise ->
                  -- node didn't change
                  pure ()
            (Nothing, Nothing, _) ->
              -- node never existed and still doesn't exist
              pure ()

runGraphMetadataFilesystemOperationsWriteDiffIO ::
  (Members [RawGraph, Embed IO, Error UserError] effs) =>
  Sem (GraphMetadataFilesystemOperationsWriteDiff ': effs) a ->
  Sem effs a
runGraphMetadataFilesystemOperationsWriteDiffIO = interpret \case
  WriteGraphDiff asLoaded changes changedNodes ->
    writeGraphDiff_
      (readNodeMetadata_ False)
      (writeNodeMetadata_ False)
      (deleteNodeMetadata_ False)
      asLoaded
      changes
      changedNodes

runGraphMetadataFilesystemOperationsWriteDiffDryRun ::
  (Members [RawGraph, Embed IO, Error UserError] effs) =>
  Sem (GraphMetadataFilesystemOperationsWriteDiff ': effs) a ->
  Sem effs a
runGraphMetadataFilesystemOperationsWriteDiffDryRun = interpret \case
  WriteGraphDiff asLoaded changes changedNodes -> do
    writeGraphDiff_
      (readNodeMetadata_ False)
      (\path node -> say $ "would write: " <> tshow node <> " -> " <> tshow path)
      (\path -> say $ "would delete at path: " <> tshow path)
      asLoaded
      changes
      changedNodes
