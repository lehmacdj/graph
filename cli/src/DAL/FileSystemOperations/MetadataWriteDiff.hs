{-# LANGUAGE TemplateHaskell #-}

module DAL.FileSystemOperations.MetadataWriteDiff where

import Control.Monad.Trans.Cont
import DAL.DirectoryFormat
import DAL.FileSystemOperations.Metadata
import DAL.Interpreters
import DAL.RawGraph
import Error.Missing
import Error.UserError
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
    Map NID (Maybe (Node Text ())) ->
    GraphMetadataFilesystemOperationsWriteDiff m ()

makeSem ''GraphMetadataFilesystemOperationsWriteDiff

writeGraphDiff_ ::
  (Members [RawGraph, Embed IO, Error UserError] r) =>
  (NID -> FilePath -> Sem RawGraphUserErrorIO (Maybe (Node Text ()))) ->
  (FilePath -> Node Text () -> Sem RawGraphUserErrorIO ()) ->
  (FilePath -> Sem RawGraphUserErrorIO ()) ->
  Map NID (Maybe (Node Text ())) ->
  Map NID (Maybe (Node Text ())) ->
  Sem r ()
writeGraphDiff_
  readNodeMetadata'
  writeNodeMetadata'
  deleteNodeMetadata'
  (force -> asLoaded)
  (force -> changes) = do
    writingPaths <-
      ifor changes \nid _ -> getMetadataFile nid <&> (,False)
    readingPaths <-
      -- we need to read every node we write to, plus any node that was loaded
      -- to check that nothing changed since we started the transaction
      ifor (changes <> asLoaded) \nid _ -> getMetadataFile nid <&> (,False)
    let blanketReadingOptions = defaultReadingOptions
        blanketWritingOptions = defaultWritingOptions
    withUnliftIORawGraphUserError \(UnliftIO unliftIO) ->
      coordinateAccessing FilesToCoordinate {..} \wrappedReaders wrappedWriters -> do
        let beginReading ::
              forall a. (HasCallStack) => NID -> ReadingOptions -> ContT a IO FilePath
            beginReading nid options =
              ContT $
                unwrappingReader (wrappedReaders ^. at nid . to (unwrapEx "missing reader")) options
        let beginWriting ::
              forall a. (HasCallStack) => NID -> WritingOptions -> ContT a IO FilePath
            beginWriting nid options =
              ContT $
                unwrappingWriter (wrappedWriters ^. at nid . to (unwrapEx "missing writer")) options
        upToDateNodes' <- ipooledForConcurrentlyN 300 readingPaths \nid _ ->
          evalContT do
            path <- beginReading nid defaultReadingOptions
            upToDateNode <- lift . unliftIO $ readNodeMetadata' nid path
            _ <- evaluateDeep upToDateNode
            -- if the node was loaded, we require that it is the same now as it was
            -- when loading, if it wasn't the node is up to date inherently
            pure $! case asLoaded ^. at nid of
              Just asLoadedNode
                | upToDateNode == asLoadedNode -> Just upToDateNode
                | otherwise -> Nothing
              Nothing -> Just upToDateNode
        let notUpToDateNids = keysSet $ filterMap (== Nothing) upToDateNodes'
        unless (null notUpToDateNids)
          $ unliftIO
            . throwText
          $ "The following nodes were changed before trying to write the diff: "
            ++ tshow notUpToDateNids
        let upToDateNodes = map (unwrapEx "node exists due to above check") upToDateNodes'
        ipooledForConcurrentlyN_ 300 writingPaths \nid _ -> do
          let upToDateNode =
                unwrapEx "upToDateNode exists for nodes we are writing" $
                  upToDateNodes ^. at nid
          let changedNode =
                unwrapEx "changedNode exists for nodes we are writing" $
                  changes ^. at nid
          evalContT case (upToDateNode, changedNode) of
            (Just _, Nothing) -> do
              path <- beginWriting nid defaultWritingOptions {forDeleting = True}
              lift . unliftIO $ deleteNodeMetadata' path
            (Nothing, Just node) -> do
              path <- beginWriting nid defaultWritingOptions
              lift . unliftIO $ writeNodeMetadata' path node
            (Just upToDateNode', Just changedNode')
              | upToDateNode' /= changedNode' -> do
                  path <- beginWriting nid defaultWritingOptions
                  lift . unliftIO $ writeNodeMetadata' path changedNode'
              | otherwise ->
                  -- node didn't change
                  pure ()
            (Nothing, Nothing) ->
              -- node never existed and still doesn't exist
              pure ()

runGraphMetadataFilesystemOperationsWriteDiffIO ::
  (Members [RawGraph, Embed IO, Error UserError] effs) =>
  Sem (GraphMetadataFilesystemOperationsWriteDiff ': effs) a ->
  Sem effs a
runGraphMetadataFilesystemOperationsWriteDiffIO = interpret \case
  WriteGraphDiff asLoaded changes ->
    writeGraphDiff_
      (readNodeMetadata_ False)
      (writeNodeMetadata_ False)
      (deleteNodeMetadata_ False)
      asLoaded
      changes

runGraphMetadataFilesystemOperationsWriteDiffDryRun ::
  (Members [RawGraph, Embed IO, Error UserError] effs) =>
  Sem (GraphMetadataFilesystemOperationsWriteDiff ': effs) a ->
  Sem effs a
runGraphMetadataFilesystemOperationsWriteDiffDryRun = interpret \case
  WriteGraphDiff asLoaded changes -> do
    writeGraphDiff_
      (readNodeMetadata_ False)
      (\path node -> say $ "would write: " <> tshow node <> " -> " <> tshow path)
      (\path -> say $ "would delete at path: " <> tshow path)
      asLoaded
      changes
