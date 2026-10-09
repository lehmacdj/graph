{-# LANGUAGE TemplateHaskell #-}

module DAL.FileSystemOperations.Data where

import DAL.DirectoryFormat
import DAL.RawGraph
import Error.UserError
import Models.NID
import MyPrelude
import System.Directory (removeFile)
import System.FileCoordination

data GraphDataFilesystemOperations :: Effect where
  ReadNodeData :: NID -> String -> GraphDataFilesystemOperations m (Maybe ByteString)
  WriteNodeData :: NID -> String -> ByteString -> GraphDataFilesystemOperations m ()
  DeleteNodeData :: NID -> String -> GraphDataFilesystemOperations m ()

makeEffect ''GraphDataFilesystemOperations

readNodeData_ ::
  (Members [RawGraph, IOE, Error UserError] effs) =>
  NID ->
  -- | File extension for the node's data
  String ->
  Eff effs (Maybe ByteString)
readNodeData_ nid fileExtension = do
  path <- getNodeDataFile nid fileExtension
  result <- liftIO $
    coordinateReading path False defaultReadingOptions $
      \path' ->
        try @IO @IOError $ readFile path'
  pure $ either (const Nothing) Just result

writeNodeData_ ::
  (Members [RawGraph, IOE, Error UserError] effs) =>
  NID ->
  -- | File extension for the node's data
  String ->
  ByteString ->
  Eff effs ()
writeNodeData_ nid extension rawData = do
  path <- getNodeDataFile nid extension
  writes <- getGraphWrites
  -- a change of its own, on record once it is made: see 'writeNodeMetadata_'
  embedCatchingErrors $
    coordinateWriting path False defaultWritingOptions $
      \path' ->
        changingGraphFile writes (writeFile path' rawData)

deleteNodeData_ ::
  (Members [RawGraph, IOE, Error UserError] effs) =>
  NID ->
  -- | File extension for the node's data
  String ->
  Eff effs ()
deleteNodeData_ nid extension = do
  path <- getNodeDataFile nid extension
  writes <- getGraphWrites
  -- a node without data has nothing to delete, and then nothing is changed
  let removeIfPresent path' =
        changingGraphFile writes (removeFile path') `catch` \e ->
          unless (isDoesNotExistError e) (throwIO e)
  embedCatchingErrors $
    coordinateWriting
      path
      False
      defaultWritingOptions {forDeleting = True}
      removeIfPresent

runGraphDataFilesystemOperationsIO ::
  (Members [RawGraph, IOE, Error UserError] r) =>
  Eff (GraphDataFilesystemOperations : r) a ->
  Eff r a
runGraphDataFilesystemOperationsIO = interpret \_ -> \case
  ReadNodeData nid extension -> readNodeData_ nid extension
  WriteNodeData nid extension rawData -> writeNodeData_ nid extension rawData
  DeleteNodeData nid extension -> deleteNodeData_ nid extension

runGraphDataFilesystemOperationsDryRun ::
  (Members [RawGraph, IOE, Error UserError] r) =>
  Eff (GraphDataFilesystemOperations : r) a ->
  Eff r a
runGraphDataFilesystemOperationsDryRun = interpret \_ -> \case
  ReadNodeData nid extension -> readNodeData_ nid extension
  WriteNodeData nid extension rawData ->
    say $
      ("overwrite " <> tshow (length rawData) <> " bytes of data for ")
        <> (tshow nid <> "." <> tshow extension)
  DeleteNodeData nid extension ->
    say $ "delete data for " <> tshow nid <> "." <> tshow extension
