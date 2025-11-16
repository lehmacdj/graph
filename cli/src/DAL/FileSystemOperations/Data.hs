{-# LANGUAGE TemplateHaskell #-}

module DAL.FileSystemOperations.Data where

import DAL.DirectoryFormat
import DAL.RawGraph
import Effectful
import Effectful.Dispatch.Dynamic
import Effectful.Error.Static
import Effectful.TH
import Error.Missing
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
  (RawGraph :> es, IOE :> es, Error UserError :> es) =>
  NID ->
  -- | File extension for the node's data
  String ->
  Eff es (Maybe ByteString)
readNodeData_ nid fileExtension = do
  path <- getNodeDataFile nid fileExtension
  result <- liftIO $
    coordinateReading path False defaultReadingOptions $
      \path' ->
        try @IO @IOError $ readFile path'
  pure $ either (const Nothing) Just result

writeNodeData_ ::
  (RawGraph :> es, IOE :> es, Error UserError :> es) =>
  NID ->
  -- | File extension for the node's data
  String ->
  ByteString ->
  Eff es ()
writeNodeData_ nid extension rawData = do
  path <- getNodeDataFile nid extension
  embedCatchingErrors $
    coordinateWriting path False defaultWritingOptions $
      \path' ->
        writeFile path' rawData

deleteNodeData_ ::
  (RawGraph :> es, IOE :> es, Error UserError :> es) =>
  NID ->
  -- | File extension for the node's data
  String ->
  Eff es ()
deleteNodeData_ nid extension = do
  path <- getNodeDataFile nid extension
  embedCatchingErrors $
    coordinateWriting path False defaultWritingOptions removeFile

runGraphDataFilesystemOperationsIO ::
  (RawGraph :> es, IOE :> es, Error UserError :> es) =>
  Eff (GraphDataFilesystemOperations : es) a ->
  Eff es a
runGraphDataFilesystemOperationsIO = interpret $ \_ -> \case
  ReadNodeData nid extension -> readNodeData_ nid extension
  WriteNodeData nid extension rawData -> writeNodeData_ nid extension rawData
  DeleteNodeData nid extension -> deleteNodeData_ nid extension

runGraphDataFilesystemOperationsDryRun ::
  (RawGraph :> es, IOE :> es, Error UserError :> es) =>
  Eff (GraphDataFilesystemOperations : es) a ->
  Eff es a
runGraphDataFilesystemOperationsDryRun = interpret $ \_ -> \case
  ReadNodeData nid extension -> readNodeData_ nid extension
  WriteNodeData nid extension rawData ->
    say $
      ("overwrite " <> tshow (length rawData) <> " bytes of data for ")
        <> (tshow nid <> "." <> tshow extension)
  DeleteNodeData nid extension ->
    say $ "delete data for " <> tshow nid <> "." <> tshow extension
