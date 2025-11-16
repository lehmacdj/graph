{-# LANGUAGE TemplateHaskell #-}

module DAL.FileSystemOperations.Metadata where

import DAL.DTO
import DAL.DirectoryFormat
import DAL.RawGraph
import Effectful
import Effectful.Dispatch.Dynamic
import Effectful.Error.Static
import Effectful.TH
import Error.Missing
import Error.UserError
import Models.NID
import Models.Node
import MyPrelude
import System.Directory (removeFile)
import System.FileCoordination

data GraphMetadataFilesystemOperations :: Effect where
  ReadNodeMetadata :: NID -> GraphMetadataFilesystemOperations m (Maybe (Node Text ()))
  WriteNodeMetadata :: Node Text () -> GraphMetadataFilesystemOperations m ()
  DeleteNodeMetadata :: NID -> GraphMetadataFilesystemOperations m ()

makeEffect ''GraphMetadataFilesystemOperations

readNodeMetadata_ ::
  (IOE :> es, Error UserError :> es) =>
  Bool ->
  NID ->
  FilePath ->
  Eff es (Maybe (Node Text ()))
readNodeMetadata_ shouldCoordinate nid path = withEarlyReturn do
  result <- embedCatchingErrors $
    (if shouldCoordinate then coordinateReading path False defaultReadingOptions else ($ path)) $
      \path' ->
        try @IO @IOError $ readFile path'
  serialized <- either (const $ returnEarly Nothing) pure result
  Just <$> decodeNode nid serialized

writeNodeMetadata_ ::
  (IOE :> es, Error UserError :> es) =>
  Bool ->
  FilePath ->
  Node Text () ->
  Eff es ()
writeNodeMetadata_ shouldCoordinate path node = do
  let dto = nodeToDTO node
  let serialized = toStrict $ encodeJSON dto
  embedCatchingErrors $
    (if shouldCoordinate then coordinateWriting path False defaultWritingOptions else ($ path)) $
      \path' ->
        writeFile path' serialized

deleteNodeMetadata_ ::
  (RawGraph :> es, IOE :> es, Error UserError :> es) =>
  Bool ->
  FilePath ->
  Eff es ()
deleteNodeMetadata_ shouldCoordinate path = do
  embedCatchingErrors $
    (if shouldCoordinate then coordinateWriting path False defaultWritingOptions else ($ path)) $
      \path' ->
        removeFile path'

runGraphMetadataFilesystemOperationsIO ::
  (RawGraph :> es, IOE :> es, Error UserError :> es) =>
  Bool ->
  Eff (GraphMetadataFilesystemOperations : es) a ->
  Eff es a
runGraphMetadataFilesystemOperationsIO useCoordination = interpret $ \_ -> \case
  ReadNodeMetadata nid -> do
    path <- getMetadataFile nid
    node <- readNodeMetadata_ useCoordination nid path
    withJust node $ \n ->
      unless (n.nid == nid) $
        sayErr $
          "Warning: NID mismatch when reading metadata file for NID "
            <> tshow nid
            <> ". File contains NID "
            <> tshow n.nid
    pure node
  WriteNodeMetadata node -> do
    f <- getMetadataFile node.nid
    writeNodeMetadata_ useCoordination f node
  DeleteNodeMetadata nid ->
    deleteNodeMetadata_ useCoordination =<< getMetadataFile nid

runGraphMetadataFilesystemOperationsDryRun ::
  (RawGraph :> es, IOE :> es, Error UserError :> es) =>
  Bool ->
  Eff (GraphMetadataFilesystemOperations : es) a ->
  Eff es a
runGraphMetadataFilesystemOperationsDryRun useCoordination = interpret $ \_ -> \case
  ReadNodeMetadata nid ->
    runGraphMetadataFilesystemOperationsIO useCoordination (readNodeMetadata nid)
  WriteNodeMetadata node -> say $ "Would write node: " <> tshow node
  DeleteNodeMetadata nid -> say $ "Would delete node with NID: " <> tshow nid
