{-# LANGUAGE TemplateHaskell #-}

module DAL.FileSystemOperations.Metadata where

import DAL.DTO
import DAL.DirectoryFormat
import DAL.RawGraph
import Error.Missing
import Error.UserError
import Models.NID
import Models.Node
import MyPrelude
import System.Directory (removeFile)
import System.FileCoordination

data GraphMetadataFilesystemOperations m a where
  ReadNodeMetadata :: NID -> GraphMetadataFilesystemOperations m (Maybe (Node Text ()))
  WriteNodeMetadata :: Node Text () -> GraphMetadataFilesystemOperations m ()
  DeleteNodeMetadata :: NID -> GraphMetadataFilesystemOperations m ()

makeSem ''GraphMetadataFilesystemOperations

readNodeMetadata_ ::
  (Members [Embed IO, Error UserError] effs) =>
  Bool ->
  NID ->
  FilePath ->
  Sem effs (Maybe (Node Text ()))
readNodeMetadata_ shouldCoordinate nid path = withEarlyReturn do
  result <- embedCatchingErrors $
    (if shouldCoordinate then coordinateReading path False defaultReadingOptions else ($ path)) $
      \path' ->
        try @IO @IOError $ readFile path'
  serialized <- either (const $ returnEarly Nothing) pure result
  Just <$> decodeNode nid serialized

writeNodeMetadata_ ::
  (Members [Embed IO, Error UserError] effs) =>
  Bool ->
  FilePath ->
  Node Text () ->
  Sem effs ()
writeNodeMetadata_ shouldCoordinate path node = do
  let dto = nodeToDTO node
  let serialized = toStrict $ encodeJSON dto
  embedCatchingErrors $
    (if shouldCoordinate then coordinateWriting path False defaultWritingOptions else ($ path)) $
      \path' ->
        writeFile path' serialized

deleteNodeMetadata_ ::
  (Members [RawGraph, Embed IO, Error UserError] effs) =>
  Bool ->
  FilePath ->
  Sem effs ()
deleteNodeMetadata_ shouldCoordinate path = do
  embedCatchingErrors $
    (if shouldCoordinate then coordinateWriting path False defaultWritingOptions else ($ path)) $
      \path' ->
        removeFile path'

runGraphMetadataFilesystemOperationsIO ::
  (Members [RawGraph, Embed IO, Error UserError] r) =>
  Bool ->
  Sem (GraphMetadataFilesystemOperations : r) a ->
  Sem r a
runGraphMetadataFilesystemOperationsIO useCoordination = interpret \case
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
  (Members [RawGraph, Embed IO, Error UserError] r) =>
  Bool ->
  Sem (GraphMetadataFilesystemOperations : r) a ->
  Sem r a
runGraphMetadataFilesystemOperationsDryRun useCoordination = interpret \case
  ReadNodeMetadata nid ->
    runGraphMetadataFilesystemOperationsIO useCoordination (readNodeMetadata nid)
  WriteNodeMetadata node -> say $ "Would write node: " <> tshow node
  DeleteNodeMetadata nid -> say $ "Would delete node with NID: " <> tshow nid
