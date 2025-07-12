{-# LANGUAGE TemplateHaskell #-}

module DAL.FileSystemOperations.Metadata where

import DAL.DTO
import DAL.DirectoryFormat
import DAL.JSON
import DAL.RawGraph
import Error.Missing
import Error.UserError
import Models.NID
import Models.Node
import MyPrelude
import System.Directory (removeFile)
import System.MacOS.NSFileCoordinator

data GraphMetadataFilesystemOperations m a where
  ReadNodeMetadata :: NID -> GraphMetadataFilesystemOperations m (Maybe (Node Text ()))
  WriteNodeMetadata :: Node Text () -> GraphMetadataFilesystemOperations m ()
  DeleteNodeMetadata :: NID -> GraphMetadataFilesystemOperations m ()

makeSem ''GraphMetadataFilesystemOperations

readNodeMetadata_ ::
  (Members [Embed IO, Error UserError] effs) =>
  Bool ->
  FilePath ->
  Sem effs (Maybe (Node Text ()))
readNodeMetadata_ shouldCoordinate path = withEarlyReturn do
  result <- embedCatchingErrors $
    (if shouldCoordinate then coordinateReading path False defaultReadingOptions else ($ path)) $
      \path' ->
        try @IO @IOError $ readFile path'
  serialized <- either (const $ returnEarly Nothing) pure result
  dto <- decodeJSON serialized
  pure $ Just (nodeFromDTO dto)

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
  Sem (GraphMetadataFilesystemOperations : r) a ->
  Sem r a
runGraphMetadataFilesystemOperationsIO = interpret \case
  ReadNodeMetadata nid -> readNodeMetadata_ True =<< getMetadataFile nid
  WriteNodeMetadata node -> (\x -> writeNodeMetadata_ True x node) =<< getMetadataFile node.nid
  DeleteNodeMetadata nid -> deleteNodeMetadata_ True =<< getMetadataFile nid

runGraphMetadataFilesystemOperationsWriteDryRun ::
  (Members [RawGraph, Embed IO, Error UserError] r) =>
  Sem (GraphMetadataFilesystemOperations : r) a ->
  Sem r a
runGraphMetadataFilesystemOperationsWriteDryRun = interpret \case
  ReadNodeMetadata nid -> readNodeMetadata_ True =<< getMetadataFile nid
  WriteNodeMetadata node -> say $ "Would write node: " <> tshow node
  DeleteNodeMetadata nid -> say $ "Would delete node with NID: " <> tshow nid
