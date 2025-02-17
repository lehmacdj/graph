{-# LANGUAGE TemplateHaskell #-}

module DAL.FileSystemOperations.Metadata where

import DAL.DTO
import DAL.JSON
import DAL.DirectoryFormat
import DAL.RawGraph
import Error.Missing
import Error.UserError
import Models.NID
import Models.Node
import MyPrelude
import System.MacOS.NSFileCoordinator
import System.Directory (removeFile)

data GraphMetadataFilesystemOperations m a where
  ReadNodeMetadata :: NID -> GraphMetadataFilesystemOperations m (Maybe (Node Text ()))
  WriteNodeMetadata :: Node Text () -> GraphMetadataFilesystemOperations m ()
  DeleteNodeMetadata :: NID -> GraphMetadataFilesystemOperations m ()

makeSem ''GraphMetadataFilesystemOperations

readNodeMetadata_ ::
  Members [Embed IO, Error UserError] effs =>
  FilePath -> Sem effs (Maybe (Node Text ()))
readNodeMetadata_ path = withEarlyReturn do
  result <- embedCatchingErrors $ coordinateReading path False defaultReadingOptions $ \path' ->
    try @IO @IOError $ readFile path'
  serialized <- either (const $ returnEarly Nothing) pure result
  dto <- decodeJSON serialized
  pure $ Just (nodeFromDTO dto)

writeNodeMetadata_ ::
  Members [Embed IO, Error UserError] effs =>
  FilePath ->
  Node Text () ->
  Sem effs ()
writeNodeMetadata_ path node  = do
  let dto = nodeToDTO node
  let serialized = toStrict $ encodeJSON dto
  embedCatchingErrors $ coordinateWriting path False defaultWritingOptions $ \path' ->
    writeFile path' serialized

deleteNodeMetadata_ ::
  Members [RawGraph, Embed IO, Error UserError] effs =>
  FilePath ->
  Sem effs ()
deleteNodeMetadata_ path = do
  embedCatchingErrors $ coordinateWriting path False defaultWritingOptions $ \path' ->
    removeFile path'

runGraphMetadataFilesystemOperationsIO ::
  Members [RawGraph, Embed IO, Error UserError] r =>
  Sem (GraphMetadataFilesystemOperations : r) a ->
  Sem r a
runGraphMetadataFilesystemOperationsIO = interpret \case
  ReadNodeMetadata nid -> readNodeMetadata_ =<< getMetadataFile nid
  WriteNodeMetadata node -> (`writeNodeMetadata_` node) =<< getMetadataFile node.nid
  DeleteNodeMetadata nid -> deleteNodeMetadata_ =<< getMetadataFile nid
