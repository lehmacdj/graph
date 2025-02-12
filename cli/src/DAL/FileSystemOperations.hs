{-# LANGUAGE TemplateHaskell #-}

module DAL.FileSystemOperations where

import DAL.DTO
import DAL.JSON
import DAL.DirectoryFormat
import Effect.RawGraph
import Error.Missing
import Error.UserError
import Models.NID
import Models.Node
import MyPrelude
import System.MacOS.NSFileCoordinator
import System.Directory (removeFile)
import Models.Graph
import Models.Edge

data GraphMetadataFilesystemOperations m a where
  ReadNodeMetadata :: NID -> GraphMetadataFilesystemOperations m (Maybe (Node Text ()))
  WriteNodeMetadata :: Node Text () -> GraphMetadataFilesystemOperations m ()
  DeleteNodeMetadata :: NID -> GraphMetadataFilesystemOperations m ()

makeSem ''GraphMetadataFilesystemOperations

readNodeMetadata_ ::
  Members [RawGraph, Embed IO, Error UserError] effs =>
  NID -> Sem effs (Maybe (Node Text ()))
readNodeMetadata_ nid = withEarlyReturn do
  path <- getMetadataFile nid
  result <- embedCatchingErrors $ coordinateReading path False defaultReadingOptions $ \path' ->
    try @IO @IOError $ readFile path'
  serialized <- either (const $ returnEarly Nothing) pure result
  dto <- decodeJSON serialized
  pure $ Just (nodeFromDTO dto)

writeNodeMetadata_ ::
  Members [RawGraph, Embed IO, Error UserError] effs =>
  Node Text () ->
  Sem effs ()
writeNodeMetadata_ node = do
  let dto = nodeToDTO node
  let serialized = toStrict $ encodeJSON dto
  path <- getMetadataFile node.nid
  embedCatchingErrors $ coordinateWriting path False defaultWritingOptions $ \path' ->
    writeFile path' serialized

deleteNodeMetadata_ ::
  Members [RawGraph, Embed IO, Error UserError] effs =>
  NID ->
  Sem effs ()
deleteNodeMetadata_ nid = do
  path <- getMetadataFile nid
  embedCatchingErrors $ coordinateWriting path False defaultWritingOptions $ \path' ->
    removeFile path'

runGraphMetadataFilesystemOperations ::
  Members [RawGraph, Embed IO, Error UserError] r =>
  Sem (GraphMetadataFilesystemOperations : r) a ->
  Sem r a
runGraphMetadataFilesystemOperations = interpret \case
  ReadNodeMetadata nid -> readNodeMetadata_ nid
  WriteNodeMetadata node -> writeNodeMetadata_ node
  DeleteNodeMetadata nid -> deleteNodeMetadata_ nid

readNodeData ::
  Members [RawGraph, Embed IO, Error UserError] effs =>
  NID ->
  Sem effs (Maybe ByteString)
readNodeData nid = do
  path <- getNodeDataFile nid
  result <- embed $ coordinateReading path False defaultReadingOptions $ \path' ->
    try @IO @IOError $ readFile path'
  pure $ either (const Nothing) Just result

writeNodeData ::
  Members [RawGraph, Embed IO, Error UserError] effs =>
  NID ->
  Maybe ByteString ->
  Sem effs ()
writeNodeData nid mData = do
  path <- getNodeDataFile nid
  embedCatchingErrors $ coordinateWriting path False defaultWritingOptions $ \path' ->
    maybe (removeFile path') (writeFile path') mData

data GraphMetadataFilesystemOperationsWriteDiff m a where
  WriteGraphDiff ::
    -- | Nodes read from the source of truth (e.g. the filesystem)
    Map NID (Maybe (Node Text ())) ->
    -- | Graph of changes, including all additions, and accurately reflecting
    -- deletions (though deletions may not be reflected if they were never
    -- loaded)
    Graph Text () ->
    -- | Edges that were deleted
    Set (Edge Text) ->
    GraphMetadataFilesystemOperationsWriteDiff m ()

makeSem ''GraphMetadataFilesystemOperationsWriteDiff

writeGraphDiff_ ::
  Members [RawGraph, Embed IO, Error UserError] effs =>
  Map NID (Maybe (Node Text ())) ->
  Graph Text () ->
  Set (Edge Text) ->
  Sem effs ()
writeGraphDiff_ = error "unimplemented"
