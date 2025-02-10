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

readNodeMetadata ::
  Members [RawGraph, Embed IO, Error Missing, Error UserError] effs =>
  NID -> Sem effs (Maybe (Node Text ()))
readNodeMetadata nid = withEarlyReturn do
  path <- getMetadataFile nid
  result <- embedCatchingErrors $ coordinateReading path False defaultReadingOptions $ \path' ->
    try @IO @IOError $ readFile path'
  serialized <- either (const $ returnEarly Nothing) pure result
  dto <- decodeJSON serialized
  pure $ Just (nodeFromDTO dto)

writeNodeMetadata ::
  Members [RawGraph, Embed IO, Error UserError] effs =>
  Node Text () ->
  Sem effs ()
writeNodeMetadata node = do
  let dto = nodeToDTO node
  let serialized = toStrict $ encodeJSON dto
  path <- getMetadataFile node.nid
  embedCatchingErrors $ coordinateWriting path False defaultWritingOptions $ \path' ->
    writeFile path' serialized

deleteNodeMetadata ::
  Members [RawGraph, Embed IO, Error UserError] effs =>
  NID ->
  Sem effs ()
deleteNodeMetadata nid = do
  path <- getMetadataFile nid
  embedCatchingErrors $ coordinateWriting path False defaultWritingOptions $ \path' ->
    removeFile path'

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
