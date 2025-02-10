module DAL.FileSystemOperations where

import DAL.DTO
import DAL.JSON
import DAL.DirectoryFormat
import Effect.RawGraph
import Error.Utils
import Models.NID
import Models.Node
import Models.Graph
import MyPrelude
import System.MacOS.NSFileCoordinator
import System.Directory (removeFile)

readNodeMetadata ::
  Members [RawGraph, Embed IO, Error Missing, Error UserError] effs =>
  NID -> Sem effs (Node Text ())
readNodeMetadata nid = do
  path <- getMetadataFile nid
  result <- embedCatchingErrors $ coordinateReading path False defaultReadingOptions $ \path' ->
    try @IO @IOError $ readFile path'
  serialized <- throwMissingIfLeft nid result
  dto <- decodeJSON serialized
  pure $ nodeFromDTO dto

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

-- I kind of want to do a function like this that writes the entire graph's metadata in one go
-- It's a little tricky though, because we need to be very careful to make sure that
-- we are updating everything necessary. i.e. backlinks naively might not be included
-- writeGraphMetadata ::
--   Members [RawGraph, Embed IO, Error UserError] effs =>
--   Graph t () ->
--   Sem effs ()
-- writeGraphMetadata graph = do
--   path <- getGraphMetadataFile
--   embedCatchingErrors $ coordinateWriting path False defaultWritingOptions $ \path' ->
--     writeFile path' serialized
