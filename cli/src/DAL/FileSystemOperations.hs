module DAL.FileSystemOperations where

import DAL.DTO
import DAL.JSON
import DAL.DirectoryFormat
import Effect.RawGraph
import Error.Utils
import Models.NID
import Models.Node
import MyPrelude
import System.MacOS.NSFileCoordinator

readNodeMetadata ::
  Members [RawGraph, Embed IO, Error Missing, Error UserError] effs =>
  NID -> Sem effs (Node Text ())
readNodeMetadata nid = do
  path <- getMetadataFile nid
  result <- embed $ coordinateReading path False defaultReadingOptions $ \path' ->
    try @IO @SomeException $ readFile path'
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
  result <- embed $ coordinateWriting path False defaultWritingOptions $ \path' ->
    try @IO @IOError $ writeFile path' serialized
  throwLeft result
