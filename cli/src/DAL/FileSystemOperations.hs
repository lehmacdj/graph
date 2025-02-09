module DAL.FileSystemOperations where

import DAL.DTO
import DAL.DecodeJSON
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
