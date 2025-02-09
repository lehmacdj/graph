module DAL.FileSystemOperations where

import MyPrelude
import System.MacOS.NSFileCoordinator
import Effect.RawGraph
import Error.Utils
import DAL.DirectoryFormat
import Models.Node
import Models.NID
import Data.Aeson
import DAL.DTO

readNodeMetadata ::
  Members [RawGraph, Embed IO, Error Missing] effs =>
  NID -> Sem effs (Node Text ())
readNodeMetadata nid = do
  path <- getMetadataFile nid
  result <- embed $ coordinateReading path False defaultReadingOptions $ \path' ->
    try @IO @SomeException $ readFile path'
  serialized <- throwMissingIfLeft nid result
  dto <- throwMissingIfLeft nid $ eitherDecode (fromStrict serialized)
  pure $ nodeFromDTO dto
