{-# LANGUAGE TemplateHaskell #-}

module DAL.FileSystemOperations.Data where

import DAL.DirectoryFormat
import DAL.RawGraph
import Error.Missing
import Error.UserError
import Models.NID
import MyPrelude
import System.Directory (removeFile)
import System.MacOS.NSFileCoordinator

data GraphDataFilesystemOperations m a where
  ReadNodeData :: NID -> String -> GraphDataFilesystemOperations m (Maybe ByteString)
  WriteNodeData :: NID -> String -> ByteString -> GraphDataFilesystemOperations m ()
  DeleteNodeData :: NID -> String -> GraphDataFilesystemOperations m ()

makeSem ''GraphDataFilesystemOperations

readNodeData_ ::
  Members [RawGraph, Embed IO, Error UserError] effs =>
  NID ->
  -- | File extension for the node's data
  String ->
  Sem effs (Maybe ByteString)
readNodeData_ nid fileExtension = do
  path <- getNodeDataFile nid fileExtension
  result <- embed $
    coordinateReading path False defaultReadingOptions $ \path' ->
      try @IO @IOError $ readFile path'
  pure $ either (const Nothing) Just result

writeNodeData_ ::
  Members [RawGraph, Embed IO, Error UserError] effs =>
  NID ->
  -- | File extension for the node's data
  String ->
  ByteString ->
  Sem effs ()
writeNodeData_ nid extension rawData = do
  path <- getNodeDataFile nid extension
  embedCatchingErrors $
    coordinateWriting path False defaultWritingOptions $ \path' ->
      writeFile path' rawData

deleteNodeData_ ::
  Members [RawGraph, Embed IO, Error UserError] effs =>
  NID ->
  -- | File extension for the node's data
  String ->
  Sem effs ()
deleteNodeData_ nid extension = do
  path <- getNodeDataFile nid extension
  embedCatchingErrors $
    coordinateWriting path False defaultWritingOptions removeFile

runGraphDataFilesystemOperationsIO ::
  Members [RawGraph, Embed IO, Error UserError] r =>
  Sem (GraphDataFilesystemOperations : r) a ->
  Sem r a
runGraphDataFilesystemOperationsIO = interpret \case
  ReadNodeData nid extension -> readNodeData_ nid extension
  WriteNodeData nid extension rawData -> writeNodeData_ nid extension rawData
  DeleteNodeData nid extension -> deleteNodeData_ nid extension
