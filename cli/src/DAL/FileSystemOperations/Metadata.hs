{-# LANGUAGE TemplateHaskell #-}

module DAL.FileSystemOperations.Metadata where

import DAL.DTO
import DAL.DirectoryFormat
import DAL.RawGraph
import DAL.Serialization (getAllNodeIds)
import Error.UserError
import Models.NID
import Models.Node
import MyPrelude
import System.Directory (pathIsSymbolicLink, removeFile)
import System.FileCoordination

data GraphMetadataFilesystemOperations :: Effect where
  ReadNodeMetadata :: NID -> GraphMetadataFilesystemOperations m (Maybe (Node Text ()))
  WriteNodeMetadata :: Node Text () -> GraphMetadataFilesystemOperations m ()
  DeleteNodeMetadata :: NID -> GraphMetadataFilesystemOperations m ()
  -- | Every node that has metadata stored in the graph
  ListNodeMetadata :: GraphMetadataFilesystemOperations m (Set NID)

makeEffect ''GraphMetadataFilesystemOperations

readNodeMetadata_ ::
  (Members [IOE, Error UserError] effs) =>
  Bool ->
  NID ->
  FilePath ->
  Eff effs (Maybe (Node Text ()))
readNodeMetadata_ shouldCoordinate nid path = withEarlyReturn do
  result <- embedCatchingErrors $
    (if shouldCoordinate then coordinateReading path False defaultReadingOptions else ($ path)) $
      \path' ->
        try @IO @IOError (readFile path') >>= \case
          Right bytes -> pure (Right bytes)
          Left e
            | isDoesNotExistError e ->
                -- nothing is there, unless it is a symbolic link to a file
                -- that isn't there
                Left . bool Nothing (Just e)
                  <$> (pathIsSymbolicLink path' `catch` \(_ :: IOError) -> pure False)
            | otherwise -> pure (Left (Just e))
  serialized <- case result of
    Right bytes -> pure bytes
    Left Nothing -> returnEarly Nothing
    -- What is there but can't be read (a file without the permission, a
    -- directory, a link that leads nowhere) is not a missing node: taking it
    -- for one would have the node created over it, or through the link.
    Left (Just e) -> throwError (UnreadableNodeFile nid e)
  Just <$> decodeNode nid serialized

writeNodeMetadata_ ::
  (Members [IOE, Error UserError] effs) =>
  Bool ->
  FilePath ->
  Node Text () ->
  Eff effs ()
writeNodeMetadata_ shouldCoordinate path node = do
  let dto = nodeToDTO node
  let serialized = toStrict $ encodeJSON dto
  embedCatchingErrors $
    (if shouldCoordinate then coordinateWriting path False defaultWritingOptions else ($ path)) $
      \path' ->
        writeFile path' serialized

deleteNodeMetadata_ ::
  (Members [RawGraph, IOE, Error UserError] effs) =>
  Bool ->
  FilePath ->
  Eff effs ()
deleteNodeMetadata_ shouldCoordinate path = do
  embedCatchingErrors $
    (if shouldCoordinate then coordinateWriting path False defaultWritingOptions else ($ path)) $
      \path' ->
        removeFile path'

runGraphMetadataFilesystemOperationsIO ::
  (Members [RawGraph, IOE, Error UserError] r) =>
  Bool ->
  Eff (GraphMetadataFilesystemOperations : r) a ->
  Eff r a
runGraphMetadataFilesystemOperationsIO useCoordination = interpret \_ -> \case
  ReadNodeMetadata nid -> do
    -- the node is returned as the file has it, even if the file says it is
    -- another node: whoever reads it compares the two (see 'damageAmong')
    path <- getMetadataFile nid
    readNodeMetadata_ useCoordination nid path
  WriteNodeMetadata node -> do
    f <- getMetadataFile node.nid
    writeNodeMetadata_ useCoordination f node
  DeleteNodeMetadata nid ->
    deleteNodeMetadata_ useCoordination =<< getMetadataFile nid
  ListNodeMetadata -> setFromList <$> (getAllNodeIds =<< getGraphFilePath)

runGraphMetadataFilesystemOperationsDryRun ::
  (Members [RawGraph, IOE, Error UserError] r) =>
  Bool ->
  Eff (GraphMetadataFilesystemOperations : r) a ->
  Eff r a
runGraphMetadataFilesystemOperationsDryRun useCoordination = interpret \_ -> \case
  ReadNodeMetadata nid ->
    runGraphMetadataFilesystemOperationsIO useCoordination (readNodeMetadata nid)
  WriteNodeMetadata node -> say $ "Would write node: " <> tshow node
  DeleteNodeMetadata nid -> say $ "Would delete node with NID: " <> tshow nid
  ListNodeMetadata ->
    runGraphMetadataFilesystemOperationsIO useCoordination listNodeMetadata
