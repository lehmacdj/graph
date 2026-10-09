{-# LANGUAGE TemplateHaskell #-}

module DAL.FileSystemOperations.Metadata where

import DAL.DTO
import DAL.DirectoryFormat
import DAL.RawGraph
import DAL.Serialization (getAllNodeIds)
import Data.Char (isDigit)
import Error.UserError
import Models.NID
import Models.Node
import MyPrelude
import System.Directory
  ( copyFile,
    doesFileExist,
    doesPathExist,
    pathIsSymbolicLink,
    removeFile,
  )
import System.FileCoordination
import System.FilePath (takeDirectory)

data GraphMetadataFilesystemOperations :: Effect where
  ReadNodeMetadata :: NID -> GraphMetadataFilesystemOperations m (Maybe (Node Text ()))
  WriteNodeMetadata :: Node Text () -> GraphMetadataFilesystemOperations m ()
  DeleteNodeMetadata :: NID -> GraphMetadataFilesystemOperations m ()
  -- | Keep a copy of the file of a node, as it is, in a file with the given
  -- name in the graph's directory, where nothing takes it for a node. For a
  -- file that can't be read as a node, before it is replaced: what it holds
  -- must not be lost.
  KeepNodeMetadataCopy :: NID -> FilePath -> GraphMetadataFilesystemOperations m ()
  -- | Remove a file that was kept of a node file, given its name
  -- ('keptNodeFileNames'), from the graph's directory
  RemoveKeptNodeMetadata :: FilePath -> GraphMetadataFilesystemOperations m ()
  -- | Every node that has metadata stored in the graph
  ListNodeMetadata :: GraphMetadataFilesystemOperations m (Set NID)
  -- | Whether writes are only described and nothing is written (a dry run),
  -- for messages that say what was done
  OnlyDescribesWrites :: GraphMetadataFilesystemOperations m Bool

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

-- | Each of the functions below that changes a file of the graph does it
-- as a change of its own ('changingGraphFile'): it is on record once it is
-- made, and the program does not end in the middle of it. That is done
-- where the file is changed, inside of what coordinates the access, so
-- that waiting for the access is no part of it.
writeNodeMetadata_ ::
  (Members [RawGraph, IOE, Error UserError] effs) =>
  Bool ->
  FilePath ->
  Node Text () ->
  Eff effs ()
writeNodeMetadata_ shouldCoordinate path node = do
  let dto = nodeToDTO node
  let serialized = toStrict $ encodeJSON dto
  writes <- getGraphWrites
  embedCatchingErrors $
    (if shouldCoordinate then coordinateWriting path False defaultWritingOptions else ($ path)) $
      \path' ->
        changingGraphFile writes (writeFile path' serialized)

deleteNodeMetadata_ ::
  (Members [RawGraph, IOE, Error UserError] effs) =>
  Bool ->
  FilePath ->
  Eff effs ()
deleteNodeMetadata_ shouldCoordinate path = do
  writes <- getGraphWrites
  embedCatchingErrors $
    (if shouldCoordinate then coordinateWriting path False defaultWritingOptions else ($ path)) $
      \path' ->
        changingGraphFile writes (removeFile path')

-- | The names that the copies kept of the node file with this name get
-- ('KeepNodeMetadataCopy'), in the order in which they are taken.
keptNodeFileNames :: FilePath -> [FilePath]
keptNodeFileNames nodeFile =
  [ nodeFile <> ".unparsable" <> suffix
  | suffix <- "" : map (("-" <>) . show) [2 :: Int ..]
  ]

-- | Whether the second name is one that a copy kept of the node file with
-- the first name gets: one of 'keptNodeFileNames', written as it is there.
-- Nothing else is, however its name starts (@….json.unparsable.txt@) and
-- whatever number it ends with (@-0@, @-1@, @-02@).
isKeptNodeFileName :: FilePath -> FilePath -> Bool
isKeptNodeFileName nodeFile name =
  case stripPrefix (nodeFile <> ".unparsable") name of
    Just "" -> True
    Just ('-' : number@(leading : _)) ->
      all isDigit number && leading /= '0' && number /= "1"
    _ -> False

-- | Whether what is at this path can be a copy kept of a node file, if it is
-- named like one ('isKeptNodeFileName'): a file, and neither a directory nor
-- a symbolic link, to whatever. A copy is made as a file
-- ('keepNodeMetadataCopy_'), and nothing else is taken for one: it is not
-- reported as left over, and never removed.
isKeptNodeFile :: FilePath -> IO Bool
isKeptNodeFile path =
  ((&&) <$> doesFileExist path <*> (not <$> pathIsSymbolicLink path))
    `catch` \(_ :: IOError) -> pure False

-- | Whether there is anything under this name, be it a symbolic link to a
-- file that isn't there: a copy of a node file is never kept under a name
-- that something has.
isNameTaken :: FilePath -> IO Bool
isNameTaken path =
  -- the first one follows a link, the second one doesn't
  doesPathExist path >>= \case
    True -> pure True
    False -> pathIsSymbolicLink path `catch` \(_ :: IOError) -> pure False

-- | The copies kept of the node file at this path, found by trying their
-- names in order until one is not there (or is no copy: 'isKeptNodeFile'). So
-- the directory is
-- not listed, which would make deleting a node as slow as the graph is big,
-- and a copy under a later name than a missing one is not found: it is left
-- over, and @fsck@ reports it.
keptNodeFilesOf :: FilePath -> IO [FilePath]
keptNodeFilesOf path = go (keptNodeFileNames path)
  where
    go = \case
      name : names ->
        isKeptNodeFile name >>= bool (pure []) ((name :) <$> go names)
      [] -> pure []

keepNodeMetadataCopy_ ::
  (Members [RawGraph, IOE, Error UserError] effs) =>
  Bool ->
  FilePath ->
  -- | the name of the copy, in the same directory
  FilePath ->
  Eff effs ()
keepNodeMetadataCopy_ shouldCoordinate path name = do
  writes <- getGraphWrites
  embedCatchingErrors $
    (if shouldCoordinate then coordinateReading path False defaultReadingOptions else ($ path)) $
      \path' -> do
        let destination = takeDirectory path' </> name
        -- copying would replace it
        whenM (isNameTaken destination) . throwIO $
          mkIOError alreadyExistsErrorType "keeping a copy" Nothing (Just destination)
        changingGraphFile writes (copyFile path' destination)

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
  KeepNodeMetadataCopy nid name -> do
    path <- getMetadataFile nid
    keepNodeMetadataCopy_ useCoordination path name
  RemoveKeptNodeMetadata name -> do
    directory <- getGraphFilePath
    writes <- getGraphWrites
    -- what is there under the name by now may be no copy any more (a
    -- directory, say), or nothing: that stays
    embedCatchingErrors $
      whenM (isKeptNodeFile (directory </> name)) $
        changingGraphFile writes (removeFile (directory </> name))
  ListNodeMetadata -> setFromList <$> (getAllNodeIds =<< getGraphFilePath)
  OnlyDescribesWrites -> pure False

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
  KeepNodeMetadataCopy nid name ->
    say $ "Would keep a copy of the file of node " <> tshow nid <> " as " <> pack name
  RemoveKeptNodeMetadata name -> say $ "Would remove the kept file " <> pack name
  ListNodeMetadata ->
    runGraphMetadataFilesystemOperationsIO useCoordination listNodeMetadata
  OnlyDescribesWrites -> pure True
