-- |
--   This module introduces a serialization format for graphs that uses a
--   directory to store all of the data associated with the graph.
--   Every node's links are serialized into a json file, nid.json.
--   Every node may be associated with auxilliary binary data which is stored in,
--   nid.data where nid is the nid of the associated node.
--   The auxilliary data is available from the console, and should be thought of
--   as an unique edge to a file.
module DAL.Serialization
  ( -- * accessing metadata about the graph
    getAllNodeIds,

    -- * accessing nodes
    serializeNodeEx,
    deserializeNodeF,
    deserializeNodeWithoutDataF,
    deserializeNode,
    withSerializedNode,
    doesNodeExist,
    removeNode,
    readGraph,
    initializeGraph,
    nodesWithPrefix,
  )
where

import Control.Lens
import DAL.DTO
import DAL.DirectoryFormat
import Data.Aeson (FromJSON, ToJSON)
import Data.Aeson qualified as Aeson
import Data.ByteString qualified as B
import Data.ByteString.Lazy qualified as BL
import Error.UserError hiding (catch, throw)
import Models.Connect
import Models.Graph
import Models.NID
import Models.Node
import MyPrelude
import System.Directory
import System.FilePath (dropExtension, takeFileName)
import System.FilePath.Glob qualified as Glob
import Utils.Base62 (isBase62Char)

-- | all of the nodes accessible under a given path
getAllNodeIds :: (MonadIO m) => FilePath -> m [NID]
getAllNodeIds base = do
  files <- liftIO $ listDirectory base
  let linkFiles = filter (".json" `isSuffixOf`) files
      nodes = mapMaybe (readMay . ("@" ++) . dropExtension) linkFiles
  pure nodes

serializeNodeEx ::
  (ToJSON (NodeDTO t), ValidTransition t) =>
  Node t (Maybe ByteString) ->
  FilePath ->
  IO ()
serializeNodeEx n base = do
  createDirectoryIfMissing True base
  BL.writeFile (metadataFile base (n ^. #nid)) (Aeson.encode . nodeToDTO $ n)
  case n.rawData of
    Just d -> B.writeFile (legacyNodeDataFile base (n ^. #nid)) d
    Nothing ->
      removeFile (legacyNodeDataFile base (n ^. #nid)) `catch` \case
        e | isDoesNotExistError e -> pure ()
        e -> throwIO e

deserializeNodeF ::
  forall t effs.
  ( FromJSON t,
    ValidTransition t,
    Member (Error UserError) effs,
    Member (Embed IO) effs
  ) =>
  FilePath ->
  NID ->
  Sem effs (Node t (Maybe ByteString))
deserializeNodeF base nid = do
  node <- deserializeNodeWithoutDataF base nid
  d <- liftIO $ tryGetBinaryData base nid
  pure $ (#augmentation .~ d) node

-- | Like deserializeNodeF but the data associated with the node is always
-- Nothing. No attempt is made to read the data from the disk.
deserializeNodeWithoutDataF ::
  forall t effs.
  ( FromJSON t,
    ValidTransition t,
    Member (Error UserError) effs,
    Member (Embed IO) effs
  ) =>
  FilePath ->
  NID ->
  Sem effs (Node t (Maybe ByteString))
deserializeNodeWithoutDataF base nid = do
  fileContents <- embedCatchingErrors (B.readFile (metadataFile base nid))
  ($> Nothing) <$> decodeNode nid fileContents

deserializeNode ::
  forall t m.
  ( FromJSON (NodeDTO t),
    ValidTransition t,
    MonadIO m
  ) =>
  FilePath ->
  NID ->
  m (Either String (Node t (Maybe ByteString)))
deserializeNode base nid = do
  fileContents <- liftIO $ B.readFile (metadataFile base nid)
  let node :: Either String (Node t (Maybe ByteString))
      node = second (($> Nothing) . nodeFromDTO) . Aeson.eitherDecode $ fromStrict fileContents
  d <- liftIO $ tryGetBinaryData base nid
  pure $ fmap (#augmentation .~ d) node

doesNodeExist :: (MonadIO m) => FilePath -> NID -> m Bool
doesNodeExist base nid = liftIO $ do
  whenM (not <$> doesDirectoryExist base) $
    error "doesNodeExist: graph directory doesn't exist"
  doesFileExist (metadataFile base nid)

-- | initializes graph with a single node with id 0 with no edges, if the
-- directory doesn't exist, this creates one
initializeGraph :: (MonadIO m) => FilePath -> m ()
initializeGraph base = liftIO $ do
  createDirectoryIfMissing True base
  serializeNodeEx (emptyNode @() @String nilNID $> Nothing) base

removeNode :: (MonadIO m) => FilePath -> NID -> m ()
removeNode base nid = do
  liftIO . removeFile $ metadataFile base nid
  -- this file doesn't exist frequently so we want to ignore this error
  liftIO . ignoreIOError . removeFile $ legacyNodeDataFile base nid

-- | Execute a function on a node stored in the filesystem at a specified location
-- ignore nodes that don't exist or if an error occurs
withSerializedNode ::
  forall t.
  ( FromJSON t,
    ToJSON t,
    ValidTransition t
  ) =>
  (Node t (Maybe ByteString) -> Node t (Maybe ByteString)) ->
  FilePath ->
  NID ->
  IO ()
withSerializedNode f base nid =
  runM . printErrors . withEffects @[Error UserError, Embed IO] $ do
    n <- deserializeNodeF @t @[Error UserError, Embed IO] base nid
    embedCatchingErrors $ serializeNodeEx (f n) base

readGraph :: (MonadIO m) => FilePath -> m (Either String (Graph String (Maybe ByteString)))
readGraph base = do
  nids <- getAllNodeIds base
  nodes <- traverse (deserializeNode base) nids
  pure $ insertNodes <$> sequence nodes <*> pure emptyGraph

-- | takes a base 62 prefix & returns all NIDs in the graph that start with
-- that prefix
nodesWithPrefix :: (HasCallStack) => FilePath -> String -> IO [NID]
nodesWithPrefix base base62Prefix
  | not $ all isBase62Char base62Prefix =
      error "not a base62 character but it should be"
  | otherwise = do
      results <- Glob.globDir1 (Glob.compile (base62Prefix ++ "*.json")) base
      pure $ unsafeNID . pack . takeWhile isBase62Char . takeFileName <$> results

tryGetBinaryData :: FilePath -> NID -> IO (Maybe ByteString)
tryGetBinaryData = (ioErrorToMaybe .) . (B.readFile .) . legacyNodeDataFile
