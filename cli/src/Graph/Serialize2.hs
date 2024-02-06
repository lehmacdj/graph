{-# LANGUAGE ScopedTypeVariables #-}

-- |
--   This module introduces a serialization format for graphs that uses a
--   directory to store all of the data associated with the graph.
--   Every node's links are serialized into a json file, nid.json.
--   Every node may be associated with auxilliary binary data which is stored in,
--   nid.data where nid is the nid of the associated node.
--   The auxilliary data is available from the console, and should be thought of
--   as an unique edge to a file.
module Graph.Serialize2
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

    -- * low level access to format information (to be used with caution)
    nodeDataFile,
  )
where

import Control.Lens
import Data.Aeson (FromJSON, ToJSON)
import qualified Data.Aeson as Aeson
import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as BL
import qualified Graph
import Graph.DataTransferObjects
import Graph.Node (dataOf, nidOf)
import Graph.Types
import MyPrelude
import System.Directory
import System.FilePath (dropExtension)
import UserError hiding (catch, throw)

-- | all of the nodes accessible under a given path
getAllNodeIds :: MonadIO m => FilePath -> m [NID]
getAllNodeIds base = do
  files <- liftIO $ listDirectory base
  let linkFiles = filter (".json" `isSuffixOf`) files
      nodes = mapMaybe (readMay . dropExtension) linkFiles
  pure nodes

linksFile :: FilePath -> NID -> FilePath
linksFile base nid = base </> (show nid ++ ".json")

nodeDataFile :: FilePath -> NID -> FilePath
nodeDataFile base nid = base </> (show nid ++ ".data")

serializeNodeEx ::
  (ToJSON (NodeDTO t), TransitionValid t) =>
  Node t ->
  FilePath ->
  IO ()
serializeNodeEx n base = do
  createDirectoryIfMissing True base
  BL.writeFile (linksFile base (nidOf n)) (Aeson.encode . nodeToDTO $ n)
  case dataOf n of
    Just d -> B.writeFile (nodeDataFile base (nidOf n)) d
    Nothing ->
      removeFile (nodeDataFile base (nidOf n)) `catch` \case
        e | isDoesNotExistError e -> pure ()
        e -> throwIO e

deserializeNodeF ::
  forall t effs.
  ( FromJSON (NodeDTO t),
    TransitionValid t,
    Member (Error UserError) effs,
    Member (Embed IO) effs
  ) =>
  FilePath ->
  NID ->
  Sem effs (Node t)
deserializeNodeF base nid = do
  node <- deserializeNodeWithoutDataF base nid
  d <- liftIO $ tryGetBinaryData base nid
  pure $ (#associatedData .~ d) node

-- | Like deserializeNodeF but the data associated with the node is always
-- Nothing. No attempt is made to read the data from the disk.
deserializeNodeWithoutDataF ::
  forall t effs.
  ( FromJSON (NodeDTO t),
    TransitionValid t,
    Member (Error UserError) effs,
    Member (Embed IO) effs
  ) =>
  FilePath ->
  NID ->
  Sem effs (Node t)
deserializeNodeWithoutDataF base nid = do
  fileContents <- fromExceptionToUserError (B.readFile (linksFile base nid))
  throwLeft
    . bimap AesonDeserialize nodeFromDTO
    . Aeson.eitherDecode
    . fromStrict
    $ fileContents

deserializeNode ::
  forall t m.
  ( FromJSON (NodeDTO t),
    TransitionValid t,
    MonadIO m
  ) =>
  FilePath ->
  NID ->
  m (Either String (Node t))
deserializeNode base nid = do
  fileContents <- liftIO $ B.readFile (linksFile base nid)
  let node :: Either String (Node t)
      node = second nodeFromDTO . Aeson.eitherDecode $ fromStrict fileContents
  d <- liftIO $ tryGetBinaryData base nid
  pure $ fmap (#associatedData .~ d) node

doesNodeExist :: MonadIO m => FilePath -> NID -> m Bool
doesNodeExist base nid = liftIO $ do
  whenM (not <$> doesDirectoryExist base) $
    error "doesNodeExist: graph directory doesn't exist"
  doesFileExist (linksFile base nid)

-- | initializes graph with a single node with id 0 with no edges, if the
-- directory doesn't exist, this creates one
initializeGraph :: MonadIO m => FilePath -> m ()
initializeGraph base = liftIO $ do
  createDirectoryIfMissing True base
  serializeNodeEx (Graph.emptyNode @String nilNID) base

removeNode :: MonadIO m => FilePath -> NID -> m ()
removeNode base nid = do
  liftIO . removeFile $ linksFile base nid
  -- this file doesn't exist frequently so we want to ignore this error
  liftIO . ignoreIOError . removeFile $ nodeDataFile base nid

-- | Execute a function on a node stored in the filesystem at a specified location
-- ignore nodes that don't exist or if an error occurs
withSerializedNode ::
  forall t.
  ( FromJSON (NodeDTO t),
    ToJSON (NodeDTO t),
    TransitionValid t
  ) =>
  (Node t -> Node t) ->
  FilePath ->
  NID ->
  IO ()
withSerializedNode f base nid =
  runM . printErrors . withEffects @[Error UserError, Embed IO] $ do
    n <- deserializeNodeF @t @[Error UserError, Embed IO] base nid
    fromExceptionToUserError $ serializeNodeEx (f n) base

readGraph :: MonadIO m => FilePath -> m (Either String (Graph String))
readGraph base = do
  nids <- getAllNodeIds base
  nodes <- traverse (deserializeNode base) nids
  pure $ Graph.insertNodes <$> sequence nodes <*> pure Graph.emptyGraph

tryGetBinaryData :: FilePath -> NID -> IO (Maybe ByteString)
tryGetBinaryData = (ioErrorToMaybe .) . (B.readFile .) . nodeDataFile
