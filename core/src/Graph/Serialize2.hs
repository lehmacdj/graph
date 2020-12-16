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
    nextNodeId,

    -- * accessing nodes
    serializeNodeEx,
    deserializeNodeF,
    deserializeNode,
    withSerializedNode,
    doesNodeExist,
    removeNode,
    readGraph,

    -- * low level access to format information (to be used with caution)
    nodeDataFile,
  )
where

import Control.Lens
import Data.Aeson (FromJSON, ToJSON)
import qualified Data.Aeson as Aeson
import qualified Data.ByteString.Lazy as B
import qualified Graph
import Graph.Node (dataOf, nidOf)
import Graph.Types
import MyPrelude
import System.Directory
import System.FilePath (dropExtension)
import UserError

-- | all of the nodes accessible under a given path
getAllNodeIds :: MonadIO m => FilePath -> m [NID]
getAllNodeIds base = do
  files <- liftIO $ listDirectory base
  let linkFiles = filter (".json" `isSuffixOf`) files
      nodes = mapMaybe (readMay . dropExtension) linkFiles
  pure nodes

-- | the next unused node id in the graph
nextNodeId :: MonadIO m => FilePath -> m Int
nextNodeId base = do
  nids <- getAllNodeIds base
  case maximum (Nothing `ncons` map Just nids) of
    Nothing -> pure 0
    Just x -> pure $ x + 1

linksFile :: FilePath -> NID -> FilePath
linksFile base nid = base </> (show nid ++ ".json")

nodeDataFile :: FilePath -> NID -> FilePath
nodeDataFile base nid = base </> (show nid ++ ".data")

-- TODO: rewrite using System.Directory.Tree
-- yields better error handling that isn't quite as sketchy

serializeNodeEx ::
  (ToJSON (Node t), TransitionValid t) =>
  Node t ->
  FilePath ->
  IO ()
serializeNodeEx n base = do
  createDirectoryIfMissing True base
  B.writeFile (linksFile base (nidOf n)) (Aeson.encode n)
  case dataOf n of
    Just d -> B.writeFile (nodeDataFile base (nidOf n)) d
    Nothing -> pure ()

deserializeNodeF ::
  forall t effs.
  ( FromJSON (Node t),
    TransitionValid t,
    Member ThrowUserError effs,
    Member (Embed IO) effs
  ) =>
  FilePath ->
  NID ->
  Sem effs (Node t)
deserializeNodeF base nid = do
  fileContents <- trapIOError' (B.readFile (linksFile base nid))
  node <- throwLeft $ left AesonDeserialize $ Aeson.eitherDecode fileContents
  d <- liftIO $ tryGetBinaryData base nid
  pure $ (nodeData .~ d) node

deserializeNode ::
  forall t m.
  ( FromJSON (Node t),
    TransitionValid t,
    MonadIO m
  ) =>
  FilePath ->
  NID ->
  m (Either String (Node t))
deserializeNode base nid = do
  fileContents <- liftIO $ B.readFile (linksFile base nid)
  let node = Aeson.eitherDecode fileContents
  d <- liftIO $ tryGetBinaryData base nid
  pure $ fmap (nodeData .~ d) node

doesNodeExist :: MonadIO m => FilePath -> NID -> m Bool
doesNodeExist base nid = liftIO $ doesFileExist (linksFile base nid)

removeNode :: MonadIO m => FilePath -> NID -> m ()
removeNode base nid = do
  liftIO . removeFile $ linksFile base nid
  -- this file doesn't exist frequently so we want to ignore this error
  liftIO . ignoreIOError . removeFile $ nodeDataFile base nid

-- | Execute a function on a node stored in the filesystem at a specified location
-- ignore nodes that don't exist or if an error occurs
withSerializedNode ::
  forall t.
  ( FromJSON (Node t),
    ToJSON (Node t),
    TransitionValid t
  ) =>
  (Node t -> Node t) ->
  FilePath ->
  NID ->
  IO ()
withSerializedNode f base nid =
  let ignoreErrors :: Sem [ThrowUserError, Embed IO] () -> Sem '[Embed IO] ()
      ignoreErrors = (`handleError` \(_ :: UserErrors) -> pure ())
   in runM . ignoreErrors . withEffects @[ThrowUserError, Embed IO] $ do
        n <- deserializeNodeF @t @[ThrowUserError, Embed IO] base nid
        trapIOError' @[ThrowUserError, Embed IO] $ serializeNodeEx (f n) base

readGraph :: MonadIO m => FilePath -> m (Either String (Graph String))
readGraph base = do
  nids <- getAllNodeIds base
  nodes <- traverse (deserializeNode base) nids
  pure $ Graph.insertNodes <$> sequence nodes <*> pure Graph.emptyGraph

tryGetBinaryData :: FilePath -> NID -> IO (Maybe LByteString)
tryGetBinaryData = (ioErrorToMaybe .) . (B.readFile .) . nodeDataFile
