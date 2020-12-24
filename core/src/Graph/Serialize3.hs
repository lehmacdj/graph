{-# LANGUAGE ScopedTypeVariables #-}

-- |
--   This module introduces a serialization format for graphs that uses a
--   directory to store all of the data associated with the graph.
--   Every node's links are serialized into a json file, nid.json.
--   Every node may be associated with auxilliary binary data which is stored in,
--   nid.data where nid is the nid of the associated node.
--   The auxilliary data is available from the console, and should be thought of
--   as an unique edge to a file.
--
--   = Usage instructions
--   All functions that act on the graph take a FilePath as the first argument.
--   This must be a valid path to a directory otherwise a IOError may be thrown.
--   Functions in this module may throw exceptions, improving exception safety
--   is a goal.
--
--   = Differences from Graph.Serialize2
--   For the most part this format is the same as Graph.Serialize2. The only
--   differnces of note are:
--   * Operates on Graph', the replacement for Graph that uses NID as edges
--   exclusively
--   * Interface is more streamlined and lives fully within IO. That is it
--   doesn't interop with effects at all.
module Graph.Serialize3
  ( -- * accessing metadata about the graph
    getAllNodeIds,
    nextNodeId,

    -- * accessing nodes
    serializeNode,
    deserializeNode,
    withSerializedNode,
    doesNodeExist,
    removeNode,

    -- * bulk operations
    readGraph,
    writeGraph,
    initializeGraph,

    -- * low level access to format information (to be used with caution)
    nodeDataFile,
  )
where

import Control.Lens
import qualified Data.Aeson as Aeson
import qualified Data.ByteString.Lazy as B
import qualified Graph'
import Graph.Node' (dataOf, emptyNode, nidOf)
import Graph.Types
import Graph.Types.New
import MyPrelude
import System.Directory
import System.FilePath (dropExtension)

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

serializeNode :: MonadIO m => FilePath -> Node' -> m ()
serializeNode base n = liftIO $ do
  createDirectoryIfMissing True base
  B.writeFile (linksFile base (nidOf n)) (Aeson.encode n)
  case dataOf n of
    Just d -> B.writeFile (nodeDataFile base (nidOf n)) d
    Nothing -> pure ()

-- | returns Left AesonParseError or a successfully parsed node. IOException
-- may be thrown by this for filesystem errors as well
deserializeNode :: MonadIO m => FilePath -> NID -> m (Either String Node')
deserializeNode base nid = do
  fileContents <- liftIO $ B.readFile (linksFile base nid)
  let node = Aeson.eitherDecode fileContents
  d <- liftIO $ tryGetBinaryData base nid
  pure $ fmap (nodeData' .~ d) node

doesNodeExist :: MonadIO m => FilePath -> NID -> m Bool
doesNodeExist base nid = liftIO $ do
  whenM (not <$> doesDirectoryExist base) $
    error "doesNodeExist: graph directory doesn't exist"
  doesFileExist (linksFile base nid)

removeNode :: MonadIO m => FilePath -> NID -> m ()
removeNode base nid = do
  liftIO . removeFile $ linksFile base nid
  -- this file doesn't exist frequently so we want to ignore this error
  liftIO . ignoreIOError . removeFile $ nodeDataFile base nid

-- | Execute a function on a node stored in the filesystem at a specified location
-- ignore nodes that don't exist or if an error occurs
withSerializedNode :: MonadIO m => FilePath -> (Node' -> Node') -> NID -> m ()
withSerializedNode base f nid = liftIO $ do
  maybeNode <- deserializeNode base nid
  case maybeNode of
    Left _ -> pure ()
    Right n -> ignoreIOError $ serializeNode base (f n)

readGraph :: MonadIO m => FilePath -> m (Either String Graph')
readGraph base = do
  nids <- getAllNodeIds base
  nodes <- traverse (deserializeNode base) nids
  pure $ Graph'.insertNodes <$> sequence nodes <*> pure Graph'.emptyGraph

writeGraph :: MonadIO m => FilePath -> Graph' -> m ()
writeGraph base g = liftIO $ do
  createDirectoryIfMissing True base
  traverse_ (serializeNode base) (Graph'.nodesOf g)

-- | initializes graph with a single node with id 0 with no edges, if the
-- directory doesn't exist, this creates one
initializeGraph :: MonadIO m => FilePath -> m ()
initializeGraph base = liftIO $ do
  createDirectoryIfMissing True base
  serializeNode base (emptyNode 0)

tryGetBinaryData :: FilePath -> NID -> IO (Maybe LByteString)
tryGetBinaryData base nid = ioErrorToMaybe $ B.readFile $ nodeDataFile base nid
