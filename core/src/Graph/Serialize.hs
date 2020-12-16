-- |
--   This module introduces a serialization format for graphs that uses a
--   directory to store all of the data associated with the graph.
--   Every nodes links are serialized into a json file, links.json.
--   Every node may be associated with auxilliary binary data which is stored in,
--   nid.data where nid is the nid of the associated node.
--   The auxilliary data is available from the console, and should be thought of
--   as an unique edge to a file.
module Graph.Serialize where

import Data.Aeson (FromJSON, ToJSON)
import qualified Data.Aeson as Aeson
import qualified Data.ByteString.Lazy as B
import Graph.Types
import MyPrelude
import System.Directory

linksFile :: FilePath -> FilePath
linksFile = (</> "links.json")

nodeDataFile :: FilePath -> NID -> FilePath
nodeDataFile base nid = base </> (show nid ++ ".data")

-- | Write the contents of a graph into a directory at the specified location.
serializeGraph ::
  (Show t, ToJSON t, Ord t) =>
  Graph t ->
  FilePath ->
  IO (Maybe ())
serializeGraph g base = (`catch` ioHandler) $ do
  createDirectoryIfMissing True base
  Just <$> B.writeFile (linksFile base) (Aeson.encode g)

ioHandler :: IOError -> IO (Maybe a)
ioHandler = pure . const Nothing

-- | Load a graph from a directory.
deserializeGraph ::
  (FromJSON t, Read t, Show t, Ord t) =>
  FilePath ->
  IO (Maybe (Graph t))
deserializeGraph base =
  (Aeson.decode <$> B.readFile (linksFile base)) `catch` ioHandler

getBinaryData :: FilePath -> NID -> IO LByteString
getBinaryData = (B.readFile .) . nodeDataFile
