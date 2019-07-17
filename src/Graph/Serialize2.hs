{-# LANGUAGE ScopedTypeVariables #-}
{-|
   This module introduces a serialization format for graphs that uses a
   directory to store all of the data associated with the graph.
   Every node's links are serialized into a json file, nid.json.
   Every node may be associated with auxilliary binary data which is stored in,
   nid.data where nid is the nid of the associated node.
   The auxilliary data is available from the console, and should be thought of
   as an unique edge to a file.
 -}
module Graph.Serialize2 where

import Data.Aeson (ToJSON, FromJSON)
import qualified Data.Aeson as Aeson

import Data.ByteString.Lazy (ByteString)
import Data.Functor ((<&>))
import Data.List (isSuffixOf)
import Data.Maybe (mapMaybe)
import qualified Data.ByteString.Lazy as B
import qualified Data.ByteString.Lazy.Char8 as B
import qualified Data.Map as Map

import Control.Lens

import System.IO
import System.Directory
import Control.Exception
import System.FilePath

import Graph.Types
import Graph (nidOf)

linksFile :: FilePath -> Id -> FilePath
linksFile base nid = base </> (show nid ++ ".json")

nodeDataFile :: FilePath -> Id -> FilePath
nodeDataFile base nid = base </> (show nid ++ ".data")

-- TODO: rewrite using System.Directory.Tree

serializeNode
  :: (Show t, ToJSON t, Ord t)
  => Node t -> FilePath -> IO ()
serializeNode n base = do
  createDirectoryIfMissing True base
  B.writeFile (linksFile base (nidOf n)) (Aeson.encode n)

-- | Write the contents of a graph into a directory at the specified location.
serializeGraph
  :: (Show t, ToJSON t, Ord t)
  => Graph t -> FilePath -> IO (Maybe ())
serializeGraph g base = (`catch` ioHandler) $ do
  createDirectoryIfMissing True base
  Just <$> mapM_ (`serializeNode` base) (Map.elems (nodeMap g))

ioHandler :: IOError -> IO (Maybe a)
ioHandler = pure . const Nothing

-- | Load a graph from a directory.
deserializeGraph
  :: (FromJSON t, Read t, Show t, Ord t)
  => FilePath -> IO (Maybe (Graph t))
deserializeGraph base = (`catch` ioHandler) $ do
  contents <- listDirectory base
  let linkFilenames = filter (".json" `isSuffixOf`) contents
  nodeIds <- mapM (readIO . dropExtension) linkFilenames
  fileContents <- mapM (B.readFile . linksFile base) nodeIds
  let nodes = mapM Aeson.decode fileContents
  pure $ nodes <&> \x -> Graph (Map.fromList (nodeIds `zip` x))

getBinaryData :: FilePath -> Id -> IO ByteString
getBinaryData = (B.readFile .) . nodeDataFile
