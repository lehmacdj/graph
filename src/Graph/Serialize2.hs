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
import Data.List (isSuffixOf)
import qualified Data.ByteString.Lazy as B
import qualified Data.Map as Map
import Text.Read (readMaybe)

import Control.Lens

import System.Directory
import Control.Exception
import System.FilePath

import Graph.Types
import Graph (nidOf, dataOf)

import Error

linksFile :: FilePath -> Id -> FilePath
linksFile base nid = base </> (show nid ++ ".json")

nodeDataFile :: FilePath -> Id -> FilePath
nodeDataFile base nid = base </> (show nid ++ ".data")

-- TODO: rewrite using System.Directory.Tree
-- yields better error handling that isn't quite as sketchy

serializeNode
  :: (ToJSON (Node t), TransitionValid t)
  => Node t -> FilePath -> IO (E ())
serializeNode n base = ioToE $ do
  createDirectoryIfMissing True base
  B.writeFile (linksFile base (nidOf n)) (Aeson.encode n)
  case dataOf n of
    Just d -> B.writeFile (nodeDataFile base (nidOf n)) d
    Nothing -> pure ()

-- | Write the contents of a graph into a directory at the specified location.
serializeGraph
  :: (ToJSON (Node t), TransitionValid t)
  => Graph t -> FilePath -> IO (E ())
serializeGraph g base = ioToE $ do
  createDirectoryIfMissing True base
  mapM_ (`serializeNode` base) (Map.elems (nodeMap g))

ioHandler :: IOError -> IO (Maybe a)
ioHandler = pure . const Nothing

ioErrorToMaybe :: IO a -> IO (Maybe a)
ioErrorToMaybe = (`catch` ioHandler) . (Just <$>)

deserializeNode
  :: (FromJSON (Node t), TransitionValid t)
  => FilePath -> Id -> IO (E (Node t))
deserializeNode base nid = do
  readRes <- ioToE (B.readFile (linksFile base nid))
  readRes `ioBindE` \fileContents -> do
    let decode x = maybeToE (UE $ "failed to decode: " ++ show x) $ Aeson.decode x
        nodeRes = decode fileContents
    nodeRes `ioBindE` \node -> do
      d <- tryGetBinaryData base nid
      pure $ _Success # (nodeData .~ d) node

-- | Load a graph from a directory.
deserializeGraph
  :: (FromJSON (Node t), TransitionValid t)
  => FilePath -> IO (E (Graph t))
deserializeGraph base = do
  cs <- listDirectory base
  let linkFilenames = filter (".json" `isSuffixOf`) cs
      getNID = maybeToE (UE "couldn't read") . readMaybe . dropExtension
      nidRes = traverse getNID linkFilenames
  nidRes `ioBindE` \nodeIds -> do
    fileContents <- mapM (B.readFile . linksFile base) nodeIds
    let decode x = maybeToE (UE $ "failed to decode: " ++ show x) $ Aeson.decode x
        nodeRes = traverse decode fileContents
    nodeRes `ioBindE` \nodes -> do
      datas <- mapM (tryGetBinaryData base) nodeIds
      let nodesFinal = zipWith (nodeData .~) datas nodes
      pure $ _Success # Graph (Map.fromList (nodeIds `zip` nodesFinal))

tryGetBinaryData :: FilePath -> Id -> IO (Maybe ByteString)
tryGetBinaryData = (ioErrorToMaybe .) . (B.readFile .) . nodeDataFile
