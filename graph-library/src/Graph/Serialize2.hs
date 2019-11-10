{-# LANGUAGE NoImplicitPrelude #-}
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

import MyPrelude

import Control.Monad.Freer
import UserError

import Data.Aeson (ToJSON, FromJSON)
import qualified Data.Aeson as Aeson

import qualified Data.ByteString.Lazy as B
import qualified Data.Map as Map
import Text.Read (readMaybe)

import Control.Lens

import System.Directory
import System.FilePath

import Graph.Types
import Graph.Node (nidOf, dataOf)

import Error

linksFile :: FilePath -> NID -> FilePath
linksFile base nid = base </> (show nid ++ ".json")

nodeDataFile :: FilePath -> NID -> FilePath
nodeDataFile base nid = base </> (show nid ++ ".data")

-- TODO: rewrite using System.Directory.Tree
-- yields better error handling that isn't quite as sketchy

serializeNode
  :: (ToJSON (Node t), TransitionValid t)
  => Node t -> FilePath -> IO (E ())
serializeNode = (ioToE .) . serializeNodeEx

serializeNodeEx
  :: (ToJSON (Node t), TransitionValid t)
  => Node t -> FilePath -> IO ()
serializeNodeEx n base = do
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
  forM_ (Map.elems (nodeMap g)) $ \n -> serializeNode n base >> pure ()

ioHandler :: IOError -> IO (Maybe a)
ioHandler = pure . const Nothing

ioErrorToMaybe :: IO a -> IO (Maybe a)
ioErrorToMaybe = (`catch` ioHandler) . (Just <$>)

deserializeNode
  :: (FromJSON (Node t), TransitionValid t)
  => FilePath -> NID -> IO (E (Node t))
deserializeNode base nid = do
  readRes <- ioToE (B.readFile (linksFile base nid))
  readRes `ioBindE` \fileContents -> do
    let decode x = maybeToE (UE $ "failed to decode: " ++ show x) $ Aeson.decode x
        nodeRes = decode fileContents
    nodeRes `ioBindE` \node -> do
      d <- tryGetBinaryData base nid
      pure $ _Success # (nodeData .~ d) node

deserializeNodeF
  :: forall t m effs.
     ( MonadIO m
     , FromJSON (Node t)
     , TransitionValid t
     , Member ThrowUserError effs
     , LastMember m effs
     )
  => FilePath -> NID -> Eff effs(Node t)
deserializeNodeF base nid = do
  fileContents <- trapIOError' (B.readFile (linksFile base nid))
  node <- throwLeft $ left AesonDeserialize $ Aeson.eitherDecode fileContents
  d <- liftIO $ tryGetBinaryData base nid
  pure $ (nodeData .~ d) node

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

-- | Execute a function on a node stored in the filesystem at a specified location
-- ignore nodes that don't exist or if an error occurs
withSerializedNode
  :: (FromJSON (Node t), ToJSON (Node t), TransitionValid t)
  => (Node t -> Node t) -> FilePath -> NID -> IO ()
withSerializedNode f base nid = do
  nr <- deserializeNode base nid
  _ <- nr `ioBindE` \n -> serializeNode (f n) base
  -- we intentionally ignore any errors that might have been returned
  pure ()

tryGetBinaryData :: FilePath -> NID -> IO (Maybe LByteString)
tryGetBinaryData = (ioErrorToMaybe .) . (B.readFile .) . nodeDataFile
