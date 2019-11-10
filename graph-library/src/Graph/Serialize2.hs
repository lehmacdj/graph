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
import Control.Monad.Freer.Error
import UserError

import Data.Aeson (ToJSON, FromJSON)
import qualified Data.Aeson as Aeson

import qualified Data.ByteString.Lazy as B

import Control.Lens

import System.Directory

import Graph.Types
import Graph.Node (nidOf, dataOf)

linksFile :: FilePath -> NID -> FilePath
linksFile base nid = base </> (show nid ++ ".json")

nodeDataFile :: FilePath -> NID -> FilePath
nodeDataFile base nid = base </> (show nid ++ ".data")

-- TODO: rewrite using System.Directory.Tree
-- yields better error handling that isn't quite as sketchy

serializeNodeEx
  :: (ToJSON (Node t), TransitionValid t)
  => Node t -> FilePath -> IO ()
serializeNodeEx n base = do
  createDirectoryIfMissing True base
  B.writeFile (linksFile base (nidOf n)) (Aeson.encode n)
  case dataOf n of
    Just d -> B.writeFile (nodeDataFile base (nidOf n)) d
    Nothing -> pure ()

ioHandler :: IOError -> IO (Maybe a)
ioHandler = pure . const Nothing

ioErrorToMaybe :: IO a -> IO (Maybe a)
ioErrorToMaybe = (`catch` ioHandler) . (Just <$>)

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

-- | Execute a function on a node stored in the filesystem at a specified location
-- ignore nodes that don't exist or if an error occurs
withSerializedNode
  :: forall t m.
     ( FromJSON (Node t)
     , ToJSON (Node t)
     , TransitionValid t
     , MonadIO m
     )
  => (Node t -> Node t) -> FilePath -> NID -> m ()
withSerializedNode f base nid =
  let ignoreErrors :: Eff [ThrowUserError, m] () -> Eff '[m] ()
      ignoreErrors = (`handleError` \(_ :: UserErrors) -> pure ())
   in runM . ignoreErrors . withEffect @[ThrowUserError, m] $ do
     n <- deserializeNodeF @t @m @[ThrowUserError, m] base nid
     trapIOError' @m @[ThrowUserError, m] $ serializeNodeEx (f n) base

tryGetBinaryData :: FilePath -> NID -> IO (Maybe LByteString)
tryGetBinaryData = (ioErrorToMaybe .) . (B.readFile .) . nodeDataFile
