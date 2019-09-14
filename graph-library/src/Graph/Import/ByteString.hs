{-# LANGUAGE FlexibleContexts #-}
module Graph.Import.ByteString where

import Data.ByteString.Lazy (ByteString)
import Data.Digest.Pure.SHA
import Control.Monad.IO.Class

import Graph
import Graph.Advanced

import Control.Monad.Unique
import Network.HTTP.Conduit (HttpException, simpleHttp)
import Control.Exception (try)

computeSHA :: ByteString -> String
computeSHA = showDigest . sha512

importUrl
  :: (MonadUnique NID m, MonadIO m)
  => NID -> String -> Graph String -> m (Either HttpException (NID, Graph String))
importUrl nid url g = do
  md <- liftIO (try (simpleHttp url) :: IO (Either HttpException ByteString))
  case md of
    Left e -> pure $ Left e
    Right d -> do
      (n, g') <- followMkEdgeFrom' "file-hashes" nid g
      (nid', g'') <- importData (nidOf n) d g'
      (m, g''') <- followMkEdgeFrom' "import-urls" nid g''
      let g'''' = insertEdge (Edge (nidOf m) url nid') g'''
      pure $ Right (nid', g'''')


-- | From an id with an edge with a specific label, add an edge to and create
-- a new file labeled with its hash
-- Returns the nid of the new node and the updated graph
importData
   :: MonadUnique NID m
   => NID -> ByteString -> Graph String -> m (NID, Graph String)
importData nid d g = do
  (n', g') <- followMkEdgeFrom' (computeSHA d) nid g
  pure (nidOf n', setData (Just d) n' g')
