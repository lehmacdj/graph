{-# LANGUAGE FlexibleContexts #-}
module Graph.Import.ByteString where

import Data.ByteString.Lazy (ByteString)
import qualified Data.ByteString.Lazy as B
import qualified Data.ByteString.Lazy.Char8 as B
import Data.Digest.Pure.SHA

import Graph
import Graph.Advanced

import Control.Monad.Unique

computeSHA :: ByteString -> String
computeSHA = showDigest . sha512

-- | From an id with an edge with a specific label, add an edge to and create
-- a new file labeled with its hash
-- Returns the nid of the new node and the updated graph
importData
   :: MonadUnique Id m
   => Id -> ByteString -> Graph String -> m (Id, Graph String)
importData nid d g = do
  (n', g') <- followMkEdgeFrom' (computeSHA d) nid g
  pure (nidOf n', setData (Just d) n' g')
