{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE NoImplicitPrelude #-}
module Effect.Graph.Import.ByteString where

import ClassyPrelude

import Data.Digest.Pure.SHA

import Graph (Edge(..))

import Control.Monad.Freer
import Control.Monad.Freer.Fresh
import Effect.Web
import Effect.Graph
import Effect.Graph.Advanced
import Effect.Throw

computeSHA :: LByteString -> String
computeSHA = showDigest . sha512

importUrl
  :: (Members [Web, Fresh, ThrowMissing] effs, HasGraph String effs)
  => NID -> String -> Eff effs NID
importUrl nid url = do
  d <- getHttp url
  fileHashes <- nid `transitionsVia` "file-hashes"
  importUrls <- nid `transitionsVia` "import-urls"
  nnid <- importData fileHashes d
  insertEdge (Edge importUrls url nnid)
  pure nnid

-- | From an id with an edge with a specific label, add an edge to and create
-- a new file labeled with its hash
-- Returns the nid of the new node and the updated graph
importData
   :: (Members [Fresh, ThrowMissing] effs, HasGraph String effs)
   => NID -> LByteString -> Eff effs NID
importData nid d = do
  nnid <- nid `transitionsVia` computeSHA d
  setData @String nnid (Just d)
  pure nnid
