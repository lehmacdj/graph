module Graph.Import.ByteString where

import Data.Digest.Pure.SHA
import Effect.FreshNID
import Effect.Time
import Error.UserError
import Effect.Web
import Graph.Effect
import Graph.SystemNodes
import Graph.Time
import Graph.Utils
import Models.Edge (Edge (..))
import MyPrelude
import Error.Missing

computeSHA :: ByteString -> String
computeSHA = showDigest . sha512 . fromStrict

importUrl ::
  ( Members [Web, FreshNID, Error Missing, GetTime] effs,
    HasGraph String effs
  ) =>
  String ->
  Sem effs NID
importUrl url = do
  d <- getHttp url
  nnid <- importData d
  insertEdge (Edge importUrlsNID url nnid)
  pure nnid

-- | From an id with an edge with a specific label, add an edge to and create
-- a new file labeled with its hash
-- Returns the nid of the new node and the updated graph
importData ::
  (Members [FreshNID, Error Missing, GetTime] effs, HasGraph String effs) =>
  ByteString ->
  Sem effs NID
importData d = do
  nnid <- fileHashesNID `transitionsVia` computeSHA d
  setData @String nnid (Just d)
  tagWithTime nnid
  pure nnid
