module Graph.Import.ByteString where

import Data.Digest.Pure.SHA
import Effect.IOWrapper.GetTime
import Effect.IOWrapper.Web
import Error.Missing
import Error.UserError (UserError)
import Graph.FreshNID
import Graph.GraphDataEditing
import Graph.GraphEditing (HasGraph)
import Graph.GraphMetadataEditing
import Graph.SystemNodes
import Graph.Time
import Graph.Utils
import Models.Edge (Edge, Edge' (..))
import Models.NID
import MyPrelude

computeSHA :: ByteString -> Text
computeSHA = pack . showDigest . sha512 . fromStrict

importUrl ::
  ( Members [Web, FreshNID, Error Missing, Error UserError, GetTime] effs,
    HasGraph Text effs
  ) =>
  URI ->
  Eff effs NID
importUrl url = do
  d <- getHttp url
  nnid <- importData d
  insertEdge (Edge importUrlsNID (renderURI url) nnid)
  pure nnid

-- | From an id with an edge with a specific label, add an edge to and create
-- a new file labeled with its hash
-- Returns the nid of the new node and the updated graph
importData ::
  (Members [FreshNID, Error Missing, GetTime] effs, HasGraph Text effs) =>
  ByteString ->
  Eff effs NID
importData d = do
  nnid <- fileHashesNID `transitionsVia` computeSHA d
  writeRawData nnid d
  tagWithTime nnid
  pure nnid
