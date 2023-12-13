{-# LANGUAGE FlexibleContexts #-}

module Effect.Graph.Import.ByteString where

import Data.Digest.Pure.SHA
import Effect.FreshNID
import Effect.Graph
import Effect.Graph.Advanced
import Effect.Time
import Effect.Web
import Graph (Edge (..))
import MyPrelude
import UserError

computeSHA :: ByteString -> String
computeSHA = showDigest . sha512 . fromStrict

importUrl ::
  ( Members [Web, FreshNID, Error Missing, GetTime] effs,
    HasGraph String effs
  ) =>
  NID ->
  String ->
  Sem effs NID
importUrl root url = do
  d <- getHttp url
  importUrls <- root `transitionsVia` "import-urls"
  nnid <- importData root d
  insertEdge (Edge importUrls url nnid)
  pure nnid

timeToDateStrings :: UTCTime -> NonNull [String]
timeToDateStrings time =
  impureNonNull
    [ formatTime' "%Y" time,
      formatTime' "%m" time,
      formatTime' "%d" time,
      formatTime' "%H:%M:%S.%q" time
    ]
  where
    formatTime' = formatTime defaultTimeLocale

-- | From an id with an edge with a specific label, add an edge to and create
-- a new file labeled with its hash
-- Returns the nid of the new node and the updated graph
importData ::
  (Members [FreshNID, Error Missing, GetTime] effs, HasGraph String effs) =>
  NID ->
  ByteString ->
  Sem effs NID
importData root d = do
  fileHashes <- root `transitionsVia` "file-hashes"
  nnid <- fileHashes `transitionsVia` computeSHA d
  setData @String nnid (Just d)
  importDates <- root `transitionsVia` "import-dates"
  timeStrings <- timeToDateStrings <$> currentTime
  transitionsViaManyTo importDates timeStrings nnid
  pure nnid
