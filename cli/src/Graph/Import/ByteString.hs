module Graph.Import.ByteString where

import Data.Digest.Pure.SHA
import Effect.IOWrapper.GetTime
import Effect.IOWrapper.Web
import Effectful.State.Dynamic
import Error.Missing
import Error.UserError (UserError)
import Graph.FreshNID
import Graph.GraphDataEditing
import Graph.GraphEditing (HasGraph)
import Graph.GraphMetadataEditing
import Graph.SystemNodes
import Graph.Time
import Graph.Utils
import Models.Edge (Edge' (..))
import Models.Graph (Graph, emptyGraph)
import Models.NID
import MyPrelude
import System.Random (mkStdGen)
import Utils.Testing

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

-- | Find the node for the data by its hash, or create one tagged with the
-- current time; data already in the graph keeps the time it was first
-- imported.
importData ::
  (Members [FreshNID, Error Missing, GetTime] effs, HasGraph Text effs) =>
  ByteString ->
  Eff effs NID
importData d = do
  nnid <-
    taggingFreshNodesWithTime $ fileHashesNID `transitionsVia` computeSHA d
  writeRawData nnid d
  pure nnid

test_importData :: TestTree
test_importData =
  testGroup
    "importData"
    [ testCase "importing data again leaves the graph unchanged" $
        importing ["a", "a"] @?= importing ["a"],
      testCase "different data is imported separately" $
        assertBool "graphs differ" (importing ["a", "b"] /= importing ["a"])
    ]
  where
    -- the metadata after importing each of the data at increasing times
    importing :: [ByteString] -> Either Missing (Graph Text ())
    importing ds =
      runPureEff
        . evalStateLocal (mkStdGen 0)
        . evalStateLocal (emptyGraph @Text @(Maybe ByteString) @NID)
        . runInMemoryGraphDataEditing @Text
        . runErrorNoCallStack @Missing
        . execStateLocal (emptyGraph @Text @())
        . runInMemoryGraphMetadataEditing @Text
        . runFreshNIDRandom
        . interpretTimeAsMonotonicIncreasingUnixTime
        $ do
          traverse_ (touchNode @Text) [fileHashesNID, importDatesNID]
          traverse_ importData ds
