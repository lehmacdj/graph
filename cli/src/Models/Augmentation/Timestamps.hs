module Models.Augmentation.Timestamps where

import Graph.GraphMetadataEditing
import Graph.MaterializePath
import Graph.Paths
import Graph.SystemNodes
import Models.Common
import Models.Connect
import Models.MaterializedPath
import Models.NID
import Models.Path.Simple
import Models.Path.TH
import MyPrelude

newtype Timestamps = Timestamps {timestamps :: Set UTCTime}
  deriving (Eq, Show, Ord, Generic)

instance ShowableAugmentation Timestamps where
  augmentationProperties (Timestamps ts) = case fromNullable ts of
    Nothing -> []
    Just ts' ->
      [ ( Just "timestamp",
          pack $ formatTime defaultTimeLocale "%Y-%m-%d %H:%M" (maximum ts')
        )
      ]

fetchTimestamps ::
  (Members '[GraphMetadataReading] r) =>
  NID ->
  Eff es Timestamps
fetchTimestamps nid = do
  mp <-
    materializePath
      nid
      [path|
        ~regex:"[0-9]{2}:[0-9]{2}:[0-9]{2}\.[0-9]{12}"/%{excludedLargeSystemNodes}
        /~regex:"[0-9]{2}"/%{excludedLargeSystemNodes}
        /~regex:"[0-9]{2}"/%{excludedLargeSystemNodes}
        /~regex:"[0-9]{4}"/%{Absolute importDatesNID}
      |]
  let listUncurry4 f = \case
        [a, b, c, d] -> f a b c d
        _ -> error "Expected exactly four elements"
  leftmostConnects mp.path
    & toList
    & mapMaybe
      ( listUncurry4 timeFromDateStrings
          . reverse
          . map (unpack . (.transition))
          . snd
      )
    & setFromList
    & Timestamps
    & pure

writeTimestamps ::
  (Members '[GraphMetadataEditing] r) =>
  NID ->
  Timestamps ->
  Eff es ()
writeTimestamps _ _ = error "unimplemented"
