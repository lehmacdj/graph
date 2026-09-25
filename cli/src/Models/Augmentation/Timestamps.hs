module Models.Augmentation.Timestamps where

import Graph.GraphMetadataEditing
import Graph.PathPattern.Resolve
import Graph.Paths
import Graph.SystemNodes
import Models.Common
import Models.NID
import Models.Path.Simple
import Models.Path.TH
import Models.PathPattern.Match (ConcreteHop (..), ConcreteRoute (..), selectedRoutes)
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
  Eff r Timestamps
fetchTimestamps nid = do
  mp <-
    resolvePath
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
  concatMap selectedRoutes mp.resolutions
    & mapMaybe
      ( listUncurry4 timeFromDateStrings
          . reverse
          . map (unpack . (.label))
          . (.hops)
      )
    & setFromList
    & Timestamps
    & pure

writeTimestamps ::
  (Members '[GraphMetadataEditing] r) =>
  NID ->
  Timestamps ->
  Eff r ()
writeTimestamps _ _ = error "unimplemented"
