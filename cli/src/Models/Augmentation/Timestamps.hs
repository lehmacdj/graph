module Models.Augmentation.Timestamps where

import Graph.GraphMetadataEditing
import Graph.SystemNodes
import Models.Common
import Models.NID
import Models.Node
import Models.Path.TH
import MyPrelude

newtype Timestamps = Timestamps {timestamps :: Set UTCTime}
  deriving (Eq, Show, Ord, Generic)

instance ShowableAugmentation Timestamps where
  augmentationProperties (Timestamps ts) = case fromNullable ts of
    Nothing -> singletonMap "timestamp" Nothing
    Just ts' ->
      singletonMap "timestamp" $
        Just . pack $
          formatTime defaultTimeLocale "%Y-%m-%d %H:%M" (maximum ts')

fetchTimestamps ::
  (Members '[GraphMetadataReading] r) =>
  NID ->
  Sem r Timestamps
fetchTimestamps nid = undefined

writeTimestamps ::
  (Members '[GraphMetadataEditing] r) =>
  NID ->
  Timestamps ->
  Sem r ()
writeTimestamps nid timestamps = undefined
