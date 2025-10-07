module Models.Augmentation.Timestamps where

import Graph.GraphMetadataEditing
import Models.Common
import Models.NID
import MyPrelude

newtype Timestamps = Timestamps {timestamps :: Set UTCTime}
  deriving (Eq, Show, Ord, Generic)

instance ShowableAugmentation Timestamps where
  augmentationProperties (Timestamps ts) = case fromNullable ts of
    Nothing -> [(Nothing, "timestamp")]
    Just ts' ->
      [(Just "timestamp", pack $ formatTime defaultTimeLocale "%Y-%m-%d %H:%M" (maximum ts'))]

fetchTimestamps ::
  (Members '[GraphMetadataReading] r) =>
  NID ->
  Sem r Timestamps
fetchTimestamps _ = error "unimplemented"

writeTimestamps ::
  (Members '[GraphMetadataEditing] r) =>
  NID ->
  Timestamps ->
  Sem r ()
writeTimestamps _ _ = error "unimplemented"
