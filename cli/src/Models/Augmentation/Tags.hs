module Models.Augmentation.Tags where

import Graph.GraphMetadataEditing
import Graph.MaterializePath
import Graph.SystemNodes
import Models.Common
import Models.Edge
import Models.MaterializedPath
import Models.NID
import Models.Path.Simple
import Models.Path.TH
import MyPrelude

newtype Tags = Tags {tags :: Set Text}
  deriving (Eq, Show, Ord, Generic)

instance ShowableAugmentation Tags where
  augmentationProperties (Tags ts) =
    [(Just "tags", ("[" ++) . (++ "]") $ intercalate ", " (toList ts))]

-- | These system nodes can be fairly large and take a long-ish time to parse /
-- traverse so we explicitly include them from the tag traversal
excludedLargeSystemNodes :: Path Text
excludedLargeSystemNodes =
  ExcludingNIDs $ setFromList [fileHashesNID, importUrlsNID, sequenceIDsNID]

fetchTags ::
  (Members '[GraphMetadataReading] r) =>
  NID ->
  Sem r Tags
fetchTags nid = do
  mp <-
    materializePath
      nid
      [path| ~*/%{excludedLargeSystemNodes}/~*/%{Absolute tagsNID} |]
  let tags = mapSet (.transition) $ concat (finalNonLoopEdges mp.path)
  pure $ Tags tags

writeTags ::
  (Members '[GraphMetadataEditing] r) =>
  NID ->
  Tags ->
  Sem r ()
writeTags _ _ = error "unimplemented"
