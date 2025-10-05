module Models.Augmentations.Tags where

import Graph.GraphMetadataEditing
import Graph.MaterializePath
import Graph.SystemNodes
import Models.Common
import Models.NID
import Models.Node
import Models.Path.TH
import MyPrelude

newtype Tags = Tags {tags :: Set Text}
  deriving (Eq, Show, Ord, Generic)

instance ShowableAugmentation Tags where
  augmentationProperties (Tags ts)
    | null ts = singletonMap "tags" Nothing
    | otherwise = singletonMap "tags" (Just $ intercalate ", " (toList ts))

fetchTags ::
  (Members '[GraphMetadataReading] r) =>
  NID ->
  Sem r Tags
fetchTags nid =
  (materializePath nid [path| ~*/%{Absolute tagsNID} |]) . path
    & finalNonLoopEdges
    & concat
    & Tags

writeTags ::
  (Members '[GraphMetadataEditing] r) =>
  NID ->
  Tags ->
  Sem r ()
writeTags nid tags = undefined
