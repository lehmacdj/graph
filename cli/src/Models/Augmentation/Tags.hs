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
    singletonMap "tags" . Just $
      ("[" ++) . (++ "]") $
        intercalate ", " (toList ts)

fetchTags ::
  (Members '[GraphMetadataReading] r) =>
  NID ->
  Sem r Tags
fetchTags nid = do
  mp <- materializePath nid [path| ~*/~*/%{Absolute tagsNID} |]
  let tags = mapSet (.transition) $ concat (finalNonLoopEdges mp.path)
  pure $ Tags tags

writeTags ::
  (Members '[GraphMetadataEditing] r) =>
  NID ->
  Tags ->
  Sem r ()
writeTags nid tags = undefined
