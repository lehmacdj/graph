module Models.Augmentation.Tags where

import Graph.GraphMetadataEditing
import Graph.MaterializePath
import Graph.Paths
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

fetchTags ::
  (Members '[GraphMetadataReading] r) =>
  NID ->
  Eff es Tags
fetchTags nid = do
  mp <-
    materializePath
      nid
      [path| ~*/%{excludedLargeSystemNodes}/~*/%{Absolute tagsNID} |]
  let tags = mapSet ((.transition) . assertSingleton) $ finalNonLoopEdges mp.path
  pure $ Tags tags

writeTags ::
  (Members '[GraphMetadataEditing] r) =>
  NID ->
  Tags ->
  Eff es ()
writeTags _ _ = error "unimplemented"
