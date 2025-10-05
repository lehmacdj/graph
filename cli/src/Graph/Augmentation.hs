module Graph.Augmentation where

import Graph.GraphMetadataEditing
import Models.NID
import Models.Node
import MyPrelude

getNodeWith ::
  (Members '[GraphMetadataReading] r) =>
  (NID -> Sem r a) ->
  NID ->
  Sem r (Maybe (Node Text a))
getNodeWith fetchAugmentation nid = do
  n <- getNodeMetadata nid
  a <- fetchAugmentation nid
  pure $ fmap ($> a) n
