module Models.Augmentation.IsThin where

import Models.Common
import Models.Node
import MyPrelude

data IsThin = Thin | Fetched
  deriving (Eq, Show, Ord, Generic)

instance ShowableAugmentation IsThin where
  augmentationProperties = \case
    Thin -> [(Nothing, "thin")]
    Fetched -> [(Nothing, "fetched")]

instance DefaultAugmentation IsThin where
  defaultAugmentation = Thin
