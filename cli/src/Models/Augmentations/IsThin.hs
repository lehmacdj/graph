module Models.Augmentations.IsThin where

import Models.Common
import Models.Node
import MyPrelude

data IsThin = Thin | Fetched
  deriving (Eq, Show, Ord, Generic)

instance ShowableAugmentation IsThin where
  augmentationLabel = Nothing
  defaultShowAugmentation = \case
    Thin -> "thin"
    Fetched -> "fetched"
  shouldShowStandaloneAugmentation = True

instance DefaultAugmentation IsThin where
  defaultAugmentation = Thin

onlyFetched :: Prism' (Node t IsThin) (Node t ())
onlyFetched = prism' (fmap (const Fetched)) \case
  n | n.augmentation == Fetched -> Just $ void n
  _ -> Nothing
