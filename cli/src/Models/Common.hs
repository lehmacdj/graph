{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE CPP #-}

module Models.Common where

import Data.Constraint
import Data.Constraint.Unsafe
import MyPrelude hiding ((\\))

#ifdef DEBUG
type ValidTransition t = (Eq t, Ord t, Show t, NFData t, HasCallStack)
type ValidTransitionNCS t = (Eq t, Ord t, NFData t, Show t)
#else
type ValidTransition t = (Eq t, Ord t, NFData t)
type ValidTransitionNCS t = (Eq t, Ord t, NFData t)
#endif

data CompactNodeShowSettings a = CompactNodeShowSettings
  { -- | Length of NIDs to show (e.g. 4 renders "000000000001" as "0001")
    nidLength :: Int,
    -- | Whether to include an @ sign before the NID
    showNidAtSign :: Bool,
    -- | Whether to show incoming connects
    showIncoming :: Bool,
    -- | Whether to show the augmentation; if so provide a name and way to show
    -- it. The function returns a list of properties to display.
    showAugmentation :: Maybe (Maybe Text, a -> [(Maybe Text, Text)])
  }

instance Contravariant CompactNodeShowSettings where
  contramap f (CompactNodeShowSettings {..}) =
    CompactNodeShowSettings
      { nidLength = nidLength,
        showNidAtSign = showNidAtSign,
        showIncoming = showIncoming,
        showAugmentation = fmap (fmap (. f)) showAugmentation
      }

defaultCompactNodeShowSettings :: CompactNodeShowSettings a
defaultCompactNodeShowSettings =
  CompactNodeShowSettings
    { nidLength = maxBound,
      showNidAtSign = True,
      showIncoming = False,
      showAugmentation = Nothing
    }

class CompactNodeShow n where
  type Augmentation n
  minimumNidLength :: CompactNodeShowSettings a -> n -> Int
  nshowSettings :: CompactNodeShowSettings (Augmentation n) -> n -> Text

instance (CompactNodeShow a) => CompactNodeShow (Maybe a) where
  type Augmentation (Maybe a) = Augmentation a
  minimumNidLength settings = \case
    Nothing -> 0
    Just x -> minimumNidLength settings x
  nshowSettings settings = \case
    Nothing -> "Nothing"
    Just x -> "Just " <> nshowSettings settings x

class ShowableAugmentation a where
  augmentationProperties :: a -> [(Maybe Text, Text)]

instance ShowableAugmentation Void where
  augmentationProperties = const mempty

instance ShowableAugmentation () where
  augmentationProperties = const mempty

instance (ShowableAugmentation a) => ShowableAugmentation (Maybe a) where
  augmentationProperties = \case
    Nothing -> mempty
    Just x -> augmentationProperties x

newtype UnshownAugmentation a = UnshownAugmentation a

instance ShowableAugmentation (UnshownAugmentation a) where
  augmentationProperties = const mempty

withoutShowingAugmentations :: forall a b. ((ShowableAugmentation a) => b) -> b
withoutShowingAugmentations x = x \\ e
  where
    e =
      mapDict
        (unsafeUnderive UnshownAugmentation)
        (Dict :: Dict (ShowableAugmentation (UnshownAugmentation a)))

nshowWith ::
  (CompactNodeShow n) =>
  (CompactNodeShowSettings (Augmentation n) -> CompactNodeShowSettings (Augmentation n)) ->
  n ->
  Text
nshowWith fsettings n =
  let minNidLength = minimumNidLength defaultCompactNodeShowSettings n
      settings =
        defaultCompactNodeShowSettings
          { nidLength = minNidLength
          }
   in nshowSettings (fsettings settings) n

nshow ::
  (CompactNodeShow n) => n -> Text
nshow = nshowWith (\x -> x {showAugmentation = Nothing})

snshow :: (CompactNodeShow n) => n -> String
snshow = unpack . nshow

-- | Default implementation of show for things that implement CompactNodeShow
nshowDefault ::
  forall a n.
  (ShowableAugmentation a, CompactNodeShow n, Augmentation n ~ a) =>
  n ->
  Text
nshowDefault =
  nshowWith \x ->
    x
      { showAugmentation =
          Just (Nothing, augmentationProperties @a)
      }
