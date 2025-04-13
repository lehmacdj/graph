{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE CPP #-}

module Models.Common where

import Data.Constraint
import Data.Constraint.Unsafe
import MyPrelude hiding ((\\))

#ifdef DEBUG
type ValidTransition t = (Eq t, Ord t, Show t, HasCallStack)
type ValidTransitionNCS t = (Eq t, Ord t, Show t)
#else
type ValidTransition t = (Eq t, Ord t)
type ValidTransitionNCS t = (Eq t, Ord t)
#endif

data CompactNodeShowSettings a = CompactNodeShowSettings
  { -- | Length of NIDs to show (e.g. 4 renders "000000000001" as "0001")
    nidLength :: Int,
    -- | Whether to include an @ sign before the NID
    showNidAtSign :: Bool,
    -- | Whether to show incoming connects
    showIncoming :: Bool,
    -- | Whether to show the augmentation; if so provide a name and way to show
    -- it
    showAugmentation :: Maybe (Text, a -> Text)
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

class ShowableAugmentation a where
  defaultShowAugmentation :: Maybe (Text, a -> Text)

instance ShowableAugmentation Void where
  defaultShowAugmentation = Nothing

instance ShowableAugmentation () where
  defaultShowAugmentation = Nothing

newtype UnshownAugmentation a = UnshownAugmentation a

instance ShowableAugmentation (UnshownAugmentation a) where
  defaultShowAugmentation = Nothing

withoutShowingAugmentations :: forall a b. ((ShowableAugmentation a) => b) -> b
withoutShowingAugmentations x = x \\ e
  where
    e =
      mapDict
        (unsafeUnderive UnshownAugmentation)
        (Dict :: Dict (ShowableAugmentation (UnshownAugmentation a)))

nshowWith ::
  (CompactNodeShow n) => Maybe (Text, Augmentation n -> Text) -> n -> Text
nshowWith showAugmentation n =
  let minNidLength = minimumNidLength defaultCompactNodeShowSettings n
      settings =
        defaultCompactNodeShowSettings
          { nidLength = minNidLength,
            showAugmentation
          }
   in nshowSettings settings n

nshow ::
  (CompactNodeShow n) => n -> Text
nshow = nshowWith Nothing

snshow :: (CompactNodeShow n) => n -> String
snshow = unpack . nshow

-- | Default implementation of show for things that implement CompactNodeShow
nshowDefault ::
  (ShowableAugmentation a, CompactNodeShow n, Augmentation n ~ a) => n -> Text
nshowDefault = nshowWith defaultShowAugmentation
