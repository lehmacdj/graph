{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE CPP #-}

module Models.Common where

import MyPrelude

#ifdef DEBUG
type ValidTransition t = (Eq t, Ord t, Show t, HasCallStack)
#else
type ValidTransition t = (Eq t, Ord t)
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

class ShowableAugmentation a where
  defaultShowAugmentation :: Maybe (Text, a -> Text)

instance {-# OVERLAPPABLE #-} ShowableAugmentation a where
  defaultShowAugmentation = Nothing

defaultCompactNodeShowSettings ::
  ShowableAugmentation a => CompactNodeShowSettings a
defaultCompactNodeShowSettings =
  CompactNodeShowSettings
    { nidLength = maxBound,
      showNidAtSign = True,
      showIncoming = False,
      showAugmentation = defaultShowAugmentation
    }

class CompactNodeShow n a where
  minimumNidLength :: CompactNodeShowSettings a -> n -> Int
  compactNodeShow :: CompactNodeShowSettings a -> n -> Text

compactNodeShowDefault :: forall n a. (CompactNodeShow n a, ShowableAugmentation a) => n -> Text
compactNodeShowDefault n =
  let defSettings = defaultCompactNodeShowSettings
      settings = defSettings {nidLength = minimumNidLength @n @a defSettings n}
   in compactNodeShow @n @a settings n
