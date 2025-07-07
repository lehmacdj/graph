{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE CPP #-}

module Models.Common where

import Data.Constraint
import Data.Constraint.Unsafe
import Data.Functor.Classes
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
    showAugmentation :: Maybe (Maybe Text, a -> Text)
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
      showIncoming = True,
      showAugmentation = Nothing
    }

class CompactNodeShow n where
  type Augmentation n
  minimumNidLength :: CompactNodeShowSettings a -> n -> Int
  nshowSettings :: CompactNodeShowSettings (Augmentation n) -> n -> Text

class ShowableAugmentation a where
  augmentationLabel :: Maybe Text
  defaultShowAugmentation :: a -> Text
  shouldShowStandaloneAugmentation :: Bool

instance ShowableAugmentation Void where
  augmentationLabel = Just "void"
  defaultShowAugmentation = const "!"
  shouldShowStandaloneAugmentation = False

instance ShowableAugmentation () where
  augmentationLabel = Just "unit"
  defaultShowAugmentation = const "()"
  shouldShowStandaloneAugmentation = False

instance (ShowableAugmentation a) => ShowableAugmentation (Maybe a) where
  augmentationLabel = augmentationLabel @a
  defaultShowAugmentation maybeX =
    pack $
      liftShowsPrec
        (\_ x -> (unpack (defaultShowAugmentation x :: Text) ++))
        (error "shouldn't need showList")
        0
        maybeX
        ""
  shouldShowStandaloneAugmentation = True

newtype UnshownAugmentation a = UnshownAugmentation a

instance ShowableAugmentation (UnshownAugmentation a) where
  augmentationLabel = Just "unshown"
  defaultShowAugmentation = const ""
  shouldShowStandaloneAugmentation = False

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
          justIfTrue
            (shouldShowStandaloneAugmentation @a)
            (augmentationLabel @a, defaultShowAugmentation @a)
      }
