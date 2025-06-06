module Models.MiscSpec where

import TestPrelude
import Models.Common
import Models.Node
import Models.NID

data Unshowable = Unshowable
  deriving (Eq)

data Showable = Showable
  deriving (Eq)

instance ShowableAugmentation Showable where
  defaultShowAugmentation = Just ("showable", const "shown")

spec_withoutShowingAugmentations :: Spec
spec_withoutShowingAugmentations = do
  let showNode :: forall t a. (Show t, Ord t, Eq a) => Node t a -> Text
      showNode x = withoutShowingAugmentations @a (tshow x)
  it "doesn't segfault" do
    showNode ((emptyNode (smallNID 0) :: Node Text ()) $> Unshowable)
      `shouldBe` "@0{out=[]}"
  it "doesn't show showable augmentations" do
    -- there probably are some edge cases where the augmentation will show
    -- due to incoherent instances, but since this is mostly for debugging
    -- purposes, I'm not too worried about it
    showNode ((emptyNode (smallNID 0) :: Node Text ()) $> Showable)
      `shouldBe` "@0{out=[]}"
