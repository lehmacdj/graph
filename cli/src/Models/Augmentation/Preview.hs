module Models.Augmentation.Preview where

import Models.Augmentation.Bundled
import Models.Augmentation.FileType
import Models.Augmentation.Tags
import Models.Augmentation.Timestamps
import Models.Node
import MyPrelude
import Prettyprinter

type PreviewAugmentation = Bundled [Tags, Timestamps, FileType]

data Style
  = -- | style of hashtag shown in front of tags
    Hashtag
  | -- | style of text for tags
    TagText
  | -- | style for separators, e.g. commas between tags
    Separator

renderNodeListing ::
  -- | Transition
  Text ->
  -- | Node metadata + augmentations to render
  Node Text PreviewAugmentation ->
  Doc Style
renderNodeListing = undefined

renderTimestamp :: TimeZone -> UTCTime -> Set UTCTime -> Doc Style
renderTimestamp timeZone currentTime timestamps =
  case maximum <$> fromNullable timestamps of
    Nothing -> mempty
    Just t -> pretty $ formatMaybeRelativeTimestamp timeZone currentTime t

renderTag :: Text -> Doc Style
renderTag t = annotate Hashtag "#" <> annotate TagText (pretty t)

renderTags :: Set Text -> Doc Style
renderTags tags =
  fillSep
    ( punctuate
        (annotate Separator comma)
        (renderTag <$> toList tags)
    )
