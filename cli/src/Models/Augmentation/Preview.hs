module Models.Augmentation.Preview where

import Models.Augmentation.Bundled
import Models.Augmentation.Tags
import Models.Augmentation.Timestamps
import Models.NID
import Models.Node
import MyPrelude hiding (group)
import Utils.Parsing (isIdentChar)
import Utils.Prettyprinter

type PreviewAugmentation = Bundled [Tags, Timestamps] -- , FileType]

renderNodeListing :: Int -> Doc Style -> Text
renderNodeListing w =
  renderStrict
    . reAnnotateS renderStyle
    . layoutPretty (LayoutOptions (AvailablePerLine w 1.0))
  where
    renderStyle :: Style -> AnsiStyle
    renderStyle TagBackground = mempty
    renderStyle TagEmphasized = colorDull Magenta <> bold
    renderStyle Time = colorDull Yellow
    renderStyle Transition = bold
    renderStyle NIDStyle = colorDull Blue

data Style
  = -- | style of text for tags
    TagBackground
  | -- | style of text for tag content
    TagEmphasized
  | -- | style for times
    Time
  | -- | transition
    Transition
  | -- | nid
    NIDStyle

docTransition :: Maybe Text -> Doc a
docTransition Nothing = "<here>"
docTransition (Just t)
  | all isIdentChar t && not (null t) = pretty t
  | otherwise = dquotes (pretty t)

docNid :: NID -> Doc a
docNid nid = "@" <> pretty (nidRepresentation nid)

docNodeListing ::
  TimeZone ->
  UTCTime ->
  -- | Final transition; there might not be one (e.g. `ls @`)
  Maybe Text ->
  -- | Node metadata + augmentations to doc
  Node Text PreviewAugmentation ->
  Doc Style
docNodeListing timeZone currentTime finalTransition node =
  group $
    vsepNonEmpty
      [ transition <+> nid,
        sepNonEmpty [tags, timestamp]
      ]
      `flatAlt` hsepNonEmpty [transition, tags, timestamp, nid]
  where
    transition = annotate Transition $ docTransition finalTransition
    nid = annotate NIDStyle $ docNid node.nid
    timestamp =
      annotate Time $
        docTimestamp timeZone currentTime node.augmentation.timestamps
    tags = docTags node.augmentation.tags

docTimestamp :: TimeZone -> UTCTime -> Set UTCTime -> Doc Style
docTimestamp timeZone currentTime timestamps =
  case maximum <$> fromNullable timestamps of
    Nothing -> mempty
    Just t -> pretty $ formatMaybeRelativeTimestamp timeZone currentTime t

docTag :: Text -> Doc Style
docTag t =
  annotate TagEmphasized ("#" <> docTransition (Just t))

docTags :: Set Text -> Doc Style
docTags tags =
  annotate TagBackground $
    fillSep
      ( punctuate
          comma
          (docTag <$> toList tags)
      )
