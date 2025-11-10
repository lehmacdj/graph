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
    renderStyle Transition = mempty
    renderStyle TransitionEmphasized = bold
    renderStyle NIDStyle = colorDull Blue

data Style
  = -- | style of text for tags
    TagBackground
  | -- | style of text for tag content
    TagEmphasized
  | -- | style for times
    Time
  | TransitionEmphasized
  | Transition
  | -- | nid
    NIDStyle

docTransitionsToNid :: [Text] -> NID -> Doc Style
docTransitionsToNid [] nid = docNid nid
docTransitionsToNid transitions nid =
  docTransitions transitions <> " " <> docNid nid

docTransitions :: [Text] -> Doc Style
docTransitions = annotate Transition . go
  where
    go [] = mempty
    go [x] = annotate TransitionEmphasized (docTransition x)
    go (x : xs) =
      docTransition x <> "/" <> go xs

docTransition :: Text -> Doc a
docTransition t
  | all isIdentChar t && not (null t) = pretty t
  | otherwise = dquotes (pretty t)

docNid :: NID -> Doc Style
docNid nid = annotate NIDStyle $ "@" <> pretty (nidRepresentation nid)

docNodeListing ::
  TimeZone ->
  UTCTime ->
  -- | transitions leading up to this node (see 'leftmostConnects')
  [Text] ->
  -- | Node metadata + augmentations to doc
  Node Text PreviewAugmentation ->
  Doc Style
docNodeListing timeZone currentTime transitions node =
  group $
    sepNonEmpty
      [ transitionsToNid,
        sepNonEmpty [tags, timestamp]
      ]
  where
    transitionsToNid = docTransitionsToNid transitions node.nid
    timestamp = docTimestamp timeZone currentTime node.augmentation.timestamps
    tags = docTags node.augmentation.tags

docTimestamp :: TimeZone -> UTCTime -> Set UTCTime -> Doc Style
docTimestamp timeZone currentTime timestamps =
  case maximum <$> fromNullable timestamps of
    Nothing -> mempty
    Just t -> annotate Time . pretty $ formatMaybeRelativeTimestamp timeZone currentTime t

docTag :: Text -> Doc Style
docTag t = annotate TagEmphasized ("#" <> docTransition t)

docTags :: Set Text -> Doc Style
docTags tags =
  annotate TagBackground $
    fillSep
      ( punctuate
          comma
          (docTag <$> toList tags)
      )
