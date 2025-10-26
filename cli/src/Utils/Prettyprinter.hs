module Utils.Prettyprinter
  ( beside,
    HorizontalWidth (..),
    module X,
    test_examples,
  )
where

import MyPrelude
import Prettyprinter as X
import Prettyprinter.Render.Terminal as X
import Utils.Testing

data HorizontalWidth = Fixed Int | Weighted Double
  deriving (Show, Eq)

-- Layout multiple documents side-by-side with flexible width allocation
beside :: [(Doc ann, HorizontalWidth)] -> Doc ann
beside [] = emptyDoc
beside items =
  column $ \startCol ->
    pageWidth $ \case
      AvailablePerLine lineWidth ribbonFrac ->
        let totalWidth = floor (fromIntegral lineWidth * ribbonFrac)
            availableWidth = totalWidth - startCol
            widths = calculateWidths availableWidth items
         in if availableWidth < sum (map fst widths)
              then vsep (map fst items) -- Stack vertically if too narrow
              else renderMultiColumn widths
      Unbounded ->
        -- With unbounded width, measure each document precisely
        renderMultiColumnUnbounded startCol items

-- Calculate actual widths for each column (for bounded case)
calculateWidths :: Int -> [(Doc ann, HorizontalWidth)] -> [(Int, Doc ann)]
calculateWidths totalWidth items =
  let -- First pass: allocate fixed widths
      fixedTotal = sum [n | (_, Fixed n) <- items]
      remaining = totalWidth - fixedTotal

      -- Second pass: distribute remaining width by weights
      weights = [w | (_, Weighted w) <- items]
      totalWeight = sum weights

      -- Assign widths
      assign (doc, Fixed n) = (n, doc)
      assign (doc, Weighted w) =
        if totalWeight > 0
          then (floor (fromIntegral remaining * w / totalWeight), doc)
          else (0, doc)
   in map assign items

-- Render documents side-by-side with specified widths (bounded case)
--
-- NOTE: This implementation does not preserve annotations. The documents are
-- rendered to Text and then reconstructed. To preserve annotations, we would
-- need to work with SimpleDocStream directly and implement line-splitting at
-- that level, which is significantly more complex.
renderMultiColumn :: [(Int, Doc ann)] -> Doc ann
renderMultiColumn [] = emptyDoc
renderMultiColumn items =
  let -- Render each document at its allocated width
      rendered = [(w, renderAtWidth w doc) | (w, doc) <- items]

      -- Split into lines
      lineGroups = [(w, lines txt) | (w, txt) <- rendered]

      -- Find maximum line count
      maxLines = if null lineGroups then 0 else maximumEx ([length ls | (_, ls) <- lineGroups] ++ [0])

      -- Pad each column to maxLines
      paddedGroups = [(w, padLines maxLines ls) | (w, ls) <- lineGroups]

      -- Combine lines horizontally
      combinedLines =
        [ concat [leftPad w (fromMaybe "" $ index ls i) | (w, ls) <- paddedGroups]
          | i <- [0 .. maxLines - 1]
        ]
   in vsep (map pretty combinedLines)
  where
    renderAtWidth :: Int -> Doc ann -> Text
    renderAtWidth w doc =
      renderStrict $ layoutPretty (LayoutOptions (AvailablePerLine w 1.0)) $ unAnnotate doc

    padLines n ls = ls ++ replicate (n - length ls) ""

    leftPad :: Int -> Text -> Text
    leftPad w txt =
      let len = length txt
          padding = if w > len then pack (replicate (w - len) ' ') else ""
       in txt <> padding

-- Render unbounded case: measure each document and lay out precisely
renderMultiColumnUnbounded :: Int -> [(Doc ann, HorizontalWidth)] -> Doc ann
renderMultiColumnUnbounded startCol items =
  buildColumns startCol items []
  where
    -- Build columns left to right, measuring as we go
    buildColumns :: Int -> [(Doc ann, HorizontalWidth)] -> [(Int, Doc ann)] -> Doc ann
    buildColumns _ [] acc = renderMultiColumn (reverse acc)
    buildColumns currentCol ((doc, hw) : rest) acc =
      case hw of
        Fixed n ->
          -- Use fixed width directly
          buildColumns (currentCol + n) rest ((n, doc) : acc)
        Weighted _ ->
          -- Measure the document's natural width
          width doc $ \w ->
            buildColumns (currentCol + w) rest ((w, doc) : acc)

-- Examples
example1 :: Doc ann
example1 =
  beside
    [ (fillSep $ map pretty $ words ("This is the first column with some text" :: Text), Weighted 1.0),
      (fillSep $ map pretty $ words ("This is the second column with more text" :: Text), Weighted 1.0),
      (fillSep $ map pretty $ words ("And a third column" :: Text), Weighted 1.0)
    ]

example2 :: Doc ann
example2 =
  beside
    [ (fillSep $ map pretty $ words ("Fixed 20 char column that will wrap at 20" :: Text), Fixed 20),
      (fillSep $ map pretty $ words ("This column gets remaining space and will wrap accordingly" :: Text), Weighted 1.0)
    ]

example3 :: Doc ann
example3 =
  beside
    [ (fillSep $ map pretty $ words ("Small weight column" :: Text), Weighted 1.0),
      (fillSep $ map pretty $ words ("Large weight column gets more space" :: Text), Weighted 2.0),
      (fillSep $ map pretty $ words ("Another small" :: Text), Weighted 1.0)
    ]

example4 :: Doc ann
example4 =
  beside
    [ (pretty ("Line 1\nLine 2\nLine 3" :: Text), Fixed 15),
      (pretty ("A\nB" :: Text), Fixed 10),
      (pretty ("Just one line" :: Text), Weighted 1.0)
    ]

-- Example showing unbounded width with natural sizing
exampleUnbounded :: Doc ann
exampleUnbounded =
  beside
    [ (pretty ("Short" :: Text), Weighted 1.0),
      (pretty ("A bit longer text" :: Text), Weighted 1.0),
      (pretty ("X" :: Text), Weighted 1.0)
    ]

-- Example with nesting and column awareness
exampleNested :: Doc ann
exampleNested =
  pretty ("Prefix: " :: Text)
    <> beside
      [ (pretty ("A" :: Text), Weighted 1.0),
        (pretty ("B" :: Text), Weighted 1.0),
        (pretty ("C" :: Text), Weighted 1.0)
      ]

-- Example showing beside correctly accounting for left content
exampleWithLeftContent :: Doc ann
exampleWithLeftContent =
  pretty ("Start: " :: Text)
    <> beside
      [ (fillSep $ map pretty $ words ("First column text" :: Text), Weighted 1.0),
        (fillSep $ map pretty $ words ("Second column text" :: Text), Weighted 1.0)
      ]

-- Tests
test_examples :: TestTree
test_examples =
  testGroup
    "Prettyprinter examples"
    [ goldenTest "example1-80width" $
        renderStrict $
          layoutPretty (LayoutOptions (AvailablePerLine 80 1.0)) example1,
      goldenTest "example2-80width" $
        renderStrict $
          layoutPretty (LayoutOptions (AvailablePerLine 80 1.0)) example2,
      goldenTest "example3-90width" $
        renderStrict $
          layoutPretty (LayoutOptions (AvailablePerLine 90 1.0)) example3,
      goldenTest "example4-60width" $
        renderStrict $
          layoutPretty (LayoutOptions (AvailablePerLine 60 1.0)) example4,
      goldenTest "example-unbounded" $
        renderStrict $
          layoutPretty (LayoutOptions Unbounded) exampleUnbounded,
      goldenTest "example-nested-80width" $
        renderStrict $
          layoutPretty (LayoutOptions (AvailablePerLine 80 1.0)) exampleNested,
      goldenTest "example-with-left-content-80width" $
        renderStrict $
          layoutPretty (LayoutOptions (AvailablePerLine 80 1.0)) exampleWithLeftContent,
      goldenTest "example1-60width-reflowing" $
        renderStrict $
          layoutPretty (LayoutOptions (AvailablePerLine 60 1.0)) example1
    ]
