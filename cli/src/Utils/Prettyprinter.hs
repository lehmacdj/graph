module Utils.Prettyprinter
  ( beside,
    HorizontalWidth (..),
    module X,
    test_examples,
  )
where

import MyPrelude
import Prettyprinter as X
import Prettyprinter.Internal qualified as PP
import Prettyprinter.Render.Terminal as X
import Utils.Testing

data HorizontalWidth = Fixed Int | Weighted Double
  deriving (Show, Eq)

-- | Reconstruct a Doc from a SimpleDocStream
-- This allows us to work with laid-out documents while preserving annotations
streamToDoc :: PP.SimpleDocStream ann -> Doc ann
streamToDoc = go []
  where
    go :: [ann] -> PP.SimpleDocStream ann -> Doc ann
    go _ PP.SFail = emptyDoc
    go _ PP.SEmpty = emptyDoc
    go annStack (PP.SChar c rest) = pretty c <> go annStack rest
    go annStack (PP.SText _ txt rest) = pretty txt <> go annStack rest
    go annStack (PP.SLine i rest) = hardline <> pretty (pack (replicate i ' ' :: [Char]) :: Text) <> go annStack rest
    go annStack (PP.SAnnPush ann rest) = annotate ann (goUntilPop (ann : annStack) rest)
    go annStack (PP.SAnnPop rest) = go annStack rest

    -- Consume content until we hit the matching SAnnPop
    goUntilPop :: [ann] -> PP.SimpleDocStream ann -> Doc ann
    goUntilPop _ PP.SFail = emptyDoc
    goUntilPop _ PP.SEmpty = emptyDoc
    goUntilPop annStack (PP.SChar c rest) = pretty c <> goUntilPop annStack rest
    goUntilPop annStack (PP.SText _ txt rest) = pretty txt <> goUntilPop annStack rest
    goUntilPop annStack (PP.SLine i rest) = hardline <> pretty (pack (replicate i ' ' :: [Char]) :: Text) <> goUntilPop annStack rest
    goUntilPop annStack (PP.SAnnPush ann rest) = annotate ann (goUntilPop (ann : annStack) rest)
    goUntilPop (_:restStack) (PP.SAnnPop rest) = go restStack rest
    goUntilPop [] (PP.SAnnPop rest) = go [] rest  -- Shouldn't happen, but handle gracefully

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
-- This implementation preserves annotations by working with SimpleDocStream
renderMultiColumn :: [(Int, Doc ann)] -> Doc ann
renderMultiColumn [] = emptyDoc
renderMultiColumn items =
  let -- Layout each document at its allocated width
      layouted = [(w, layoutPretty (LayoutOptions (AvailablePerLine w 1.0)) doc) | (w, doc) <- items]

      -- Split each laid-out stream into lines (preserving annotations)
      lineGroups = [(w, splitStreamLines stream) | (w, stream) <- layouted]

      -- Find maximum line count
      maxLines = if null lineGroups then 0 else maximumEx ([length ls | (_, ls) <- lineGroups] ++ [0])

      -- Pad each column to maxLines with empty streams
      paddedGroups = [(w, padLines maxLines ls) | (w, ls) <- lineGroups]

      -- Combine lines horizontally (preserving annotations)
      -- Don't pad the last column
      combinedLines =
        [ mconcat $ zipWith
            (\isLast (w, ls) -> streamToDoc $ (if isLast then id else padStreamToWidth w) (fromMaybe PP.SEmpty $ index ls i))
            (if null paddedGroups then [] else replicate (length paddedGroups - 1) False ++ [True])
            paddedGroups
          | i <- [0 .. maxLines - 1]
        ]
   in vsep combinedLines
  where
    -- Split a SimpleDocStream into lines at SLine boundaries
    splitStreamLines :: PP.SimpleDocStream ann -> [PP.SimpleDocStream ann]
    splitStreamLines = go
      where
        go PP.SFail = []
        go PP.SEmpty = [PP.SEmpty]
        go (PP.SChar c rest) = consToFirstLine (PP.SChar c PP.SEmpty) (go rest)
        go (PP.SText len txt rest) = consToFirstLine (PP.SText len txt PP.SEmpty) (go rest)
        go (PP.SLine _ rest) = PP.SEmpty : go rest  -- New line starts here, discard indentation
        go (PP.SAnnPush ann rest) =
          case go rest of
            [] -> [PP.SAnnPush ann PP.SEmpty]
            (firstLine : otherLines) -> PP.SAnnPush ann firstLine : otherLines
        go (PP.SAnnPop rest) = consToFirstLine (PP.SAnnPop PP.SEmpty) (go rest)

        consToFirstLine :: PP.SimpleDocStream ann -> [PP.SimpleDocStream ann] -> [PP.SimpleDocStream ann]
        consToFirstLine s [] = [s]
        consToFirstLine s (firstLine : rest) = appendStream s firstLine : rest

    -- Append two SimpleDocStreams
    appendStream :: PP.SimpleDocStream ann -> PP.SimpleDocStream ann -> PP.SimpleDocStream ann
    appendStream PP.SFail r = r
    appendStream PP.SEmpty r = r
    appendStream (PP.SChar c rest) r = PP.SChar c (appendStream rest r)
    appendStream (PP.SText len txt rest) r = PP.SText len txt (appendStream rest r)
    appendStream (PP.SLine i rest) r = PP.SLine i (appendStream rest r)
    appendStream (PP.SAnnPush ann rest) r = PP.SAnnPush ann (appendStream rest r)
    appendStream (PP.SAnnPop rest) r = PP.SAnnPop (appendStream rest r)

    padLines :: Int -> [PP.SimpleDocStream ann] -> [PP.SimpleDocStream ann]
    padLines n ls = ls ++ replicate (n - length ls) PP.SEmpty

    -- Pad a stream to a specific width by measuring and adding spaces
    padStreamToWidth :: Int -> PP.SimpleDocStream ann -> PP.SimpleDocStream ann
    padStreamToWidth targetWidth stream =
      let currentWidth = streamWidth stream
          paddingNeeded = max 0 (targetWidth - currentWidth)
       in if paddingNeeded > 0
            then appendStream stream (PP.SText paddingNeeded (pack (replicate paddingNeeded ' ')) PP.SEmpty)
            else stream

    -- Calculate the display width of a SimpleDocStream
    streamWidth :: PP.SimpleDocStream ann -> Int
    streamWidth = go
      where
        go PP.SFail = 0
        go PP.SEmpty = 0
        go (PP.SChar _ rest) = 1 + go rest
        go (PP.SText len _ rest) = len + go rest
        go (PP.SLine _ rest) = 0 + go rest  -- Lines don't contribute to width
        go (PP.SAnnPush _ rest) = go rest   -- Annotations don't contribute to width
        go (PP.SAnnPop rest) = go rest

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

-- Example with annotations (colors) to demonstrate annotation preservation
exampleWithAnnotations :: Doc AnsiStyle
exampleWithAnnotations =
  beside
    [ (annotate (color Red) $ fillSep $ map pretty $ words ("Red column text" :: Text), Weighted 1.0),
      (annotate (color Green) $ fillSep $ map pretty $ words ("Green column text" :: Text), Weighted 1.0),
      (annotate (color Blue) $ fillSep $ map pretty $ words ("Blue column" :: Text), Weighted 1.0)
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
          layoutPretty (LayoutOptions (AvailablePerLine 60 1.0)) example1,
      -- Test annotation preservation by rendering with ANSI colors
      goldenTestBinary "example-with-annotations-80width" $
        encodeUtf8 $
          renderStrict $
            layoutPretty (LayoutOptions (AvailablePerLine 80 1.0)) exampleWithAnnotations
    ]
