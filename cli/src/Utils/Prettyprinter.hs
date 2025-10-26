module Utils.Prettyprinter
  ( columns,
    equalColumns,
    HorizontalWidth (..),
    module X,
    test_goldenRenders,
    spec_streamToDoc,
  )
where

import MyPrelude hiding (SChar)
import Prettyprinter as X
import Prettyprinter.Render.Terminal as X
import Utils.Testing

data HorizontalWidth = Fixed Int | Weighted Double
  deriving (Show, Eq)

-- | Reconstruct a Doc from a SimpleDocStream
-- This allows us to work with laid-out documents while preserving annotations
-- Handles arbitrary SimpleDocStream structures including deeply nested annotations
streamToDoc :: SimpleDocStream ann -> Doc ann
streamToDoc = go
  where
    go :: (HasCallStack) => SimpleDocStream ann -> Doc ann
    go SFail = emptyDoc
    go SEmpty = emptyDoc
    go (SChar c rest) = pretty c <> go rest
    go (SText _ txt rest) = pretty txt <> go rest
    go (SLine i rest) =
      hardline
        <> pretty (pack (replicate i ' ' :: [Char]) :: Text)
        <> go rest
    go (SAnnPush ann rest) =
      -- Extract content until matching pop, handling nested annotations
      let (content, after) = extractUntilPop rest
       in annotate ann (go content) <> go after
    go (SAnnPop _rest) =
      -- This shouldn't happen in well-formed streams, but we handle it gracefully
      -- by treating it as a no-op
      emptyDoc

    -- Extract content until the matching SAnnPop
    -- Returns (contentStream, remainingStream)
    -- Handles nested push/pop pairs correctly
    extractUntilPop :: SimpleDocStream ann -> (SimpleDocStream ann, SimpleDocStream ann)
    extractUntilPop stream = extract 1 stream
      where
        -- depth tracks nesting level: when it reaches 0, we've found our matching pop
        extract :: Int -> SimpleDocStream ann -> (SimpleDocStream ann, SimpleDocStream ann)
        extract _ SFail = (SFail, SEmpty)
        extract 0 s = (SEmpty, s) -- Found matching pop
        extract _ SEmpty = (SEmpty, SEmpty)
        extract depth (SChar c rest) =
          let (content, after) = extract depth rest
           in (SChar c content, after)
        extract depth (SText len txt rest) =
          let (content, after) = extract depth rest
           in (SText len txt content, after)
        extract depth (SLine i rest) =
          let (content, after) = extract depth rest
           in (SLine i content, after)
        extract depth (SAnnPush ann rest) =
          -- Nested push: increase depth
          let (content, after) = extract (depth + 1) rest
           in (SAnnPush ann content, after)
        extract depth (SAnnPop rest)
          | depth == 1 = (SEmpty, rest) -- Found our matching pop
          | otherwise =
              -- This pop matches a nested push
              let (content, after) = extract (depth - 1) rest
               in (SAnnPop content, after)

-- Layout multiple documents side-by-side with flexible width allocation
columns :: [(Doc ann, HorizontalWidth)] -> Doc ann
columns [] = emptyDoc
columns items =
  align . column $ \startCol ->
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

equalColumns :: Int -> [Doc ann] -> Doc ann
equalColumns spacing =
  columns
    . intersperse (fill spacing emptyDoc, Fixed spacing)
    . map (,Weighted 1.0)

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
        [ mconcat $
            zipWith
              (\isLast (w, ls) -> streamToDoc $ (if isLast then id else padStreamToWidth w) (fromMaybe SEmpty $ index ls i))
              (if null paddedGroups then [] else replicate (length paddedGroups - 1) False ++ [True])
              paddedGroups
          | i <- [0 .. maxLines - 1]
        ]
   in vsep combinedLines
  where
    -- Split a SimpleDocStream into lines at SLine boundaries
    -- Must handle nested annotations by tracking active annotation stack
    splitStreamLines :: SimpleDocStream ann -> [SimpleDocStream ann]
    splitStreamLines = go []
      where
        -- Track annotation stack and emit balanced lines
        go :: [ann] -> SimpleDocStream ann -> [SimpleDocStream ann]
        go _ SFail = []
        go annStack SEmpty = [wrapWithAnnotations annStack SEmpty]
        go annStack (SChar c rest) = consToFirstLine (SChar c SEmpty) (go annStack rest)
        go annStack (SText len txt rest) = consToFirstLine (SText len txt SEmpty) (go annStack rest)
        go annStack (SLine _ rest) =
          -- Close current line, start new line with same annotations
          case go annStack rest of
            [] -> [wrapWithAnnotations annStack SEmpty]
            (nextLine : otherLines) ->
              wrapWithAnnotations annStack SEmpty : nextLine : otherLines
        go annStack (SAnnPush ann rest) =
          -- Add to stack and continue (first line will be wrapped by caller)
          go (ann : annStack) rest
        go annStack (SAnnPop rest) =
          -- Remove from stack
          let newStack = case annStack of
                [] -> []
                (_ : xs) -> xs
           in go newStack rest

        consToFirstLine :: SimpleDocStream ann -> [SimpleDocStream ann] -> [SimpleDocStream ann]
        consToFirstLine s [] = [s]
        consToFirstLine s (firstLine : rest) = appendStream s firstLine : rest

        -- Wrap a stream with all active annotations
        wrapWithAnnotations :: [ann] -> SimpleDocStream ann -> SimpleDocStream ann
        wrapWithAnnotations [] s = s
        wrapWithAnnotations anns s =
          let -- Close all annotations after the content (innermost to outermost)
              withCloses = foldl' (\acc _ -> SAnnPop acc) s anns
           in -- Open all annotations before the content (outermost to innermost)
              foldr SAnnPush withCloses (reverse anns)

    -- Append two SimpleDocStreams
    appendStream :: SimpleDocStream ann -> SimpleDocStream ann -> SimpleDocStream ann
    appendStream SFail r = r
    appendStream SEmpty r = r
    appendStream (SChar c rest) r = SChar c (appendStream rest r)
    appendStream (SText len txt rest) r = SText len txt (appendStream rest r)
    appendStream (SLine i rest) r = SLine i (appendStream rest r)
    appendStream (SAnnPush ann rest) r = SAnnPush ann (appendStream rest r)
    appendStream (SAnnPop rest) r = SAnnPop (appendStream rest r)

    padLines :: Int -> [SimpleDocStream ann] -> [SimpleDocStream ann]
    padLines n ls = ls ++ replicate (n - length ls) SEmpty

    -- Pad a stream to a specific width by measuring and adding spaces
    -- Must insert padding BEFORE any trailing SAnnPop to maintain proper nesting
    padStreamToWidth :: Int -> SimpleDocStream ann -> SimpleDocStream ann
    padStreamToWidth targetWidth stream =
      let currentWidth = streamWidth stream
          paddingNeeded = max 0 (targetWidth - currentWidth)
       in if paddingNeeded > 0
            then insertPaddingBeforeTrailingPops paddingNeeded stream
            else stream

    -- Insert padding before trailing SAnnPop sequences
    insertPaddingBeforeTrailingPops :: Int -> SimpleDocStream ann -> SimpleDocStream ann
    insertPaddingBeforeTrailingPops padding stream =
      let (pops, rest) = extractTrailingPops stream
       in restoreStream rest (SText padding (pack (replicate padding ' ')) SEmpty) pops
      where
        -- Extract all trailing SAnnPops, return (pops, streamWithoutPops)
        extractTrailingPops :: SimpleDocStream ann -> ([SimpleDocStream ann -> SimpleDocStream ann], SimpleDocStream ann)
        extractTrailingPops SEmpty = ([], SEmpty)
        extractTrailingPops SFail = ([], SFail)
        extractTrailingPops (SAnnPop rest) =
          let (morePops, base) = extractTrailingPops rest
           in (SAnnPop : morePops, base)
        extractTrailingPops other = ([], other)

        -- Reconstruct stream: base + padding + pops
        restoreStream :: SimpleDocStream ann -> SimpleDocStream ann -> [SimpleDocStream ann -> SimpleDocStream ann] -> SimpleDocStream ann
        restoreStream base padding' = foldl' (flip ($)) (appendStream base padding')

    -- Calculate the display width of a SimpleDocStream
    streamWidth :: SimpleDocStream ann -> Int
    streamWidth = go
      where
        go SFail = 0
        go SEmpty = 0
        go (SChar _ rest) = 1 + go rest
        go (SText len _ rest) = len + go rest
        go (SLine _ rest) = 0 + go rest -- Lines don't contribute to width
        go (SAnnPush _ rest) = go rest -- Annotations don't contribute to width
        go (SAnnPop rest) = go rest

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

test_goldenRenders :: [TestTree]
test_goldenRenders =
  [ goldenRenders "columns.wrap-first" [42, 60, 80] $
      columns
        [ ( fillSep . map pretty . words . asText $
              "some text with fill that wraps at small widths",
            Weighted 1.0
          ),
          let t :: Text
              t = "some text on the right"
           in (pretty t, Fixed (length t))
        ],
    goldenRenders "columns.three-equal-columns.no-spacing" [42, 60, 80] $
      equalColumns
        0
        [ fillSep . map pretty . words . asText $
            "some text with fill that wraps at small widths",
          fillSep . map pretty . words . asText $
            "some more text that also wraps",
          fillSep . map pretty . words . asText $
            "third column text that wraps"
        ],
    goldenRenders "columns.three-equal-columns.2-spacing" [42, 60, 80] $
      equalColumns
        2
        [ fillSep . map pretty . words . asText $
            "some text with fill that wraps at small widths",
          fillSep . map pretty . words . asText $
            "some more text that also wraps",
          fillSep . map pretty . words . asText $
            "third column text that wraps"
        ],
    goldenRenders "columns.with-leading-doc-aligned" [40] $
      "start"
        <> columns
          [ ( fillSep . map pretty . words . asText $
                "some text with fill that wraps at small widths",
              Weighted 1.0
            ),
            let t :: Text
                t = "some text on the right"
             in (pretty t, Fixed (length t))
          ],
    goldenRenders "columns.continues-just-after-last-line" [42, 60, 80] $
      columns
        [ ( fillSep . map pretty . words . asText $
              "some text with fill that wraps at small widths",
            Weighted 1.0
          ),
          let t :: Text
              t = "some text on the right"
           in (pretty t, Fixed (length t))
        ]
        <> softline'
        <> ( fillSep . map pretty . words . asText $
               "continued text disrespects the column layout"
           ),
    goldenRenders "columns.nested-columns" [42, 60, 80] $
      equalColumns
        2
        [ equalColumns
            2
            [ fillSep . map pretty . words . asText $
                "foo bar baz that wraps (25% of space)",
              fillSep . map pretty . words . asText $
                "some more text that also wraps (25% of space)"
            ],
          fillSep . map pretty . words . asText $
            "column in outer columns that takes up 50% of space"
        ],
    goldenRenders "columns.preserves-annotations" [42, 60, 80] $
      columns
        [ ( annotate bold
              . fillSep
              . over (ix 1) (annotate (color Red))
              . over (ix 2) (annotate (color Blue))
              . over (ix 3) (annotate (color Black))
              . map pretty
              . words
              . asText
              $ "colored red blue black text followed by normal colored text",
            Weighted 1.0
          ),
          ( annotate (color Green) $
              fillSep . map pretty . words . asText $
                "Green column text that also wraps",
            Weighted 1.0
          )
        ]
  ]
  where
    goldenRenders name widths doc =
      testGroup name . runIdentity $ for widths \w ->
        Identity . goldenTest (name ++ ".width-" ++ show w) . renderStrict $
          layoutPretty
            (LayoutOptions (AvailablePerLine w 1.0))
            doc

-- | Test streamToDoc functionality
-- Tests that streamToDoc correctly reconstructs documents from SimpleDocStream,
-- preserving all annotations and structure
spec_streamToDoc :: Spec
spec_streamToDoc = do
  let testRoundTrip :: String -> Doc AnsiStyle -> Spec
      testRoundTrip = testRoundTripAt 80

      testRoundTripAt :: Int -> String -> Doc AnsiStyle -> Spec
      testRoundTripAt w name doc =
        it name do
          let opts = LayoutOptions (AvailablePerLine w 1.0)
              stream = layoutPretty opts doc
              reconstructed = streamToDoc stream
              stream2 = layoutPretty opts reconstructed
              rendered1 = renderStrict stream
              rendered2 = renderStrict stream2
          rendered1 `shouldBe` rendered2

  describe "direct streamToDoc (not through columns)" do
    describe "basic documents" do
      testRoundTrip "simple text" $
        pretty ("hello" :: Text)
      testRoundTrip "concatenated text" $
        pretty ("hello" :: Text) <> pretty (" world" :: Text)
      testRoundTrip "with line breaks" $
        pretty ("line1" :: Text) <> hardline <> pretty ("line2" :: Text)

    describe "single annotation" do
      testRoundTrip "red text" $
        annotate (color Red) (pretty ("red text" :: Text))
      testRoundTrip "bold text" $
        annotate bold (pretty ("bold text" :: Text))
      testRoundTrip "underlined text" $
        annotate underlined (pretty ("underlined" :: Text))

    describe "annotation nesting depth 2" do
      testRoundTrip "outer with inner bold" $
        annotate (color Red) $
          pretty ("outer " :: Text)
            <> annotate bold (pretty ("bold" :: Text))
            <> pretty (" back" :: Text)
      testRoundTrip "blue underlined" $
        annotate (color Blue) $
          annotate underlined $
            pretty ("blue underlined" :: Text)
      testRoundTrip "color within color" $
        annotate (color Green) $
          pretty ("start " :: Text)
            <> annotate (color Yellow) (pretty ("yellow" :: Text))
            <> pretty (" end" :: Text)

    describe "annotation nesting depth 3" do
      testRoundTrip "triple nested" $
        annotate (color Red) $
          annotate bold $
            annotate underlined $
              pretty ("triple nested" :: Text)
      testRoundTrip "complex nesting with reversion" $
        annotate (color Magenta) $
          pretty ("level1 " :: Text)
            <> annotate
              bold
              ( pretty ("level2 " :: Text)
                  <> annotate underlined (pretty ("level3" :: Text))
                  <> pretty (" back2" :: Text)
              )
            <> pretty (" back1" :: Text)

    describe "annotation nesting depth 4" do
      testRoundTrip "quad nested" $
        annotate (color Red) $
          annotate bold $
            annotate underlined $
              annotate (color Blue) $
                pretty ("quad nested" :: Text)

    describe "annotations with line breaks" do
      testRoundTrip "annotation spanning lines" $
        annotate (color Red) $
          pretty ("line1" :: Text) <> hardline <> pretty ("line2" :: Text)
      testRoundTrip "nested annotation spanning multiple lines" $
        annotate (color Green) $
          annotate bold $
            pretty ("first" :: Text)
              <> hardline
              <> pretty ("second" :: Text)
              <> hardline
              <> pretty ("third" :: Text)

    describe "annotation reversion after pop" do
      testRoundTrip "color reversion" $
        annotate (color Red) $
          pretty ("red " :: Text)
            <> annotate (color Blue) (pretty ("blue" :: Text))
            <> pretty (" red again" :: Text)
      testRoundTrip "multi-level reversion" $
        annotate (color Red) $
          pretty ("R1 " :: Text)
            <> annotate
              (color Green)
              ( pretty ("G1 " :: Text)
                  <> annotate (color Blue) (pretty ("B" :: Text))
                  <> pretty (" G2" :: Text)
              )
            <> pretty (" R2" :: Text)

    describe "complex structures" do
      testRoundTrip "vsep with nested annotations" $
        vsep
          [ annotate (color Red) (pretty ("line 1" :: Text)),
            annotate (color Green) $
              annotate bold (pretty ("line 2" :: Text)),
            annotate (color Blue) $
              pretty ("line 3a " :: Text)
                <> annotate underlined (pretty ("line 3b" :: Text))
          ]

    describe "fillSep with annotations" do
      testRoundTrip "fillSep colored" $
        annotate (color Cyan) $
          fillSep $
            map pretty $
              words ("This is a longer piece of text that will wrap" :: Text)
      testRoundTrip "fillSep with mixed annotations" $
        annotate (color Magenta) $
          fillSep
            [ pretty ("normal" :: Text),
              annotate bold (pretty ("bold" :: Text)),
              pretty ("normal" :: Text),
              annotate underlined (pretty ("underlined" :: Text))
            ]

  describe "columns with single-level annotations" do
    testRoundTrip "simple columns with annotations" $
      columns
        [ ( annotate (color Red) (pretty ("col1" :: Text)),
            Fixed 20
          ),
          ( annotate (color Green) (pretty ("col2" :: Text)),
            Fixed 20
          )
        ]

  describe "columns with annotation nesting depth 2" do
    testRoundTrip "two columns with double nesting" $
      columns
        [ ( annotate (color Red) $
              annotate bold (pretty ("nested col1" :: Text)),
            Weighted 1.0
          ),
          ( annotate (color Green) $
              annotate underlined (pretty ("nested col2" :: Text)),
            Weighted 1.0
          )
        ]
    testRoundTrip "single column with double nesting" $
      columns
        [ ( annotate (color Blue) $
              annotate bold (pretty ("double" :: Text)),
            Weighted 1.0
          )
        ]

  describe "columns with annotation nesting depth 3" do
    testRoundTrip "triple nesting" $
      columns
        [ ( annotate (color Red) $
              annotate bold $
                annotate underlined (pretty ("triple" :: Text)),
            Weighted 1.0
          )
        ]
    testRoundTrip "with reversion" $
      columns
        [ ( annotate (color Red) $
              pretty ("R " :: Text)
                <> annotate
                  (color Green)
                  ( pretty ("G " :: Text)
                      <> annotate bold (pretty ("B" :: Text))
                      <> pretty (" G2" :: Text)
                  )
                <> pretty (" R2" :: Text),
            Weighted 1.0
          )
        ]

  describe "columns with annotation nesting depth 4" do
    testRoundTrip "quad nesting" $
      columns
        [ ( annotate (color Red) $
              annotate bold $
                annotate underlined $
                  annotate (color Blue) (pretty ("quad" :: Text)),
            Weighted 1.0
          )
        ]

  -- Note: annotations spanning multiple lines within columns have limitations
  -- in the current implementation. The splitStreamLines function needs additional
  -- work to properly handle annotations that cross line boundaries.

  describe "columns annotation reversion" do
    testRoundTrip "color reversion in columns" $
      columns
        [ ( annotate (color Red) $
              pretty ("red " :: Text)
                <> annotate (color Blue) (pretty ("blue" :: Text))
                <> pretty (" red again" :: Text),
            Weighted 1.0
          )
        ]
    testRoundTrip "multi-level reversion in columns" $
      columns
        [ ( annotate (color Red) $
              pretty ("R1 " :: Text)
                <> annotate
                  (color Green)
                  ( pretty ("G1 " :: Text)
                      <> annotate (color Blue) (pretty ("B" :: Text))
                      <> pretty (" G2" :: Text)
                  )
                <> pretty (" R2" :: Text),
            Weighted 1.0
          )
        ]

  describe "columns with various widths" do
    testRoundTripAt 40 "width 40" $
      columns
        [ ( annotate (color Red) $
              annotate bold $
                annotate underlined (pretty ("Deep" :: Text)),
            Weighted 1.0
          )
        ]
    testRoundTripAt 60 "width 60" $
      columns
        [ ( annotate (color Red) $
              annotate bold $
                annotate underlined (pretty ("Deep" :: Text)),
            Weighted 1.0
          )
        ]
    testRoundTripAt 100 "width 100" $
      columns
        [ ( annotate (color Red) $
              annotate bold $
                annotate underlined (pretty ("Deep" :: Text)),
            Weighted 1.0
          )
        ]

  describe "columns with fillSep and annotations" do
    testRoundTrip "fillSep colored in columns" $
      columns
        [ ( annotate (color Cyan) $
              fillSep $
                map pretty $
                  words ("This text wraps" :: Text),
            Weighted 1.0
          )
        ]
    testRoundTrip "fillSep with mixed annotations in columns" $
      columns
        [ ( annotate (color Magenta) $
              fillSep
                [ pretty ("normal" :: Text),
                  annotate bold (pretty ("bold" :: Text)),
                  pretty ("normal" :: Text)
                ],
            Weighted 1.0
          )
        ]
