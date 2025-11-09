module Utils.Prettyprinter
  ( columns,
    equalColumns,
    HorizontalWidth (..),
    module X,
    test_goldenRenders,
    vsepNonEmpty,
    hsepNonEmpty,
    sepNonEmpty,
  )
where

import MyPrelude hiding (SChar, group)
import Prettyprinter as X
import Prettyprinter.Render.Terminal as X
import Utils.Testing hiding (after)

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
    extractUntilPop = extractAnn 1
      where
        -- depth tracks nesting level: when it reaches 0, we've found our matching pop
        extractAnn :: Int -> SimpleDocStream ann -> (SimpleDocStream ann, SimpleDocStream ann)
        extractAnn _ SFail = (SFail, SEmpty)
        extractAnn 0 s = (SEmpty, s) -- Found matching pop
        extractAnn _ SEmpty = (SEmpty, SEmpty)
        extractAnn depth (SChar c rest) =
          let (content, after) = extractAnn depth rest
           in (SChar c content, after)
        extractAnn depth (SText len txt rest) =
          let (content, after) = extractAnn depth rest
           in (SText len txt content, after)
        extractAnn depth (SLine i rest) =
          let (content, after) = extractAnn depth rest
           in (SLine i content, after)
        extractAnn depth (SAnnPush ann rest) =
          -- Nested push: increase depth
          let (content, after) = extractAnn (depth + 1) rest
           in (SAnnPush ann content, after)
        extractAnn depth (SAnnPop rest)
          | depth == 1 = (SEmpty, rest) -- Found our matching pop
          | otherwise =
              -- This pop matches a nested push
              let (content, after) = extractAnn (depth - 1) rest
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

      isEmptyStream :: SimpleDocStream ann -> Bool
      isEmptyStream SEmpty = True
      isEmptyStream SFail = True
      isEmptyStream _ = False

      -- Check if all columns after index colIdx are empty on line i
      allColumnsAfterAreEmpty :: Int -> Int -> Bool
      allColumnsAfterAreEmpty colIdx i =
        let columnsAfter = drop (colIdx + 1) paddedGroups
         in all (\(_, ls) -> isEmptyStream $ fromMaybe SEmpty $ index ls i) columnsAfter

      -- Combine lines horizontally (preserving annotations)
      -- Don't pad a column if it's the last column OR if all columns after it are empty
      combinedLines =
        [ mconcat $
            zipWith3
              ( \colIdx isLastCol (w, ls) ->
                  let stream = fromMaybe SEmpty $ index ls i
                      shouldPad = not isLastCol && not (allColumnsAfterAreEmpty colIdx i)
                   in streamToDoc $ (if shouldPad then padStreamToWidth w else id) stream
              )
              [0 ..]
              (if null paddedGroups then [] else replicate (length paddedGroups - 1) False ++ [True])
              paddedGroups
          | i <- [0 .. maxLines - 1]
        ]
   in vsep combinedLines
  where
    -- Split a SimpleDocStream into lines at SLine boundaries
    -- Must handle nested annotations by tracking active annotation stack
    -- When we hit an SLine, close all open annotations, then reopen them on the next line
    splitStreamLines :: SimpleDocStream ann -> [SimpleDocStream ann]
    splitStreamLines = go []
      where
        -- Track annotation stack and accumulate lines
        -- Returns list of line streams
        go :: [ann] -> SimpleDocStream ann -> [SimpleDocStream ann]
        go _ SFail = []
        go _ SEmpty = []
        go annStack (SChar c rest) =
          consToFirstLine (SChar c SEmpty) (go annStack rest)
        go annStack (SText len txt rest) =
          consToFirstLine (SText len txt SEmpty) (go annStack rest)
        go annStack (SLine _ rest) =
          -- Close all open annotations, start new line, reopen annotations
          let closeAnns = foldl' (\acc _ -> SAnnPop acc) SEmpty annStack
              lines' = go annStack rest
           in case lines' of
                [] -> [closeAnns]
                (firstLine : restLines) ->
                  let reopenAnns = foldr SAnnPush firstLine (reverse annStack)
                   in closeAnns : reopenAnns : restLines
        go annStack (SAnnPush ann rest) =
          consToFirstLine (SAnnPush ann SEmpty) (go (ann : annStack) rest)
        go annStack (SAnnPop rest) =
          let newStack = case annStack of
                [] -> []
                (_ : xs) -> xs
           in consToFirstLine (SAnnPop SEmpty) (go newStack rest)

        consToFirstLine :: SimpleDocStream ann -> [SimpleDocStream ann] -> [SimpleDocStream ann]
        consToFirstLine s [] = [s]
        consToFirstLine s (firstLine : rest) = appendStream s firstLine : rest

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

-- | Concatenates documents with spaces in between like 'hsep', but doesn't
-- insert spaces if a doc is empty.
hsepNonEmpty :: [Doc ann] -> Doc ann
hsepNonEmpty =
  concatWith
    ( \d1 d2 -> width d1 \w1 ->
        if w1 == 0 then d2 else space <> d2
    )

-- | Concatenates documents with newlines in between like 'vsep', but doesn't
-- insert newlines/spaces if a doc is empty.
vsepNonEmpty :: [Doc ann] -> Doc ann
vsepNonEmpty =
  concatWith
    ( \d1 d2 -> width d1 \w1 ->
        if w1 == 0 then d2 else line <> d2
    )

-- | Concatenates documents with newlines in between like 'vsep', but doesn't
-- insert newlines/spaces if a doc is empty.
sepNonEmpty :: [Doc ann] -> Doc ann
sepNonEmpty = group . vsepNonEmpty

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
    goldenRenders "columns.preserves-annotations" [39, 60, 80] $
      columns
        [ ( annotate bold
              . fillSep
              . over (ix 1) (annotate (colorDull Red))
              . over (ix 2) (annotate (colorDull Blue))
              . over (ix 3) (annotate (colorDull Black))
              . map pretty
              . words
              . asText
              $ "bolded red blue black text followed by normal colored text",
            Weighted 1.0
          ),
          ( annotate (colorDull Green) $
              fillSep . map pretty . words . asText $
                "Green column text that also wraps",
            Weighted 1.0
          )
        ],
    goldenRenders "quchen-prettyprinter-pull-261" [80] $
      -- see https://github.com/quchen/prettyprinter/pull/261 for details
      -- I want to make sure this bug actually gets fixed in case I ever
      -- decide to leave my fork
      annotate
        (color Red)
        ( pretty '1'
            <> annotate
              bold
              ( pretty '2'
                  <> annotate
                    (bgColor Magenta)
                    (pretty '3')
              )
        ),
    goldenRenders "streamToDoc.preserves-annotation" [80] $
      streamToDoc $
        SChar 'a' (SAnnPush (color Red) (SChar 'b' (SAnnPop (SChar 'c' SEmpty)))),
    goldenRenders "streamToDoc.preserves-nested-annotations" [80] $
      streamToDoc
        ( SChar '0'
            . SAnnPush (color Magenta)
            . SChar '1'
            . SAnnPush bold
            . SChar '2'
            . SAnnPush (bgColorDull Blue)
            . SChar '3'
            . SAnnPush (colorDull Red)
            . SChar '4'
            . SAnnPop
            . SChar '3'
            . SAnnPop
            . SChar '2'
            . SAnnPop
            . SChar '1'
            . SAnnPop
            . SChar '0'
            $ SEmpty
        ),
    goldenRenders "columns.preserves-annotations.newline-in-column" [40] $
      columns
        [ ( annotate (colorDull Blue)
              . fillSep
              . map pretty
              . words
              . asText
              $ "some blue text with\nnewlines that wraps at small widths",
            Weighted 1.0
          ),
          let t :: Text
              t = "some text\non the right"
           in (pretty t, Fixed (length t))
        ]
  ]
  where
    goldenRenders name widths doc =
      testGroup name . runIdentity $ for widths \w ->
        Identity . goldenTest (name ++ ".width-" ++ show w) . renderStrict $
          layoutPretty
            (LayoutOptions (AvailablePerLine w 1.0))
            -- trailing newline is easier for viewing/editing as golden files
            (doc <> hardline)
