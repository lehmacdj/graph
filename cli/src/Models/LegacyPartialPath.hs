module Models.LegacyPartialPath
  ( getLegacyPartialPath,
    getGroupPartialPath,
    PathEnd (..),
    pathEndBefore,
    LegacyPartialPath (..),
    test_pathEndBefore,
    test_pLegacyPartialPath,
    test_getLegacyPartialPath,
  )
where

import Data.Char (isAlpha, isSpace)
import Data.List.NonEmpty qualified as NE
import Graph.SystemNodes (tagsNID)
import Models.Path.Parse
import Models.Path.Simple
import MyPrelude hiding (many, try)
import Text.Megaparsec (try)
import Text.Megaparsec.Char
import Utils.Parsing
import Utils.Testing

-- | Try to parse a path, returning a partial path that is the last one
-- in the input string, this partial path will be the input for completion
getLegacyPartialPath :: Text -> Maybe LegacyPartialPath
getLegacyPartialPath i = case runParser pLastLegacyPartialPath "<completion>" i of
  Left _ -> Nothing
  Right r -> Just r

pPathSegment :: Parser Path
pPathSegment = convertDirectivesToErrors pathTerm

-- | a list of path segments that are interpreted as being separated by
-- concatenation, followed by a string that represents a partial transition or something else
data LegacyPartialPath
  = LegacyPartialPath [Path] String
  | MissingSlash [Path]
  deriving (Show, Eq, Ord)

pLegacyPartialPath :: Parser LegacyPartialPath
pLegacyPartialPath = do
  prepath' <-
    try (Right [Absolute tagsNID] <$ (symbol "#" *> eof))
      <|> try (Left <$> pPathSegment `sepBy` symbol "/")
      <|> (Right <$> pPathSegment `endBy` symbol "/")
  case prepath' of
    Right prepath -> pure $ LegacyPartialPath prepath ""
    Left prepath -> case unsnoc prepath of
      Just (path, Literal end) -> pure $ LegacyPartialPath path (unpack end)
      -- this is the case when parsing a tag-like path like "#foobar"
      Just ([], tn@(Absolute _) :/ Literal end) -> pure $ LegacyPartialPath [tn] (unpack end)
      -- if the last thing in the path isn't a literal, we can't complete it
      -- and the only thing that could come next is a slash
      Just _ -> pure $ MissingSlash prepath
      Nothing -> pure $ LegacyPartialPath [] ""

pLastLegacyPartialPath :: Parser LegacyPartialPath
pLastLegacyPartialPath =
  c *> many (try wholeArgument) *> pLegacyPartialPath <* eof
  where
    -- a path that is an argument before the one that is completed, which
    -- a space ends, so that after @mv a @ a new path starts; a path that
    -- the cursor is in is no such argument (with a directive in it the
    -- path can't be completed, and is not read as an argument followed by
    -- an empty one either). The path is read as far as it goes, so the
    -- space after a @/@ (@mv a/ @) is in the path, as commands read it
    wholeArgument = do
      (text, _) <- match pPath
      guard (maybe False isSpace (lastMay text))
    c = lexeme (many (oneOf (":.-_" :: String) <|> alphaNumChar) <* lookAhead s)

-- | A path that is all that is typed of a group, from its opening
-- parenthesis or its last @&@ or union operator, for completion.
getGroupPartialPath :: Text -> Maybe LegacyPartialPath
getGroupPartialPath i =
  case runParser (whitespace *> pLegacyPartialPath <* eof) "<completion>" i of
    Left _ -> Nothing
    Right r -> Just r

-- | What of a command line, read up to the cursor, leads to the path that
-- is completed there. Quotes, regexes and URLs are read whole, so that
-- what is in them is no operator or parenthesis.
data PathEnd = PathEnd
  { -- | The line outside the groups that are open at the cursor, from its
    -- last @&@ or union operator outside parentheses (the command and the
    -- arguments before the path when there is none): what leads to the
    -- outermost open group, or to the cursor.
    outside :: Text,
    -- | For each group that is open at the cursor, outermost first, what
    -- is typed of it from its last @&@ or union operator, or from its
    -- opening parenthesis: each side of the group starts where the group
    -- does, so the sides before it are left out.
    groups :: [Text],
    -- | What is typed after the opening quote of the label that the cursor
    -- is in, if it is in one; the quote and this are not in the texts
    -- above.
    quoted :: Maybe Text
  }
  deriving (Show, Eq, Ord)

-- | A group of a command line being read: everything in it so far and what
-- is in it from its last operator, both reversed.
data Frame = Frame
  { -- | The parentheses of a directive's arguments, not a group.
    directive :: Bool,
    whole :: String,
    side :: String
  }

-- | Where the cursor is in a command line, given the line up to it:
-- Nothing when it is in a regex, a URL or a directive's arguments.
pathEndBefore :: Text -> Maybe PathEnd
pathEndBefore = go (Frame False "" "" :| []) . unpack
  where
    go :: NonEmpty Frame -> String -> Maybe PathEnd
    go frames@(frame :| outer) = \case
      [] -> finish frames Nothing
      input@(c : rest)
        | c `elem` ['"', '\''],
          ":xeger" `isPrefixOf` frame.side ->
            -- a regex is read up to its delimiter, escapes and all
            case break (== c) rest of
              (pattern', _ : rest') -> go (adding (c : pattern' <> [c])) rest'
              _ -> Nothing
        | c == '"' -> case stringBody rest of
            Right (body, rest') -> go (adding ('"' : body <> "\"")) rest'
            Left typed -> finish frames (Just typed)
        | any (`isPrefixOf` input) ["http://", "https://"],
          maybe True (not . isIdentChar) (headMay frame.whole) ->
            -- a URL extends to the next space
            case break isSpace input of
              (_, []) -> Nothing
              (url, rest') -> go (adding url) rest'
        | c == '(' ->
            let opened = Frame (directiveBefore frame.side) "" ""
             in go (opened :| (frame : outer)) rest
        | c == ')',
          o : os <- outer ->
            let closed = "(" <> reverse frame.whole <> ")"
             in go (append closed o :| os) rest
        | c `elem` ("&|+" :: String) ->
            go (frame {whole = c : frame.whole, side = ""} :| outer) rest
        | otherwise -> go (adding [c]) rest
      where
        adding t = append t frame :| outer
    append t f =
      f {whole = reverse t <> f.whole, side = reverse t <> f.side}
    -- a % and a name before the parenthesis
    directiveBefore reversedSide = case span isAlpha reversedSide of
      (_ : _, '%' : _) -> True
      _ -> False
    -- the characters of a string up to its closing quote, and what
    -- follows it; all of them when it is not closed
    stringBody = loop ""
      where
        loop acc = \case
          '"' : rest -> Right (reverse acc, rest)
          '\\' : x : rest -> loop (x : '\\' : acc) rest
          x : rest -> loop (x : acc) rest
          [] -> Left (reverse acc)
    finish frames typed
      | any (.directive) frames = Nothing
      | otherwise =
          Just
            PathEnd
              { outside = text (NE.last frames),
                groups = reverse (map text (NE.init frames)),
                quoted = pack <$> typed
              }
    text f = pack (reverse f.side)

test_pLegacyPartialPath :: TestTree
test_pLegacyPartialPath =
  testGroup
    "pLegacyPartialPath"
    [ "#" `parsesTo` LegacyPartialPath [Absolute tagsNID] "",
      "#foo" `parsesTo` LegacyPartialPath [Absolute tagsNID] "foo",
      "(a + b)" `parsesTo` MissingSlash [Literal "a" :|| Literal "b"],
      "a/b/" `parsesTo` LegacyPartialPath [Literal "a", Literal "b"] "",
      "a/b" `parsesTo` LegacyPartialPath [Literal "a"] "b"
    ]
  where
    parsesTo i = testCase ("parse: " ++ show i) . testParserParses pLegacyPartialPath i

-- | What completion reads before the cursor: the command, and the path that
-- is completed after the paths before it.
test_getLegacyPartialPath :: TestTree
test_getLegacyPartialPath =
  testGroup
    "getLegacyPartialPath"
    [ "ls a/b" `reads'` Just (LegacyPartialPath [Literal "a"] "b"),
      "mv a b/c" `reads'` Just (LegacyPartialPath [Literal "b"] "c"),
      "mv %last/a b/c" `reads'` Just (LegacyPartialPath [Literal "b"] "c"),
      -- a space after a whole path starts the next one
      "mv a " `reads'` Just (LegacyPartialPath [] ""),
      "mv a b " `reads'` Just (LegacyPartialPath [] ""),
      "mv a\t\t" `reads'` Just (LegacyPartialPath [] ""),
      "ls %last/a " `reads'` Just (LegacyPartialPath [] ""),
      -- but not one after a /, which the path goes on after
      "mv a/ " `reads'` Just (LegacyPartialPath [Literal "a"] ""),
      "mv a / b" `reads'` Just (LegacyPartialPath [Literal "a"] "b"),
      -- a path with a directive gets nothing
      "ls %last/a" `reads'` Nothing,
      "ls %last/" `reads'` Nothing,
      "ls %history(0)/a" `reads'` Nothing,
      "ls c/%last/a" `reads'` Nothing,
      "mv a %last/b" `reads'` Nothing
    ]
  where
    reads' i e = testCase (show i) $ getLegacyPartialPath i @?= e

test_pathEndBefore :: TestTree
test_pathEndBefore =
  testGroup
    "pathEndBefore"
    [ "ls a/b" `endsAs` Just (PathEnd "ls a/b" [] Nothing),
      "ls a & b/c" `endsAs` Just (PathEnd " b/c" [] Nothing),
      "ls a/(b + c" `endsAs` Just (PathEnd "ls a/" [" c"] Nothing),
      "ls a/(b | c/d" `endsAs` Just (PathEnd "ls a/" [" c/d"] Nothing),
      "ls a/(b ++ c" `endsAs` Just (PathEnd "ls a/" [" c"] Nothing),
      "ls (a + b)/e + (c + d)" `endsAs` Just (PathEnd " (c + d)" [] Nothing),
      "ls (a + b)/(e + (c + d)"
        `endsAs` Just (PathEnd "ls (a + b)/" [" (c + d)"] Nothing),
      "ls a & b/(a + b & c/(d + e"
        `endsAs` Just (PathEnd " b/" [" c/", " e"] Nothing),
      "ls (a " `endsAs` Just (PathEnd "ls " ["a "] Nothing),
      "ls #foo + #bar/" `endsAs` Just (PathEnd " #bar/" [] Nothing),
      -- what is in a quote, a regex or a URL is no operator or parenthesis
      "ls \"a & (b\"/c" `endsAs` Just (PathEnd "ls \"a & (b\"/c" [] Nothing),
      "ls \"a\\\" & (b\"/c"
        `endsAs` Just (PathEnd "ls \"a\\\" & (b\"/c" [] Nothing),
      "ls regex:'(\\' & c" `endsAs` Just (PathEnd " c" [] Nothing),
      "ls regex:\"(\" & c" `endsAs` Just (PathEnd " c" [] Nothing),
      "ls https://a.b/?c&(d e"
        `endsAs` Just (PathEnd "ls https://a.b/?c&(d e" [] Nothing),
      -- a quote that is not closed
      "ls \"a b" `endsAs` Just (PathEnd "ls " [] (Just "a b")),
      "ls x/(y & \"a (b\\\""
        `endsAs` Just (PathEnd "ls x/" [" "] (Just "a (b\\\"")),
      -- in a regex, a URL or a directive's arguments
      "ls regex:\"a" `endsAs` Nothing,
      "ls https://a.b/" `endsAs` Nothing,
      "ls %targets(a & b" `endsAs` Nothing,
      "ls %targets(a)/b" `endsAs` Just (PathEnd "ls %targets(a)/b" [] Nothing)
    ]
  where
    endsAs i e = testCase (show i) $ pathEndBefore i @?= e
