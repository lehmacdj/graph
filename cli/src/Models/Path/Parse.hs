-- | Megaparsec support for parsing strings that make up paths.
-- Generic over the custom error type, it doesn't throw any.
module Models.Path.Parse where

import Control.Monad.Combinators.Expr
import Data.Functor
import Graph.SystemNodes (tagsNID)
import Models.NID
import Models.Path.ParsedPath (ParsedPath)
import Models.Path.PartialPath
import MyPrelude hiding (many, try)
import Text.Megaparsec (try)
import Text.Megaparsec.Char (char)
import Utils.Base62 (isBase62Char)
import Utils.Parsing
import Utils.Testing

pTransition :: Parser Text
pTransition = ident <|> stringLiteral <?> "<transition>"

pFullNID :: Parser (NIDVal Partial)
pFullNID = lexeme p <?> "@<nid>"
  where
    p = do
      offset <- getOffset
      (sourceRange, base62Chars) <- withSourceRange do
        _ <- char '@'
        takeWhile1P Nothing isBase62Char
      let result
            | length base62Chars < nidDigits =
                pure . Left $ IncompleteNID {..}
            | length base62Chars == nidDigits =
                pure . Right $ unsafeNID base62Chars
            | otherwise = fail "too many base62 characters in NID"
      result

-- | Parse a small integer and convert to NID
pSmallNID :: Parser NID
pSmallNID = lexeme $ smallNID <$> (char '@' *> positiveNumber)

partialPathTerm' :: Parser (NIDVal Partial) -> Parser PartialPath
partialPathTerm' pNID =
  try (Absolute <$> pNID)
    <|> try pTag
    <|> (symbol "@" $> One)
    <|> (symbol "*" $> Wild)
    <|> (symbol ("%" <> neverName) $> Zero)
    <|> between
      (symbol "!{")
      (symbol "}")
      (ExcludingNIDs . setFromList <$> pNID `sepEndBy1` symbol ",")
    <|> (RegexMatch <$> try pRegex)
    <|> (uncurry Directive <$> withSourceRange pDirective)
    <|> (Literal <$> pTransition)
    <|> parens (pPartialPath' pNID)
  where
    pTag :: Parser PartialPath
    pTag = (Absolute (Right tagsNID) :/) . Literal <$> (char '#' *> pTransition)

pDirective :: Parser (Directive 'Partial)
pDirective =
  try (HttpResource <$> pHttpURI)
    <|> choice (map (uncurry pNamedDirective) namedDirectives)
    <|> (Splice <$> between (symbol "%{") (symbol "}") (many (noneOf ['}'])))

-- | How a directive that is a name after @%@ is written after its name.
data DirectiveForm
  = -- | Nothing follows the name: it stands for this directive.
    BareDirective (Directive 'Partial)
  | -- | Arguments follow in parentheses, which this parses.
    DirectiveArguments (Parser (Directive 'Partial))

-- | The directives that are a name after @%@: what 'pDirective' parses
-- besides URLs and splices, and what completion offers ('percentNames').
namedDirectives :: [(Text, DirectiveForm)]
namedDirectives =
  [ ("history", DirectiveArguments (LocationFromHistory <$> number)),
    ("last", BareDirective (LocationFromHistory 1)),
    ("targets", DirectiveArguments (Targets <$> pPartialPath)),
    ("override", DirectiveArguments pOverride)
  ]

pNamedDirective :: Text -> DirectiveForm -> Parser (Directive 'Partial)
pNamedDirective name = \case
  BareDirective d -> d <$ symbol ("%" <> name)
  DirectiveArguments p -> pDirectiveNamed name p

-- | The name after @%@ of the path that matches nothing.
neverName :: Text
neverName = "never"

-- | Every name that a path can have after a @%@, with whether arguments in
-- parentheses follow it: 'neverName' and 'namedDirectives'.
percentNames :: [(Text, Bool)]
percentNames =
  (neverName, False) : [(n, takesArguments f) | (n, f) <- namedDirectives]
  where
    takesArguments = \case
      BareDirective _ -> False
      DirectiveArguments _ -> True

-- | @{plus = shared}, p@: settings, then the path they apply to.
pOverride :: Parser (Directive 'Partial)
pOverride = do
  settings <-
    between (symbol "{") (symbol "}") (pSetting `sepBy` symbol ",")
  settings' <- foldM addSetting (OverrideSettings Nothing) settings
  _ <- symbol ","
  Override settings' <$> pPartialPath
  where
    pSetting = do
      offset <- getOffset
      (range, setting) <- withSourceRange do
        key <- ident
        _ <- symbol "="
        value <- ident
        pure (key, value)
      pure ((offset, range), setting)
    addSetting s (range, (key, value)) = case key of
      "plus" -> case s.plus of
        Just _ -> invalid range "The setting plus is given twice"
        Nothing -> case value of
          "shared" -> pure s {plus = Just Shared}
          "relational" -> pure s {plus = Just Relational}
          _ ->
            invalid range $
              "Unknown value "
                <> value
                <> " for plus; it accepts shared or relational"
      -- it was a setting once; it limits the walks of the whole path
      "max-repairs" ->
        invalid range $
          "max-repairs is not a setting of %override any more: it is an "
            <> "option of mk and tag, written before the path "
            <> "(mk --max-repairs N <path>, with N a positive number or "
            <> "unlimited), since it limits the walks of the whole path; "
            <> "remove the setting max-repairs = "
            <> value
      _ ->
        invalid range $
          "Unknown setting "
            <> key
            <> " in %override; the only setting is plus"
    invalid (offset, range) = failAt offset . InvalidOverrideSetting range

pHttpURI :: Parser URI
pHttpURI = lexeme (lookAhead (string "http://" <|> string "https://") *> pURI)

pDirectiveNamed :: Text -> Parser a -> Parser a
pDirectiveNamed name = between (symbol ("%" <> name <> "(")) (symbol ")")

partialPathTerm :: Parser PartialPath
partialPathTerm = partialPathTerm' pFullNID

pathTerm :: Parser ParsedPath
pathTerm = parsedParserFromPartialParser partialPathTerm

table :: [[Operator Parser PartialPath]]
table =
  [ [Prefix (Backwards <$ symbol "~")],
    [InfixL $ (:/) <$ symbol "/"],
    [InfixL $ (:&) <$ symbol "&"]
  ]

-- | The union operators, which share the lowest precedence. A chain of one
-- of them associates to the left; two different ones need parentheses.
pUnionOperator ::
  Parser (SourceRange, Text, PartialPath -> PartialPath -> PartialPath)
pUnionOperator = do
  (range, (op, f)) <-
    withSourceRange $
      (("++", (:++)) <$ string "++")
        <|> (("+", (:+)) <$ string "+")
        <|> (("|", (:||)) <$ string "|")
  whitespace
  pure (range, op, f)

pPartialPath' :: Parser (NIDVal Partial) -> Parser PartialPath
pPartialPath' pNID = do
  first <- operand
  rest <- many do
    offset <- getOffset
    (range, op, f) <- pUnionOperator
    r <- operand
    pure ((offset, range, op), f, r)
  case rest of
    ((_, _, op), _, _) : more
      | (offset, range, op') : _ <-
          [o | (o@(_, _, op'), _, _) <- more, op' /= op] ->
          failAt offset (MixedUnionOperators range op op')
    _ -> pure (foldl' (\l (_, f, r) -> f l r) first rest)
  where
    operand = makeExprParser (partialPathTerm' pNID) table

-- | Fail with a custom error at an earlier offset.
failAt :: Int -> CustomParseError -> Parser a
failAt offset =
  parseError . FancyError offset . singletonSet . ErrorCustom

pPartialPath :: Parser PartialPath
pPartialPath = pPartialPath' pFullNID

registerErrors :: Either (NonNull [ParseError']) a -> Parser a
registerErrors = \case
  Left (splitFirst -> (e, es)) -> do
    for_ es registerParseError
    parseError e
  Right p -> pure p

parsedParserFromPartialParser :: Parser PartialPath -> Parser ParsedPath
parsedParserFromPartialParser p = p >>= registerErrors . parsedFromPartial

pPath' :: Parser NID -> Parser ParsedPath
pPath' pNID = parsedParserFromPartialParser (pPartialPath' (Right <$> pNID))

pPath :: Parser ParsedPath
pPath = parsedParserFromPartialParser pPartialPath

-- | A whole path, with its directives and with space around it: what the
-- path quasiquoter reads, and what a caller that only has a path parses.
parsePath :: Text -> Either ParseErrorBundle' ParsedPath
parsePath = runParser (whitespace *> pPath <* eof) ""

test_pPath :: TestTree
test_pPath =
  testGroup
    "pPath"
    [ "#foo" `parsesTo` (mkAbsolute tagsNID :/ Literal "foo"),
      "@" `parsesTo` One,
      "*" `parsesTo` Wild,
      "%never" `parsesTo` Zero,
      "foo" `parsesTo` Literal "foo",
      "@000000000002" `parsesTo` mkAbsolute (smallNID 2),
      "@asdf" `parsesTo` Absolute (Left (IncompleteNID testOffset testRange "asdf")),
      "!{ @000000000001, } " `parsesTo` mkExcludingNIDs (setFromList [smallNID 1]),
      "!{@000000000001, @000000000002}" `parsesTo` mkExcludingNIDs (setFromList [smallNID 1, smallNID 2]),
      "!{@000000000001, @asdf,}"
        `parsesTo` ExcludingNIDs
          ( setFromList
              [ Right $ smallNID 1,
                Left $ IncompleteNID testOffset testRange "asdf"
              ]
          ),
      "foo/@000000000002" `parsesTo` (Literal "foo" :/ mkAbsolute (smallNID 2)),
      "foo/bar" `parsesTo` (Literal "foo" :/ Literal "bar"),
      "regex:\"^foo.*bar$\"" `parsesTo` RegexMatch [re|^foo.*bar$|],
      "~foo/bar" `parsesTo` (Backwards (Literal "foo") :/ Literal "bar"),
      "~foo/~bar" `parsesTo` (Backwards (Literal "foo") :/ Backwards (Literal "bar")),
      "~(foo/~bar)" `parsesTo` Backwards (Literal "foo" :/ Backwards (Literal "bar")),
      "~@" `parsesTo` Backwards One,
      "foo + ~@" `parsesTo` (Literal "foo" :+ Backwards One),
      "~@ & ~@000000000001" `parsesTo` (Backwards One :& Backwards (mkAbsolute (smallNID 1))),
      "(foo/bar)/baz" `parsesTo` (Literal "foo" :/ Literal "bar" :/ Literal "baz"),
      "foo/(bar/baz)" `parsesTo` (Literal "foo" :/ (Literal "bar" :/ Literal "baz")),
      "foo / bar & baz" `parsesTo` (Literal "foo" :/ Literal "bar" :& Literal "baz"),
      "foo/bar&baz+qux"
        `parsesTo` (Literal "foo" :/ Literal "bar" :& Literal "baz" :+ Literal "qux"),
      "foo/bar&baz+qux/%never" `parsesTo` (Literal "foo" :/ Literal "bar" :& Literal "baz" :+ Literal "qux" :/ Zero),
      "foo/bar&(baz+qux)"
        `parsesTo` (Literal "foo" :/ Literal "bar" :& (Literal "baz" :+ Literal "qux")),
      "foo/(bar&baz+qux)/%never" `parsesTo` (Literal "foo" :/ (Literal "bar" :& Literal "baz" :+ Literal "qux") :/ Zero),
      "foo/%targets(@000000000002 + regex:\"^foo$\")/%never"
        `parsesTo` ( Literal "foo"
                       :/ Directive
                         testRange
                         ( Targets
                             (mkAbsolute (smallNID 2) :+ RegexMatch [re|^foo$|])
                         )
                       :/ Zero
                   ),
      "foo/%history(2)/bar"
        `parsesTo` ( Literal "foo"
                       :/ Directive testRange (LocationFromHistory 2)
                       :/ Literal "bar"
                   ),
      -- approximation of the pattern we use to detect paths
      "~*/%{mkAbsolute nid}"
        `parsesTo` (Backwards Wild :/ Directive testRange (Splice "mkAbsolute nid")),
      "regex:\"asdf\"" `parsesTo` RegexMatch [re|asdf|],
      "regex:'asdf'" `parsesTo` RegexMatch [re|asdf|],
      "regex:'\\.'" `parsesTo` RegexMatch [re|\.|],
      "~regex:\"asdf\"" `parsesTo` Backwards (RegexMatch [re|asdf|]),
      "regex:\"x\" & a" `parsesTo` (RegexMatch [re|x|] :& Literal "a"),
      "regex:'x' / a" `parsesTo` (RegexMatch [re|x|] :/ Literal "a"),
      -- escape sequences need to not be interpreted in regexes
      [rq|~regex:"\."|] `parsesTo` Backwards (RegexMatch [re|\.|]),
      -- regression: caught this as an error while working on Graph.Timestamps
      [rq|~regex:"[0-9]{2}:[0-9]{2}:[0-9]{2}\.[0-9]{12}"|]
        `parsesTo` Backwards
          (RegexMatch [re|[0-9]{2}:[0-9]{2}:[0-9]{2}\.[0-9]{12}|]),
      [rq|%{excludedLargeSystemNodes}|]
        `parsesTo` Directive testRange (Splice "excludedLargeSystemNodes"),
      "foo/https://example.com/resource"
        `parsesTo` ( Literal "foo"
                       :/ Directive
                         testRange
                         (HttpResource [quri|https://example.com/resource|])
                   ),
      "https://example.com/resource / foo"
        `parsesTo` ( Directive
                       testRange
                       (HttpResource [quri|https://example.com/resource|])
                       :/ Literal "foo"
                   ),
      [rq|"https://example.com/resource"|]
        `parsesTo` Literal "https://example.com/resource",
      parseFails "foo/bar&",
      parseFails "foo/bar&+qux",
      parseFails "foo+",
      parseFails "+foo",
      parseFails "~",
      parseFails "foo~bar",
      parseFails "%last()",
      parseFails "regex:\"",
      -- we could allow this, but seems wise to exclude for ambiguity reasons
      parseFails "!@000000000001",
      parseFails "!{}",
      "a | b" `parsesTo` (Literal "a" :|| Literal "b"),
      "a ++ b" `parsesTo` (Literal "a" :++ Literal "b"),
      "a|b|c" `parsesTo` (Literal "a" :|| Literal "b" :|| Literal "c"),
      "a ++ b/c & d"
        `parsesTo` (Literal "a" :++ Literal "b" :/ Literal "c" :& Literal "d"),
      "(a + b) | c" `parsesTo` ((Literal "a" :+ Literal "b") :|| Literal "c"),
      "a ++ (b | c)" `parsesTo` (Literal "a" :++ (Literal "b" :|| Literal "c")),
      parseFails "a + b | c",
      parseFails "a | b ++ c",
      parseFails "a ++ b + c",
      parseFails "a |",
      parseFails "a +++ b"
    ]
  where
    parsesTo :: Text -> PartialPath -> TestTree
    parsesTo input =
      testCase ("parse: " ++ show input)
        . testParserParses pPartialPath input
    parseFails :: Text -> TestTree
    parseFails input =
      testCase ("parse fails: " ++ show input) $
        testParserFails (pPartialPath <* eof) input
    mkAbsolute :: NID -> PartialPath
    mkAbsolute nid = Absolute (Right nid)
    mkExcludingNIDs :: Set NID -> PartialPath
    mkExcludingNIDs nids = ExcludingNIDs (mapSet Right nids)

test_pDirective :: TestTree
test_pDirective =
  testGroup
    "pDirective"
    [ "%history(23)" `parsesTo` LocationFromHistory 23,
      "%history(-5)" `parsesTo` LocationFromHistory -5,
      "%last" `parsesTo` LocationFromHistory 1,
      "%targets(@000000000002)" `parsesTo` Targets (mkAbsolute (smallNID 2)),
      "%targets(@/foo + #bar)"
        `parsesTo` Targets
          ( (One :/ Literal "foo")
              :+ (mkAbsolute tagsNID :/ Literal "bar")
          ),
      -- this needs to be Haskell code to actually work, but we just want to
      -- parse anything here
      "%override({plus = shared}, a + b)"
        `parsesTo` Override
          (OverrideSettings (Just Shared))
          (Literal "a" :+ Literal "b"),
      "%override({ plus=relational }, %targets(a) + b)"
        `parsesTo` Override
          (OverrideSettings (Just Relational))
          (Directive testRange (Targets (Literal "a")) :+ Literal "b"),
      "%override({}, a)"
        `parsesTo` Override
          (OverrideSettings Nothing)
          (Literal "a"),
      "%{some text here}" `parsesTo` Splice "some text here",
      "%{Absolute nid}" `parsesTo` Splice "Absolute nid",
      "https://example.com/resource"
        `parsesTo` HttpResource [quri|https://example.com/resource|],
      parseFails "%last()",
      parseFails "%history()",
      parseFails "%history(foo)",
      parseFails "%targets()",
      parseFails "%targets(foo",
      parseFails "%override({plus = shared}, )",
      parseFails "%override({plus = shared} a)",
      parseFails "%override({plus = both}, a)",
      parseFails "%override({minus = shared}, a)",
      parseFails "%override({plus = shared, plus = shared}, a)",
      parseFails "%override(a)",
      -- max-alternatives was a setting once; it is unknown like any other
      parseFails "%override({max-alternatives = 100000}, a)",
      parseFails "%override({max-alternatives = unlimited}, a)",
      -- so was max-repairs; it is an option of mk and tag now, and the
      -- error says so whatever its value
      "%override({max-repairs = 5000}, a)"
        `failsWith` "an option of mk and tag",
      "%override({max-repairs = unlimited}, a)"
        `failsWith` "mk --max-repairs N <path>",
      "%override({plus = shared, max-repairs = many}, a)"
        `failsWith` "remove the setting max-repairs = many",
      "%override({minus = shared}, a)"
        `failsWith` "the only setting is plus",
      -- these were old regex syntax
      parseFails "re\"asdf\"",
      parseFails "re'asdf'",
      -- don't want to parse things that are barely URIs as HttpResource
      parseFails "http",
      parseFails "https",
      parseFails "https:",
      parseFails "http:"
    ]
  where
    parsesTo :: Text -> Directive 'Partial -> TestTree
    parsesTo input =
      testCase ("parse: " ++ show input)
        . testParserParses pDirective input
    parseFails :: Text -> TestTree
    parseFails input =
      testCase ("parse fails: " ++ show input) $
        testParserFails (pDirective <* eof) input
    failsWith :: Text -> String -> TestTree
    failsWith input expected =
      testCase ("parse fails saying " ++ show expected ++ ": " ++ show input) $
        case runParserTest (pDirective <* eof) input of
          Right x -> assertFailure ("parsed to " ++ show x)
          Left err ->
            let shown = errorBundlePretty err
             in expected
                  `isInfixOf` shown
                  @? ("expected " ++ show expected ++ " in:\n" ++ shown)
    mkAbsolute :: NID -> PartialPath
    mkAbsolute nid = Absolute (Right nid)
