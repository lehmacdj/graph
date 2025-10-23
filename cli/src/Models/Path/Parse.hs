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
    <|> (symbol "%never" $> Zero)
    <|> between
      (symbol "!{")
      (symbol "}")
      (ExcludingNIDs . setFromList <$> pNID `sepEndBy1` symbol ",")
    <|> (RegexMatch <$> try pRegex)
    <|> (Literal <$> pTransition)
    <|> (uncurry Directive <$> withSourceRange pDirective)
    <|> parens (pPartialPath' pNID)
  where
    pTag :: Parser PartialPath
    pTag = (Absolute (Right tagsNID) :/) . Literal <$> (char '#' *> pTransition)

pDirective :: Parser (Directive 'Partial)
pDirective =
  try (LocationFromHistory <$> pDirectiveNamed "history" number)
    <|> try (LocationFromHistory 1 <$ symbol "%last")
    <|> (Targets <$> pDirectiveNamed "targets" pPartialPath)
    <|> (Splice <$> between (symbol "%{") (symbol "}") (many (noneOf ['}'])))

pDirectiveNamed :: Text -> Parser a -> Parser a
pDirectiveNamed name = between (symbol ("%" <> name <> "(")) (symbol ")")

pRegex :: Parser CheckedRegex
pRegex = label "re\"<regex>\"" do
  _ <- symbol "re"
  str <-
    between (char '"') (char '"') (takeWhileP Nothing (/= '"'))
      <|> between (char '\'') (char '\'') (takeWhileP Nothing (/= '\''))
  str
    & compileRegex . encodeUtf8
    & codiagonal . bimap fail pure

partialPathTerm :: Parser PartialPath
partialPathTerm = partialPathTerm' pFullNID

pathTerm :: Parser ParsedPath
pathTerm = parsedParserFromPartialParser partialPathTerm

table :: [[Operator Parser PartialPath]]
table =
  [ [Prefix (Backwards <$ symbol "~")],
    [InfixL $ (:/) <$ symbol "/"],
    [InfixL $ (:&) <$ symbol "&"],
    [InfixL $ (:+) <$ symbol "+"]
  ]

pPartialPath' :: Parser (NIDVal Partial) -> Parser PartialPath
pPartialPath' pNID =
  makeExprParser (partialPathTerm' pNID) table

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
      "re\"^foo.*bar$\"" `parsesTo` RegexMatch [re|^foo.*bar$|],
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
      "foo/%targets(@000000000002 + re\"^foo$\")/%never"
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
      "re\"asdf\"" `parsesTo` RegexMatch [re|asdf|],
      "re'asdf'" `parsesTo` RegexMatch [re|asdf|],
      "re'\\.'" `parsesTo` RegexMatch [re|\.|],
      "~re\"asdf\"" `parsesTo` Backwards (RegexMatch [re|asdf|]),
      -- escape sequences need to not be interpreted in regexes
      [rq|~re"\."|] `parsesTo` Backwards (RegexMatch [re|\.|]),
      -- regression: caught this as an error while working on Graph.Timestamps
      [rq|~re"[0-9]{2}:[0-9]{2}:[0-9]{2}\.[0-9]{12}"|]
        `parsesTo` Backwards
          (RegexMatch [re|[0-9]{2}:[0-9]{2}:[0-9]{2}\.[0-9]{12}|]),
      [rq|%{excludedLargeSystemNodes}|]
        `parsesTo` Directive testRange (Splice "excludedLargeSystemNodes"),
      parseFails "foo/bar&",
      parseFails "foo/bar&+qux",
      parseFails "foo+",
      parseFails "+foo",
      parseFails "~",
      parseFails "foo~bar",
      parseFails "%last()",
      parseFails "re\"",
      -- we could allow this, but seems wise to exclude for ambiguity reasons
      parseFails "!@000000000001",
      parseFails "!{}"
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
      "%{some text here}" `parsesTo` Splice "some text here",
      "%{Absolute nid}" `parsesTo` Splice "Absolute nid",
      parseFails "%last()",
      parseFails "%history()",
      parseFails "%history(foo)",
      parseFails "%targets()",
      parseFails "%targets(foo"
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
    mkAbsolute :: NID -> PartialPath
    mkAbsolute nid = Absolute (Right nid)
