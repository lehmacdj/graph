-- | Megaparsec support for parsing strings that make up paths.
-- Generic over the custom error type, it doesn't throw any.
module Models.Path.Parse where

-- ( pPath,
--   pPath',
--   pathTerm,
--   test_pPath,
--   test_pDirective,
-- )

import Control.Monad.Combinators.Expr
import Data.Functor
import Graph.SystemNodes (tagsNID)
import Models.NID
import Models.Path.ParsedPath
import MyPrelude hiding (many, try)
import Text.Megaparsec (try)
import Text.Megaparsec.Char (char)
import Utils.Parsing
import Utils.Testing

pathTerm' :: Parser NID -> Parser t -> Parser (ParsedPath t)
pathTerm' pNID pTransition =
  try (Absolute <$> pNID)
    <|> try ((Absolute tagsNID :/) . Literal <$> (char '#' *> pTransition))
    <|> (symbol "@" $> One)
    <|> (symbol "*" $> Wild)
    <|> (symbol "%never" $> Zero)
    <|> between
      (symbol "!{")
      (symbol "}")
      (ExcludingNIDs . setFromList <$> pNID `sepEndBy1` symbol ",")
    <|> (RegexMatch <$> try pRegex)
    <|> (Literal <$> pTransition)
    <|> (uncurry Directive <$> withSourceRange (pDirective pTransition))
    <|> parens (pPath' pNID pTransition)

pDirective :: Parser t -> Parser (Directive t)
pDirective pTransition =
  try (LocationFromHistory <$> pDirectiveNamed "history" number)
    <|> try (LocationFromHistory 1 <$ symbol "%last")
    <|> (Targets <$> pDirectiveNamed "targets" (pPath pTransition))
    <|> (Splice <$> between (symbol "%{") (symbol "}") (many (noneOf ['}'])))

pDirectiveNamed :: Text -> Parser a -> Parser a
pDirectiveNamed name = between (symbol ("%" <> name <> "(")) (symbol ")")

pRegex :: Parser CheckedRegex
pRegex = do
  _ <- symbol "re"
  str <-
    between (char '"') (char '"') (takeWhileP Nothing (/= '"'))
      <|> between (char '\'') (char '\'') (takeWhileP Nothing (/= '\''))
  str
    & compileRegex . encodeUtf8
    & codiagonal . bimap fail pure

pathTerm :: Parser t -> Parser (ParsedPath t)
pathTerm = pathTerm' pFullNID

binary :: Text -> (ParsedPath t -> ParsedPath t -> ParsedPath t) -> Operator Parser (ParsedPath t)
binary name f = InfixL (f <$ symbol name)

table :: [[Operator Parser (ParsedPath t)]]
table =
  [ [Prefix (Backwards <$ symbol "~")],
    [binary "/" (:/)],
    [binary "&" (:&)],
    [binary "+" (:+)]
  ]

pPath' :: Parser NID -> Parser t -> Parser (ParsedPath t)
pPath' pNID pTransition = makeExprParser (pathTerm' pNID pTransition) table

pPath :: Parser t -> Parser (ParsedPath t)
pPath = pPath' pFullNID

test_pPath :: TestTree
test_pPath =
  testGroup
    "pPath"
    [ "#foo" `parsesTo` (Absolute tagsNID :/ Literal "foo"),
      "@" `parsesTo` One,
      "*" `parsesTo` Wild,
      "%never" `parsesTo` Zero,
      "foo" `parsesTo` Literal "foo",
      "@000000000002" `parsesTo` Absolute (smallNID 2),
      "!{ @000000000001, } " `parsesTo` ExcludingNIDs (setFromList [smallNID 1]),
      "!{@000000000001, @000000000002}" `parsesTo` ExcludingNIDs (setFromList [smallNID 1, smallNID 2]),
      "foo/@000000000002" `parsesTo` (Literal "foo" :/ Absolute (smallNID 2)),
      "foo/bar" `parsesTo` (Literal "foo" :/ Literal "bar"),
      "re\"^foo.*bar$\"" `parsesTo` RegexMatch [re|^foo.*bar$|],
      "~foo/bar" `parsesTo` (Backwards (Literal "foo") :/ Literal "bar"),
      "~foo/~bar" `parsesTo` (Backwards (Literal "foo") :/ Backwards (Literal "bar")),
      "~(foo/~bar)" `parsesTo` Backwards (Literal "foo" :/ Backwards (Literal "bar")),
      "~@" `parsesTo` Backwards One,
      "foo + ~@" `parsesTo` (Literal "foo" :+ Backwards One),
      "~@ & ~@000000000001" `parsesTo` (Backwards One :& Backwards (Absolute (smallNID 1))),
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
                         testAnn
                         ( Targets
                             (Absolute (smallNID 2) :+ RegexMatch [re|^foo$|])
                         )
                       :/ Zero
                   ),
      "foo/%history(2)/bar"
        `parsesTo` ( Literal "foo"
                       :/ Directive testAnn (LocationFromHistory 2)
                       :/ Literal "bar"
                   ),
      -- approximation of the pattern we use to detect paths
      "~*/%{Absolute nid}"
        `parsesTo` (Backwards Wild :/ Directive testAnn (Splice "Absolute nid")),
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
        `parsesTo` Directive testAnn (Splice "excludedLargeSystemNodes"),
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
    parsesTo :: Text -> ParsedPath String -> TestTree
    parsesTo input =
      testCase ("parse: " ++ show input)
        . testParserParses (pPath transition) input
    parseFails :: Text -> TestTree
    parseFails input =
      testCase ("parse fails: " ++ show input) $
        testParserFails (pPath transition <* eof) input

test_pDirective :: TestTree
test_pDirective =
  testGroup
    "pDirective"
    [ "%history(23)" `parsesTo` LocationFromHistory 23,
      "%history(-5)" `parsesTo` LocationFromHistory -5,
      "%last" `parsesTo` LocationFromHistory 1,
      "%targets(@000000000002)" `parsesTo` Targets (Absolute (smallNID 2)),
      "%targets(@/foo + #bar)"
        `parsesTo` Targets
          ( (One :/ Literal "foo")
              :+ (Absolute tagsNID :/ Literal "bar")
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
    parsesTo :: Text -> Directive String -> TestTree
    parsesTo input =
      testCase ("parse: " ++ show input)
        . testParserParses (pDirective transition) input
    parseFails :: Text -> TestTree
    parseFails input =
      testCase ("parse fails: " ++ show input) $
        testParserFails (pDirective transition <* eof) input
