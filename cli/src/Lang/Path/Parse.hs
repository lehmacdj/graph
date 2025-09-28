-- | Megaparsec support for parsing strings that make up paths.
-- Generic over the custom error type, it doesn't throw any.
module Lang.Path.Parse
  ( pPath,
    pPath',
    pathTerm,
    test_pPath,
  )
where

import Control.Monad.Combinators.Expr
import Data.Functor
import Graph.SystemNodes (tagsNID)
import Lang.Parsing
import Lang.ParsingSpec
import Models.NID
import Models.Path.ParsedPath
import MyPrelude hiding (try)
import TestPrelude hiding (try)
import Text.Megaparsec (try)
import Text.Megaparsec.Char (char)

pathTerm' :: Parser NID -> Parser t -> Parser (ParsedPath t)
pathTerm' pNID pTransition =
  try (Absolute <$> pNID)
    <|> try ((Absolute tagsNID :/) . Literal <$> (char '#' *> pTransition))
    <|> (symbol "@" $> One)
    <|> (symbol "*" $> Wild)
    <|> (symbol "!" $> Zero)
    <|> (RegexMatch <$> try pRegex)
    <|> (Literal <$> pTransition)
    <|> parens (pPath' pNID pTransition)

pRegex :: Parser CheckedRegex
pRegex = do
  str <- symbol "re" *> stringLiteral
  str
    & compileRegex . encodeUtf8 . pack
    & codiagonal . bimap fail pure

pathTerm :: Parser t -> Parser (ParsedPath t)
pathTerm = pathTerm' pFullNID

binary :: String -> (ParsedPath t -> ParsedPath t -> ParsedPath t) -> Operator Parser (ParsedPath t)
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
      "!" `parsesTo` Zero,
      "foo" `parsesTo` Literal "foo",
      "@000000000002" `parsesTo` Absolute (smallNID 2),
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
      "foo/bar&baz+qux/!" `parsesTo` (Literal "foo" :/ Literal "bar" :& Literal "baz" :+ Literal "qux" :/ Zero),
      "foo/bar&(baz+qux)"
        `parsesTo` (Literal "foo" :/ Literal "bar" :& (Literal "baz" :+ Literal "qux")),
      "foo/(bar&baz+qux)/!" `parsesTo` (Literal "foo" :/ (Literal "bar" :& Literal "baz" :+ Literal "qux") :/ Zero),
      parseFails "foo/bar&",
      parseFails "foo/bar&+qux",
      parseFails "foo+",
      parseFails "+foo",
      parseFails "~",
      parseFails "foo~bar",
      parseFails "re\""
    ]
  where
    parsesTo :: String -> ParsedPath String -> TestTree
    parsesTo input = testCase ("parse: " ++ show input) . testParserParses (pPath transition) input
    parseFails input =
      testCase ("parse fails: " ++ show input) $ testParserFails (pPath transition <* eof) input
