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
import Lang.Path
import Models.NID
import MyPrelude hiding (try)
import TestPrelude hiding (try)
import Text.Megaparsec (try, (<?>))
import Text.Megaparsec.Char (char)

pathTerm' :: Parser NID -> Parser t -> Parser (Path t)
pathTerm' pNID pTransition =
  try (Absolute <$> pNID)
    <|> try ((Absolute tagsNID :/) . Literal <$> (char '#' *> pTransition))
    <|> (symbol "@" $> One)
    <|> (symbol "*" $> Wild)
    -- <|> (symbol "!" $> Zero)
    <|> (Literal <$> pTransition)
    <|> parens (pPath' pNID pTransition)

pathTerm :: Parser t -> Parser (Path t)
pathTerm = pathTerm' pFullNID

binary :: String -> (Path t -> Path t -> Path t) -> Operator Parser (Path t)
binary name f = InfixL (f <$ symbol name)

table :: [[Operator Parser (Path t)]]
table =
  [ [binary "/" (:/)],
    [binary "&" (:&)],
    [binary "+" (:+)]
  ]

pPath' :: Parser NID -> Parser t -> Parser (Path t)
pPath' pNID pTransition = do
  path <- makeExprParser (pathTerm' pNID pTransition) table
  guard (isValidPath path) <?> "valid path"
  pure path

pPath :: Parser t -> Parser (Path t)
pPath = pPath' pFullNID

test_pPath :: TestTree
test_pPath =
  testGroup
    "pPath"
    [ "#foo" `parsesTo` (Absolute tagsNID :/ Literal "foo"),
      "@" `parsesTo` One,
      "*" `parsesTo` Wild,
      -- "!" `parsesTo` Zero,
      "foo" `parsesTo` Literal "foo",
      "foo/bar" `parsesTo` (Literal "foo" :/ Literal "bar"),
      "(foo/bar)/baz" `parsesTo` (Literal "foo" :/ Literal "bar" :/ Literal "baz"),
      "foo/(bar/baz)" `parsesTo` (Literal "foo" :/ (Literal "bar" :/ Literal "baz")),
      "foo / bar & baz" `parsesTo` (Literal "foo" :/ Literal "bar" :& Literal "baz"),
      "foo/bar&baz+qux"
        `parsesTo` (Literal "foo" :/ Literal "bar" :& Literal "baz" :+ Literal "qux"),
      -- "foo/bar&baz+qux/!" `parsesTo` (Literal "foo" :/ Literal "bar" :& Literal "baz" :+ Literal "qux" :/ Zero),
      "foo/bar&(baz+qux)"
        `parsesTo` (Literal "foo" :/ Literal "bar" :& (Literal "baz" :+ Literal "qux")),
      -- "foo/(bar&baz+qux)/!" `parsesTo` (Literal "foo" :/ (Literal "bar" :& Literal "baz" :+ Literal "qux") :/ Zero),
      parseFails "foo/bar&",
      parseFails "foo/bar&+qux"
    ]
  where
    parsesTo :: String -> Path String -> TestTree
    parsesTo input = testCase ("parse: " ++ show input) . testParserParses (pPath transition) input
    parseFails input =
      testCase ("parse fails: " ++ show input) $ testParserFails (pPath transition) input
