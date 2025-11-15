module Utils.Parsing
  ( module X,
    module Utils.Parsing,
  )
where

import Data.Char
import Models.Path
import MyPrelude hiding (many, some, try)
import Text.Megaparsec (try)
import Text.Megaparsec qualified as Megaparsec
import Text.Megaparsec.Char
import Text.Megaparsec.Char as X (string)
import Text.Megaparsec.Char.Lexer qualified as L
import Utils.Parsing.Common as X
import Utils.Testing

s :: Parser ()
s = L.space space1 empty empty

whitespace :: Parser ()
whitespace = s

lexeme :: Parser a -> Parser a
lexeme = L.lexeme s

-- | The additional characters that are allowed in a transition without quoting
-- it.
-- extraIdentChars :: [Char]
extraIdentChars :: String
extraIdentChars = "-._'"

isIdentChar :: Char -> Bool
isIdentChar = (||) <$> isAlphaNum <*> (`elem` extraIdentChars)

identChar :: Parser Char
identChar = alphaNumChar <|> oneOf extraIdentChars

symbol :: Text -> Parser Text
symbol = L.symbol s

ident :: Parser Text
ident = L.lexeme s $ pack <$> some identChar

stringLiteral :: Parser Text
stringLiteral =
  label "<string literal>" $
    L.lexeme s $
      pack <$> (char '"' >> manyTill L.charLiteral (char '"'))

positiveNumber :: Parser Int
positiveNumber = L.lexeme s L.decimal

number :: Parser Int
number = label "<integer>" do
  minusSign <- optional (symbol "-")
  n <- positiveNumber
  case minusSign of
    Just _ -> pure $ negate n
    Nothing -> pure n

anyText :: Parser Text
anyText = pack <$> some anySingle

parens :: Parser a -> Parser a
parens = between (symbol "(") (symbol ")")

braces :: Parser a -> Parser a
braces = between (symbol "{") (symbol "}")

brackets :: Parser a -> Parser a
brackets = between (symbol "[") (symbol "]")

-- these all lookahead for space to ensure that arguments are space separated
-- the intended paradigm for commands is that commands all require a following
-- space or ; (when in a Seq) or } (when in braces), then perform their own
-- logic for argument separation as needed. All other parsers are expected to
-- consume any following space indiscriminately.

command :: Text -> Parser Text
command i = L.lexeme s (string i <* lookAhead (space1 <|> eof <|> ((symbol ";" <|> symbol "}") $> ())))

commandFrom :: [Text] -> Parser Text
commandFrom [] = empty
commandFrom [x] = command x
commandFrom (x : xs) = try (command x) <|> commandFrom xs

sepBy2 :: Parser a -> Parser sep -> Parser (TwoElemList a)
sepBy2 p sep = twoElemList <$> p <*> (sep *> p) <*> many (sep *> p)

via1 :: Parser a -> Parser sep -> Parser (a, NonNull [(sep, a)])
via1 p sep = do
  initial <- p
  remainder <- some ((,) <$> sep <*> p)
  pure (initial, impureNonNull remainder)

convertDirectivesToErrors ::
  Parser (Path' 'WithDirectives) ->
  Parser (Path' 'Prenormal)
convertDirectivesToErrors p = handleDirectivesWith interpretDirective =<< p
  where
    interpretDirective SourceRange {..} _ = do
      customFailure (IllegalDirective startPos endPos)

getOffset :: Parser Int
getOffset = do
  opts <- ask
  case opts of
    ParserOptions {useFakeSourceRanges = True} -> pure 0
    ParserOptions {useFakeSourceRanges = False} -> Megaparsec.getOffset

getSourcePos :: Parser SourcePos
getSourcePos = do
  opts <- ask
  case opts of
    ParserOptions {useFakeSourceRanges = True} -> pure $ initialPos "<test>"
    ParserOptions {useFakeSourceRanges = False} -> Megaparsec.getSourcePos

withSourceRange :: Parser a -> Parser (SourceRange, a)
withSourceRange p = do
  start <- getSourcePos
  a <- p
  end <- getSourcePos
  pure (SourceRange start end, a)

debugParser :: (Eq a, Show a) => Parser a -> Text -> IO ()
debugParser parser input = do
  case runParserTest parser input of
    Right x -> say $ "Parser succeeded with: " ++ tshow x
    Left err -> say $ "Parser failed with:\n" ++ pack (errorBundlePretty err)

unit_command_emptyStillRequiresSpace :: Assertion
unit_command_emptyStillRequiresSpace = testParserFails (command "") "1"

unit_command_eofTerminated :: Assertion
unit_command_eofTerminated = testParserParses (command "t") "t" "t"

unit_command_unterminated :: Assertion
unit_command_unterminated = testParserFails (command "t") "tf"

unit_command_incomplete :: Assertion
unit_command_incomplete = testParserFails (command "tf") "t"

unit_command_spaceTerminated :: Assertion
unit_command_spaceTerminated =
  testParserParses (command "t" <* string "blech") "t blech" "t"
