module Lang.Parsing where

import Control.Monad.Fail
import Data.Char
import Data.Void
import Graph (NID)
import qualified Graph.Types.NID as BigNID
import MyPrelude hiding (some, try)
import Text.Megaparsec
import Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer as L

type Parser = Parsec Void String

s :: Parser ()
s = L.space space1 empty empty

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

symbol :: String -> Parser String
symbol = L.symbol s

ident :: Parser String
ident = L.lexeme s $ some identChar

stringLiteral :: Parser String
stringLiteral = L.lexeme s $ char '"' >> manyTill L.charLiteral (char '"')

transition :: Parser String
transition = ident <|> stringLiteral

-- | chars allowed in base64url encoding
base64Chars :: String
base64Chars = ['a' .. 'z'] ++ ['A' .. 'Z'] ++ ['0' .. '9'] ++ "_-"

bigNID :: Parser BigNID.NID
bigNID = L.lexeme s p
  where
    p = do
      chars <- replicateM 32 $ oneOf base64Chars :: Parser String
      case readMay chars of
        Just nid -> pure nid
        Nothing ->
          fail "couldn't parse NID; expected 32 length base64url encoded string"

nodeId :: Parser NID
nodeId = L.lexeme s L.decimal

positiveNumber :: Parser Int
positiveNumber = L.lexeme s L.decimal

number :: Parser Int
number = do
  minusSign <- optional (symbol "-")
  n <- positiveNumber
  case minusSign of
    Just _ -> pure $ negate n
    Nothing -> pure n

anyText :: Parser Text
anyText = pack <$> some anySingle

parens :: Parser a -> Parser a
parens = between (symbol "(") (symbol ")")

-- these all lookahead for space to ensure that arguments are space separated
-- the intended paradigm for commands is that commands all require a following
-- space, then perform their own logic for argument separation as needed.
-- All other parsers are expected to consume any following space
-- indiscriminately.

command :: String -> Parser String
command i = L.lexeme s (string i <* lookAhead (space1 <|> eof))

commandFrom :: [String] -> Parser String
commandFrom [] = empty
commandFrom [x] = command x
commandFrom (x : xs) = try (command x) <|> commandFrom xs
