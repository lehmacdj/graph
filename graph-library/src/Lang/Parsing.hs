module Lang.Parsing where

import Data.Void
import Data.Char

import Text.Megaparsec
import Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer as L

import Graph (NID)

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

ident :: Parser String
ident = L.lexeme s $ some identChar

stringLiteral :: Parser String
stringLiteral = L.lexeme s $ char '"' >> manyTill L.charLiteral (char '"')

symbol :: String -> Parser String
symbol = L.symbol s

transition :: Parser String
transition = ident <|> stringLiteral

nodeId :: Parser NID
nodeId = L.lexeme s L.decimal

number :: Parser Int
number = L.lexeme s L.decimal

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
commandFrom [x] = symbol x
commandFrom (x:xs) = try (command x) <|> commandFrom xs
