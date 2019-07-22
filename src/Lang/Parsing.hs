module Lang.Parsing where

import Data.Void

import Text.Megaparsec
import Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer as L

import Graph (Id)

type Parser = Parsec Void String

s :: Parser ()
s = L.space space1 empty empty

identChar :: Parser Char
identChar = alphaNumChar <|> oneOf "-_"

ident :: Parser String
ident = some identChar

stringLiteral :: Parser String
stringLiteral = char '"' >> manyTill L.charLiteral (char '"')

symbol :: String -> Parser String
symbol = L.symbol s

-- these all lookahead for space to ensure that arguments are space separated

command :: String -> Parser String
command i = L.lexeme s (string i <* lookAhead (space1 <|> eof))

nodeId :: Parser Id
nodeId = L.lexeme s (L.decimal <* lookAhead (space1 <|> eof))

transition :: Parser String
transition = L.lexeme s ((ident <|> stringLiteral) <* lookAhead (space1 <|> eof))

commandFrom :: [String] -> Parser String
commandFrom [] = empty
commandFrom [x] = symbol x
commandFrom (x:xs) = try (command x) <|> commandFrom xs
