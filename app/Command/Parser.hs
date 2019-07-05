module Command.Parser
  ( pCommand
  , parseCommand
  ) where

import Data.Void
import Data.Functor
import Control.Arrow (left)

import Text.Megaparsec
import Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer as L

import Data.Graph (Id)

import Command

type Parser = Parsec Void String

s :: Parser ()
s = L.space space1 empty empty

-- these primitive parsers both require lookAhead space1 to separate their arguments

symbol :: String -> Parser String
symbol i = L.lexeme s (string i <* lookAhead (space1 <|> eof))

identChar :: Parser Char
identChar = alphaNumChar

ident :: Parser String
ident = L.lexeme s (some alphaNumChar <* lookAhead (space1 <|> eof))

stringLiteral :: Parser String
stringLiteral = L.lexeme s (char '"' >> manyTill L.charLiteral (char '"'))

symbolFrom :: [String] -> Parser String
symbolFrom [] = empty
symbolFrom [x] = symbol x
symbolFrom (x:xs) = try (symbol x) <|> symbolFrom xs

pCommand :: Parser Command
pCommand
  = try pQuit
  <|> try pChangeNode
  <|> try pDualize
  <|> try pMakeNode
  <|> try pNodeId
  <|> try pListOut
  <|> try pListIn
  <|> try pAddEdgeTo
  <|> try pAddEdgeFrom
  <|> try pDump
  <|> try pDebug
  <|> try pLoad
  <|> try pGoto
  <|> try pRemoveEdgeOut
  <|> try pRemoveEdgeIn
  <|> try pCloneNode

transition :: Parser String
transition = ident <|> stringLiteral

nodeId :: Parser Id
nodeId = L.lexeme s (L.decimal <* lookAhead (space1 <|> eof))

pQuit :: Parser Command
pQuit = symbolFrom [":quit", ":q"] $> Quit

pChangeNode :: Parser Command
pChangeNode = (symbolFrom ["change-node", "cd", "cn"] $> ChangeNode) <*> transition

pDualize :: Parser Command
pDualize = symbolFrom ["dualize", "d"] $> Dualize

pMakeNode :: Parser Command
pMakeNode = (symbolFrom ["mknode", "mn"] $> MakeNode) <*> transition

pNodeId :: Parser Command
pNodeId = symbol "nid" $> NodeId

pListOut :: Parser Command
pListOut = symbolFrom ["ls", "lso"] $> ListOut

pListIn :: Parser Command
pListIn = symbol "lsi" $> ListIn

pAddEdgeTo :: Parser Command
pAddEdgeTo = (symbolFrom ["edge-to", "et"] $> AddEdgeTo) <*> nodeId <*> transition

pAddEdgeFrom :: Parser Command
pAddEdgeFrom = (symbolFrom ["edge-from", "ef"] $> AddEdgeFrom) <*> nodeId <*> transition

pDump :: Parser Command
pDump = (symbolFrom [":dump", ":d"] $> Dump) <*> some anySingle

pLoad :: Parser Command
pLoad = (symbolFrom [":load", ":l"] $> Load) <*> some anySingle

pGoto :: Parser Command
pGoto = (symbolFrom ["goto", "g"] $> Goto) <*> nodeId

pDebug :: Parser Command
pDebug = symbolFrom [":debug", ":d"] $> Debug

pRemoveEdgeOut :: Parser Command
pRemoveEdgeOut = (symbolFrom ["remove-out", "rmo", "rm"] $> RemoveEdgeOut) <*> transition

pRemoveEdgeIn :: Parser Command
pRemoveEdgeIn = (symbolFrom ["remove-in", "rmi"] $> RemoveEdgeIn) <*> transition

pCloneNode :: Parser Command
pCloneNode = (symbolFrom ["clone-node", "cp"] $> CloneNode) <*> nodeId

parseCommand :: String -> Either String Command
parseCommand = left show . runParser pCommand "<interactive>"
