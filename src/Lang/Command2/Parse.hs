module Lang.Command2.Parse where

import Text.Megaparsec

import Data.Functor

import Lang.Parsing
import Lang.APath
import Lang.APath.Parse
import Lang.Command2

import Control.Arrow (left)

apath :: Parser (APath String)
apath = pAPath transition

pChangeNode :: Parser Command
pChangeNode = (command "cd" $> ChangeNode) <*> apath

pDualize :: Parser Command
pDualize = command "d" $> Dualize

pMake :: Parser Command
pMake = (command "mk" $> Make) <*> apath

pMerge :: Parser Command
pMerge = (command "mg" $> Merge) <*> apath

pClone :: Parser Command
pClone = (command "cl" $> Clone) <*> apath <*> apath

pList :: Parser Command
pList = command "ls" $> List

pAddLinksToFrom :: Parser Command
pAddLinksToFrom = (command "tf" $> AddLinksToFrom) <*> apath <*> apath

pAddLinksFromTo :: Parser Command
pAddLinksFromTo = (command "ft" $> AddLinksFromTo) <*> apath <*> apath

pRemove :: Parser Command
pRemove = (command "rm" $> Remove) <*> apath

pAt :: Parser Command
pAt = (command "at" $> At) <*> apath <*> pCommand

pDump :: Parser Command
pDump = (commandFrom [":dump", ":d"] $> Dump) <*> some anySingle

pLoad :: Parser Command
pLoad = (commandFrom [":load", ":l"] $> Load) <*> some anySingle

pNodeId :: Parser Command
pNodeId = command "nid" $> NodeId

pDebug :: Parser Command
pDebug = commandFrom [":debug", ":d"] $> Debug

pShowImage :: Parser Command
pShowImage = commandFrom ["show-image", "si"] $> ShowImage

pImport :: Parser Command
pImport = (commandFrom [":import", ":i"] $> Import) <*> some anySingle

pImportUrl :: Parser Command
pImportUrl = (commandFrom [":import-url", ":iurl", "wget"] $> ImportUrl) <*> some anySingle

pCommand :: Parser Command
pCommand =
  try pChangeNode
  <|> try pDualize
  <|> try pMake
  <|> try pMerge
  <|> try pClone
  <|> try pList
  <|> try pAddLinksToFrom
  <|> try pAddLinksFromTo
  <|> try pRemove
  <|> try pAt
  <|> try pDump
  <|> try pLoad
  <|> try pNodeId
  <|> try pDebug
  <|> try pShowImage
  <|> try pImport
  <|> try pImportUrl

parseCommand :: String -> Either String Command
parseCommand = left show . runParser pCommand "<interactive>"
