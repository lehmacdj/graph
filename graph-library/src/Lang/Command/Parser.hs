module Lang.Command.Parser
  ( pCommand
  , parseCommand
  ) where

import Data.Functor
import Control.Arrow (left)

import Text.Megaparsec

import Lang.Command
import Lang.Parsing

pCommand :: Parser Command
pCommand
  = try pChangeNode
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
  <|> try pShowImage
  <|> try pSetBinaryData
  <|> try pImport
  <|> try pImportUrl

pChangeNode :: Parser Command
pChangeNode = (commandFrom ["change-node", "cd", "cn"] $> ChangeNode) <*> transition

pDualize :: Parser Command
pDualize = commandFrom ["dualize", "d"] $> Dualize

pMakeNode :: Parser Command
pMakeNode = (commandFrom ["mknode", "mn"] $> MakeNode) <*> transition

pNodeId :: Parser Command
pNodeId = command "nid" $> NodeId

pListOut :: Parser Command
pListOut = commandFrom ["ls", "lso"] $> ListOut

pListIn :: Parser Command
pListIn = command "lsi" $> ListIn

pAddEdgeTo :: Parser Command
pAddEdgeTo = (commandFrom ["edge-to", "et"] $> AddEdgeTo) <*> nodeId <*> transition

pAddEdgeFrom :: Parser Command
pAddEdgeFrom = (commandFrom ["edge-from", "ef"] $> AddEdgeFrom) <*> nodeId <*> transition

pDump :: Parser Command
pDump = (commandFrom [":dump", ":d"] $> Dump) <*> some anySingle

pLoad :: Parser Command
pLoad = (commandFrom [":load", ":l"] $> Load) <*> some anySingle

pGoto :: Parser Command
pGoto = (commandFrom ["goto", "g"] $> Goto) <*> nodeId

pDebug :: Parser Command
pDebug = commandFrom [":debug", ":d"] $> Debug

pRemoveEdgeOut :: Parser Command
pRemoveEdgeOut = (commandFrom ["remove-out", "rmo", "rm"] $> RemoveEdgeOut) <*> transition

pRemoveEdgeIn :: Parser Command
pRemoveEdgeIn = (commandFrom ["remove-in", "rmi"] $> RemoveEdgeIn) <*> transition

pCloneNode :: Parser Command
pCloneNode = (commandFrom ["clone-node", "cp"] $> CloneNode) <*> nodeId

pShowImage :: Parser Command
pShowImage = commandFrom ["show-image", "si"] $> ShowImage

pSetBinaryData :: Parser Command
pSetBinaryData = (commandFrom ["set-binary-data", "sbd"] $> SetBinaryData) <*> some anySingle

pImport :: Parser Command
pImport = (commandFrom [":import", ":i"] $> Import) <*> some anySingle

pImportUrl :: Parser Command
pImportUrl = (commandFrom [":import-url", ":iurl", "wget"] $> ImportUrl) <*> some anySingle

parseCommand :: String -> Either String Command
parseCommand = left show . runParser pCommand "<interactive>"
