module Lang.Command.Parse where

import Control.Arrow (left)
import Data.Functor
import Lang.APath
import Lang.APath.Parse
import Lang.Command
import Lang.Parsing
import Text.Megaparsec

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
pClone = (command "cl" $> Clone) <*> apath <*> transition

pList :: Parser Command
pList = command "ls" $> ListOut

pQuery :: Parser Command
pQuery = (commandFrom ["q", "query"] $> Query) <*> apath <*> transition

pTag :: Parser Command
pTag = (commandFrom ["tag", "t"] $> Tag) <*> apath <*> apath

pRemove :: Parser Command
pRemove = (command "rm" $> Remove) <*> apath

pRemoveNode :: Parser Command
pRemoveNode = (command "rmnf" $> RemoveNode) <*> apath

pAt :: Parser Command
pAt = (command "at" $> At) <*> apath <*> pCommand

pLoad :: Parser Command
pLoad = (commandFrom [":load", ":l"] $> Load) <*> some anySingle

pNodeId :: Parser Command
pNodeId = command "nid" $> NodeId

pDebug :: Parser Command
pDebug = commandFrom [":debug", ":d"] $> Debug

pDedup :: Parser Command
pDedup = (commandFrom ["dedup", "dd"] $> Dedup) <*> transition

pShowImage :: Parser Command
pShowImage = commandFrom ["show-image", "si"] $> ShowImage

pImport :: Parser Command
pImport = (commandFrom [":import", ":i"] $> Import) <*> some anySingle

pImportUrl :: Parser Command
pImportUrl = (commandFrom [":import-url", ":iurl", "wget"] $> ImportUrl) <*> some anySingle

pCheck :: Parser Command
pCheck = command "fsck" $> Check

pFix :: Parser Command
pFix = command "fix" $> Fix

pMove :: Parser Command
pMove = (command "mv" $> Move) <*> apath <*> apath

pRename :: Parser Command
pRename = (command "rn" $> Rename) <*> apath <*> apath

pEdit :: Parser Command
pEdit = command "vi" $> Edit

pCommand :: Parser Command
pCommand =
  try pChangeNode
    <|> try pDualize
    <|> try pMake
    <|> try pMerge
    <|> try pClone
    <|> try pList
    <|> try pQuery
    <|> try pTag
    <|> try pRemove
    <|> try pRemoveNode
    <|> try pAt
    <|> try pDedup
    <|> try pLoad
    <|> try pNodeId
    <|> try pDebug
    <|> try pShowImage
    <|> try pImport
    <|> try pImportUrl
    <|> try pCheck
    <|> try pFix
    <|> try pMove
    <|> try pRename
    <|> try pEdit

parseCommand :: String -> Either String Command
parseCommand = left errorBundlePretty . runParser pCommand "<interactive>"
