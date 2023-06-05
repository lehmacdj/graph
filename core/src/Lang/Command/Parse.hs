module Lang.Command.Parse where

import Control.Arrow (left)
import Data.Functor
import Lang.Command
import Lang.Parsing
import Lang.Path
import Lang.Path.Parse
import MyPrelude hiding (some, try)
import Text.Megaparsec

path :: Parser (Path String)
path = pPath transition

pChangeNode :: Parser Command
pChangeNode = (command "cd" $> ChangeNode) <*> path

pDualize :: Parser Command
pDualize = command "d" $> Dualize

pMake :: Parser Command
pMake = (command "mk" $> Make) <*> path

pMerge :: Parser Command
pMerge = (command "mg" $> Merge) <*> path

pClone :: Parser Command
pClone = (command "cl" $> Clone) <*> path <*> transition

pList :: Parser Command
pList = command "ls" $> ListOut

pQuery :: Parser Command
pQuery = (commandFrom ["q", "query"] $> Query) <*> path <*> transition

pTag :: Parser Command
pTag = (commandFrom ["tag", "t"] $> Tag) <*> path <*> path

pText :: Parser Command
pText = (commandFrom ["text", "mkt"] $> Text) <*> transition <*> some anySingle

pDescribe :: Parser Command
pDescribe = (commandFrom ["describe", "desc"] $> Describe) <*> some anySingle

pRemove :: Parser Command
pRemove = (command "rm" $> Remove) <*> path

pRemoveNode :: Parser Command
pRemoveNode = (command "rmnf" $> RemoveNode) <*> path

pAt :: Parser Command
pAt = (command "at" $> At) <*> path <*> pCommandTerm

pNodeId :: Parser Command
pNodeId = command "nid" $> NodeId

pDebug :: Parser Command
pDebug = commandFrom [":debug", ":d"] $> Debug

pDedup :: Parser Command
pDedup = (commandFrom ["dedup", "dd"] $> Dedup) <*> transition

pFlatten :: Parser Command
pFlatten = (command "flatten" $> Flatten) <*> transition

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
pMove = (command "mv" $> Move) <*> path <*> path

pRename :: Parser Command
pRename = (command "rn" $> Rename) <*> path <*> path

pAlias :: Parser Command
pAlias = (commandFrom ["alias", "cp"] $> Alias) <*> path <*> path

pEdit :: Parser Command
pEdit = command "vi" $> Edit

pBack :: Parser Command
pBack = (command "back" $> Back) <*> number

pMaterialize :: Parser Command
pMaterialize = (command "materialize" $> Materialize) <*> some anySingle

pExec :: Parser Command
pExec = (commandFrom ["exec", "x"] $> Exec) <*> path

pCollect :: Parser Command
pCollect = (command "collect" $> Collect) <*> transition

pAddText :: Parser Command
pAddText = (commandFrom ["add-text", "al"] $> AddText) <*> anyText

pCommandTerm :: Parser Command
pCommandTerm =
  try pChangeNode
    <|> try pDualize
    <|> try pMake
    <|> try pMerge
    <|> try pClone
    <|> try pList
    <|> try pQuery
    <|> try pTag
    <|> try pText
    <|> try pDescribe
    <|> try pRemove
    <|> try pRemoveNode
    <|> try pAt
    <|> try pDedup
    <|> try pFlatten
    <|> try pNodeId
    <|> try pDebug
    <|> try pShowImage
    <|> try pImport
    <|> try pImportUrl
    <|> try pCheck
    <|> try pFix
    <|> try pMove
    <|> try pRename
    <|> try pAlias
    <|> try pEdit
    <|> try pBack
    <|> try pMaterialize
    <|> try pExec
    <|> try pCollect
    <|> try pAddText
    <|> try (braces pCommand)

pCommand :: Parser Command
pCommand =
  try (Seq <$> (pCommandTerm `sepBy2` symbol ";"))
    <|> pCommandTerm

parseCommand :: String -> Either String Command
parseCommand = left errorBundlePretty . runParser pCommand "<interactive>"
