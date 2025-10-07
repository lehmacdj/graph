module Models.Command.Parse
  ( parseCommand,
    test_parseCommand,
  )
where

import Control.Arrow (left)
import Data.Functor
import Graph.SystemNodes (tagsNID)
import Models.Command
import Models.Path.Parse
import Models.Path.ParsedPath
import Models.Path.Simple (Path)
import Models.Path.Simple qualified as Simple
import MyPrelude hiding (some, try)
import Text.Megaparsec (try)
import Utils.Parsing
import Utils.Testing

path' :: Parser (ParsedPath String)
path' = pPath transition

path :: Parser (Path String)
path = convertDirectivesToErrors path'

tpath :: Parser (ParsedPath Text)
tpath = pPath ttransition

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

pCollect :: Parser Command
pCollect = (command "collect" $> Collect) <*> transition

pAddText :: Parser Command
pAddText = (commandFrom ["add-text", "al"] $> AddText) <*> anyText

pV2Path :: Parser Command
pV2Path = (commandFrom ["debug-v2-path", "v2"] $> V2Path) <*> tpath

pCommandTerm :: Parser Command
pCommandTerm =
  try pChangeNode
    <|> try pV2Path
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
    <|> try pCollect
    <|> try pAddText
    <|> try (braces pCommand)

pCommand :: Parser Command
pCommand =
  try (Seq <$> (pCommandTerm `sepBy2` symbol ";"))
    <|> pCommandTerm

parseCommand :: Text -> Either String Command
parseCommand = left errorBundlePretty . runParser pCommand "<interactive>"

test_parseCommand :: TestTree
test_parseCommand =
  testGroup
    "parseCommand"
    [ "t hello/world @"
        `parsesTo` Tag (Literal "hello" Simple.:/ Literal "world") One,
      "tag \"hello/world\" @"
        `parsesTo` Tag (Literal "hello/world") One,
      "tag hello-world @; tag foo-bar @"
        `parsesTo` Seq (twoElemList (Tag (Literal "hello-world") One) (Tag (Literal "foo-bar") One) []),
      "at * {nid; si}"
        `parsesTo` At Wild (Seq (twoElemList NodeId ShowImage [])),
      "{at * nid; si}"
        `parsesTo` Seq (twoElemList (At Wild NodeId) ShowImage []),
      -- these tests would be nice to be able to support eventually but don't
      -- because I hacked ; into the parser haphazardly requiring mandatory
      -- braces surrounding it
      "at * nid; si"
        `parsesTo` Seq (twoElemList (At Wild NodeId) ShowImage []),
      "{nid}" `parsesTo` NodeId,
      "{nid; si}" `parsesTo` Seq (twoElemList NodeId ShowImage []),
      "at #foo nid" `parsesTo` At (Absolute tagsNID Simple.:/ Literal "foo") NodeId,
      -- regression test: this used to parse to t ouch hello-world; incorrectly
      -- not requiring a space between t and ouch
      parseFails "touch hello-world",
      parseFails " t hello/world @;",
      -- @ used to be # here; not failing could cause accidental misuse
      parseFails "t hello/world #"
    ]
  where
    parsesTo :: Text -> Command -> TestTree
    parsesTo input expected =
      testCase ("parse: " ++ show input) $
        Right expected
          @=? parseCommand input
    parseFails :: Text -> TestTree
    parseFails input =
      testCase ("doesn't parse: " ++ show input) $
        isLeft (parseCommand input)
          @? "parseCommand didn't fail"
