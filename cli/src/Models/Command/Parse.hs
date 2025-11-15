module Models.Command.Parse
  ( parseCommand,
    test_parseCommand,
  )
where

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

path :: Parser Path
path = convertDirectivesToErrors pPath

pChangeNode :: Parser Command
pChangeNode = (command "cd" $> ChangeNode) <*> path

pDualize :: Parser Command
pDualize = command "d" $> Dualize

pMake :: Parser Command
pMake = (command "mk" $> Make) <*> path

pMerge :: Parser Command
pMerge = (command "mg" $> Merge) <*> path

pTag :: Parser Command
pTag = (commandFrom ["tag", "t"] $> Tag) <*> path <*> path

pText :: Parser Command
pText = (commandFrom ["text", "mkt"] $> Text) <*> pTransition <*> (pack <$> some anySingle)

pDescribe :: Parser Command
pDescribe = (commandFrom ["describe", "desc"] $> Describe) <*> (pack <$> some anySingle)

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
pDedup = (commandFrom ["dedup", "dd"] $> Dedup) <*> pTransition

pShowImage :: Parser Command
pShowImage = commandFrom ["show-image", "si"] $> ShowImage

pImport :: Parser Command
pImport = (commandFrom [":import", ":i"] $> Import) <*> some anySingle

pImportUrl :: Parser Command
pImportUrl = (commandFrom [":import-url", ":iurl", "wget"] $> ImportUrl) <*> pHttpURI

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

pPreview :: Parser Command
pPreview = (commandFrom ["preview", "ls"] $> Preview) <*> option Wild pPath

pCommandTerm :: Parser Command
pCommandTerm =
  try pChangeNode
    <|> try pPreview
    <|> try pDualize
    <|> try pMake
    <|> try pMerge
    <|> try pTag
    <|> try pText
    <|> try pDescribe
    <|> try pRemove
    <|> try pRemoveNode
    <|> try pAt
    <|> try pDedup
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
      -- "t #something https://example.com/resource"
      --   `parsesTo` Tag
      --     (mkAbsolute tagsNID :/ Literal "something")
      --     (HttpResource [quri|https://example.com/resource|]),
      -- "t https://example.com/resource #something"
      --   `parsesTo` ( Tag
      --                  (mkAbsolute tagsNID :/ Literal "something")
      --                  (HttpResource [quri|https://example.com/resource|])
      --              ),
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
