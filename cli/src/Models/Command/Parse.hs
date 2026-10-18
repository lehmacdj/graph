module Models.Command.Parse
  ( parseCommand,
    parseCommandLine,
    commandNames,
    importNames,
    startsCommand,
    test_parseCommand,
    test_startsCommand,
  )
where

import Data.Char (isDigit)
import Data.Functor
import Graph.SystemNodes (tagsNID)
import Models.Command
import Models.NID (nidDigits, unsafeNID)
import Models.Path.Parse
import Models.Path.ParsedPath
import MyPrelude hiding (some, try)
import Text.Megaparsec (try)
import Text.Megaparsec.Char (char)
import Utils.Base62 (isBase62Char)
import Utils.Parsing
import Utils.Testing

-- | Every command's path keeps its directives; they are expanded when the
-- command runs (see 'Graph.Command.expandDirectives').
path :: Parser ParsedPath
path = pPath

-- | Splices can never work in the CLI, so a command line containing one is
-- rejected as a whole, before anything runs (checked after parsing so that
-- the error isn't lost when @;@ backtracks).
rejectSplices :: Command -> Parser Command
rejectSplices c = case concatMap splices (commandPaths c) of
  [] -> pure c
  (range, expr) : _ -> customFailure (SpliceInCommand range expr)
  where
    splices = \case
      Backwards q -> splices q
      l :/ r -> splices l ++ splices r
      Union _ l r -> splices l ++ splices r
      DefaultUnion _ l r -> splices l ++ splices r
      l :& r -> splices l ++ splices r
      Directive range (Splice expr) -> [(range, expr)]
      Directive _ (Targets q) -> splices q
      Directive _ (Override _ q) -> splices q
      _ -> []

-- | Every path a command (and the commands it contains) takes.
commandPaths :: Command -> [ParsedPath]
commandPaths = \case
  ChangeNode p -> [p]
  Make _ p -> [p]
  Merge args -> map mergeArgumentPath (toList args)
  Tag _ p q -> [p, q]
  Remove _ p -> [p]
  RemoveNode _ p -> [p]
  At p c -> p : commandPaths c
  Move p q -> [p, q]
  Rename _ p q -> [p, q]
  Copy p q -> [p, q]
  Alias _ p q -> [p, q]
  Seq cs -> concatMap commandPaths (toList cs)
  Preview p -> [p]
  Dualize -> []
  Text _ _ -> []
  Describe _ -> []
  Dedup _ -> []
  NodeId -> []
  Debug -> []
  ShowImage -> []
  Import _ -> []
  Check -> []
  Fix _ _ -> []
  Edit -> []
  Back _ -> []

pChangeNode :: Parser Command
pChangeNode = ChangeNode <$> path

pDualize :: Parser Command
pDualize = pure Dualize

pMake :: Parser Command
pMake = do
  -- the option is gone; without this its name would be read as a label
  gone <- optional (try (command "--allow-orphans"))
  when (isJust gone) . fail $
    "mk has no --allow-orphans any more: every node that mk creates is "
      <> "connected to the graph, so write mk <path>"
  Make <$> pMaxRepairs "mk" <*> path

-- | @--max-repairs N@ before the paths of @mk@ or @tag@ (whose name is the
-- argument): how many walks the command may make of its path, a positive
-- number written in decimal digits or @unlimited@.
pMaxRepairs :: Text -> Parser MaxRepairs
pMaxRepairs name = do
  given <- optional do
    _ <- try (command "--max-repairs")
    offset <- getOffset
    value <- ident <?> "a positive number or unlimited"
    either (failAt offset) pure (repairsLimit value)
  -- without this a second one would be read as a label
  offset <- getOffset
  twice <- optional (try (command "--max-repairs"))
  when (isJust twice) . failAt offset $
    name <> " takes --max-repairs at most once"
  pure given
  where
    -- the error is shown at the option or its value, not after it
    failAt offset =
      parseError . FancyError offset . singletonSet . ErrorFail . unpack
    repairsLimit value
      | value == "unlimited" = Right UnlimitedRepairs
      | not (null value) && all isDigit value =
          case readMay value :: Maybe Integer of
            Just n
              | n > toInteger (maxBound :: Int) ->
                  Left $
                    "The value "
                      <> value
                      <> " for --max-repairs is too large; write unlimited "
                      <> "for no limit"
              | n > 0 -> Right (AtMostRepairs (fromInteger n))
            _ -> Left unknown
      | otherwise = Left unknown
      where
        unknown =
          "Unknown value "
            <> value
            <> " for --max-repairs; it accepts a positive number or unlimited"

-- | @mg p1 p2 …@: at least two paths; @--keep@ before at most one of them
-- marks the node that survives.
pMerge :: Parser Command
pMerge = do
  args <-
    twoElemList
      <$> pMergeArgument
      <*> pMergeArgument
      <*> MyPrelude.many pMergeArgument
  when (length (filter isKept (toList args)) > 1) $
    fail "mg takes --keep before at most one of its arguments"
  pure (Merge args)
  where
    pMergeArgument =
      (try (command "--keep" $> Kept) <*> path) <|> (Merged <$> path)

pTag :: Parser Command
pTag = Tag <$> pMaxRepairs "tag" <*> path <*> path

pText :: Parser Command
pText = Text <$> pTransition <*> (pack <$> some anySingle)

pDescribe :: Parser Command
pDescribe = Describe . pack <$> some anySingle

-- | @--dry-run@ before the path of @rm@ or @rmnf@: list what would be
-- deleted and delete nothing.
pRemoveMode :: Parser RemoveMode
pRemoveMode =
  try (command "--dry-run" $> ListDeletions) <|> pure DeleteSelected

pRemove :: Parser Command
pRemove = Remove <$> pRemoveMode <*> path

pRemoveNode :: Parser Command
pRemoveNode = RemoveNode <$> pRemoveMode <*> path

pAt :: Parser Command
pAt = At <$> path <*> pCommandTerm

pNodeId :: Parser Command
pNodeId = pure NodeId

pDebug :: Parser Command
pDebug = pure Debug

pDedup :: Parser Command
pDedup = Dedup <$> pTransition

pShowImage :: Parser Command
pShowImage = pure ShowImage

pImport :: Parser Command
pImport = Import <$> some anySingle

pCheck :: Parser Command
pCheck = pure Check

-- | @fix [--dry-run] [\@nid…]@: the nodes are whole ids, since a node that
-- fix is to rebuild has no file to look a shorter one up in. An id is taken
-- in each form that @fsck@ shows one in: @\@<id>@, or the name of its file,
-- @<id>.json@; and bare.
pFix :: Parser Command
pFix =
  Fix
    <$> (try (command "--dry-run" $> ListRepairs) <|> pure ApplyRepairs)
    <*> ( MyPrelude.many nid <&> \case
            [] -> AllDamage
            nids -> DamageOf (setFromList nids)
        )
  where
    -- fails without reading anything where no id begins, which ends the ids
    nid = lexeme do
      digits <-
        (char '@' *> takeWhileP Nothing isBase62Char)
          <|> takeWhile1P Nothing isBase62Char
      unless (length digits == nidDigits) . fail $
        "fix takes whole node ids, "
          <> show nidDigits
          <> " letters and digits, as fsck shows them: @<id> or <id>.json"
      unsafeNID digits <$ optional (string ".json")

pMove :: Parser Command
pMove = Move <$> path <*> path

-- | @--all-parents@ lets @rn@/@cl@ create the edge under every parent the
-- destination prefix resolves to.
pDestinationParents :: Parser DestinationParents
pDestinationParents =
  try (command "--all-parents" $> AllParents) <|> pure OneParent

pRename :: Parser Command
pRename = Rename <$> pDestinationParents <*> path <*> path

pCopy :: Parser Command
pCopy = Copy <$> path <*> path

pAlias :: Parser Command
pAlias = Alias <$> pDestinationParents <*> path <*> path

pEdit :: Parser Command
pEdit = pure Edit

pBack :: Parser Command
pBack = Back <$> number

pPreview :: Parser Command
pPreview = Preview <$> option Wild path

-- | Every command: the names it is written with, and the parser of what
-- follows the name. 'pCommandTerm' tries them in this order; completion
-- offers their names ('commandNames').
commandSyntaxes :: [([Text], Parser Command)]
commandSyntaxes =
  [ (["cd"], pChangeNode),
    (["preview", "ls"], pPreview),
    (["d"], pDualize),
    (["mk"], pMake),
    (["mg"], pMerge),
    (["tag", "t"], pTag),
    (["text", "mkt"], pText),
    (["describe", "desc"], pDescribe),
    (["rm"], pRemove),
    (["rmnf"], pRemoveNode),
    (["at"], pAt),
    (["dedup", "dd"], pDedup),
    (["nid"], pNodeId),
    ([":debug", ":d"], pDebug),
    (["show-image", "si"], pShowImage),
    (importNames, pImport),
    (["fsck"], pCheck),
    (["fix"], pFix),
    (["mv"], pMove),
    (["rn"], pRename),
    (["cp"], pCopy),
    (["alias", "clone", "cl"], pAlias),
    (["vi"], pEdit),
    (["back"], pBack)
  ]

-- | The names of the command that imports a directory, whose argument is a
-- file path.
importNames :: [Text]
importNames = [":import", ":i"]

-- | The name of every command, in the order of 'commandSyntaxes'.
commandNames :: [Text]
commandNames = concatMap fst commandSyntaxes

pCommandTerm :: Parser Command
pCommandTerm =
  choice [try (commandFrom names *> rest) | (names, rest) <- commandSyntaxes]
    <|> try (braces pCommand)

-- | One command, or several separated by @;@. A command that can't be
-- parsed is reported where it is: after a @;@ nothing goes back to read the
-- line as only the commands before it.
pCommand :: Parser Command
pCommand =
  (pCommandTerm `sepBy1` symbol ";") <&> \case
    [c] -> c
    c1 : c2 : cs -> Seq (twoElemList c1 c2 cs)
    [] -> error "pCommand: sepBy1 gave no command"

-- | Whether a command can begin where this text ends: the text is empty,
-- or it ends in a @;@ or @{@ that the whole commands before it allow, or
-- in an @at@ and its path, as the command parser reads them (spaces
-- aside).
startsCommand :: Text -> Bool
startsCommand =
  isRight . runParser (s *> before) "<completion>"
  where
    before :: Parser ()
    before =
      eof
        <|> try (pCommandTerm *> symbol ";" *> before)
        <|> try (symbol "{" *> before)
        <|> try (command "at" *> path *> before)

-- | Parse a whole command line; trailing input is an error rather than
-- silently ignored (@rm a b@ used to run @rm a@).
parseCommand :: Text -> Either String Command
parseCommand = left errorBundlePretty . parseCommandLine

-- | 'parseCommand' with its errors as megaparsec gives them, for a caller
-- that shows where they are ('Utils.Parsing.Problems.syntaxProblems').
parseCommandLine :: Text -> Either ParseErrorBundle' Command
parseCommandLine =
  runParser (pCommand <* eof >>= rejectSplices) "<interactive>"

test_startsCommand :: TestTree
test_startsCommand =
  testGroup
    "startsCommand"
    [ starts "",
      starts "  ",
      starts "nid; ",
      starts "nid;",
      starts "{",
      starts "{nid; ",
      starts "at a ",
      starts "at (a & b) at c {nid; ",
      starts "{ls a}; ",
      doesNotStart "ls ",
      doesNotStart "nid ",
      doesNotStart "at ",
      doesNotStart "mk a; ls ",
      doesNotStart "ls %override({",
      doesNotStart "ls \"a; ",
      -- the text of these is the rest of the line, ; and all
      doesNotStart "desc a; ",
      doesNotStart "text a; "
    ]
  where
    starts t = testCase ("starts after " ++ show t) $ startsCommand t @? ""
    doesNotStart t =
      testCase ("does not start after " ++ show t) $
        not (startsCommand t) @? ""

test_parseCommand :: TestTree
test_parseCommand =
  testGroup
    "parseCommand"
    [ "t hello/world @"
        `parsesTo` Tag Nothing (Literal "hello" :/ Literal "world") One,
      "tag \"hello/world\" @"
        `parsesTo` Tag Nothing (Literal "hello/world") One,
      "tag hello-world @; tag foo-bar @"
        `parsesTo` Seq
          ( twoElemList
              (Tag Nothing (Literal "hello-world") One)
              (Tag Nothing (Literal "foo-bar") One)
              []
          ),
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
      "fsck" `parsesTo` Check,
      "fix" `parsesTo` Fix ApplyRepairs AllDamage,
      "fix --dry-run" `parsesTo` Fix ListRepairs AllDamage,
      "fix @000000000001 @000000000002"
        `parsesTo` Fix
          ApplyRepairs
          (DamageOf (setFromList [smallNID 1, smallNID 2])),
      "fix --dry-run @000000000001"
        `parsesTo` Fix ListRepairs (DamageOf (singletonSet (smallNID 1))),
      "fix @000000000001; fsck"
        `parsesTo` Seq
          ( twoElemList
              (Fix ApplyRepairs (DamageOf (singletonSet (smallNID 1))))
              Check
              []
          ),
      "fix 000000000001 000000000002.json"
        `parsesTo` Fix
          ApplyRepairs
          (DamageOf (setFromList [smallNID 1, smallNID 2])),
      "fix @0001" `failsWith` "fix takes whole node ids",
      "fix @0000000000001" `failsWith` "fix takes whole node ids",
      "fix @" `failsWith` "fix takes whole node ids",
      "fix @-1" `failsWith` "fix takes whole node ids",
      "fix a" `failsWith` "fix takes whole node ids",
      parseFails "fix 000000000001.txt",
      parseFails "fix @000000000001 --dry-run",
      "mk a" `parsesTo` Make Nothing (Literal "a"),
      "mk --allow-orphans a" `failsWith` "no --allow-orphans any more",
      "mk --max-repairs 2 x/c & d"
        `parsesTo` Make
          (Just (AtMostRepairs 2))
          (Literal "x" :/ Literal "c" :& Literal "d"),
      "mk --max-repairs 007 a"
        `parsesTo` Make (Just (AtMostRepairs 7)) (Literal "a"),
      "mk --max-repairs unlimited a"
        `parsesTo` Make (Just UnlimitedRepairs) (Literal "a"),
      "t --max-repairs 10 a @"
        `parsesTo` Tag (Just (AtMostRepairs 10)) (Literal "a") One,
      "tag --max-repairs unlimited a/b #c"
        `parsesTo` Tag
          (Just UnlimitedRepairs)
          (Literal "a" :/ Literal "b")
          (Absolute tagsNID :/ Literal "c"),
      "at * mk --max-repairs 3 a"
        `parsesTo` At Wild (Make (Just (AtMostRepairs 3)) (Literal "a")),
      "mk --max-repairs 0 a" `failsWith` "Unknown value 0 for --max-repairs",
      "mk --max-repairs -3 a" `failsWith` "Unknown value -3 for --max-repairs",
      "mk --max-repairs many a"
        `failsWith` "it accepts a positive number or unlimited",
      "mk --max-repairs 0x10 a" `failsWith` "Unknown value 0x10",
      "mk --max-repairs 1e3 a" `failsWith` "Unknown value 1e3",
      "mk --max-repairs 99999999999999999999999 a" `failsWith` "too large",
      "mk --max-repairs 1 --max-repairs 2 a" `failsWith` "at most once",
      "t --max-repairs 1 --max-repairs 2 a @"
        `failsWith` "tag takes --max-repairs at most once",
      parseFails "mk --max-repairs",
      parseFails "mk --max-repairs a",
      -- the option goes before the paths, and only mk and tag take it
      parseFails "mk a --max-repairs 2",
      parseFails "tag a --max-repairs 2 @",
      parseFails "ls --max-repairs 2 a",
      parseFails "rm --max-repairs 2 a",
      -- it was a setting of %override, which limited the whole path
      "mk %override({max-repairs = 2}, x/c) & d"
        `failsWith` "an option of mk and tag",
      "cp a b" `parsesTo` Copy (Literal "a") (Literal "b"),
      "cl a b" `parsesTo` Alias OneParent (Literal "a") (Literal "b"),
      "clone a b" `parsesTo` Alias OneParent (Literal "a") (Literal "b"),
      "cl --all-parents a b"
        `parsesTo` Alias AllParents (Literal "a") (Literal "b"),
      "rn a b" `parsesTo` Rename OneParent (Literal "a") (Literal "b"),
      "rn --all-parents a c/z"
        `parsesTo` Rename
          AllParents
          (Literal "a")
          (Literal "c" :/ Literal "z"),
      -- the flag goes before the paths
      parseFails "rn a c/z --all-parents",
      parseFails "rn --all-parents a",
      "{nid; si}" `parsesTo` Seq (twoElemList NodeId ShowImage []),
      "at #foo nid" `parsesTo` At (Absolute tagsNID :/ Literal "foo") NodeId,
      "t #something https://example.com/resource"
        `parsesTo` Tag
          Nothing
          (Absolute tagsNID :/ Literal "something")
          (directive (HttpResource [quri|https://example.com/resource|])),
      "t https://example.com/resource #something"
        `parsesTo` Tag
          Nothing
          (directive (HttpResource [quri|https://example.com/resource|]))
          (Absolute tagsNID :/ Literal "something"),
      -- regression test: this used to parse to t ouch hello-world; incorrectly
      -- not requiring a space between t and ouch
      parseFails "touch hello-world",
      parseFails " t hello/world @;",
      -- @ used to be # here; not failing could cause accidental misuse
      parseFails "t hello/world #",
      -- trailing input used to be dropped, e.g. running only @rm a@
      parseFails "rm a b",
      parseFails "mv a b c",
      parseFails "ls foo~bar",
      "ls a " `parsesTo` Preview (Literal "a"),
      -- directives are kept in every command's paths
      "mk a/%targets(b)"
        `parsesTo` Make Nothing (Literal "a" :/ directive (Targets (Literal "b"))),
      "t %last %history(2)"
        `parsesTo` Tag
          Nothing
          (directive (LocationFromHistory 1))
          (directive (LocationFromHistory 2)),
      "rm %targets(a)" `parsesTo` Remove DeleteSelected (directive (Targets (Literal "a"))),
      "cd %last" `parsesTo` ChangeNode (directive (LocationFromHistory 1)),
      "at %targets(a) mk b/%last"
        `parsesTo` At
          (directive (Targets (Literal "a")))
          (Make Nothing (Literal "b" :/ directive (LocationFromHistory 1))),
      "mv %last %history(2)"
        `parsesTo` Move
          (directive (LocationFromHistory 1))
          (directive (LocationFromHistory 2)),
      "rn --all-parents a %targets(b)/c"
        `parsesTo` Rename
          AllParents
          (Literal "a")
          (directive (Targets (Literal "b")) :/ Literal "c"),
      "mk https://example.com/x"
        `parsesTo` Make
          Nothing
          (directive (HttpResource [quri|https://example.com/x|])),
      -- splices can never work in the CLI: rejected before anything runs
      "mk %{foo}" `failsWith` "SpliceInCommand",
      "mk z; mk q/%{foo}" `failsWith` "SpliceInCommand",
      "ls a/%targets(b/%{foo})" `failsWith` "SpliceInCommand",
      "t #t %{foo}" `failsWith` "SpliceInCommand",
      "mg %targets(a) %last"
        `parsesTo` Merge
          ( twoElemList
              (Merged (directive (Targets (Literal "a"))))
              (Merged (directive (LocationFromHistory 1)))
              []
          ),
      "mg a b c"
        `parsesTo` Merge
          ( twoElemList
              (Merged (Literal "a"))
              (Merged (Literal "b"))
              [Merged (Literal "c")]
          ),
      "mg --keep a b"
        `parsesTo` Merge
          (twoElemList (Kept (Literal "a")) (Merged (Literal "b")) []),
      "mg a --keep b c"
        `parsesTo` Merge
          ( twoElemList
              (Merged (Literal "a"))
              (Kept (Literal "b"))
              [Merged (Literal "c")]
          ),
      "mg a b --keep c/d"
        `parsesTo` Merge
          ( twoElemList
              (Merged (Literal "a"))
              (Merged (Literal "b"))
              [Kept (Literal "c" :/ Literal "d")]
          ),
      "mg --keep %last a"
        `parsesTo` Merge
          ( twoElemList
              (Kept (directive (LocationFromHistory 1)))
              (Merged (Literal "a"))
              []
          ),
      -- each argument is one node: a single path is not enough
      parseFails "mg a",
      parseFails "mg --keep a",
      parseFails "mg a --keep",
      "mg --keep a --keep b" `failsWith` "at most one",
      "mg --keep a b --keep c" `failsWith` "at most one",
      "rmnf %last"
        `parsesTo` RemoveNode DeleteSelected (directive (LocationFromHistory 1)),
      "rm --dry-run a/b"
        `parsesTo` Remove ListDeletions (Literal "a" :/ Literal "b"),
      "rmnf --dry-run a" `parsesTo` RemoveNode ListDeletions (Literal "a"),
      -- the option goes before the path; a quoted label is still a label
      parseFails "rm a --dry-run",
      "rm \"--dry-run\"" `parsesTo` Remove DeleteSelected (Literal "--dry-run"),
      "cp %last/x %targets(a)"
        `parsesTo` Copy
          (directive (LocationFromHistory 1) :/ Literal "x")
          (directive (Targets (Literal "a"))),
      "cl --all-parents %last/x %targets(a)/y"
        `parsesTo` Alias
          AllParents
          (directive (LocationFromHistory 1) :/ Literal "x")
          (directive (Targets (Literal "a")) :/ Literal "y"),
      -- an error in a later command of a ; line is reported where it is,
      -- not at the first ;
      "cd a; back" `failsWith` "<interactive>:1:11:",
      "mk n4; ls (" `failsWith` "<interactive>:1:12:",
      "nid; nid; rm a b" `failsWith` "<interactive>:1:16:",
      "{nid; back}" `failsWith` "<interactive>:1:11:",
      "cd a;" `failsWith` "<interactive>:1:6:",
      -- and one after a whole line is still an error there
      "nid; rm a b" `failsWith` "<interactive>:1:11:"
    ]
  where
    directive = Directive testRange
    -- a directive's source range is ignored by comparing the 'show'n
    -- commands (which print the surface syntax) when there are directives
    parsesTo :: Text -> Command -> TestTree
    parsesTo input expected =
      testCase ("parse: " ++ show input) $
        let actual = parseCommand input
         in if hasDirectives expected
              then Right (show expected) @=? map show actual
              else Right expected @=? actual
    hasDirectives c = any (`isInfixOf` show c) ["%", "://"]
    failsWith :: Text -> String -> TestTree
    failsWith input code =
      testCase ("doesn't parse: " ++ show input) $
        case parseCommand input of
          Right c -> assertFailure ("parsed to " ++ show c)
          Left err ->
            code
              `isInfixOf` err
              @? ("expected " ++ code ++ " in:\n" ++ err)
    parseFails :: Text -> TestTree
    parseFails input =
      testCase ("doesn't parse: " ++ show input) $
        isLeft (parseCommand input)
          @? "parseCommand didn't fail"
