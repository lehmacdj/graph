module Executable.GraphEditor.GoldenSpec where

import DAL.DTO (nodeToDTO)
import DAL.DirectoryFormat (metadataFile)
import DAL.DumpGraph
import DAL.Serialization (serializeNodeEx)
import Data.Aeson qualified as Aeson
import Data.Attoparsec.ByteString
import Data.Attoparsec.ByteString.Char8 qualified as Char8
import Data.Attoparsec.Combinator (lookAhead)
import Data.ByteString qualified as B
import Data.ByteString.Lazy qualified as BL
import Data.Text qualified as T
import Graph.SystemNodes.Init (systemEdges)
import Models.Connect
import Models.Edge
import Models.Graph
import Models.NID
import Models.Node (Node, Node' (..), emptyNode)
import MyPrelude
import System.Directory
  ( createDirectory,
    createFileLink,
    doesPathExist,
    listDirectory,
    removeFile,
    renameFile,
  )
import System.Environment (getEnvironment)
import System.FilePath
import System.FilePath.Glob
import System.Process.Typed
import Test.Tasty.Golden
import Utils.Testing

data GoldenTest = GoldenTest
  { -- | An inline base graph, written as lines @0 -a-> 1@ (edges between
    -- @smallNID@s, node 0 being the origin) and @1 data "text"@ (node
    -- data, as a Haskell string literal); see 'writeBaseGraph' for the
    -- lines that damage the node files
    baseGraph :: Maybe ByteString,
    -- | Relative path (from the root of the project) to a graph to use as a
    -- template for the temporary graph that the test is run on.
    template :: Maybe ByteString,
    -- | Whether the input is one command line given to @ge -e@ (the section
    -- is then headed @expression@) instead of the REPL's standard input
    oneShot :: Bool,
    -- | input to be fed to @ge@ via stdin, or the command line for @-e@
    input :: ByteString,
    stdOut :: ByteString,
    stdErr :: ByteString,
    -- | The exit code of @ge@ (minus the signal number if a signal ended
    -- it). The section is left out when it is 0.
    exitCode :: Int,
    graphDump :: ByteString,
    -- | Node data after the run, only for tests with an inline base graph
    -- (the graph dump does not show data)
    dataDump :: Maybe ByteString
  }
  deriving (Eq, Show)

pathForTemplate :: ByteString -> FilePath
pathForTemplate = ("../examples" </>) . unpack . decodeUtf8

-- | Whether the test is one-shot and its template
pInputHeader :: Parser (Bool, Maybe ByteString)
pInputHeader = (<?> "input header") $ do
  oneShot <- (string "input" $> False) <|> (string "expression" $> True)
  (oneShot,) <$> optional (string "@example:" *> takeTill isEqualsSign)
  where
    isEqualsSign = (== 61)

pSectionHeader :: Parser a -> Parser a
pSectionHeader p =
  string "==========" *> p <* string "==========\n" <?> "section header"

-- | One line including its newline; the last line of a file may lack one.
pLine :: Parser ByteString
pLine = takeWhileIncluding (/= 10) <|> takeWhile1 (/= 10)

-- | The lines of the @section@ section up to (and consuming) @end@, named
-- @next@ in errors. There is no fallback: a missing or misspelled @end@
-- header is a parse error rather than an empty body. With @strict@, a line
-- that looks like a section header is an error too.
pBody :: Bool -> String -> String -> Parser () -> Parser ByteString
pBody strict section next end = go []
  where
    go acc =
      (end $> concat (reverse acc)) <|> do
        done <- atEnd
        when done . fail $
          "the " <> section <> " section is not followed by " <> next
        l <- pLine
        when (strict && "==========" `isPrefixOf` l) . fail $
          "unexpected header "
            <> show (T.strip (decodeUtf8 l))
            <> " in the "
            <> section
            <> " section"
        go (l : acc)

pGoldenTest :: Parser GoldenTest
pGoldenTest = (<?> "golden test") do
  baseGraph <-
    optional
      ( pSectionHeader (void $ string "graph")
          *> pBody
            False
            "graph"
            "an input header"
            (void . lookAhead $ pSectionHeader pInputHeader)
      )
  (oneShot, template) <- pSectionHeader pInputHeader <?> "input header"
  GoldenTest baseGraph template oneShot
    <$> body (if oneShot then "expression" else "input") "stdout"
    <*> (unterminated <$> body "stdout" "stderr")
    <*> ( unterminated
            <$> pBody
              False
              "stderr"
              "an exit-code or graph-dump header"
              (void . lookAhead $ exitCodeHeader <|> graphDumpHeader)
        )
    <*> Char8.option 0 (exitCodeHeader *> Char8.signed Char8.decimal <* "\n")
    <* (graphDumpHeader <?> "graph-dump header")
    <*> pBody
      True
      "graph-dump"
      "a data header or the end of the file"
      (lookAhead dataHeader <|> endOfInput)
    <*> optional (dataHeader *> takeByteString)
    <* endOfInput
  where
    dataHeader = pSectionHeader (void $ string "data")
    exitCodeHeader = pSectionHeader (void $ string "exit-code")
    graphDumpHeader = pSectionHeader (void $ string "graph-dump")
    body section next =
      pBody
        False
        section
        ("a " <> headerText next <> " header")
        (pSectionHeader (void $ string (fromString next)))
    headerText name = "==========" <> name <> "=========="

-- | Marks output that does not end with a newline (@ge@ ended in the middle
-- of a line), so that the next section header still starts a line.
noFinalNewline :: ByteString
noFinalNewline = "\n<no newline at the end>\n"

terminated :: ByteString -> ByteString
terminated output
  | null output || "\n" `isSuffixOf` output = output
  | otherwise = output <> noFinalNewline

unterminated :: ByteString -> ByteString
unterminated output = fromMaybe output (stripSuffix noFinalNewline output)

-- | Parse a golden test and check that it has something to run.
parseGoldenTest :: ByteString -> Either String GoldenTest
parseGoldenTest = parseOnly pGoldenTest >=> validateGoldenTest

validateGoldenTest :: GoldenTest -> Either String GoldenTest
validateGoldenTest golden
  | null (words (decodeUtf8 golden.input :: Text)) =
      Left "the golden test has no input"
  | otherwise = Right golden

sectionHeader :: ByteString -> LByteString
sectionHeader name = "==========" <> fromStrict name <> "==========\n"

renderGoldenTest :: GoldenTest -> LByteString
renderGoldenTest GoldenTest {..} =
  concat
    [ maybe "" ((sectionHeader "graph" <>) . fromStrict) baseGraph,
      sectionHeader
        ( (if oneShot then "expression" else "input")
            <> maybe "" ("@example:" <>) template
        ),
      fromStrict input,
      sectionHeader "stdout",
      fromStrict (terminated stdOut),
      sectionHeader "stderr",
      fromStrict (terminated stdErr),
      if exitCode == 0
        then ""
        else sectionHeader "exit-code" <> fromStrict (encodeUtf8 (tshow exitCode)) <> "\n",
      sectionHeader "graph-dump",
      fromStrict graphDump,
      maybe "" ((sectionHeader "data" <>) . fromStrict) dataDump
    ]

-- | Runs `ge` with the input and produces a new `GoldenTest` containing the
-- actual output produced by running it.
--
-- @ge@ reads and rewrites the history file of the REPL in the user's data
-- directory on every run, so the run gets a data directory of its own.
runGraphEditor :: GoldenTest -> IO GoldenTest
runGraphEditor test@GoldenTest {..} =
  withTempGraph (pathForTemplate <$> template) $ \tmpGraph ->
    withSystemTempDirectory "ge-data" $ \dataHome -> do
      for_ baseGraph (writeBaseGraph tmpGraph)
      environment <- getEnvironment
      (exitCode', stdOut', stdErr') <-
        readProcess $
          proc
            "ge"
            ( [ "--test-only-nid-generation-seed",
                "0",
                "--test-only-monotonic-increasing-deterministic-time",
                tmpGraph
              ]
                <> ( if oneShot
                       then ["-e", unpack (T.strip (decodeUtf8 input))]
                       else []
                   )
            )
            & setStdin
              (byteStringInput (if oneShot then "" else fromStrict input))
            & setEnv (geEnvironment dataHome environment)
      graphDump' <- dumpGraph tmpGraph
      dataDump' <- for baseGraph \_ -> do
        files <-
          sort . filter ((== ".data") . takeExtension)
            <$> listDirectory tmpGraph
        fmap concat . for files $ \file -> do
          bytes <- B.readFile (tmpGraph </> file)
          pure . encodeUtf8 $ pack file <> ": " <> tshow bytes <> "\n"
      pure $
        test
          { stdOut = toStrict stdOut',
            stdErr = toStrict stdErr',
            exitCode = case exitCode' of
              ExitSuccess -> 0
              ExitFailure code -> code,
            graphDump = toStrict $ builderToLazy graphDump',
            dataDump = dataDump'
          }

-- | The environment that a test runs @ge@ in, given the one of the tests: a
-- data directory of its own, and an editor that does nothing and succeeds
-- instead of whatever the user has (@vi@ would start it).
geEnvironment :: FilePath -> [(String, String)] -> [(String, String)]
geEnvironment dataHome environment =
  [("XDG_DATA_HOME", dataHome), ("EDITOR", "true")]
    <> filter ((`notElem` ["XDG_DATA_HOME", "EDITOR"]) . fst) environment

data BaseGraphLine
  = BaseEdge (Edge Text)
  | -- | An edge that only one of its two nodes records
    BaseHalfEdge RecordSide (Edge Text)
  | BaseData NID ByteString
  | BaseNode NID
  | BaseDamage Damage
  | -- | The graph has none of the system nodes that @ge@ gives every graph
    BaseNoSystemNodes

-- | Which node's record of an edge
data RecordSide = OutgoingRecord | IncomingRecord

-- | Something done to a node's file after the graph is written
data Damage
  = MissingFile NID
  | RawFile NID ByteString
  | -- | The file of the first node says it is the second node
    FileWithId NID NID

-- | Write the nodes of an inline base graph. Lines are @0 -a-> 1@ (an edge;
-- the label may be a quoted string such as @0 -"a b"-> 1@), @1 data "text"@
-- (node data) or @2@ (a node without edges).
--
-- The system nodes of @ge@ and the edges between them (the origin among
-- them) are written as well, as every graph has them, unless there is a line
-- @no system nodes@: then the graph is one that an old @ge@ made, or that
-- lost them, and @ge@ creates them when it opens the graph.
--
-- A graph that @ge@ cannot produce is written with: @0 -a-> 1 !out@ (only
-- node 0's outgoing record of the edge) and @0 -a-> 1 !in@ (only node 1's
-- incoming record), @missing 2@ (no file for node 2), @raw 2 "text"@ (the
-- file of node 2 holds exactly that text) and @id 2 3@ (the file of node 2
-- says its id is 3). The last three are applied in order once every node's
-- file is written. A node is a small number or a whole id (@\@pbYxBO6fzBQV@),
-- for damage to the system nodes.
writeBaseGraph :: FilePath -> ByteString -> IO ()
writeBaseGraph directory text = do
  items <-
    either (fail . ("invalid base graph: " <>)) pure $
      traverse parseLine (filter (not . null . words) (lines (decodeUtf8 text)))
  let systemParts
        | null [() | BaseNoSystemNodes <- items] = systemEdges
        | otherwise = []
      edges = systemParts <> [e | BaseEdge e <- items]
      halfEdges = [(side, e) | BaseHalfEdge side e <- items]
      damages = [d | BaseDamage d <- items]
      dataByNID = mapFromList [(n, d) | BaseData n d <- items] :: Map NID ByteString
      extraNodes =
        [n | BaseNode n <- items]
          <> keys dataByNID
          <> concat [[e.source, e.sink] | (_, e) <- halfEdges]
          <> map damagedNode damages
      graph :: Graph Text ()
      graph =
        foldl'
          (\g n -> if n `member` nidSet g then g else insertNode (emptyNode n) g)
          (insertEdges edges (insertNode (emptyNode nilNID) emptyGraph))
          extraNodes
      nodes :: Map NID (Node Text ())
      nodes =
        foldl'
          (flip addRecord)
          (mapFromList [(n.nid, n) | n <- nodesOf graph])
          halfEdges
  for_ nodes \n ->
    serializeNodeEx (n $> lookup n.nid dataByNID) directory
  for_ damages \case
    MissingFile n -> removeFile (metadataFile directory n)
    RawFile n content -> B.writeFile (metadataFile directory n) content
    FileWithId n claimed ->
      for_ (lookup n nodes) \original ->
        BL.writeFile
          (metadataFile directory n)
          (Aeson.encode (nodeToDTO (original & #nid .~ claimed)))
  where
    damagedNode = \case
      MissingFile n -> n
      RawFile n _ -> n
      FileWithId n _ -> n
    addRecord (side, Edge source label sink) = case side of
      OutgoingRecord ->
        ix source . #outgoing %~ insertSet (Connect label sink)
      IncomingRecord ->
        ix sink . #incoming %~ insertSet (Connect label source)
    parseLine :: Text -> Either String BaseGraphLine
    parseLine line = case words line of
      ["no", "system", "nodes"] -> Right BaseNoSystemNodes
      [n] -> BaseNode <$> nodeId n
      ["missing", n] -> BaseDamage . MissingFile <$> nodeId n
      ["id", n, claimed] ->
        BaseDamage <$> (FileWithId <$> nodeId n <*> nodeId claimed)
      ("raw" : n : _) ->
        (\n' -> BaseDamage . RawFile n') <$> nodeId n <*> literalAfter n
      (n : "data" : _) -> BaseData <$> nodeId n <*> literalAfter "data"
      _
        | Just line' <- stripSuffix "!out" (T.strip line) ->
            BaseHalfEdge OutgoingRecord <$> parseEdge line'
        | Just line' <- stripSuffix "!in" (T.strip line) ->
            BaseHalfEdge IncomingRecord <$> parseEdge line'
        | otherwise -> BaseEdge <$> parseEdge line
      where
        -- the Haskell string literal that follows the first @word@
        literalAfter word =
          let literal =
                T.strip (drop (length word) (snd (T.breakOn word line)))
           in case readMay (unpack literal) of
                Just (str :: String) -> Right (encodeUtf8 (pack str))
                Nothing -> Left ("invalid string literal in: " <> unpack line)
    parseEdge :: Text -> Either String (Edge Text)
    parseEdge line = do
      let (source, rest) = T.breakOn " -" line
          (labelArrow, sink) = T.breakOnEnd "-> " (drop 2 rest)
      rawLabel <-
        maybe (Left ("invalid line: " <> unpack line)) (Right . T.strip) $
          stripSuffix "-> " labelArrow
      label <-
        if "\"" `isPrefixOf` rawLabel
          then maybe (Left ("invalid label: " <> unpack rawLabel)) (Right . pack) (readMay (unpack rawLabel) :: Maybe String)
          else Right rawLabel
      (\a b -> Edge a label b) <$> nodeId (T.strip source) <*> nodeId (T.strip sink)
    -- a small number, or a whole id such as that of a system node
    nodeId t =
      maybe (Left ("invalid node: " <> unpack t)) Right $
        (smallNID <$> readMay t) <|> readMay t

-- | Run @ge@ on a graph with a data directory of its own (see
-- 'runGraphEditor'): its exit code, stdout and stderr.
runGe :: FilePath -> [String] -> ByteString -> IO (Int, ByteString, ByteString)
runGe graph arguments stdin =
  withSystemTempDirectory "ge-data" $ \dataHome -> do
    environment <- getEnvironment
    (exitCode, out, err) <-
      readProcess $
        proc
          "ge"
          ( [ "--test-only-nid-generation-seed",
              "0",
              "--test-only-monotonic-increasing-deterministic-time",
              graph
            ]
              <> arguments
          )
          & setStdin (byteStringInput (fromStrict stdin))
          & setEnv (geEnvironment dataHome environment)
    pure
      ( case exitCode of
          ExitSuccess -> 0
          ExitFailure code -> code,
        toStrict out,
        toStrict err
      )

-- | Every message about damage ends by pointing to @fsck@ and @fix@. That
-- advice has to be true for every damaged graph that the goldens start
-- from: either @fix@ repairs the graph, after which @fsck@ finds nothing and
-- the golden's own commands meet no damage, or @fix@ itself says what it
-- did not repair.
mkAdviceTest :: FilePath -> TestTree
mkAdviceTest path = testCase (takeBaseName path) do
  golden <- either (assertFailure . ((path <> ": ") <>)) pure . parseGoldenTest =<< B.readFile path
  for_ golden.baseGraph \graph -> withTempGraph Nothing \directory -> do
    writeBaseGraph directory graph
    (fixed, fixOut, fixErr) <- runGe directory ["-e", "fix"] ""
    if fixed /= 0
      then do
        ("not repaired:" `isInfixOf` fixOut)
          @? ("fix failed without saying what it didn't repair: " <> show fixErr)
        ("damage-not-repaired" `isInfixOf` fixErr)
          @? ("fix failed for another reason: " <> show fixErr)
      else do
        (checked, fsckOut, fsckErr) <- runGe directory ["-e", "fsck"] ""
        (checked == 0)
          @? ("fsck still finds damage after fix: " <> show (fsckOut, fsckErr))
        (_, _, err) <-
          if golden.oneShot
            then runGe directory ["-e", unpack (T.strip (decodeUtf8 golden.input))] ""
            else runGe directory [] golden.input
        for_ ["damaged-graph", "unparsable-node-file", "cannot-open-graph", "inconsistent-node-records"] \code ->
          not ((code :: ByteString) `isInfixOf` err)
            @? ("the commands still meet damage after fix: " <> show err)

-- | In a regex of a path, @^@ and @$@ are the start and the very end of a
-- label, whatever the label holds.
spec_regexAnchors :: Spec
spec_regexAnchors = do
  let -- the origin's edges, each to a node of its own: 1 a, 2 a and a
      -- newline, 3 b, 4 and 5 with a newline in the middle, 6 with one at
      -- the start
      graph =
        "0 -a-> 1\n0 -\"a\\n\"-> 2\n0 -b-> 3\n0 -\"x\\na\"-> 4\n"
          <> "0 -\"a\\nx\"-> 5\n0 -\"\\na\"-> 6\n"
      -- the nodes that @ls@ lists
      listed out =
        [ T.take 12 afterAt
        | afterAt <- drop 1 (T.splitOn "@" (decodeUtf8 out))
        ]
      finds :: (HasCallStack) => String -> [Int] -> Spec
      finds path expected =
        it (path <> " finds " <> show expected) $
          withTempGraph Nothing \directory -> do
            writeBaseGraph directory graph
            (code, out, err) <- runGe directory ["-e", "ls " <> path] ""
            (code, err) `shouldBe` (0, "")
            sort (listed out)
              `shouldBe` sort [tshow n & drop 1 | n <- map smallNID expected]
  describe "$ is the very end of a label, not the place before a last newline" do
    "regex:\"^(a|b)$\"" `finds` [1, 3]
    "a | b" `finds` [1, 3]
    "regex:\"a$\"" `finds` [1, 4, 6]
    "regex:\"^a$\"" `finds` [1]
  describe "^ is the start of a label only" do
    "regex:\"^a\"" `finds` [1, 2, 5]
    "regex:\"^x\"" `finds` [4]
  describe "a newline in the middle of a label is no end and no start" do
    "regex:\"x$\"" `finds` [5]
    "regex:\"^a\\nx$\"" `finds` [5]
    "regex:\"^x\\na$\"" `finds` [4]
  describe "a regex that asks for lines itself gets them" do
    "regex:\"(?m)a$\"" `finds` [1, 2, 4, 5, 6]
    "regex:\"(?m)^a\"" `finds` [1, 2, 4, 5, 6]

spec_dryRun :: Spec
spec_dryRun = do
  it "says what a change would delete, then what it would write, by file name" $
    withTempGraph Nothing \directory -> do
      -- the origin -t-> each of sixty nodes -c-> a node of its own; deleting
      -- the sixty deletes their files and writes those of their neighbours.
      -- (No two of the ids differ only in the case of a letter: such file
      -- names are one file where the tests run.)
      let middle = [62 * tens + ones | tens <- [1 .. 6], ones <- [0 .. 9]] :: [Int]
          ownOf = (620 +)
          edges =
            ["0 -t-> " <> tshow n | n <- middle]
              <> [tshow n <> " -c-> " <> tshow (ownOf n) | n <- middle]
          fileOf = takeFileName . metadataFile directory . smallNID
          kept = map ((fileOf 70 <> ".unparsable") <>) ["", "-2"]
      writeBaseGraph directory (encodeUtf8 (unlines edges))
      for_ kept \name -> B.writeFile (directory </> name) "{not json"
      before <- directoryAsItIs directory
      let dryRun = runGe directory ["--dry-run", "-e", "rmnf t"] ""
          -- of each line that says what would become of a file, what would
          -- and the file's name
          changes out =
            [ (would, takeFileName (unpack file))
            | line <- lines (decodeUtf8 out),
              (would, said) <-
                [ ("delete" :: Text, "would delete at path: "),
                  ("write", "would write: ")
                ],
              said `isPrefixOf` line,
              -- a path is shown as a string is, in quotes
              _ : file : _ <- [reverse (T.splitOn "\"" line)]
            ]
      (code, out, err) <- dryRun
      (code, err) `shouldBe` (0, "")
      changes out
        `shouldBe` ( map ("delete",) (sort (map fileOf middle <> kept))
                       <> map ("write",) (sort (map fileOf (0 : map ownOf middle)))
                   )
      -- and the same text every time
      for_ [1 .. 5 :: Int] \_ -> dryRun `shouldReturn` (code, out, err)
      directoryAsItIs directory `shouldReturn` before
  it "says what opening the graph would add, and adds nothing" $
    withTempGraph Nothing \directory -> do
      -- a graph that has none of the system nodes, and a command that reads
      writeBaseGraph directory "no system nodes\n0 -a-> 1\n"
      before <- sort <$> listDirectory directory
      (code, _, err) <- runGe directory ["--dry-run", "-e", "nid"] ""
      code `shouldBe` 0
      unpack (decodeUtf8 err)
        `shouldContain` ( "warning: would create the system nodes that have no"
                            <> " file: @0daCJjMrQel8, "
                        )
      unpack (decodeUtf8 err) `shouldNotContain` "created the system nodes"
      (sort <$> listDirectory directory) `shouldReturn` before
      -- and what it says when it does add them
      (_, _, err') <- runGe directory ["-e", "nid"] ""
      unpack (decodeUtf8 err')
        `shouldContain` ( "warning: created the system nodes that had no file:"
                            <> " @0daCJjMrQel8, "
                        )

-- | A directory as it is: the name of everything in it, with what each file
-- holds (nothing for what can't be read as a file: a directory, a symbolic
-- link to a file that isn't there)
directoryAsItIs :: FilePath -> IO [(FilePath, Maybe ByteString)]
directoryAsItIs directory = do
  names <- sort <$> listDirectory directory
  for names \name ->
    (name,)
      <$> (Just <$> B.readFile (directory </> name))
        `catch` \(_ :: IOError) -> pure Nothing

spec_oneDecision :: Spec
spec_oneDecision = do
  -- A command that resolves something in one step and changes something in
  -- another one: the first step reads past the damage, and takes b/c + h for
  -- the one node that h leads to
  let graph =
        "0 -a-> 1\n0 -b-> 2\n2 -c-> 3\n4 -d-> 2\n0 -e-> 4\n1 -f-> 2\n"
          <> "0 -g-> 3\n0 -h-> 5\n0 -i-> 6\n1 data \"one\"\n2 data \"two\"\n"
      commands =
        [ "mg (b/c + h) i",
          "mg i (b/c + h)",
          "mg %targets(b/c + h) i",
          "at (b/c + h) rmnf @",
          "rmnf %targets(h + b/c)"
        ]
      damages =
        [ ("a file that can't be parsed", "raw 2 \"{not json\"\n"),
          ("a node without a file", "missing 2\n")
        ]
      writesNothing damaged command =
        for_ [[], ["--dry-run"]] \mode ->
          withTempGraph Nothing \directory -> do
            writeBaseGraph directory damaged
            before <- directoryAsItIs directory
            (code, out, err) <- runGe directory (mode <> ["-e", command]) ""
            code `shouldBe` 1
            unpack (decodeUtf8 err)
              `shouldStartWith` "this change was not written: the graph is damaged"
            -- the refusal names the damage, so no warning does as well
            unpack (decodeUtf8 err) `shouldNotContain` "warning:"
            -- a dry run says what it would write
            toLower (unpack (decodeUtf8 out)) `shouldNotContain` "would "
            directoryAsItIs directory `shouldReturn` before
  for_ damages \(name, damage) -> for_ commands \command ->
    it (command <> " writes nothing past " <> name <> ", also in a dry run") $
      writesNothing (graph <> damage) command
  -- A lone record of which one step of the command reads one file (to find
  -- the targets) and a later step the other one (at a target): the command
  -- has read both files, like the one that does the same in one step (the
  -- last of each)
  let lone :: [(Text, [String])]
      lone =
        [ ("0 -b-> 2", ["at b rmnf @", "rmnf %targets(b)", "at b mk x", "at b vi", "rmnf b"]),
          ( "2 -c-> 3",
            ["at (b/c + h) rmnf @", "rmnf %targets(h + b/c)", "rmnf (h + b/c)"]
          )
        ]
      -- the graph in which only the source of that edge records it
      withLone edge =
        encodeUtf8 $
          T.replace (edge <> "\n") (edge <> " !out\n") (decodeUtf8 graph)
  for_ lone \(edge, loneCommands) -> for_ loneCommands \command ->
    it
      ( command
          <> " writes nothing once it has read both files of the lone record "
          <> unpack edge
          <> ", also in a dry run"
      )
      $ writesNothing (withLone edge) command
  it "a command of several steps is not refused for what its own steps wrote" $
    -- on an undamaged graph; in a dry run every step finds the files as they
    -- were, so the commands there are ones whose steps don't build on each
    -- other
    for_ [False, True] \dry -> withTempGraph Nothing \directory -> do
      writeBaseGraph directory graph
      let several =
            [ ("at (a + h) mk z", True),
              ("at @ {mk x ; at x mk y ; mv x/y @ ; rmnf x}", False),
              ("at (a + h) {mk p ; rm p ; mk q}", False),
              ("at (a + h) {mk p ; mk q ; tag t @}", True),
              ("mg h i", True),
              ("at (b/c + h + e) rmnf @", True)
            ]
      for_ [command | (command, alsoDry) <- several, alsoDry || not dry] \command -> do
        (code, _, err) <-
          runGe directory (["--dry-run" | dry] <> ["-e", command]) ""
        (command, code, err) `shouldBe` (command, 0, "")
      (checked, _, _) <- runGe directory ["-e", "fsck"] ""
      checked `shouldBe` 0
  -- A lone record of which an earlier step reads one file only in writing
  -- (that of a neighbour of the node that it deletes or merges away) and a
  -- later step the other one. A dry run writes nothing and has read that
  -- file all the same, so it ends as the real run does.
  let neighbour = "0 -a-> 1\n0 -e-> 5\n5 -m-> 6\n6 -k-> 7 !out\n0 -d-> 4\n"
      n :: Int -> String
      n k = "@00000000000" <> show k
      afterWriting :: [(String, Int)]
      afterWriting =
        [ ("at " <> n 1 <> " {rmnf " <> n 5 <> " ; at " <> n 7 <> " mk z}", 1),
          ("at e {rmnf @ ; at " <> n 7 <> " mk z}", 1),
          ("at " <> n 1 <> " {mg " <> n 4 <> " " <> n 5 <> " ; at " <> n 7 <> " mk z}", 1),
          ("at " <> n 1 <> " {rmnf " <> n 5 <> " ; rmnf " <> n 7 <> "}", 1),
          ("at " <> n 1 <> " {rmnf " <> n 5 <> " ; at " <> n 7 <> " rmnf @}", 1),
          -- these only read after it, and warn
          ("at " <> n 1 <> " {rmnf " <> n 5 <> " ; ls " <> n 7 <> "}", 0),
          ("at " <> n 1 <> " {mg " <> n 5 <> " " <> n 4 <> " ; ls " <> n 7 <> "}", 0)
        ]
  for_ afterWriting \(command, expected) ->
    it (command <> " ends in a dry run as it does in the real run") $
      for_ [False, True] \dry -> withTempGraph Nothing \directory -> do
        writeBaseGraph directory neighbour
        before <- directoryAsItIs directory
        (code, _, err) <-
          runGe directory (["--dry-run" | dry] <> ["-e", command]) ""
        (dry, code) `shouldBe` (dry, expected)
        unpack (decodeUtf8 err)
          `shouldStartWith` ( if expected == 1
                                then "this change was not written: the graph is damaged"
                                else "warning: the graph is damaged where this command read it"
                            )
        unpack (decodeUtf8 err)
          `shouldContain` ( "- the edge @000000000006 -\"k\"-> @000000000007 is"
                              <> " among the outgoing edges in 000000000006.json"
                              <> " but not among the incoming edges in"
                              <> " 000000000007.json"
                          )
        -- the real run has written its first step by then
        afterwards <- directoryAsItIs directory
        (dry, afterwards == before) `shouldBe` (dry, dry)

spec_unusableNodeFiles :: Spec
spec_unusableNodeFiles = do
  -- what is under the name of a node file and is no file is a node file that
  -- can't be used, as one that can't be parsed, for every command
  let graph = "0 -a-> 1\n0 -b-> 2\n2 -c-> 3\n"
      n2 = smallNID 2
      forms :: [(String, FilePath -> NID -> IO (), String)]
      forms =
        [ ( "a directory",
            \directory nid -> do
              removeFile (metadataFile directory nid)
              createDirectory (metadataFile directory nid),
            "it is not a file but a directory"
          ),
          ( "a symbolic link to a file that isn't there",
            \directory nid -> do
              removeFile (metadataFile directory nid)
              createFileLink
                (directory </> "elsewhere" </> "target.json")
                (metadataFile directory nid),
            "as a symbolic link to a file that isn't there can't"
          )
        ]
      ge directory command = do
        (code, out, err) <- runGe directory ["-e", command] ""
        pure (code, unpack (decodeUtf8 out), unpack (decodeUtf8 err))
      warnsOf why err = do
        err `shouldStartWith` "warning: the graph is damaged where this command read it"
        err `shouldContain` why
      refusedFor why (code, _, err) = do
        code `shouldBe` (1 :: Int)
        err `shouldStartWith` "this change was not written: the graph is damaged"
        err `shouldContain` why
        -- it is there, and no command says that it isn't
        err `shouldNotContain` "there is no 000000000002.json"
  for_ forms \(name, make, why) -> describe ("a node file that is " <> name) do
    let damaged test = withTempGraph Nothing \directory -> do
          writeBaseGraph directory graph
          make directory n2
          before <- directoryAsItIs directory
          test directory
          directoryAsItIs directory `shouldReturn` before
    it "is read as a node without edges, with a warning" $ damaged \directory -> do
      (code, out, err) <- ge directory "ls"
      code `shouldBe` 0
      out `shouldContain` "@000000000002"
      warnsOf why err
      -- cd goes there, and at runs its command there
      (code', out', err') <- ge directory "cd b ; nid ; ls"
      code' `shouldBe` 0
      out' `shouldContain` "@000000000002"
      warnsOf why err'
      (code'', out'', err'') <- ge directory "at b nid"
      (code'', out'') `shouldBe` (0, "@000000000002\n")
      err'' `shouldNotContain` "at did nothing"
    it "is not written, over or through" $ damaged \directory -> do
      -- the node is there, so this has nothing to make: it only reads
      (code, _, err) <- ge directory "mk --allow-orphans @000000000002"
      code `shouldBe` 0
      warnsOf why err
      ge directory "mk x/@000000000002" >>= refusedFor why
      ge directory "at @000000000002 mk y" >>= refusedFor why
      ge directory "rmnf b" >>= refusedFor why
      ge directory "rm b" >>= refusedFor why
      doesPathExist (directory </> "elsewhere") `shouldReturn` False
  for_ forms \(name, make, why) ->
    it ("opens a graph whose origin is " <> name <> ", with a warning") $
      withTempGraph Nothing \directory -> do
        writeBaseGraph directory graph
        make directory nilNID
        (code, _, err) <- ge directory "nid"
        code `shouldBe` 0
        warnsOf why err
        -- but changes nothing there
        (code', _, err') <- ge directory "mk x"
        code' `shouldBe` 1
        err' `shouldContain` why
        err' `shouldNotContain` "there is no 000000000000.json"
  it "still uses a symbolic link to a node file as that node file" $
    withTempGraph Nothing \directory -> do
      writeBaseGraph directory graph
      renameFile (metadataFile directory n2) (directory </> "elsewhere.json")
      createFileLink (directory </> "elsewhere.json") (metadataFile directory n2)
      (code, _, err) <- ge directory "at b mk x ; ls b/x"
      (code, err) `shouldBe` (0, "")

spec_keptFilesOfDeletedNode :: Spec
spec_keptFilesOfDeletedNode =
  it "lists the kept files that a dry run would delete with a node, and deletes them" $
    withTempGraph Nothing \directory -> do
      writeBaseGraph directory "0 -a-> 1\n0 -b-> 2\n2 -c-> 1\n"
      let kept = map ((directory </>) . ("000000000002.json.unparsable" <>)) ["", "-2"]
      for_ kept (`B.writeFile` "{not json")
      before <- directoryAsItIs directory
      (code, out, _) <- runGe directory ["--dry-run", "-e", "rmnf b"] ""
      code `shouldBe` 0
      for_ kept \file ->
        unpack (decodeUtf8 out) `shouldContain` ("would delete at path: " <> show file)
      directoryAsItIs directory `shouldReturn` before
      (code', _, err') <- runGe directory ["-e", "rmnf b"] ""
      (code', err') `shouldBe` (0, "")
      (filter ("000000000002" `isPrefixOf`) <$> listDirectory directory)
        `shouldReturn` []
      (checked, _, _) <- runGe directory ["-e", "fsck"] ""
      checked `shouldBe` 0

test_adviceIsTrue :: IO TestTree
test_adviceIsTrue =
  testGroup "fix does what the messages about damage say"
    . map mkAdviceTest
    <$> globDir1 (compile "*.golden") "test/golden/ge/damaged"

mkGoldenTest :: FilePath -> TestTree
mkGoldenTest path =
  let testName = takeBaseName path
   in goldenVsStringDiff testName (\ref new -> ["diff", "-u", ref, new]) path do
        (B.readFile path <&> parseGoldenTest) >>= \case
          Right golden -> runGraphEditor golden <&> renderGoldenTest
          Left err ->
            error $ path <> ": this golden test is malformed:\n" <> err

inputFiles :: IO [FilePath]
inputFiles = globDir1 (compile "**/*.golden") "test/golden/ge"

test_integration :: IO [TestTree]
test_integration = map mkGoldenTest <$> inputFiles

test_parseGoldenTest :: TestTree
test_parseGoldenTest =
  testGroup
    "parseGoldenTest"
    [ testCase "parses every section" $
        parseGoldenTest
          ( wellFormed
              <> "==========graph-dump==========\ndump\n"
              <> "==========data==========\nd\n"
          )
          @?= Right
            GoldenTest
              { baseGraph = Just "0 -a-> 1\n",
                template = Nothing,
                oneShot = False,
                input = "rn a b\n",
                stdOut = "out\n",
                stdErr = "",
                exitCode = 0,
                graphDump = "dump\n",
                dataDump = Just "d\n"
              },
      testCase "reads an example template" $
        (.template)
          <$> parseGoldenTest
            ( "==========input@example:small==========\nls\n"
                <> "==========stdout==========\n"
                <> "==========stderr==========\n"
                <> "==========graph-dump==========\n"
            )
            @?= Right (Just "small"),
      testCase "keeps a graph dump without a final newline" $
        (.graphDump)
          <$> parseGoldenTest (wellFormed <> "==========graph-dump==========\ndump")
            @?= Right "dump",
      testCase "round-trips a rendered golden test" do
        let golden =
              GoldenTest
                { baseGraph = Nothing,
                  template = Just "small",
                  oneShot = False,
                  input = "ls\n",
                  stdOut = "g> ok\n",
                  stdErr = "oops\n",
                  exitCode = 1,
                  graphDump = "@1{}\n",
                  dataDump = Just "1.data: \"x\"\n"
                }
        parseGoldenTest (toStrict (renderGoldenTest golden)) @?= Right golden,
      testCase "round-trips output that ends in the middle of a line" do
        let golden =
              GoldenTest
                { baseGraph = Nothing,
                  template = Nothing,
                  oneShot = False,
                  input = "ls\n",
                  stdOut = "g> ",
                  stdErr = "ge: failed",
                  exitCode = 1,
                  graphDump = "",
                  dataDump = Nothing
                }
        parseGoldenTest (toStrict (renderGoldenTest golden)) @?= Right golden,
      testCase "reads the exit code" $
        (.exitCode)
          <$> parseGoldenTest
            ( wellFormed
                <> "==========exit-code==========\n-11\n"
                <> "==========graph-dump==========\ndump\n"
            )
            @?= Right (-11),
      testCase "leaves out an exit code of 0" $
        renderGoldenTest
          GoldenTest
            { baseGraph = Nothing,
              template = Nothing,
              oneShot = False,
              input = "ls\n",
              stdOut = "",
              stdErr = "",
              exitCode = 0,
              graphDump = "",
              dataDump = Nothing
            }
          @?= "==========input==========\nls\n"
          <> "==========stdout==========\n"
          <> "==========stderr==========\n"
          <> "==========graph-dump==========\n",
      testCase "round-trips a one-shot test" do
        let golden =
              GoldenTest
                { baseGraph = Just "0 -a-> 1\n",
                  template = Just "small",
                  oneShot = True,
                  input = "rm b\n",
                  stdOut = "",
                  stdErr = "oops\n",
                  exitCode = 1,
                  graphDump = "",
                  dataDump = Nothing
                }
        "==========expression@example:small==========\nrm b\n"
          `isInfixOf` toStrict (renderGoldenTest golden)
          @? "the command is not in an expression section"
        parseGoldenTest (toStrict (renderGoldenTest golden)) @?= Right golden,
      rejects "an exit code that is not a number" $
        wellFormed
          <> "==========exit-code==========\none\n"
          <> "==========graph-dump==========\ndump\n",
      rejects "a missing stdout header" $
        "==========input==========\nrn a b\n"
          <> "==========stderr==========\n"
          <> "==========graph-dump==========\n",
      rejects "a misspelled stderr header" $
        "==========input==========\nrn a b\n"
          <> "==========stdout==========\n"
          <> "==========stder==========\n"
          <> "==========graph-dump==========\n",
      rejects "a misspelled data header" $
        wellFormed
          <> "==========graph-dump==========\ndump\n"
          <> "==========dat==========\nd\n",
      rejects "input without any later section" $
        "==========input==========\nrn a b\n",
      rejects "blank input" $
        "==========input==========\n \n"
          <> "==========stdout==========\n"
          <> "==========stderr==========\n"
          <> "==========graph-dump==========\n"
    ]
  where
    wellFormed =
      "==========graph==========\n0 -a-> 1\n"
        <> "==========input==========\nrn a b\n"
        <> "==========stdout==========\nout\n"
        <> "==========stderr==========\n"
    rejects name golden =
      testCase ("rejects " <> name) $
        isLeft (parseGoldenTest golden) @? "parseGoldenTest didn't fail"
