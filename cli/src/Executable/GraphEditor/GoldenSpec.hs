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
import Models.Connect
import Models.Edge
import Models.Graph
import Models.NID
import Models.Node (Node, Node' (..), emptyNode)
import MyPrelude
import System.Directory (listDirectory, removeFile)
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
    -- | input to be fed to @ge@ via stdin
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

pInputHeader :: Parser (Maybe ByteString)
pInputHeader = (<?> "input header") $ do
  _ <- string "input"
  optional $ string "@example:" *> takeTill isEqualsSign
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
pGoldenTest =
  GoldenTest
    <$> optional
      ( pSectionHeader (void $ string "graph")
          *> pBody
            False
            "graph"
            "an input header"
            (void . lookAhead $ pSectionHeader pInputHeader)
      )
    <*> (pSectionHeader pInputHeader <?> "input header")
    <*> body "input" "stdout"
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
    <?> "golden test"
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
        ( maybe
            "input"
            ("input@example:" <>)
            template
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
            [ "--test-only-nid-generation-seed",
              "0",
              "--test-only-monotonic-increasing-deterministic-time",
              tmpGraph
            ]
            & setStdin (byteStringInput (fromStrict input))
            & setEnv
              ( ("XDG_DATA_HOME", dataHome)
                  : filter ((/= "XDG_DATA_HOME") . fst) environment
              )
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

data BaseGraphLine
  = BaseEdge (Edge Text)
  | -- | An edge that only one of its two nodes records
    BaseHalfEdge RecordSide (Edge Text)
  | BaseData NID ByteString
  | BaseNode NID
  | BaseDamage Damage

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
-- A graph that @ge@ cannot produce is written with: @0 -a-> 1 !out@ (only
-- node 0's outgoing record of the edge) and @0 -a-> 1 !in@ (only node 1's
-- incoming record), @missing 2@ (no file for node 2), @raw 2 "text"@ (the
-- file of node 2 holds exactly that text) and @id 2 3@ (the file of node 2
-- says its id is 3). The last three are applied in order once every node's
-- file is written.
writeBaseGraph :: FilePath -> ByteString -> IO ()
writeBaseGraph directory text = do
  items <-
    either (fail . ("invalid base graph: " <>)) pure $
      traverse parseLine (filter (not . null . words) (lines (decodeUtf8 text)))
  let edges = [e | BaseEdge e <- items]
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
    nodeId t = maybe (Left ("invalid node: " <> unpack t)) (Right . smallNID) (readMay t)

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
