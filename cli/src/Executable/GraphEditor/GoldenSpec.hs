module Executable.GraphEditor.GoldenSpec where

import DAL.DumpGraph
import DAL.Serialization (serializeNodeEx)
import Data.Attoparsec.ByteString
import Data.Attoparsec.Combinator (lookAhead)
import Data.ByteString qualified as B
import Data.Text qualified as T
import Models.Edge
import Models.Graph
import Models.NID
import Models.Node (Node' (..), emptyNode)
import MyPrelude
import System.Directory (listDirectory)
import System.FilePath
import System.FilePath.Glob
import System.Process.Typed
import Test.Tasty.Golden
import Utils.Testing

data GoldenTest = GoldenTest
  { -- | An inline base graph, written as lines @0 -a-> 1@ (edges between
    -- @smallNID@s, node 0 being the origin) and @1 data "text"@ (node
    -- data, as a Haskell string literal)
    baseGraph :: Maybe ByteString,
    -- | Relative path (from the root of the project) to a graph to use as a
    -- template for the temporary graph that the test is run on.
    template :: Maybe ByteString,
    -- | input to be fed to @ge@ via stdin
    input :: ByteString,
    stdOut :: ByteString,
    stdErr :: ByteString,
    graphDump :: ByteString,
    -- | Node data after the run, only for tests with an inline base graph
    -- (the graph dump does not show data)
    dataDump :: Maybe ByteString
  }
  deriving (Show)

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

pBody :: Parser () -> Parser ByteString
pBody end =
  concat
    <$> (manyTill (takeWhileIncluding (not . nl)) end <|> pure [])
    <?> "body"
  where
    nl = (== 10)

pGoldenTest :: Parser GoldenTest
pGoldenTest =
  GoldenTest
    <$> optional
      ( pSectionHeader (void $ string "graph")
          *> pBody (void . lookAhead $ pSectionHeader pInputHeader)
      )
    <*> pSectionHeader pInputHeader
    <*> (pBody (pSectionHeader (void $ string "stdout")) <?> "input")
    <*> (pBody (pSectionHeader (void $ string "stderr")) <?> "stdout")
    <*> (pBody (pSectionHeader (void $ string "graph-dump")) <?> "stderr")
    <*> (pBody (pSectionHeader endOfInput) <?> "graph-dump")
    <*> pure Nothing
    <?> "golden test"

parseGoldenTest :: ByteString -> Either String GoldenTest
parseGoldenTest = parseOnly pGoldenTest

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
      fromStrict stdOut,
      sectionHeader "stderr",
      fromStrict stdErr,
      sectionHeader "graph-dump",
      fromStrict graphDump,
      maybe "" ((sectionHeader "data" <>) . fromStrict) dataDump
    ]

-- | Runs `ge` with the input and produces a new `GoldenTest` containing the
-- actual output produced by running it
runGraphEditor :: GoldenTest -> IO GoldenTest
runGraphEditor test@GoldenTest {..} =
  withTempGraph (pathForTemplate <$> template) $ \tmpGraph -> do
    for_ baseGraph (writeBaseGraph tmpGraph)
    (stdOut', stdErr') <-
      readProcess_ $
        proc
          "ge"
          [ "--test-only-nid-generation-seed",
            "0",
            "--test-only-monotonic-increasing-deterministic-time",
            "--no-dry-run-writing",
            tmpGraph
          ]
          & setStdin (byteStringInput (fromStrict input))
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
          graphDump = toStrict $ builderToLazy graphDump',
          dataDump = dataDump'
        }

-- | Write the nodes of an inline base graph.
writeBaseGraph :: FilePath -> ByteString -> IO ()
writeBaseGraph directory text = do
  (edges, nodeData) <-
    either (fail . ("invalid base graph: " <>)) pure $
      partitionEithers <$> traverse parseLine (filter (not . null . words) (lines (decodeUtf8 text)))
  let graph :: Graph Text ()
      graph = insertEdges edges (insertNode (emptyNode nilNID) emptyGraph)
      dataByNID = mapFromList nodeData :: Map NID ByteString
      withData = foldl' (\g n -> if n `member` nidSet g then g else insertNode (emptyNode n) g) graph (keys dataByNID)
  for_ (nodesOf withData) \n ->
    serializeNodeEx (n $> lookup n.nid dataByNID) directory
  where
    parseLine :: Text -> Either String (Either (Edge Text) (NID, ByteString))
    parseLine line = case words line of
      [s, arrow, t]
        | Just label <- stripPrefix "-" arrow >>= stripSuffix "->" ->
            (\a b -> Left (Edge a label b)) <$> nodeId s <*> nodeId t
      (n : "data" : _) ->
        let literal = drop 4 (snd (T.breakOn "data" line))
         in case readMay (unpack literal) of
              Just (str :: String) -> (\x -> Right (x, encodeUtf8 (pack str))) <$> nodeId n
              Nothing -> Left ("invalid data line: " <> unpack line)
      _ -> Left ("invalid line: " <> unpack line)
    nodeId t = maybe (Left ("invalid node: " <> unpack t)) (Right . smallNID) (readMay t)

mkGoldenTest :: FilePath -> TestTree
mkGoldenTest path =
  let testName = takeBaseName path
   in goldenVsStringDiff testName (\ref new -> ["diff", "-u", ref, new]) path do
        (B.readFile path <&> parseGoldenTest) >>= \case
          Right golden -> runGraphEditor golden <&> renderGoldenTest
          Left err -> error $ "failed to parse golden input:\n" <> err

inputFiles :: IO [FilePath]
inputFiles = globDir1 (compile "**/*.golden") "test/golden/ge"

test_integration :: IO [TestTree]
test_integration = map mkGoldenTest <$> inputFiles
