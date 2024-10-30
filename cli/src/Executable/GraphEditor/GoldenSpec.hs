{-# LANGUAGE OverloadedStrings #-}

module Executable.GraphEditor.GoldenSpec where

import Data.Attoparsec.ByteString
import qualified Data.ByteString as B
import System.FilePath
import System.FilePath.Glob
import System.Process.Typed
import Test.Tasty.Golden
import TestPrelude

data GoldenTest = GoldenTest
  { -- | Relative path (from the root of the project) to a graph to use as a
    -- template for the temporary graph that the test is run on.
    template :: Maybe ByteString,
    -- | input to be fed to @ge@ via stdin
    input :: ByteString,
    stdOut :: ByteString,
    stdErr :: ByteString,
    graphDump :: ByteString
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
pBody end = concat <$> manyTill (takeWhileIncluding (not . nl)) end <?> "body"
  where
    nl = (== 10)

pGoldenTest :: Parser GoldenTest
pGoldenTest =
  GoldenTest
    <$> pSectionHeader pInputHeader
    <*> (pBody (pSectionHeader (void $ string "stdout")) <?> "input")
    <*> (pBody (pSectionHeader (void $ string "stderr")) <?> "stdout")
    <*> (pBody (pSectionHeader (void $ string "graph-dump")) <?> "stderr")
    <*> (pBody (pSectionHeader endOfInput) <?> "graph-dump")
    <?> "golden test"

parseGoldenTest :: ByteString -> Either String GoldenTest
parseGoldenTest = parseOnly pGoldenTest

sectionHeader :: ByteString -> LByteString
sectionHeader name = "==========" <> fromStrict name <> "==========\n"

renderGoldenTest :: GoldenTest -> LByteString
renderGoldenTest GoldenTest {..} =
  concat
    [ sectionHeader
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
      fromStrict graphDump
    ]

-- | Runs `ge` with the input and produces a new `GoldenTest` containing the
-- actual output produced by running it
runGraphEditor :: GoldenTest -> IO GoldenTest
runGraphEditor test@GoldenTest {..} =
  withTempGraph (pathForTemplate <$> template) $ \tmpGraph -> do
    (stdOut', stdErr') <-
      readProcess_ $
        proc
          "ge"
          [ "--test-only-nid-generation-seed",
            "0",
            "--test-only-monotonic-increasing-deterministic-time",
            tmpGraph
          ]
          & setStdin (byteStringInput (fromStrict input))
    (graphDump', _) <- readProcess_ $ proc "dump-graph" [tmpGraph]
    pure $
      test
        { stdOut = toStrict stdOut',
          stdErr = toStrict stdErr',
          graphDump = toStrict graphDump'
        }

mkGoldenTest :: FilePath -> TestTree
mkGoldenTest path =
  let testName = takeBaseName path
   in goldenVsString testName path do
        (B.readFile path <&> parseGoldenTest) >>= \case
          Right goldenTest -> runGraphEditor goldenTest <&> renderGoldenTest
          Left err -> error $ "failed to parse golden input:\n" <> err

inputFiles :: IO [FilePath]
inputFiles = globDir1 (compile "**/*.golden") "test/golden/ge"

test_integration :: IO [TestTree]
test_integration = map mkGoldenTest <$> inputFiles
