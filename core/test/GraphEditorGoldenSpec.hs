{-# LANGUAGE OverloadedStrings #-}

module GraphEditorGoldenSpec where

import Data.Attoparsec.ByteString
import qualified Data.ByteString as B
import System.FilePath
import System.FilePath.Glob
import System.Process.Typed
import Test.Tasty.Golden
import TestPrelude

data GraphEditorConfig = GraphEditorConfig
  { -- | Relative path (from the root of the project) to a graph to use as a
    -- template for the temporary graph that the test is run on.
    template :: Maybe FilePath,
    -- | input to be fed to @ge@ via stdin
    input :: ByteString
  }
  deriving (Show)

pTemplateDirective :: Parser FilePath
pTemplateDirective =
  ("examples" </>) . unpack . decodeUtf8
    <$> (string "@from-example " *> takeTill nl <* anyWord8)
  where
    nl = (== 10)

pGraphEditorConfig :: Parser GraphEditorConfig
pGraphEditorConfig =
  GraphEditorConfig
    <$> optional pTemplateDirective
    <*> takeByteString

parseGraphEditorConfig :: ByteString -> Either String GraphEditorConfig
parseGraphEditorConfig = parseOnly pGraphEditorConfig

combineOutErrDump :: LByteString -> LByteString -> LByteString -> LByteString
combineOutErrDump out err dump =
  "==========stdout==========\n"
    <> out
    <> "==========stderr==========\n"
    <> err
    <> "==========graph-dump==========\n"
    <> dump

runGraphEditor :: GraphEditorConfig -> IO LByteString
runGraphEditor GraphEditorConfig {..} = withTempGraph template $ \tmpGraph -> do
  (out, err) <-
    readProcess_ $
      proc "ge" [tmpGraph] & setStdin (byteStringInput (fromStrict input))
  (dump, _) <- readProcess_ $ proc "dump-graph" [tmpGraph]
  pure $ combineOutErrDump out err dump

mkGoldenTest :: FilePath -> TestTree
mkGoldenTest path =
  let testName = takeBaseName path
      goldenPath = replaceExtension path "golden"
   in goldenVsString testName goldenPath do
        (B.readFile path <&> parseGraphEditorConfig) >>= \case
          Right config -> runGraphEditor config
          Left err -> error $ "failed to parse golden input:\n" <> err

inputFiles :: IO [FilePath]
inputFiles = globDir1 (compile "**/*.gs") "test/golden/ge"

test_graphEditor_golden_integrationTests :: IO [TestTree]
test_graphEditor_golden_integrationTests = map mkGoldenTest <$> inputFiles
