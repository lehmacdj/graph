{-# LANGUAGE OverloadedStrings #-}

module GraphEditorGoldenSpec where

import System.FilePath
import System.FilePath.Glob
import System.Process.Typed
import Test.Tasty.Golden
import TestPrelude

inputFiles :: IO [FilePath]
inputFiles = globDir1 (compile "**/*.gs") "test/golden/ge"

combineOutErrDump :: LByteString -> LByteString -> LByteString -> LByteString
combineOutErrDump out err dump =
  "==========stdout==========\n"
    <> out
    <> "==========stderr==========\n"
    <> err
    <> "==========graph-dump==========\n"
    <> dump

runGraphEditor :: FilePath -> IO LByteString
runGraphEditor path = withTempGraph $ \tmpGraph -> do
  (out, err) <- readProcess_ $ shell $ "<" ++ path ++ " ge " ++ tmpGraph
  (dump, _) <- readProcess_ $ proc "dump-graph" [tmpGraph]
  pure $ combineOutErrDump out err dump

mkGoldenTest :: FilePath -> TestTree
mkGoldenTest path =
  let testName = takeBaseName path
      goldenPath = replaceExtension path "golden"
   in goldenVsString testName goldenPath (runGraphEditor path)

test_graphEditor_golden_integrationTests :: IO [TestTree]
test_graphEditor_golden_integrationTests = map mkGoldenTest <$> inputFiles
