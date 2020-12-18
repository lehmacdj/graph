module GraphEditorGoldenSpec where

import qualified Data.ByteString.Lazy.Char8 as BS
import System.FilePath
import System.FilePath.Glob
import System.Process.Typed
import Test.Tasty.Golden
import TestPrelude

inputFiles :: IO [FilePath]
inputFiles = globDir1 (compile "**/*.gs") "test/golden/ge"

combineOutErr :: LByteString -> LByteString -> LByteString
combineOutErr out err =
  out <> BS.pack "<<<<<<<<<out========err>>>>>>>>\n" <> err

runGraphEditor :: FilePath -> IO LByteString
runGraphEditor path = withTempGraph $ \tmpGraph -> do
  (out, err) <- readProcess_ $ shell $ "<" ++ path ++ " ge " ++ tmpGraph
  pure $ combineOutErr out err

mkGoldenTest :: FilePath -> TestTree
mkGoldenTest path =
  let testName = takeBaseName path
      goldenPath = replaceExtension path "golden"
   in goldenVsString testName goldenPath (runGraphEditor path)

test_graphEditor_golden_integrationTests :: IO [TestTree]
test_graphEditor_golden_integrationTests = map mkGoldenTest <$> inputFiles
