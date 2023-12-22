module TestPrelude
  ( module MyPrelude,
    module X,
    withTempGraph,
    withEmptyTempGraph,
    representedByJson,
    runTests,
  )
where

import Data.Aeson
import Graph.Serialize2 (initializeGraph)
import MyPrelude hiding (assert)
import System.Directory (copyFile)
import System.Directory.Tree (AnchoredDirTree ((:/)))
import qualified System.Directory.Tree as DT
import Test.Hspec.Expectations as X
import Test.Tasty as X
import Test.Tasty.HUnit as X
import Test.Tasty.QuickCheck as X

-- | initializes a graph that is either empty, or based on a template graph
-- at the specified location
withTempGraph ::
  -- | Template graph (or Nothing for an empty graph)
  Maybe FilePath ->
  (FilePath -> IO a) ->
  IO a
withTempGraph m_template action = do
  withSystemTempDirectory "test.g" $ \tmpDir -> do
    case m_template of
      Just template -> do
        _ :/ readTree <- DT.build template
        whenNonNull (DT.failures readTree) \failures ->
          sayErr $ "reading template failed with: " <> tshow failures
        -- we need to overwrite the basename to @.@ so that we don't have an
        -- excess directory layer in the output
        let treeToWrite = tmpDir :/ (readTree & DT._name .~ ".")
        _ :/ writtenTree <- DT.writeDirectoryWith (flip copyFile) treeToWrite
        whenNonNull (DT.failures writtenTree) \failures ->
          sayErr $ "copying template failed with: " <> tshow failures
      Nothing -> initializeGraph tmpDir
    action tmpDir

withEmptyTempGraph :: (FilePath -> IO a) -> IO a
withEmptyTempGraph = withTempGraph Nothing

representedByJson ::
  (ToJSON a, FromJSON a, Eq a, Show a) => a -> LByteString -> TestTree
representedByJson x j =
  testGroup
    (show x ++ " has correct json representation")
    [ testCase "serializes" $ encode x `shouldBe` j,
      testCase "deserializes" $ eitherDecode j `shouldBe` Right x
    ]

-- | Convenience export for running tests in GHCi with a more ergonomic name
runTests :: TestTree -> IO ()
runTests = defaultMain
