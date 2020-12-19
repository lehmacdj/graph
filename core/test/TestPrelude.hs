module TestPrelude
  ( module MyPrelude,
    module Test.Tasty,
    module Test.Tasty.HUnit,
    withTempGraph,
  )
where

import Graph.Serialize2 (initializeGraph)
import MyPrelude hiding (assert)
import System.Directory (createDirectoryIfMissing)
import Test.Tasty
import Test.Tasty.HUnit

-- | initializes an empty graph putting it in a temporary directory that is
-- deleted after use
withTempGraph :: (FilePath -> IO a) -> IO a
withTempGraph action = do
  createDirectoryIfMissing False ".tmp"
  withTempDirectory ".tmp" "test.g" $ \tmpDir -> do
    initializeGraph tmpDir
    action tmpDir
