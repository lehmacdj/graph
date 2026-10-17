-- | Test helpers that make a graph on disk. Kept apart from 'Utils.Testing'
-- because they need the storage layer ('DAL.Serialization'), which the
-- modules that only test pure code (and are to build for WebAssembly) must
-- not reach.
module Utils.Testing.TempGraph
  ( withTempGraph,
    withEmptyTempGraph,
  )
where

import DAL.Serialization (initializeGraph)
import MyPrelude
import System.Directory (copyFile)
import System.Directory.Tree (AnchoredDirTree ((:/)))
import System.Directory.Tree qualified as DT

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
