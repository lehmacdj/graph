{-# LANGUAGE TemplateHaskell #-}

module Effect.IOWrapper.Editor where

import MyPrelude
import System.Directory
import System.Environment
import System.Process.Typed

data Editor m r where
  InvokeEditor :: [FilePath] -> Editor m ()

makeSem ''Editor

-- | Makes the assumption that the graph is implemented using the filesystem
-- structure with nid.json/nid.data for each node
interpretEditorAsIOVimFSGraph ::
  (Member (Embed IO) effs) =>
  -- | interprets effect
  Sem (Editor : effs) ~> Sem effs
interpretEditorAsIOVimFSGraph = interpret $ \case
  InvokeEditor paths -> do
    editor <- embed $ getEnv "EDITOR"
    editorToUse <- fromMaybe "vim" <$> embed (findExecutable editor)
    runProcess_ $ proc editorToUse paths
