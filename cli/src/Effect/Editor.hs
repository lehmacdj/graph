{-# LANGUAGE TemplateHaskell #-}

module Effect.Editor where

import DAL.Serialization
import Models.NID (NID)
import MyPrelude
import System.Directory
import System.Environment
import System.Process.Typed

data Editor m r where
  InvokeEditor :: [NID] -> Editor m ()

makeSem ''Editor

-- | Makes the assumption that the graph is implemented using the filesystem
-- structure with nid.json/nid.data for each node
interpretEditorAsIOVimFSGraph ::
  (Member (Embed IO) effs) =>
  -- | a filepath to find the graph under
  FilePath ->
  -- | interprets effect
  Sem (Editor : effs) ~> Sem effs
interpretEditorAsIOVimFSGraph location = interpret $ \case
  InvokeEditor nids -> do
    editor <- embed $ getEnv "EDITOR"
    editorToUse <- fromMaybe "vim" <$> embed (findExecutable editor)
    runProcess_ $ proc editorToUse (nodeDataFile location <$> nids)
