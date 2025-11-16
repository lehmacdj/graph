{-# LANGUAGE TemplateHaskell #-}

module Effect.IOWrapper.Editor where

import Effectful
import Effectful.Dispatch.Dynamic
import Effectful.TH
import MyPrelude
import System.Directory
import System.Environment
import System.Process.Typed

data Editor :: Effect where
  InvokeEditor :: [FilePath] -> Editor m ()

makeEffect ''Editor

-- | Makes the assumption that the graph is implemented using the filesystem
-- structure with nid.json/nid.data for each node
interpretEditorAsIOVim ::
  (IOE :> es) =>
  -- | interprets effect
  Eff (Editor : es) a ->
  Eff es a
interpretEditorAsIOVim = interpret $ \_ -> \case
  InvokeEditor paths -> do
    editor <- liftIO $ getEnv "EDITOR"
    editorToUse <- fromMaybe "vim" <$> liftIO (findExecutable editor)
    runProcess_ $ proc editorToUse paths
