{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Effect.Editor where

import Graph (NID)
import Graph.Serialize2
import MyPrelude
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
  InvokeEditor nids -> runProcess_ $ proc "vim" (nodeDataFile location <$> nids)
