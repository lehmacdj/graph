{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Effect.Editor where

import Control.Monad.Freer.TH
import Graph (NID)
import Graph.Serialize2
import MyPrelude
import System.Process.Typed

data Editor r where
  InvokeEditor :: [NID] -> Editor ()

makeEffect ''Editor

-- | Makes the assumption that the graph is implemented using the filesystem
-- structure with nid.json/nid.data for each node
interpretEditorAsIOVimFSGraph ::
  (MonadIO m, LastMember m effs) =>
  -- | a filepath to find the graph under
  FilePath ->
  -- | interprets effect
  Eff (Editor : effs) ~> Eff effs
interpretEditorAsIOVimFSGraph location = interpret $ \case
  InvokeEditor nids -> runProcess_ $ proc "vim" (nodeDataFile location <$> nids)
