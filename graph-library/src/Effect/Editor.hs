{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE TemplateHaskell #-}

module Effect.Editor where

import MyPrelude

import Control.Monad.Freer.TH

import Effect.Executable
import Graph.Serialize2

data Editor r where
  InvokeEditor :: [NID] -> Editor ()
makeEffect ''Editor

-- | Makes the assumption that the graph is implemented using the filesystem
-- structure with nid.json/nid.data for each node
interpretEditorAsExecutableVimFSGraph
  :: Member Execute effs
  => FilePath -- ^ a filepath to find the graph under
  -> Eff (Editor : effs) ~> Eff effs -- ^ interprets effect
interpretEditorAsExecutableVimFSGraph location = interpret $ \case
  InvokeEditor nids -> do
    executeProcess "vim" (nodeDataFile location <$> nids)
