{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE ViewPatterns #-}

module Effect.Graph.Check where

import MyPrelude

import Control.Monad.Freer

import Graph hiding (insertEdge, insertNode, setData)
import Graph.Connect

import Effect.Graph
import Effect.Console
import Effect.Graph.Advanced

-- fsck
--   :: forall t effs. (Member Console effs, HasGraph t effs)
--   => Eff effs ()
-- fsck = undefined
