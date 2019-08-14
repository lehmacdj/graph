{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Effect.Graph.Advanced where

import ClassyPrelude

import Control.Monad.Freer
import Control.Monad.Freer.Fresh
import Data.Witherable

import Graph hiding (insertEdge)
import Graph.Connect

import Effect.Graph
import Effect.NodeLocated
import Effect.Throw

getNodes :: Member (ReadGraph t) effs => [Id] -> Eff effs [Node t]
getNodes = wither getNode

getNode'
  :: Members [ReadGraph t, ThrowMissing] effs
  => Id -> Eff effs (Node t)
getNode' nid = getNode nid >>= \case
  Nothing -> throwMissing nid
  Just n -> pure n

currentNode
  :: Members [ReadGraph t, NodeLocated, ThrowMissing] effs
  => Eff effs (Node t)
currentNode = currentLocation >>= getNode'

-- | nid `transitionsVia` t finds a node that can be transitioned to via the
-- transition t from the node at nid. ThrowMissing if nid not in graph
transitionsVia
  :: (Members [Fresh, ThrowMissing] effs, HasGraph t effs)
  => Id -> t -> Eff effs Id
transitionsVia nid t = do
  n <- getNode' nid
  case matchConnect t (outgoingConnectsOf n) of
    [] -> nid `transitionsFreshVia` t
    -- generalize the choice of node here to an effect?
    -- makes sense if we run into trouble with this choosing an arbitrary trans
    n':_ -> pure n'

-- | nid `transitionsFreshVia` t creates a new transition t from the node at nid
-- and returns the error, or throws if something went wrong
transitionsFreshVia
  :: forall t effs. (Members [Fresh, ThrowMissing] effs, HasGraph t effs)
  => Id -> t -> Eff effs Id
transitionsFreshVia nid t = do
  -- we need to actually try to fetch to throw if it is missing
  _ <- getNode' @t nid
  nid' <- fresh
  insertEdge (Edge nid t nid')
  pure nid'
