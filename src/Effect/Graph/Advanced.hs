{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE ViewPatterns #-}

module Effect.Graph.Advanced where

import ClassyPrelude

import Control.Monad.Freer
import Control.Monad.Freer.Fresh
import Control.Lens
import Data.Witherable

import Graph hiding (insertEdge, insertNode, setData)
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

-- | Make it so that the node in the graph with nid = nidOf n, has supersets of
-- in and out transitions present in n.
-- Also sets the data to the data in n.
-- We can't delete edges, because in some implementations that requires
-- mapping over every other node, which might be too expensive.
insertNode
  :: forall t effs. (Member ThrowMissing effs,  HasGraph t effs)
  => Node t -> Eff effs ()
insertNode n = do
  let nid = nidOf n
  touchNode @t nid
  setData @t nid (dataOf n)
  let
    esOut = toListOf (folded . to (outgoingEdge nid)) $ outgoingConnectsOf n
    esIn = toListOf (folded . to (`incomingEdge` nid)) $ incomingConnectsOf n
  forM_ esIn $ insertEdge
  forM_ esOut $ insertEdge
  pure ()

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

mergeNode
  :: forall t effs. (Members [Fresh, ThrowMissing] effs, HasGraph t effs)
  => Id -> Id -> Eff effs Id
mergeNode nid1 nid2 = do
  n1 <- getNode' nid1
  n2 <- getNode' nid2
  let
    cin = nodeIncoming %~ selfLoopify nid2 nid1 . (`union` incomingConnectsOf n2)
    cout = nodeOutgoing %~ selfLoopify nid2 nid1 . (`union` outgoingConnectsOf n2)
    nNew = cin . cout $ n1
  deleteNode @t nid1
  deleteNode @t nid2
  insertNode @t nNew
  pure (nidOf nNew)

mergeNodes
  :: forall t effs. (Members [Fresh, ThrowMissing] effs, HasGraph t effs)
  => NonNull [Id] -> Eff effs Id
mergeNodes (splitFirst -> (nid, nids)) = foldM (mergeNode @t) nid nids
