{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE AllowAmbiguousTypes #-}

module Effect.Graph.Advanced where

import MyPrelude

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
    esOut = toListOf (nodeOutgoing . folded . to (outgoingEdge nid)) n
    esIn = toListOf (nodeIncoming . folded . to (`incomingEdge` nid)) n
  forM_ esIn $ insertEdge
  forM_ esOut $ insertEdge
  pure ()

currentNode
  :: Members [ReadGraph t, GetLocation, ThrowMissing] effs
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
  touchNode @t nid'
  insertEdge (Edge nid t nid')
  pure nid'

-- | The list of nodes are taken in the graph, unless they don't exist or
-- except for the last edge which is always unique
transitionsViaManyFresh
  :: forall t effs. (Members [Fresh, ThrowMissing] effs, HasGraph t effs)
  => Id -> [t] -> Eff effs Id
transitionsViaManyFresh nid = \case
  [] -> pure nid
  [x] -> transitionsFreshVia nid x
  x:xs -> transitionsVia nid x >>= (`transitionsViaManyFresh` xs)

-- | Create one node that has all of the connects of the two nodes combined.
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

-- | Create one node that unions together all of the connects of all of the
-- other nodes
mergeNodes
  :: forall t effs mono.
    ( Members [Fresh, ThrowMissing] effs
    , HasGraph t effs
    , IsSequence mono
    , Element mono ~ Id
    )
  => NonNull mono -> Eff effs Id
mergeNodes nids = foldlM1 (mergeNode @t) nids

-- | Create a node with the same transitions as the original node.
-- Self loops are preserved as self loops on the new node.
cloneNode
  :: forall t effs. (Members [Fresh, ThrowMissing] effs, HasGraph t effs)
  => Id -> Eff effs Id
cloneNode nid = do
  nid' <- fresh
  n <- getNode' nid
  let
    i = selfLoopify nid nid' $ incomingConnectsOf n
    o = selfLoopify nid nid' $ outgoingConnectsOf n
  insertNode @t (Node nid' i o (dataOf n))
  pure nid'
