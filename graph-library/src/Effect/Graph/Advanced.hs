{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Effect.Graph.Advanced where

import Control.Lens
import Data.Witherable.Class
import Effect.FreshNID
import Effect.Graph
import Effect.NodeLocated
import Effect.Throw
import Graph hiding (insertEdge, insertNode, setData)
import Graph.Connect
import MyPrelude

getNodes :: Member (ReadGraph t) effs => [NID] -> Eff effs [Node t]
getNodes = wither getNode

getNode' ::
  Members [ReadGraph t, ThrowMissing] effs =>
  NID ->
  Eff effs (Node t)
getNode' nid =
  getNode nid >>= \case
    Nothing -> throwMissing nid
    Just n -> pure n

-- | Make it so that the node in the graph with nid = nidOf n, has supersets of
-- in and out transitions present in n.
-- Also sets the data to the data in n.
-- We can't delete edges, because in some implementations that requires
-- mapping over every other node, which might be too expensive.
insertNode ::
  forall t effs.
  (Member ThrowMissing effs, HasGraph t effs) =>
  Node t ->
  Eff effs ()
insertNode n = do
  let nid = nidOf n
  touchNode @t nid
  setData @t nid (dataOf n)
  let esOut = toListOf (nodeOutgoing . folded . to (outgoingEdge nid)) n
      esIn = toListOf (nodeIncoming . folded . to (`incomingEdge` nid)) n
  forM_ esIn $ insertEdge
  forM_ esOut $ insertEdge
  pure ()

currentNode ::
  Members [ReadGraph t, GetLocation, ThrowMissing] effs =>
  Eff effs (Node t)
currentNode = currentLocation >>= getNode'

-- | nid `transitionsVia` t finds a node that can be transitioned to via the
-- transition t from the node at nid. ThrowMissing if nid not in graph
transitionsVia ::
  (Members [FreshNID, ThrowMissing] effs, HasGraph t effs) =>
  NID ->
  t ->
  Eff effs NID
transitionsVia nid t = do
  n <- getNode' nid
  case matchConnect t (outgoingConnectsOf n) of
    [] -> nid `transitionsFreshVia` t
    -- generalize the choice of node here to an effect?
    -- makes sense if we run into trouble with this choosing an arbitrary trans
    n' : _ -> pure n'

-- | nid `transitionsFreshVia` t creates a new transition t from the node at nid
-- and returns the error, or throws if something went wrong
transitionsFreshVia ::
  forall t effs.
  (Members [FreshNID, ThrowMissing] effs, HasGraph t effs) =>
  NID ->
  t ->
  Eff effs NID
transitionsFreshVia nid t = do
  -- we need to actually try to fetch to throw if it is missing
  _ <- getNode' @t nid
  nid' <- freshNID
  touchNode @t nid'
  insertEdge (Edge nid t nid')
  pure nid'

-- | The list of nodes are taken in the graph, unless they don't exist or
-- except for the last edge which is always unique
transitionsViaManyFresh ::
  forall t effs.
  (Members [FreshNID, ThrowMissing] effs, HasGraph t effs) =>
  NID ->
  [t] ->
  Eff effs NID
transitionsViaManyFresh nid = \case
  [] -> pure nid
  [x] -> transitionsFreshVia nid x
  x : xs -> transitionsVia nid x >>= (`transitionsViaManyFresh` xs)

transitionsViaMany ::
  forall t effs.
  (Members [FreshNID, ThrowMissing] effs, HasGraph t effs) =>
  NID ->
  [t] ->
  Eff effs NID
transitionsViaMany nid = \case
  [] -> pure nid
  x : xs -> transitionsVia nid x >>= (`transitionsViaManyFresh` xs)

transitionsViaManyTo ::
  forall t effs seq.
  ( Members [FreshNID, ThrowMissing] effs,
    HasGraph t effs,
    IsSequence seq,
    Element seq ~ t
  ) =>
  NID ->
  NonNull seq ->
  NID ->
  Eff effs ()
transitionsViaManyTo s transitions t = do
  secondToLast <- transitionsViaMany s (toList (init transitions))
  insertEdge (Edge secondToLast (last transitions) t)

-- | Create one node that has all of the connects of the two nodes combined.
mergeNode ::
  forall t effs.
  (Members [FreshNID, ThrowMissing] effs, HasGraph t effs) =>
  NID ->
  NID ->
  Eff effs NID
mergeNode nid1 nid2 = do
  n1 <- getNode' nid1
  n2 <- getNode' nid2
  let cin = nodeIncoming %~ selfLoopify nid2 nid1 . (`union` incomingConnectsOf n2)
      cout = nodeOutgoing %~ selfLoopify nid2 nid1 . (`union` outgoingConnectsOf n2)
      nNew = cin . cout $ n1
  deleteNode @t nid1
  deleteNode @t nid2
  insertNode @t nNew
  pure (nidOf nNew)

-- | Create one node that unions together all of the connects of all of the
-- other nodes
mergeNodes ::
  forall t effs mono.
  ( Members [FreshNID, ThrowMissing] effs,
    HasGraph t effs,
    IsSequence mono,
    Element mono ~ NID
  ) =>
  NonNull mono ->
  Eff effs NID
mergeNodes nids = foldlM1 (mergeNode @t) nids

-- | Create a node with the same transitions as the original node.
-- Self loops are preserved as self loops on the new node.
cloneNode ::
  forall t effs.
  (Members [FreshNID, ThrowMissing] effs, HasGraph t effs) =>
  NID ->
  Eff effs NID
cloneNode nid = do
  nid' <- freshNID
  n <- getNode' nid
  let i = selfLoopify nid nid' $ incomingConnectsOf n
      o = selfLoopify nid nid' $ outgoingConnectsOf n
  insertNode @t (Node nid' i o (dataOf n))
  pure nid'
