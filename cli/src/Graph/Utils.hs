{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE LambdaCase #-}

module Graph.Utils where

import Control.Lens
import Data.Monoid (First (First))
import Error.Missing
import Error.UserError
import Graph.Effect
import Graph.FreshNID
import Graph.NodeLocated
import Models.Connect
import Models.Edge
import Models.Node
import MyPrelude
import Witherable

getNodes :: (Member (ReadGraph t (Maybe ByteString)) effs) => [NID] -> Sem effs [Node t (Maybe ByteString)]
getNodes = wither getNode

getNodeSem ::
  (Members [ReadGraph t a, Error Missing] effs) =>
  NID ->
  Sem effs (Node t a)
getNodeSem nid =
  getNode nid >>= \case
    Nothing -> throwMissing nid
    Just n -> pure n

getNodeDatalessSem ::
  (Members [ReadGraphDataless t, Error Missing] effs) =>
  NID ->
  Sem effs (Node t (Maybe ByteString))
getNodeDatalessSem nid =
  getNodeDataless nid >>= \case
    Nothing -> throwMissing nid
    Just n -> pure n

-- | Make it so that the node in the graph with n ^. #nid, has supersets of
-- in and out transitions present in n.
-- Also sets the data to the data in n.
-- We can't delete edges, because in some implementations that requires
-- mapping over every other node, which might be too expensive.
insertNode ::
  forall t effs.
  (Member (Error Missing) effs, HasGraph t effs) =>
  Node t (Maybe ByteString) ->
  Sem effs ()
insertNode n = do
  let nid = n ^. #nid
  touchNode @t nid
  setData @t nid n . rawData
  let esOut = toListOf (#outgoing . folded . to (outgoingEdge nid)) n
      esIn = toListOf (#incoming . folded . to (`incomingEdge` nid)) n
  forM_ esIn insertEdge
  forM_ esOut insertEdge

currentNode ::
  (Members [ReadGraph t (Maybe ByteString), GetLocation, Error Missing] effs) =>
  Sem effs (Node t (Maybe ByteString))
currentNode = currentLocation >>= getNodeSem

-- | nid `transitionsVia` t finds a node that can be transitioned to via the
-- transition t from the node at nid. ThrowMissing if nid not in graph
transitionsVia ::
  (Members [FreshNID, Error Missing] effs, HasGraph t effs) =>
  NID ->
  t ->
  Sem effs NID
transitionsVia nid t = do
  n <- getNodeSem nid
  case matchConnect t n . outgoing of
    [] -> nid `transitionsFreshVia` t
    -- generalize the choice of node here to an parameter?
    -- makes sense if we run into trouble with this choosing an arbitrary trans
    n' : _ -> pure n'

-- | nid `transitionsFreshVia` t creates a new transition t from the node at nid
-- and returns the error, or throws if something went wrong
transitionsFreshVia ::
  forall t effs.
  (Members [FreshNID, Error Missing] effs, HasGraph t effs) =>
  NID ->
  t ->
  Sem effs NID
transitionsFreshVia nid t = do
  -- we need to actually try to fetch to throw if it is missing
  _ <- getNodeSem @t nid
  nid' <- freshNID
  touchNode @t nid'
  insertEdge (Edge nid t nid')
  pure nid'

-- | The list of nodes are taken in the graph, unless they don't exist or
-- except for the last edge which is always unique
transitionsViaManyFresh ::
  forall t effs.
  (Members [FreshNID, Error Missing] effs, HasGraph t effs) =>
  NID ->
  [t] ->
  Sem effs NID
transitionsViaManyFresh nid = \case
  [] -> pure nid
  [x] -> transitionsFreshVia nid x
  x : xs -> transitionsVia nid x >>= (`transitionsViaManyFresh` xs)

transitionsViaMany ::
  forall t effs.
  (Members [FreshNID, Error Missing] effs, HasGraph t effs) =>
  NID ->
  [t] ->
  Sem effs NID
transitionsViaMany nid = \case
  [] -> pure nid
  x : xs -> transitionsVia nid x >>= (`transitionsViaMany` xs)

transitionsViaManyTo ::
  forall t effs seq.
  ( Members [FreshNID, Error Missing] effs,
    HasGraph t effs,
    IsSequence seq,
    Element seq ~ t
  ) =>
  NID ->
  NonNull seq ->
  NID ->
  Sem effs ()
transitionsViaManyTo s transitions t = do
  secondToLast <- transitionsViaMany s (toList (init transitions))
  insertEdge (Edge secondToLast (last transitions) t)

-- | Create one node that has all of the connects of the two nodes combined.
mergeNode ::
  forall t effs.
  (Members [FreshNID, Error Missing] effs, HasGraph t effs) =>
  NID ->
  NID ->
  Sem effs NID
mergeNode nid1 nid2 = do
  n1 <- getNodeSem nid1
  n2 <- getNodeSem nid2
  let cin = #incoming %~ selfLoopify nid2 nid1 . (`union` view #incoming n2)
      cout = #outgoing %~ selfLoopify nid2 nid1 . (`union` n2 . outgoing)
      -- take n2's data first so it is easier to overwrite data if desireable
      cdata = #augmentation .~ alaf First foldMap (view #augmentation) [n2, n1]
      nNew = cdata . cin . cout $ n1
  deleteNode @t nid1
  deleteNode @t nid2
  insertNode @t nNew
  pure (nNew ^. #nid)

-- | Create one node that unions together all of the connects of all of the
-- other nodes
mergeNodes ::
  forall t effs mono.
  ( Members [FreshNID, Error Missing] effs,
    HasGraph t effs,
    IsSequence mono,
    Element mono ~ NID
  ) =>
  NonNull mono ->
  Sem effs NID
mergeNodes = foldlM1 (mergeNode @t)

-- | Create a node with the same transitions as the original node.
-- Self loops are preserved as self loops on the new node.
cloneNode ::
  forall t effs.
  (Members [FreshNID, Error Missing] effs, HasGraph t effs) =>
  NID ->
  Sem effs NID
cloneNode nid = do
  nid' <- freshNID
  n <- getNodeSem nid
  let i = selfLoopify nid nid' n . incoming
      o = selfLoopify nid nid' n . outgoing
  insertNode @t (Node nid' i o n . rawData)
  pure nid'

-- | Caution: using this function is unsafe if the number re-written to is
-- defined in the graph. It will probably overwrite stuff in unpredictable
-- ways in that case.
unsafeRenumberNode ::
  (Member (Error Missing) effs, HasGraph String effs) =>
  -- | Node to rewrite on left, number to be rewritten to on right
  (NID, NID) ->
  Sem effs ()
unsafeRenumberNode (nid, nid') = do
  n <- getNodeSem nid
  let i = selfLoopify nid nid' n . incoming
      o = selfLoopify nid nid' n . outgoing
  insertNode @String (Node nid' i o n . rawData)
  deleteNode @String nid
