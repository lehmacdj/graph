{-# LANGUAGE AllowAmbiguousTypes #-}

module Graph.Utils where

import Control.Lens
import Error.Missing
import Graph.Effect
import Graph.FreshNID
import Graph.NodeLocated
import Models.Connect
import Models.Edge
import Models.Node
import MyPrelude
import Witherable

getNodes :: (ReadGraph t (Maybe ByteString) :> effs) => [NID] -> Eff effs [Node t (Maybe ByteString)]
getNodes = wither getNode

getNodeSem ::
  (Members [ReadGraph t a, Error Missing] effs) =>
  NID ->
  Eff effs (Node t a)
getNodeSem nid =
  getNode nid >>= \case
    Nothing -> throwMissing nid
    Just n -> pure n

getNodeDatalessSem ::
  (Members [ReadGraphDataless t, Error Missing] effs) =>
  NID ->
  Eff effs (Node t (Maybe ByteString))
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
  (Error Missing :> effs, HasGraph t effs) =>
  Node t (Maybe ByteString) ->
  Eff effs ()
insertNode n = do
  let nid = n ^. #nid
  touchNode @t nid
  setData @t nid n.rawData
  let esOut = toListOf (#outgoing . folded . to (outgoingEdge nid)) n
      esIn = toListOf (#incoming . folded . to (`incomingEdge` nid)) n
  forM_ esIn insertEdge
  forM_ esOut insertEdge

currentNode ::
  (Members [ReadGraph t (Maybe ByteString), GetLocation, Error Missing] effs) =>
  Eff effs (Node t (Maybe ByteString))
currentNode = currentLocation >>= getNodeSem

-- | nid `transitionsVia` t finds a node that can be transitioned to via the
-- transition t from the node at nid. ThrowMissing if nid not in graph
transitionsVia ::
  (Members [FreshNID, Error Missing] effs, HasGraph t effs) =>
  NID ->
  t ->
  Eff effs NID
transitionsVia nid t = do
  n <- getNodeSem nid
  case matchConnect t n.outgoing of
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
  Eff effs NID
transitionsFreshVia nid t = do
  -- we need to actually try to fetch to throwError if it is missing
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
  Eff effs NID
transitionsViaManyFresh nid = \case
  [] -> pure nid
  [x] -> transitionsFreshVia nid x
  x : xs -> transitionsVia nid x >>= (`transitionsViaManyFresh` xs)

transitionsViaMany ::
  forall t effs.
  (Members [FreshNID, Error Missing] effs, HasGraph t effs) =>
  NID ->
  [t] ->
  Eff effs NID
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
  Eff effs ()
transitionsViaManyTo s transitions t = do
  secondToLast <- transitionsViaMany s (toList (init transitions))
  insertEdge (Edge secondToLast (last transitions) t)

-- | Merge @other@ into @survivor@: @survivor@ keeps its NID and its data
-- and gains all of @other@'s edges (edges between the two become self
-- loops); @other@ and its data are deleted. Merging a node into itself does
-- nothing.
mergeNode ::
  forall t effs.
  (Members [FreshNID, Error Missing] effs, HasGraph t effs) =>
  NID ->
  NID ->
  Eff effs ()
mergeNode survivor other
  | survivor == other = pure ()
  | otherwise = do
      n1 <- getNodeSem survivor
      n2 <- getNodeSem other
      let cin =
            #incoming
              %~ selfLoopify other survivor . (`union` view #incoming n2)
          cout =
            #outgoing %~ selfLoopify other survivor . (`union` n2.outgoing)
          nNew = cin . cout $ n1
      deleteNode @t survivor
      deleteNode @t other
      insertNode @t nNew

-- | Merge each of @others@ into @survivor@ (see 'mergeNode'): it keeps its
-- NID and its data, and the data of the others is discarded.
mergeNodes ::
  forall t effs mono.
  ( Members [FreshNID, Error Missing] effs,
    HasGraph t effs,
    MonoFoldable mono,
    Element mono ~ NID
  ) =>
  NID ->
  mono ->
  Eff effs ()
mergeNodes survivor = mapM_ (mergeNode @t survivor)

-- | Create a node with the same transitions as the original node.
-- Self loops are preserved as self loops on the new node.
cloneNode ::
  forall t effs.
  (Members [FreshNID, Error Missing] effs, HasGraph t effs) =>
  NID ->
  Eff effs NID
cloneNode nid = do
  nid' <- freshNID
  n <- getNodeSem nid
  let i = selfLoopify nid nid' n.incoming
      o = selfLoopify nid nid' n.outgoing
  insertNode @t (Node nid' i o n.rawData)
  pure nid'

-- | Caution: using this function is unsafe if the number re-written to is
-- defined in the graph. It will probably overwrite stuff in unpredictable
-- ways in that case.
unsafeRenumberNode ::
  (Error Missing :> effs, HasGraph Text effs) =>
  -- | Node to rewrite on left, number to be rewritten to on right
  (NID, NID) ->
  Eff effs ()
unsafeRenumberNode (nid, nid') = do
  n <- getNodeSem nid
  let i = selfLoopify nid nid' n.incoming
      o = selfLoopify nid nid' n.outgoing
  insertNode @Text (Node nid' i o n.rawData)
  deleteNode @Text nid
