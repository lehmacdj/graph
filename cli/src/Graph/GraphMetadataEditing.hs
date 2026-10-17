{-# LANGUAGE TemplateHaskell #-}

-- | The effect of reading and changing the nodes and edges of a graph (its
-- metadata), and the interpreters of it that keep the graph in memory.
--
-- What stores the graph interprets this effect elsewhere: the node files
-- in "DAL.Interpreters.GraphMetadataEditing". This module, and what is
-- built on it, depends on no storage.
module Graph.GraphMetadataEditing where

import Effectful.State.Dynamic
import Models.Augmentation.IsThin
import Models.Edge
import Models.Graph (Graph, emptyGraph)
import Models.Graph qualified
import Models.NID
import Models.Node
import MyPrelude

-- | Marker indicating that something promises to only read the graph metadata.
-- I gave up on trying to enforce this at the type level because it ended up
-- being really hard to get type inference to infer all the things I wanted it
-- to.
type GraphMetadataReading' t = GraphMetadataEditing' t

type GraphMetadataReading = GraphMetadataReading' Text

data GraphMetadataEditing' t :: Effect where
  GetNodeMetadata :: NID -> GraphMetadataEditing' t m (Maybe (Node t ()))
  -- This barely needs to exist because InsertEdge can also be used to create
  -- a node if it doesn't exist. The only reason you would want to use this
  -- is if you need to create a disconnected node for some reason, or if we
  -- intercept touch node to provide some other functionality (e.g. to update a
  -- timestamp on the node, without needing to know an edge that points to the
  -- node, but you'd probably want to build that by intercepting
  -- GraphMetadataFilesystemOperations to only update when the FS is actually
  -- updated)
  -- I may want to turn this into an assertion that the node needs to be created
  -- and allow nodes to not be created if we insert an edge to a non-existing
  -- node and then later remove that same edge without committing the changes to
  -- the disk inbetween. If we do this, it would be essential to actually create
  -- edges.
  TouchNode :: NID -> GraphMetadataEditing' t m ()
  DeleteNode :: NID -> GraphMetadataEditing' t m ()
  InsertEdge :: Edge t -> GraphMetadataEditing' t m ()
  DeleteEdge :: Edge t -> GraphMetadataEditing' t m ()
  -- | Every node in the graph. This is expensive for large graphs; it is
  -- meant for whole-graph operations like consistency checks.
  ListNodes :: GraphMetadataEditing' t m (Set NID)

makeEffect ''GraphMetadataEditing'

type GraphMetadataEditing = GraphMetadataEditing' Text

-- | Makes it so all reads to a node will return the same result within the
-- action by caching fetched nodes in memory
cachingReadingInMemory ::
  forall t a r.
  ( GraphMetadataReading' t :> r,
    ValidTransition t,
    HasCallStack
  ) =>
  Eff (GraphMetadataReading' t : r) a ->
  Eff r a
cachingReadingInMemory =
  evalStateLocal @(Set NID) mempty
    . evalStateLocal @(Graph t IsThin) emptyGraph
    . cachingReadingInState
    . inject

cachingReadingInState ::
  forall t a r.
  ( GraphMetadataReading' t :> r,
    Members
      [ -- the graph as it's been fetched so far
        State (Graph t IsThin),
        -- the set of nodes we tried to fetch but did not exist
        State (Set NID)
      ]
      r,
    ValidTransition t,
    HasCallStack
  ) =>
  Eff (GraphMetadataReading' t : r) a ->
  Eff r a
cachingReadingInState = interpret \_ -> \case
  GetNodeMetadata nid -> withEarlyReturn do
    whenM (gets @(Set NID) (member nid)) (returnEarly Nothing)
    cached <-
      gets @(Graph t IsThin) $
        preview (ix nid . asideMetadata (only Fetched))
    withJust cached (returnEarly . Just)
    n <-
      getNodeMetadata nid `onNothingM` do
        modify @(Set NID) $ insertSet nid
        returnEarly Nothing
    -- stored as it was read: storing it with @at@ would rewrite the nodes
    -- read before it to agree with it about the edges between them
    modify @(Graph t IsThin) $ #nodeMap . at nid ?~ (Fetched <$ n)
    pure $ Just n
  TouchNode _ -> error "TouchNode is unsafe while caching reading"
  DeleteNode _ -> error "DeleteNode is unsafe while caching reading"
  InsertEdge _ -> error "InsertEdge is unsafe while caching reading"
  DeleteEdge _ -> error "DeleteEdge is unsafe while caching reading"
  ListNodes -> listNodes @t

runInMemoryGraphMetadataEditing ::
  forall t r a.
  (State (Graph t ()) :> r, ValidTransition t) =>
  Eff (GraphMetadataEditing' t : r) a ->
  Eff r a
runInMemoryGraphMetadataEditing = interpret \_ -> \case
  GetNodeMetadata nid -> gets @(Graph t ()) $ view (at nid)
  TouchNode nid ->
    modify @(Graph t ()) $
      -- it is important that we only write emptyNode if the node does not exist
      -- otherwise we will overwrite the node with an empty node
      at nid %~ Just . fromMaybe (emptyNode nid)
  DeleteNode nid -> modify @(Graph t ()) $ at nid .~ Nothing
  InsertEdge edge -> modify $ Models.Graph.insertEdge edge
  DeleteEdge edge -> modify $ Models.Graph.deleteEdge edge
  ListNodes -> gets @(Graph t ()) $ keysSet . view #nodeMap

graphFromGraphMetadataEditing ::
  forall t a.
  ( ValidTransition t,
    HasCallStack
  ) =>
  Eff '[GraphMetadataEditing' t] a ->
  Graph t ()
graphFromGraphMetadataEditing =
  runPureEff
    . execStateLocal emptyGraph
    . runInMemoryGraphMetadataEditing
    . inject
