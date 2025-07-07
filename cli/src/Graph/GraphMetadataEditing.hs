{-# LANGUAGE TemplateHaskell #-}

module Graph.GraphMetadataEditing where

import Control.Lens (prism')
import DAL.FileSystemOperations.Metadata
import DAL.FileSystemOperations.MetadataWriteDiff
import Error.UserError
import Error.Warn
import Models.Edge
import Models.Graph (Graph, emptyGraph)
import Models.Graph qualified
import Models.NID
import Models.Node
import MyPrelude
import Polysemy.Scoped
import Polysemy.State
import Polysemy.Tagged

data IsThin = Thin | Fetched
  deriving (Eq, Show, Ord, Generic)

instance ShowableAugmentation IsThin where
  augmentationLabel = Nothing
  defaultShowAugmentation = \case
    Thin -> "thin"
    Fetched -> "fetched"
  shouldShowStandaloneAugmentation = True

instance DefaultAugmentation IsThin where
  defaultAugmentation = Thin

onlyFetched :: Prism' (Node t IsThin) (Node t ())
onlyFetched = prism' (fmap (const Fetched)) \case
  n | n.augmentation == Fetched -> Just $ void n
  _ -> Nothing

-- | Marker indicating that something promises to only read the graph metadata.
-- I gave up on trying to enforce this at the type level because it ended up
-- being really hard to get type inference to infer all the things I wanted it
-- to.
type GraphMetadataReading t = GraphMetadataEditing t

data GraphMetadataEditing t m a where
  GetNodeMetadata :: NID -> GraphMetadataEditing t m (Maybe (Node t ()))
  -- This barely needs to exist because InsertEdge can also be used to create
  -- a node if it doesn't exist. The only reason you would want to use this
  -- is if you need to create a disconnected node for some reason, or if we
  -- intercept touch node to provide some other functionality (e.g. to update a
  -- timestamp on the node, without needing to know an edge that points to the
  -- node, but you'd probably want to build that by intercepting
  -- GraphMetadataFilesystemOperations to only update when the FS is actually
  -- updated)
  TouchNode :: NID -> GraphMetadataEditing t m ()
  DeleteNode :: NID -> GraphMetadataEditing t m ()
  InsertEdge :: Edge t -> GraphMetadataEditing t m ()
  DeleteEdge :: Edge t -> GraphMetadataEditing t m ()

makeSem ''GraphMetadataEditing

-- | Makes it so all reads to a node will return the same result within the
-- action by caching fetched nodes in memory
cachingReadingInMemory ::
  forall t a r.
  ( Member (GraphMetadataReading t) r,
    ValidTransition t,
    HasCallStack
  ) =>
  Sem (GraphMetadataReading t : r) a ->
  Sem r a
cachingReadingInMemory =
  evalState @(Set NID) mempty
    . evalState @(Graph t IsThin) emptyGraph
    . cachingReadingInState
    . raiseUnder @(State (Graph t IsThin))
    . raiseUnder @(State (Set NID))

cachingReadingInState ::
  forall t a r.
  ( Member (GraphMetadataReading t) r,
    Members [State (Graph t IsThin), State (Set NID)] r,
    ValidTransition t,
    HasCallStack
  ) =>
  Sem (GraphMetadataReading t : r) a ->
  Sem r a
cachingReadingInState = interpret \case
  GetNodeMetadata nid -> withEarlyReturn do
    whenM (gets @(Set NID) (member nid)) (returnEarly Nothing)
    cached <- gets @(Graph t IsThin) $ preview (ix nid . onlyFetched)
    withJust cached (returnEarly . Just)
    n <-
      getNodeMetadata nid `onNothingM` do
        modify @(Set NID) $ insertSet nid
        returnEarly Nothing
    modify @(Graph t IsThin) $ at nid ?~ (Fetched <$ n)
    pure $ Just n
  TouchNode _ -> error "TouchNode is unsafe while caching reading"
  DeleteNode _ -> error "DeleteNode is unsafe while caching reading"
  InsertEdge _ -> error "InsertEdge is unsafe while caching reading"
  DeleteEdge _ -> error "DeleteEdge is unsafe while caching reading"

runInMemoryGraphMetadataEditing ::
  forall t r a.
  (Member (State (Graph t ())) r, ValidTransition t) =>
  Sem (GraphMetadataEditing t : r) a ->
  Sem r a
runInMemoryGraphMetadataEditing = interpret \case
  GetNodeMetadata nid -> gets @(Graph t ()) $ view (at nid)
  TouchNode nid ->
    modify @(Graph t ()) $
      -- it is important that we only write emptyNode if the node does not exist
      -- otherwise we will overwrite the node with an empty node
      at nid
        . _Just
        .~ emptyNode nid
  DeleteNode nid -> modify @(Graph t ()) $ at nid .~ Nothing
  InsertEdge edge -> modify $ Models.Graph.insertEdge edge
  DeleteEdge edge -> modify $ Models.Graph.deleteEdge edge

-- | Warning: the operations of this effect are not atomic. You should not use
-- this effect if other programs/threads might modify the same files at the
-- same time.
runGraphMetadataEditing ::
  forall r a.
  (Members [GraphMetadataFilesystemOperations, Warn UserError] r) =>
  Sem (GraphMetadataEditing Text : r) a ->
  Sem r a
runGraphMetadataEditing = interpret \case
  GetNodeMetadata nid -> readNodeMetadata nid
  TouchNode nid -> withEarlyReturn do
    n <- readNodeMetadata nid
    whenJust n $ returnEarly ()
    writeNodeMetadata $ emptyNode nid
  DeleteNode nid -> withEarlyReturn do
    n <- readNodeMetadata nid
    whenNothing n $ returnEarly ()
    deleteNodeMetadata nid
  InsertEdge edge -> withEarlyReturn do
    source <- readNodeMetadata edge.source
    let outConnectExists = source <&> \s -> outConnect edge `member` s.outgoing
    sink <- readNodeMetadata edge.sink
    let inConnectExists = sink <&> \s -> inConnect edge `member` s.incoming
    when (outConnectExists == Just True && inConnectExists == Just True) do
      returnEarly ()
    when (outConnectExists == Just True) do
      warnText $ "inconsistent edge: " ++ tshow edge ++ ", sink: " ++ tshow sink
    when (inConnectExists == Just True) do
      warnText $ "inconsistent edge: " ++ tshow edge ++ ", source: " ++ tshow source
    let source' = fromMaybe (emptyNode edge.source) source
    let sink' = fromMaybe (emptyNode edge.sink) sink
    writeNodeMetadata (source' & #outgoing %~ insertSet (outConnect edge))
    writeNodeMetadata (sink' & #incoming %~ insertSet (inConnect edge))
  DeleteEdge edge -> withEarlyReturn do
    source <- readNodeMetadata edge.source
    let outConnectExists = source <&> \s -> outConnect edge `member` s.outgoing
    sink <- readNodeMetadata edge.sink
    let inConnectExists = sink <&> \s -> inConnect edge `member` s.incoming
    when (outConnectExists /= Just False && inConnectExists == outConnectExists) do
      -- either both nodes exist or neither node contains the edge
      returnEarly ()
    when (outConnectExists == Just False) do
      warnText $ "inconsistent edge: " ++ tshow edge ++ ", sink: " ++ tshow sink
    when (inConnectExists == Just False) do
      warnText $ "inconsistent edge: " ++ tshow edge ++ ", source: " ++ tshow source
    -- avoid writing nodes to the disk if they don't exist
    withJust source \source' ->
      writeNodeMetadata (source' & #outgoing %~ deleteSet (outConnect edge))
    withJust sink \sink' ->
      writeNodeMetadata (sink' & #incoming %~ deleteSet (inConnect edge))

-- Auxilliary type for @Tagged@ in @runCachedGraphMetadataEditing@
data CachedGraphMetadataEditingTag
  = Cache
  | Underlying
  | AsLoaded
  | Changes
  | DeletedNodes
  | DeletedEdges

-- | Update a graph transactionally by first building up a diff of all changes
-- in memory and then writing them to the disk using
-- @GraphMetadataFilesystemOperationsWriteDiff@
-- Actual transactionality is dependent on the transactionality of the
-- @GraphMetadataFilesystemOperationsWriteDiff@ effect.
runGraphMetadataEditingTransactionally ::
  forall r a.
  ( Members
      [ GraphMetadataFilesystemOperations,
        GraphMetadataFilesystemOperationsWriteDiff
      ]
      r
  ) =>
  Sem (GraphMetadataEditing Text : r) a ->
  Sem r a
runGraphMetadataEditingTransactionally action = do
  -- the way I'm implementing transactional behavior is based on my
  -- understanding of C#'s Unit of Work pattern, thus my interpreter that
  -- builds up the effects representing the changes before we write them
  -- is called @runUnitOfWork@
  let runUnitOfWork ::
        forall q b.
        ( Members
            [ Tagged Cache (GraphMetadataEditing Text),
              Tagged Underlying GraphMetadataFilesystemOperations,
              Tagged AsLoaded (State (Map NID (Maybe (Node Text ())))),
              Tagged Changes (State (Graph Text ())),
              Tagged DeletedEdges (State (Set (Edge Text)))
            ]
            q
        ) =>
        Sem (GraphMetadataEditing Text : q) b ->
        Sem q b
      runUnitOfWork = interpret @(GraphMetadataEditing Text) \case
        GetNodeMetadata nid -> withEarlyReturn do
          changesNode <- tag @Cache $ getNodeMetadata nid
          loadedNode <- tag @AsLoaded $ gets $ view (at nid)
          whenJust loadedNode do
            -- when loaded it is safe to rely on the cache because we
            -- populated the cache from the underlying node
            -- (and reconciled with pending changes we had already made)
            returnEarly changesNode
          underlyingNode <- tag @Underlying $ readNodeMetadata nid
          tag @AsLoaded $ modify $ at nid ?~ underlyingNode
          deletedEdges <- tag @DeletedEdges $ get
          -- we might already have a node in the cache even if we did not load
          -- because we create nodes for any changes we make
          let reconciledNode =
                mergeMaybes mergeNodesEx underlyingNode changesNode
                  & _Just
                    %~ withoutEdges deletedEdges
          tag @Changes $ modify $ at nid .~ reconciledNode
          pure reconciledNode
        TouchNode nid -> tag @Cache $ touchNode nid
        DeleteNode nid -> withEarlyReturn do
          -- TODO: this is sus and needs to be more carefully considered
          tag @Cache $ deleteNode nid
          -- we need to load the node to know what edges to delete
          -- deferring the read would just make our lives harder so we do it now
          maybeLoadedNode <- runUnitOfWork (getNodeMetadata nid)
          loadedNode <- unwrapReturningDefault () maybeLoadedNode
          tag @DeletedEdges $
            modify $
              union (mapSet (nid `outgoingEdge`) loadedNode.outgoing)
                . union (mapSet (`incomingEdge` nid) loadedNode.incoming)
        InsertEdge edge -> do
          tag @Cache $ insertEdge edge
          tag @DeletedEdges $ modify $ deleteSet edge
        DeleteEdge edge -> do
          hasEdge <- tag @Changes $ gets (`Models.Graph.containsEdge` edge)
          tag @Cache $ deleteEdge edge
          unless hasEdge $ tag @DeletedEdges $ modify $ insertSet edge
  let applyGraphDiff (deletedEdges, (loaded, (changes, result))) =
        writeGraphDiff loaded changes deletedEdges >> pure result
  action
    -- we use this to load nodes from the underlying storage
    -- we write data back to the disk directly with @writeGraphDiff@
    & raiseUnder @(Tagged Underlying GraphMetadataFilesystemOperations)
    -- state that we use to save up the "unit of work" that we will write back
    -- to the disk at once later
    & raiseUnder3
      @(Tagged Changes (State (Graph Text ())))
      @(Tagged AsLoaded (State (Map NID (Maybe (Node Text ())))))
      @(Tagged DeletedEdges (State (Set (Edge Text))))
    -- this just exists so that we can avoid duplicating the implementation of
    -- runInMemoryGraphMetadataEditing
    & raiseUnder @(Tagged Cache (GraphMetadataEditing Text))
    -- build up the unit of work in the effects above
    & runUnitOfWork
    -- run the Cache GraphMetadataEditing effect
    & raiseUnder @(State (Graph Text ()))
    & tag @Changes
      . runInMemoryGraphMetadataEditing
      . untag @Cache
    & runState Models.Graph.emptyGraph
      . untag @Changes
    & runState mempty
      . untag @AsLoaded
    & runState mempty
      . untag @DeletedEdges
    & subsume
      . untag @Underlying
    & (>>= applyGraphDiff)

-- | If I move to Effectful there is Effectful.Provider_ that does something
-- very similar to Scoped_
-- It should be pretty easy to support this in in-other-words as well.
runScopedGraphMetadataEditingTransactionally ::
  ( Members
      [ GraphMetadataFilesystemOperations,
        GraphMetadataFilesystemOperationsWriteDiff
      ]
      r
  ) =>
  Sem (Scoped_ (GraphMetadataEditing Text) : r) a ->
  Sem r a
runScopedGraphMetadataEditingTransactionally =
  runScopedNew \() -> runGraphMetadataEditingTransactionally
