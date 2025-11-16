{-# LANGUAGE TemplateHaskell #-}

module Graph.GraphMetadataEditing where

import DAL.FileSystemOperations.Metadata
import DAL.FileSystemOperations.MetadataWriteDiff
import Data.Map.Strict qualified as Map
import Error.UserError
import Error.Warn
import Models.Augmentation.Bundled
import Models.Augmentation.IsThin
import Models.Augmentation.NodeEdits
import Models.Edge
import Models.Graph (Graph (..), emptyGraph)
import Models.Graph qualified
import Models.NID
import Models.Node
import MyPrelude
import Polysemy.Scoped
import Polysemy.State

-- | Marker indicating that something promises to only read the graph metadata.
-- I gave up on trying to enforce this at the type level because it ended up
-- being really hard to get type inference to infer all the things I wanted it
-- to.
type GraphMetadataReading' t = GraphMetadataEditing' t

type GraphMetadataReading = GraphMetadataReading' Text

data GraphMetadataEditing' t m a where
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

makeSem ''GraphMetadataEditing'

type GraphMetadataEditing = GraphMetadataEditing' Text

-- | Makes it so all reads to a node will return the same result within the
-- action by caching fetched nodes in memory
cachingReadingInMemory ::
  forall t a r.
  ( Member (GraphMetadataReading' t) r,
    ValidTransition t,
    HasCallStack
  ) =>
  Sem (GraphMetadataReading' t : r) a ->
  Sem r a
cachingReadingInMemory =
  evalState @(Set NID) mempty
    . evalState @(Graph t IsThin) emptyGraph
    . cachingReadingInState
    . raiseUnder @(State (Graph t IsThin))
    . raiseUnder @(State (Set NID))

cachingReadingInState ::
  forall t a r.
  ( Member (GraphMetadataReading' t) r,
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
  Sem (GraphMetadataReading' t : r) a ->
  Sem r a
cachingReadingInState = interpret \case
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
    modify @(Graph t IsThin) $ at nid ?~ (Fetched <$ n)
    pure $ Just n
  TouchNode _ -> error "TouchNode is unsafe while caching reading"
  DeleteNode _ -> error "DeleteNode is unsafe while caching reading"
  InsertEdge _ -> error "InsertEdge is unsafe while caching reading"
  DeleteEdge _ -> error "DeleteEdge is unsafe while caching reading"

runInMemoryGraphMetadataEditing ::
  forall t r a.
  (Member (State (Graph t ())) r, ValidTransition t) =>
  Sem (GraphMetadataEditing' t : r) a ->
  Sem r a
runInMemoryGraphMetadataEditing = interpret \case
  GetNodeMetadata nid -> gets @(Graph t ()) $ view (at nid)
  TouchNode nid ->
    modify @(Graph t ()) $
      -- it is important that we only write emptyNode if the node does not exist
      -- otherwise we will overwrite the node with an empty node
      at nid . _Just .~ emptyNode nid
  DeleteNode nid -> modify @(Graph t ()) $ at nid .~ Nothing
  InsertEdge edge -> modify $ Models.Graph.insertEdge edge
  DeleteEdge edge -> modify $ Models.Graph.deleteEdge edge

graphFromGraphMetadataEditing ::
  forall t a.
  ( ValidTransition t,
    HasCallStack
  ) =>
  Sem '[GraphMetadataEditing' t] a ->
  Graph t ()
graphFromGraphMetadataEditing =
  run . execState emptyGraph . runInMemoryGraphMetadataEditing . raiseUnder

-- | Warning: the operations of this effect are not atomic. You should not use
-- this effect if other programs/threads might modify the same files at the
-- same time.
runGraphMetadataEditing ::
  forall r a.
  (Members [GraphMetadataFilesystemOperations, Warn UserError] r) =>
  Sem (GraphMetadataEditing : r) a ->
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

type GraphWithEdits = Graph Text (Bundled [NodeEdits Text, IsThin])

cachingGraphMetadataEditingInState ::
  forall r a.
  ( Member GraphMetadataFilesystemOperations r,
    Members
      [ -- the graph as it's been fetched so far (including changes we made)
        State GraphWithEdits,
        -- the set of nodes that should be considered non-existing
        -- nodes end up in this set either if:
        -- - we fetched them and they did not exist
        -- - we deleted them and they haven't been re-created since
        State (Set NID),
        -- nodes as they were when we loaded them
        State (Map NID (Maybe (Node Text ())))
      ]
      r,
    HasCallStack
  ) =>
  Sem (GraphMetadataEditing' Text : r) a ->
  Sem r a
cachingGraphMetadataEditingInState = interpret \case
  GetNodeMetadata nid -> withEarlyReturn do
    -- if a node is in the deleted set, we need to treat it as non-existing
    whenM (gets @(Set NID) (member nid)) (returnEarly Nothing)
    cached <- gets @GraphWithEdits (preview (ix nid))
    -- if we've already fetched this node, we can trust it and return it
    let onlyFetched =
          _Just . alongsideMetadata (lensA @IsThin) . asideMetadata (only Fetched)
    withJust (preview onlyFetched cached :: Maybe (Node Text ())) (returnEarly . Just . void)
    n <- readNodeMetadata nid
    modify' @(Map NID (Maybe (Node Text ()))) $ at nid ?~ n
    let cached' = fromMaybe (emptyNode nid) cached
        n' =
          n
            <&> applyEditsPreservingDeleted cached'.augmentation.edits
              . ($> (cached'.augmentation & lensA @IsThin .~ Fetched))
    modify' @GraphWithEdits $ at nid .~ n'
    pure $! void <$> n'
  TouchNode nid -> do
    modify' @(Set NID) $ deleteSet nid
    modify' @GraphWithEdits $
      at nid
        %~ Just . \case
          Nothing ->
            emptyNode @() nid
              $> injDefaultA @(NodeEdits Text) (NodeEdits (singleton Touch))
          Just node ->
            node & #augmentation %~ updateA @(NodeEdits Text) (appendEdit Touch)
  DeleteNode nid -> do
    -- ensure node has been loaded if it exists so we know which edges to delete
    n <- cachingGraphMetadataEditingInState (getNodeMetadata nid)
    forOf_ (_Just . #outgoing . folded) n $ \n' ->
      modify' @GraphWithEdits (Models.Graph.deleteEdge (nid `outgoingEdge` n'))
    forOf_ (_Just . #incoming . folded) n $ \n' ->
      modify' @GraphWithEdits (Models.Graph.deleteEdge (n' `incomingEdge` nid))
    modify' @(Set NID) $ insertSet nid
    modify' @GraphWithEdits $
      at nid . _Just
        -- edges of the node have already been deleted from the deleteEdge
        -- calls above
        %~ (#augmentation %~ updateA @(NodeEdits Text) (appendEdit Delete))
  InsertEdge edge -> do
    modify' @(Set NID) $ deleteSet edge.source . deleteSet edge.sink
    modify' @GraphWithEdits $
      id @_ @GraphWithEdits
        . ( at edge.source
              %~ Just . \case
                Nothing ->
                  outStubNode'
                    edge
                    ( injDefaultA @(NodeEdits Text)
                        (NodeEdits (singleton (InsertOutgoing (outConnect edge))))
                    )
                Just n ->
                  n
                    & #outgoing %~ insertSet (outConnect edge)
                    & #augmentation
                      %~ updateA @(NodeEdits Text)
                        (appendEdit (InsertOutgoing (outConnect edge)))
          )
        . ( at edge.sink
              %~ Just . \case
                Nothing ->
                  inStubNode'
                    edge
                    ( injDefaultA @(NodeEdits Text)
                        (NodeEdits (singleton (InsertIncoming (inConnect edge))))
                    )
                Just n ->
                  n
                    & #incoming %~ insertSet (inConnect edge)
                    & #augmentation
                      %~ updateA @(NodeEdits Text)
                        (appendEdit (InsertIncoming (inConnect edge)))
          )
  DeleteEdge edge -> do
    modify' @GraphWithEdits $
      id @_ @GraphWithEdits
        . ( ix edge.source
              %~ ( (#outgoing %~ deleteSet (outConnect edge))
                     . ( #augmentation
                           %~ updateA @(NodeEdits Text)
                             (appendEdit (DeleteOutgoing (outConnect edge)))
                       )
                 )
          )
        . ( ix edge.sink
              %~ ( (#incoming %~ deleteSet (inConnect edge))
                     . ( #augmentation
                           %~ updateA @(NodeEdits Text)
                             (appendEdit (DeleteIncoming (inConnect edge)))
                       )
                 )
          )

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
  Sem (GraphMetadataEditing : r) a ->
  Sem r a
runGraphMetadataEditingTransactionally action = do
  let applyGraphDiff (nonExistent, (asLoaded, (Graph nodeMap, result))) = do
        let allChanges = flip Map.mapMaybeWithKey nodeMap \nid node ->
              -- the node or Nothing if it should be deleted
              let deleted = justIfTrue (nid `notMember` nonExistent) (void node)
               in -- we only need to edit the nodes that actually changed
                  -- this is any node with non-null edits that is different from
                  -- how it was when we loaded it
                  justIfTrue
                    (not (null node.augmentation.edits))
                    ( node ^. #augmentation . lensA @(NodeEdits Text),
                      justIfTrue (has (ix nid) asLoaded) deleted
                    )
        writeGraphDiff asLoaded (fst <$> allChanges) (Map.mapMaybe snd allChanges)
        pure result
  action
    & raiseUnder3
      @(State GraphWithEdits)
      @(State (Map NID (Maybe (Node Text ()))))
      @(State (Set NID))
    -- this just exists so that we can avoid duplicating the implementation of
    -- runInMemoryGraphMetadataEditing
    & cachingGraphMetadataEditingInState
    & runState Models.Graph.emptyGraph
    & runState mempty
    & runState mempty
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
  Sem (Scoped_ GraphMetadataEditing : r) a ->
  Sem r a
runScopedGraphMetadataEditingTransactionally =
  runScopedNew \() -> runGraphMetadataEditingTransactionally
