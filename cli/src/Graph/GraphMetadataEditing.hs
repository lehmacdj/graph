{-# LANGUAGE TemplateHaskell #-}

module Graph.GraphMetadataEditing where

import DAL.FileSystemOperations.Metadata
import DAL.FileSystemOperations.MetadataWriteDiff
import Data.Map.Strict qualified as Map
import Effectful.State.Dynamic
import Error.UserError
import Error.Warn
import Models.Augmentation.Bundled
import Models.Augmentation.IsThin
import Models.Augmentation.NodeEdits
import Models.Edge
import Models.Graph (Graph, Graph' (..), emptyGraph)
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
    modify @(Graph t IsThin) $ at nid ?~ (Fetched <$ n)
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

-- | Warning: the operations of this effect are not atomic. You should not use
-- this effect if other programs/threads might modify the same files at the
-- same time.
runGraphMetadataEditing ::
  forall r a.
  (Members [GraphMetadataFilesystemOperations, Warn UserError] r) =>
  Eff (GraphMetadataEditing : r) a ->
  Eff r a
runGraphMetadataEditing = interpret \_ -> \case
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
  ListNodes -> listNodeMetadata

type GraphWithEdits = Graph Text (Bundled [NodeEdits Text, IsThin])

cachingGraphMetadataEditingInState ::
  forall r a.
  ( GraphMetadataFilesystemOperations :> r,
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
  Eff (GraphMetadataEditing' Text : r) a ->
  Eff r a
cachingGraphMetadataEditingInState = interpret \_ -> \case
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
        markFetched = lensA @IsThin .~ Fetched
        n' = case n of
          Just onDisk ->
            Just . applyEditsPreservingDeleted cached'.augmentation.edits $
              onDisk $> markFetched cached'.augmentation
          -- a node that is not on disk exists only if we created it earlier
          -- in this transaction
          Nothing ->
            applyEditsFrom
              nid
              (markFetched cached'.augmentation)
              cached'.augmentation.edits
              Nothing
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
    -- delete the edges as edits so the neighbors lose them too
    forOf_ (_Just . #outgoing . folded) n $ \n' ->
      cachingGraphMetadataEditingInState (deleteEdge (nid `outgoingEdge` n'))
    forOf_ (_Just . #incoming . folded) n $ \n' ->
      cachingGraphMetadataEditingInState (deleteEdge (n' `incomingEdge` nid))
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
    -- an endpoint we haven't loaded gets a thin stub recording the edit, which
    -- is applied once the node is loaded or written
    let stub nid edit =
          emptyNode @() nid
            $> injDefaultA @(NodeEdits Text) (NodeEdits (singleton edit))
        deleteOutgoing = DeleteOutgoing (outConnect edge)
        deleteIncoming = DeleteIncoming (inConnect edge)
    modify' @GraphWithEdits $
      id @_ @GraphWithEdits
        . ( at edge.source
              %~ Just . \case
                Nothing -> stub edge.source deleteOutgoing
                Just n ->
                  n
                    & #outgoing %~ deleteSet (outConnect edge)
                    & #augmentation
                      %~ updateA @(NodeEdits Text) (appendEdit deleteOutgoing)
          )
        . ( at edge.sink
              %~ Just . \case
                Nothing -> stub edge.sink deleteIncoming
                Just n ->
                  n
                    & #incoming %~ deleteSet (inConnect edge)
                    & #augmentation
                      %~ updateA @(NodeEdits Text) (appendEdit deleteIncoming)
          )
  ListNodes -> do
    onDisk <- listNodeMetadata
    -- nodes we created exist even though they haven't been written yet
    Graph nodeMap <- get @GraphWithEdits
    let created =
          keysSet . flip Map.filterWithKey nodeMap $ \nid node ->
            isJust $
              applyEditsFrom @Text nid () node.augmentation.edits Nothing
    nonExistent <- get @(Set NID)
    pure $ (onDisk <> created) `difference` nonExistent

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
  Eff (GraphMetadataEditing : r) a ->
  Eff r a
runGraphMetadataEditingTransactionally action = do
  let applyGraphDiff (((result, Graph nodeMap), asLoaded), nonExistent) = do
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
        -- a transaction that only read the graph has nothing to write
        unless (null allChanges) $
          writeGraphDiff asLoaded (fst <$> allChanges) (Map.mapMaybe snd allChanges)
        pure result
  action
    & inject
    -- this just exists so that we can avoid duplicating the implementation of
    -- runInMemoryGraphMetadataEditing
    & cachingGraphMetadataEditingInState
    & runStateLocal @GraphWithEdits Models.Graph.emptyGraph
    & runStateLocal @(Map NID (Maybe (Node Text ()))) mempty
    & runStateLocal @(Set NID) mempty
    & (>>= applyGraphDiff)
