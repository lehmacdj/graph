{-# LANGUAGE TemplateHaskell #-}

module Graph.GraphMetadataEditing where

import DAL.FileSystemOperations.Metadata
import DAL.FileSystemOperations.MetadataWriteDiff
import Data.List.NonEmpty qualified as NonEmpty
import Data.Map.Strict qualified as Map
import Effectful.State.Dynamic
import Error.UserError
import Error.Warn
import Models.Augmentation.Bundled
import Models.Augmentation.IsThin
import Models.Augmentation.NodeEdits
import Models.Damage
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

type NodeWithEdits = Node Text (Bundled [NodeEdits Text, IsThin])

-- | Where 'cachingGraphMetadataEditingStoringIn' keeps the node with an id
newtype NodeSlot
  = NodeSlot
      ( forall f.
        (Functor f) =>
        NID ->
        LensLike' f GraphWithEdits (Maybe NodeWithEdits)
      )

-- | Each node is kept as its own file and its own edits say, whatever the
-- other nodes say about the edges they share with it. What a transaction
-- sees of a node therefore doesn't depend on which nodes it read before:
-- a step forwards reads the outgoing edges of the node it leaves, and a
-- step backwards its incoming edges.
ownRecords :: NodeSlot
ownRecords = NodeSlot \nid -> #nodeMap . at nid

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
cachingGraphMetadataEditingInState =
  cachingGraphMetadataEditingStoringIn ownRecords

-- | 'cachingGraphMetadataEditingInState' with a choice of where a node is
-- kept. The choice exists for the tests, which compare 'ownRecords' with
-- what this used to do (see @reconcilingRecords@ there).
cachingGraphMetadataEditingStoringIn ::
  forall r a.
  ( GraphMetadataFilesystemOperations :> r,
    Members
      [ State GraphWithEdits,
        State (Set NID),
        State (Map NID (Maybe (Node Text ())))
      ]
      r,
    HasCallStack
  ) =>
  NodeSlot ->
  Eff (GraphMetadataEditing' Text : r) a ->
  Eff r a
cachingGraphMetadataEditingStoringIn (NodeSlot slot) = interpret \_ -> \case
  GetNodeMetadata nid -> withEarlyReturn do
    -- if a node is in the deleted set, we need to treat it as non-existing
    whenM (gets @(Set NID) (member nid)) (returnEarly Nothing)
    cached <- gets @GraphWithEdits (view (slot nid))
    -- if we've already fetched this node, we can trust it and return it
    let onlyFetched =
          _Just . alongsideMetadata (lensA @IsThin) . asideMetadata (only Fetched)
    withJust (preview onlyFetched cached :: Maybe (Node Text ())) (returnEarly . Just . void)
    n <- readNodeMetadata nid
    modify' @(Map NID (Maybe (Node Text ()))) $ at nid ?~ n
    let cached' = fromMaybe (emptyNode nid) cached
        markFetched = lensA @IsThin .~ Fetched
        n' = case n of
          -- a file is the node it is named after, whatever id it says:
          -- that is how every edge to it finds it ('damageAmong' reports
          -- the file)
          Just onDisk ->
            Just . applyEditsPreservingDeleted cached'.augmentation.edits $
              (onDisk & #nid .~ nid) $> markFetched cached'.augmentation
          -- a node that is not on disk exists only if we created it earlier
          -- in this transaction
          Nothing ->
            applyEditsFrom
              nid
              (markFetched cached'.augmentation)
              cached'.augmentation.edits
              Nothing
    modify' @GraphWithEdits $ slot nid .~ n'
    -- remembered, so that an edit that doesn't create the node (deleting one
    -- of its edges) isn't taken to leave a node behind
    when (isNothing n') $ modify' @(Set NID) (insertSet nid)
    pure $! void <$> n'
  TouchNode nid -> do
    modify' @(Set NID) $ deleteSet nid
    modify' @GraphWithEdits $
      slot nid
        %~ Just . \case
          Nothing ->
            emptyNode @() nid
              $> injDefaultA @(NodeEdits Text) (NodeEdits (singleton Touch))
          Just node ->
            node & #augmentation %~ updateA @(NodeEdits Text) (appendEdit Touch)
  DeleteNode nid -> do
    -- ensure node has been loaded if it exists so we know which edges to delete
    n <- again (getNodeMetadata nid)
    -- delete the edges as edits so the neighbors lose them too
    forOf_ (_Just . #outgoing . folded) n $ \n' ->
      again (deleteEdge (nid `outgoingEdge` n'))
    forOf_ (_Just . #incoming . folded) n $ \n' ->
      again (deleteEdge (n' `incomingEdge` nid))
    modify' @(Set NID) $ insertSet nid
    modify' @GraphWithEdits $
      slot nid . _Just
        -- edges of the node have already been deleted from the deleteEdge
        -- calls above
        %~ (#augmentation %~ updateA @(NodeEdits Text) (appendEdit Delete))
  InsertEdge edge -> do
    modify' @(Set NID) $ deleteSet edge.source . deleteSet edge.sink
    modify' @GraphWithEdits $
      id @_ @GraphWithEdits
        . ( slot edge.source
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
        . ( slot edge.sink
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
        . ( slot edge.source
              %~ Just . \case
                Nothing -> stub edge.source deleteOutgoing
                Just n ->
                  n
                    & #outgoing %~ deleteSet (outConnect edge)
                    & #augmentation
                      %~ updateA @(NodeEdits Text) (appendEdit deleteOutgoing)
          )
        . ( slot edge.sink
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
  where
    again :: Eff (GraphMetadataEditing' Text : r) b -> Eff r b
    again = cachingGraphMetadataEditingStoringIn (NodeSlot slot)

-- | Update a graph transactionally by first building up a diff of all changes
-- in memory and then writing them to the disk using
-- @GraphMetadataFilesystemOperationsWriteDiff@
-- Actual transactionality is dependent on the transactionality of the
-- @GraphMetadataFilesystemOperationsWriteDiff@ effect.
--
-- The files of the nodes that the transaction read are compared with each
-- other once it is done, and with the ones that the command it is part of
-- read in its earlier steps ('filesReadByCommand'): a command that read both
-- files of an edge has read them, in however many transactions. No file is
-- read for this. If they are damaged (see 'damageAmong'):
--
-- * a transaction that changes something fails before it writes anything,
--   with 'DamagedGraph'. Writing checks this again together with the nodes
--   that the transaction changes without having read them. It fails in the
--   same way if the command that it is part of has read past damage before
--   it, in another transaction ('asOneDecision').
-- * a transaction that changes nothing, or that failed for another reason,
--   notes each piece of damage as read past ('noteDamageReadPast'): the
--   command line warns of it.
--
-- A node file that is there but can't be used as a node (its content is not
-- a node, or it can't be read at all) is damage like any other, and doesn't
-- fail the transaction that reads it either: the node is read as a node
-- without edges. It is there, and other nodes may record edges with it, but
-- nothing is known of what its own file recorded.
--
-- Changing the data of a node is changing something, although no node file
-- is written for it: the first argument says, once the action is done,
-- whether it did that. (Deleting a node that has no file changes no node
-- file and deletes its data.)
runGraphMetadataEditingTransactionally ::
  forall r a.
  ( Members
      [ GraphMetadataFilesystemOperations,
        GraphMetadataFilesystemOperationsWriteDiff,
        Warn UserError,
        State DamageReadPast,
        Error UserError
      ]
      r
  ) =>
  Eff r Bool ->
  Eff (GraphMetadataEditing : r) a ->
  Eff r a
runGraphMetadataEditingTransactionally changedData action = do
  ((((result, graph), asLoaded), nonExistent), unusable) <-
    tryError @UserError (inject action)
      & cachingGraphMetadataEditingInState
      & runStateLocal @GraphWithEdits Models.Graph.emptyGraph
      & runStateLocal @(Map NID (Maybe (Node Text ()))) mempty
      & runStateLocal @(Set NID) mempty
      & readingPastUnusableFiles
  -- the node files that the command read before this transaction, in its
  -- earlier steps, and the ones that the transaction read, which are the
  -- later reading of a file that both read
  earlierFiles <- filesReadByCommand
  let -- nothing is said about the edges that other nodes record with a node
      -- whose file can't be used: what that file records is not known
      own = asLoaded `Map.withoutKeys` keysSet unusable
      files = own <> (earlierFiles `Map.withoutKeys` keysSet unusable)
      -- The files of this transaction among themselves, and the ones of
      -- them that the command had not read before, or not as they are now,
      -- with all of its files. (What the files that it had read show among
      -- themselves was found by the step that read them.)
      damage =
        damageAmong own
          <> damageNear (filesNewTo earlierFiles own) files
          <> setFromList (toList unusable)
      (edits, changedNodes) = transactionChanges graph asLoaded nonExistent
  setFilesReadByCommand files
  case result of
    Left (_, e) -> noteDamageReadPast damage >> throwError e
    Right x -> do
      onlyRead <- (null edits &&) . not <$> changedData
      if onlyRead
        then noteDamageReadPast damage
        else do
          -- what the command read before this transaction decided what the
          -- transaction changes ('asOneDecision')
          earlier <- damageReadPastByCommand
          for_ (NonEmpty.nonEmpty (toList (earlier <> damage))) $
            throwError . DamagedGraph
          -- a transaction that changed only data has no node file to write
          unless (null edits) do
            written <- writeGraphDiff asLoaded edits changedNodes files
            -- so that what the command wrote itself is never taken for a
            -- file that disagrees with what it read before; in a dry run,
            -- the files that writing read, which the command has read then
            setFilesReadByCommand (written <> files)
      pure x

-- | Read a node file that is there but can't be used as a node as a node
-- without edges instead of failing. Returns those files as the damage that
-- they are, by the node each is named after.
readingPastUnusableFiles ::
  (Members [GraphMetadataFilesystemOperations, Error UserError] r) =>
  Eff r a ->
  Eff r (a, Map NID Damage)
readingPastUnusableFiles =
  runStateLocal @(Map NID Damage) mempty
    . interpose @GraphMetadataFilesystemOperations
      ( \_ -> \case
          ReadNodeMetadata nid ->
            readNodeMetadata nid `catchError` \_ e -> case damageOfReadError e of
              Just damage -> do
                modify' @(Map NID Damage) (insertMap nid damage)
                pure (Just (emptyNode nid))
              Nothing -> throwError e
          WriteNodeMetadata node -> writeNodeMetadata node
          DeleteNodeMetadata nid -> deleteNodeMetadata nid
          ListNodeMetadata -> listNodeMetadata
      )
    . raise

-- | What a finished transaction has to write: the edits of every node it
-- changed and, for the changed nodes it had loaded, the node those edits
-- give ('Nothing' if they delete it).
transactionChanges ::
  GraphWithEdits ->
  -- | nodes as they were when the transaction loaded them
  Map NID (Maybe (Node Text ())) ->
  -- | nodes that don't exist once the transaction is done
  Set NID ->
  (Map NID (NodeEdits Text), Map NID (Maybe (Node Text ())))
transactionChanges (Graph nodeMap) asLoaded nonExistent =
  (fst <$> allChanges, Map.mapMaybe snd allChanges)
  where
    allChanges = flip Map.mapMaybeWithKey nodeMap \nid node ->
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
