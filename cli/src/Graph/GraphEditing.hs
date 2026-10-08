-- | Editing both the metadata and the data of a graph together.
module Graph.GraphEditing
  ( GraphEditing,
    HasGraph,
    deletingDataWithNodes,
    runGraphEditingTransactionally,
    runScopedGraphEditingTransactionally,
  )
where

import DAL.FileSystemOperations.Data
import DAL.FileSystemOperations.Metadata
import DAL.FileSystemOperations.MetadataWriteDiff
import Effectful.Provider.List
import Effectful.State.Dynamic (State)
import Error.UserError
import Error.Warn
import Graph.Dualizeable
import Graph.GraphDataEditing
import Graph.GraphMetadataEditing
import Models.Node (ValidTransition)
import MyPrelude

type GraphEditing :: [Effect]
type GraphEditing = '[GraphMetadataEditing, GraphDataEditing]

-- | Editing a graph with transitions of type @t@: its metadata and its data
type HasGraph t effs =
  ( GraphMetadataEditing' t :> effs,
    GraphDataEditing :> effs,
    ValidTransition t
  )

-- | A node's data belongs to the node, so deleting the node deletes its data
-- too.
deletingDataWithNodes ::
  (Members GraphEditing r) =>
  Eff r a ->
  Eff r a
deletingDataWithNodes = interpose @GraphMetadataEditing \_ -> \case
  GetNodeMetadata nid -> getNodeMetadata nid
  TouchNode nid -> touchNode nid
  DeleteNode nid -> deleteNode nid >> deleteRawData nid
  InsertEdge e -> insertEdge e
  DeleteEdge e -> deleteEdge e
  ListNodes -> listNodes

-- | Build up every change in memory and then write them to the filesystem
-- once the action finishes: first the metadata, then the data. If the action
-- or writing the metadata fails, the data is not written. A change to data
-- alone is refused like any other change if the node files that the action
-- read are damaged.
runGraphEditingTransactionally ::
  ( Members
      [ GraphMetadataFilesystemOperations,
        GraphMetadataFilesystemOperationsWriteDiff,
        GraphDataFilesystemOperations,
        Warn UserError,
        State DamageReadPast,
        Error UserError
      ]
      r
  ) =>
  Eff (GraphMetadataEditing : GraphDataEditing : r) a ->
  Eff r a
runGraphEditingTransactionally =
  runGraphDataEditingTransactionally
    . runGraphMetadataEditingTransactionally
      (not . null <$> pendingDataChanges)
    . deletingDataWithNodes

-- | Every 'provideList_' is its own transaction, viewing the graph dualized
-- while 'Dualizeable' says so.
runScopedGraphEditingTransactionally ::
  ( Members
      [ GraphMetadataFilesystemOperations,
        GraphMetadataFilesystemOperationsWriteDiff,
        GraphDataFilesystemOperations,
        Warn UserError,
        State DamageReadPast,
        Error UserError,
        Dualizeable
      ]
      r
  ) =>
  Eff (ProviderList_ GraphEditing () : r) a ->
  Eff r a
runScopedGraphEditingTransactionally =
  runProviderList_ \() ->
    runGraphEditingTransactionally . dualizingGraphMetadataEditing @Text
