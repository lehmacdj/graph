-- | The interpreters of 'GraphEditing' that keep the graph in its node files
-- and the files of the nodes' data.
module DAL.Interpreters.GraphEditing
  ( runGraphEditingTransactionally,
    runScopedGraphEditingTransactionally,
  )
where

import DAL.FileSystemOperations.Data
import DAL.FileSystemOperations.Metadata
import DAL.FileSystemOperations.MetadataWriteDiff
import DAL.Interpreters.GraphDataEditing
import DAL.Interpreters.GraphMetadataEditing
import Effectful.Provider.List
import Effectful.State.Dynamic (State)
import Error.UserError
import Error.Warn
import Graph.Dualizeable
import Graph.GraphDataEditing
import Graph.GraphEditing
import Graph.GraphMetadataEditing
import MyPrelude

-- | Build up every change in memory and then write them to the filesystem
-- once the action finishes: first the metadata, then the data. If the action
-- or writing the metadata fails, the data is not written. If writing the
-- data fails, the metadata stays written, and the error says so
-- ('DataNotWritten'). A change to data alone is refused like any other change
-- if the node files that the action read are damaged.
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
