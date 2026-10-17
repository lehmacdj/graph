-- | The interpreters of 'GraphDataEditing' that keep the data of the nodes
-- in files: through 'GraphDataFilesystemOperations'.
module DAL.Interpreters.GraphDataEditing where

import DAL.FileSystemOperations.Data
import Data.List.NonEmpty qualified as NonEmpty
import Effectful.State.Dynamic
import Error.UserError
import Graph.GraphDataEditing
import Models.NID
import MyPrelude

-- | The extension of the file storing a node's data. This is the only format
-- for node data at the moment.
legacyDataExtension :: String
legacyDataExtension = ".data"

-- | Read and write node data directly to the filesystem.
runGraphDataEditing ::
  (GraphDataFilesystemOperations :> r) =>
  Eff (GraphDataEditing : r) a ->
  Eff r a
runGraphDataEditing = interpret \_ -> \case
  GetRawData nid -> readNodeData nid legacyDataExtension
  WriteRawData nid d -> writeNodeData nid legacyDataExtension d
  DeleteRawData nid -> deleteNodeData nid legacyDataExtension
  EditingDataOutside _ -> pure ()
  PendingDataChanges -> pure mempty

-- | Hold all writes in memory until the action finishes and then write them
-- to the filesystem. Reads see the writes made so far. If the action throws,
-- nothing is written. If the data of a node can't be written or removed,
-- the data of the others is all the same, and then 'DataNotWritten' says
-- which could not be: by then the rest of the change is written (the node
-- files, which the action writes when it finishes).
runGraphDataEditingTransactionally ::
  (Members [GraphDataFilesystemOperations, Error UserError] r) =>
  Eff (GraphDataEditing : r) a ->
  Eff r a
runGraphDataEditingTransactionally action = do
  ((result, pending), _) <-
    action
      & reinterpret
        ( runStateLocal @(Set NID) mempty
            . runStateLocal @(Map NID (Maybe ByteString)) mempty
        )
        \_ -> \case
          GetRawData nid ->
            gets @(Map NID (Maybe ByteString)) (lookup nid) >>= \case
              Just d -> pure d
              Nothing -> readNodeData nid legacyDataExtension
          WriteRawData nid d ->
            modify @(Map NID (Maybe ByteString)) $ insertMap nid (Just d)
          DeleteRawData nid ->
            modify @(Map NID (Maybe ByteString)) $ insertMap nid Nothing
          EditingDataOutside nid -> modify @(Set NID) $ insertSet nid
          PendingDataChanges ->
            (<>) <$> gets @(Map NID (Maybe ByteString)) keysSet <*> get @(Set NID)
  failed <- fmap catMaybes . for (mapToList pending) $ \(nid, d) ->
    tryError @UserError (runGraphDataEditing (setRawData nid d)) <&> \case
      Left (_, e) -> Just (maybe DeletingFile (const ReplacingFile) d, nid, e)
      Right () -> Nothing
  for_ (NonEmpty.nonEmpty failed) (throwError . DataNotWritten)
  pure result
