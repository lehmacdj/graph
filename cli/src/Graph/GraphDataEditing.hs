{-# LANGUAGE TemplateHaskell #-}

module Graph.GraphDataEditing where

import DAL.FileSystemOperations.Data
import Effectful.State.Dynamic
import Models.Graph
import Models.NID
import Models.Node
import MyPrelude

data GraphDataEditing :: Effect where
  GetRawData :: NID -> GraphDataEditing m (Maybe ByteString)
  WriteRawData :: NID -> ByteString -> GraphDataEditing m ()
  DeleteRawData :: NID -> GraphDataEditing m ()

makeEffect ''GraphDataEditing

-- | Write the data, deleting it if Nothing
setRawData :: (GraphDataEditing :> r) => NID -> Maybe ByteString -> Eff r ()
setRawData nid = maybe (deleteRawData nid) (writeRawData nid)

runInMemoryGraphDataEditing ::
  forall t r a.
  (ValidTransition t) =>
  (State (Graph t (Maybe ByteString)) :> r) =>
  Eff (GraphDataEditing : r) a ->
  Eff r a
runInMemoryGraphDataEditing = interpret \_ -> \case
  GetRawData nid ->
    gets @(Graph t (Maybe ByteString)) $ view (at nid . _Just . #augmentation)
  WriteRawData nid bs ->
    modify @(Graph t (Maybe ByteString)) $ at nid . _Just . #augmentation ?~ bs
  DeleteRawData nid ->
    modify @(Graph t (Maybe ByteString)) $ at nid . _Just . #augmentation .~ Nothing

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

-- | Hold all writes in memory until the action finishes and then write them
-- to the filesystem. Reads see the writes made so far. If the action throws,
-- nothing is written.
runGraphDataEditingTransactionally ::
  (GraphDataFilesystemOperations :> r) =>
  Eff (GraphDataEditing : r) a ->
  Eff r a
runGraphDataEditingTransactionally action = do
  (result, pending) <-
    action
      & reinterpret
        (runStateLocal @(Map NID (Maybe ByteString)) mempty)
        \_ -> \case
          GetRawData nid ->
            gets @(Map NID (Maybe ByteString)) (lookup nid) >>= \case
              Just d -> pure d
              Nothing -> readNodeData nid legacyDataExtension
          WriteRawData nid d ->
            modify @(Map NID (Maybe ByteString)) $ insertMap nid (Just d)
          DeleteRawData nid ->
            modify @(Map NID (Maybe ByteString)) $ insertMap nid Nothing
  runGraphDataEditing $ traverse_ (uncurry setRawData) (mapToList pending)
  pure result
