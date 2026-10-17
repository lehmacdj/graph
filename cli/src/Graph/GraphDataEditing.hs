{-# LANGUAGE TemplateHaskell #-}

-- | The effect of reading and changing the data of a graph's nodes, and the
-- interpreter of it that keeps the data in memory. What stores the data
-- interprets it elsewhere: the files of the nodes' data in
-- "DAL.Interpreters.GraphDataEditing".
module Graph.GraphDataEditing where

import Effectful.State.Dynamic
import Models.Graph
import Models.NID
import Models.Node
import MyPrelude

data GraphDataEditing :: Effect where
  GetRawData :: NID -> GraphDataEditing m (Maybe ByteString)
  WriteRawData :: NID -> ByteString -> GraphDataEditing m ()
  DeleteRawData :: NID -> GraphDataEditing m ()
  -- | The data of the node is about to be changed by something that doesn't
  -- go through this effect: the editor of @vi@, which works on the file.
  -- Nothing is written for it, but it counts among the 'PendingDataChanges',
  -- so that what is refused for a change of data is refused for it.
  EditingDataOutside :: NID -> GraphDataEditing m ()
  -- | The nodes whose data was written or deleted and is not in the files
  -- yet, or is about to be changed outside: none unless the changes are held
  -- back to be written together ('runGraphDataEditingTransactionally')
  PendingDataChanges :: GraphDataEditing m (Set NID)

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
  EditingDataOutside _ -> pure ()
  PendingDataChanges -> pure mempty
