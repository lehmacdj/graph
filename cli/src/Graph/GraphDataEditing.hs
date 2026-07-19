{-# LANGUAGE TemplateHaskell #-}

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

makeEffect ''GraphDataEditing

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
