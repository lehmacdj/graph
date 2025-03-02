{-# LANGUAGE TemplateHaskell #-}

module Graph.GraphDataEditing where

import Models.Graph
import Models.NID
import Models.Node
import MyPrelude
import Polysemy.State

data GraphDataEditing m a where
  GetRawData :: NID -> GraphDataEditing m (Maybe ByteString)
  WriteRawData :: NID -> ByteString -> GraphDataEditing m ()
  DeleteRawData :: NID -> GraphDataEditing m ()

makeSem ''GraphDataEditing

runInMemoryGraphDataEditing ::
  forall t r a.
  (ValidTransition t) =>
  (Member (State (Graph t (Maybe ByteString))) r) =>
  Sem (GraphDataEditing : r) a ->
  Sem r a
runInMemoryGraphDataEditing = interpret \case
  GetRawData nid ->
    gets @(Graph t (Maybe ByteString)) $ view (at nid . _Just . #augmentation)
  WriteRawData nid bs ->
    modify @(Graph t (Maybe ByteString)) $ at nid . _Just . #augmentation ?~ bs
  DeleteRawData nid ->
    modify @(Graph t (Maybe ByteString)) $ at nid . _Just . #augmentation .~ Nothing
