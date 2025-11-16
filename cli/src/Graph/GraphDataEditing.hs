{-# LANGUAGE TemplateHaskell #-}

module Graph.GraphDataEditing where

import Effectful
import Effectful.Dispatch.Dynamic
import Effectful.State.Static.Local
import Effectful.TH
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
  forall t es a.
  (ValidTransition t) =>
  (State (Graph t (Maybe ByteString)) :> es) =>
  Eff (GraphDataEditing : es) a ->
  Eff es a
runInMemoryGraphDataEditing = interpret $ \_ -> \case
  GetRawData nid ->
    gets @(Graph t (Maybe ByteString)) $ view (at nid . _Just . #augmentation)
  WriteRawData nid bs ->
    modify @(Graph t (Maybe ByteString)) $ at nid . _Just . #augmentation ?~ bs
  DeleteRawData nid ->
    modify @(Graph t (Maybe ByteString)) $ at nid . _Just . #augmentation .~ Nothing
