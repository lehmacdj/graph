{-# LANGUAGE TemplateHaskell #-}

module Graph.GraphMetadataEditing where

import Models.Edge
import Models.NID
import Models.Node
import MyPrelude

data GraphMetadataEditing t m a where
  GetNodeMetadata :: NID -> GraphMetadataEditing t m (Maybe (Node t ()))
  TouchNode :: NID -> GraphMetadataEditing t m Bool
  DeleteNode :: NID -> GraphMetadataEditing t m Bool
  InsertEdge :: Edge t -> GraphMetadataEditing t m Bool
  DeleteEdge :: Edge t -> GraphMetadataEditing t m Bool

makeSem ''GraphMetadataEditing
