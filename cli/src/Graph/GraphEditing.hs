-- | Editing both the metadata and the data of a graph together.
--
-- The interpreters that store the graph are elsewhere: the node files in
-- "DAL.Interpreters.GraphEditing".
module Graph.GraphEditing
  ( GraphEditing,
    HasGraph,
    deletingDataWithNodes,
  )
where

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
