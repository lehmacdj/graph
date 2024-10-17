module Graph.SystemNodes.Init
  ( createSystemNodes,
  )
where

import Graph.Effect
import Models.Types
import MyPrelude
import Graph.SystemNodes

createSystemNodes :: HasGraph String effs => Sem effs ()
createSystemNodes = do
  touchNode systemNodeNID
  touchNode nilNID
  insertEdge (Edge systemNodeNID "origin" nilNID)
  touchNode tagsNID
  insertEdge (Edge systemNodeNID "tags" tagsNID)
  touchNode importDatesNID
  insertEdge (Edge systemNodeNID "import-dates" importDatesNID)
  touchNode fileHashesNID
  insertEdge (Edge systemNodeNID "file-hashes" fileHashesNID)
  touchNode importUrlsNID
  insertEdge (Edge systemNodeNID "import-urls" importUrlsNID)
  touchNode historicalSystemNodesNID
  insertEdge (Edge systemNodeNID "historical-special-nodes" historicalSystemNodesNID)
  touchNode sequenceIDsNID
  insertEdge (Edge historicalSystemNodesNID "sequence-ids" sequenceIDsNID)
  touchNode mimeTypesNID
  insertEdge (Edge systemNodeNID "mime-types" mimeTypesNID)
  touchNode fileExtensionsNID
  insertEdge (Edge systemNodeNID "file-extensions" fileExtensionsNID)
