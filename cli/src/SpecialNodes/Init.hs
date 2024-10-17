module SpecialNodes.Init
  ( createSpecialNodes,
  )
where

import Effect.Graph
import Models.Types
import MyPrelude
import SpecialNodes

createSpecialNodes :: HasGraph String effs => Sem effs ()
createSpecialNodes = do
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
  touchNode historicalSpecialNodesNID
  insertEdge (Edge systemNodeNID "historical-special-nodes" historicalSpecialNodesNID)
  touchNode sequenceIDsNID
  insertEdge (Edge historicalSpecialNodesNID "sequence-ids" sequenceIDsNID)
  touchNode mimeTypesNID
  insertEdge (Edge systemNodeNID "mime-types" mimeTypesNID)
  touchNode fileExtensionsNID
  insertEdge (Edge systemNodeNID "file-extensions" fileExtensionsNID)
