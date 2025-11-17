module Graph.Create where

import Graph.GraphMetadataEditing
import Models.Edge
import Models.NID
import Models.NormalizedPath
import MyPrelude

-- | Create all the nodes/edges such that resolving the path would produce the
-- exact same result.
createNPath ::
  (Member GraphMetadataEditing r) =>
  NormalizedPath NID ->
  Sem r ()
createNPath =
  traverseViaTransitions_ \source direction sink -> case direction of
    DPOutgoing' (DPLiteral transition) ->
      touchNode source *> touchNode sink *> insertEdge (Edge {..})
    DPIncoming' (DPLiteral transition) ->
      touchNode source *> touchNode sink *> insertEdge (dualizeEdge Edge {..})
    _ -> error "non-DPLiteral transitions are illegal for `NormalizePath NID`"
