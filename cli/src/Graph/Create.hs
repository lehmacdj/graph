module Graph.Create where

import Graph.GraphMetadataEditing
import Models.NID
import Models.NormalizedPath
import MyPrelude

-- | Create all the nodes/edges such that resolving the path would produce the
-- exact same result.
createNPath ::
  (Member GraphMetadataEditing r) =>
  NormalizedPath NID ->
  Sem r ()
createNPath = undefined
