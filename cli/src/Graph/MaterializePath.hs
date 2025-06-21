module Graph.MaterializePath where

import Graph.GraphMetadataEditing
import Models.Connect (Connect (..))
import Models.Graph (Graph, emptyGraph, insertNode, nodesMatchedBy, nodesMatching, singletonGraph)
import Models.NID
import Models.Node
import Models.NormalizedPath
import Models.Path
import MyPrelude hiding ((\\))

data IsThin = Thin | Fetched
  deriving (Eq, Show, Ord, Generic)

instance ShowableAugmentation IsThin where
  augmentationLabel = Nothing
  defaultShowAugmentation = \case
    Thin -> "thin"
    Fetched -> "fetched"
  shouldShowStandaloneAugmentation = True

data MaterializedPathResult t = MatrializedPathResult
  { path :: NormalizedPath ConcreteAnchor t,
    graph :: Graph t IsThin,
    nonexistentNodes :: [NID]
  }
  deriving (Eq, Show, Generic)

-- | Traverse a path, fetching node metadata and noting which nodes are missing.
materializePath ::
  forall t r.
  ( Members '[GraphMetadataReading t] r,
    ValidTransition t
  ) =>
  -- | The node to start from
  NID ->
  Path t ->
  Sem r (MaterializedPathResult t)
materializePath nid path = undefined
