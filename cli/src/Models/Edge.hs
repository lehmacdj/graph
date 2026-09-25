module Models.Edge where

import Models.Connect
import Models.NID
import MyPrelude

-- | unbiased representation of an edge
-- The type of node identifiers is a parameter; in general we only use the type
-- alias 'Edge' that fixes it to 'NID'.
data Edge' n t = Edge
  { source :: n,
    transition :: t,
    sink :: n
  }
  deriving (Eq, Ord, Generic, NFData)

-- | An edge between nodes identified by 'NID'. See 'Edge'' for more
-- information.
type Edge t = Edge' NID t

instance (Show t, CompactNIDShow n) => CompactNodeShow (Edge' n t) where
  type Augmentation (Edge' n t) = Void
  minimumNidLength settings c =
    max
      (minimumNidLength settings c.source)
      (minimumNidLength settings c.sink)
  nshowSettings settings Edge {..} =
    nshowSettings settings source
      ++ (">" ++ tshow transition ++ ">")
      ++ nshowSettings settings sink

instance (Show t, CompactNIDShow n) => Show (Edge' n t) where
  show = unpack . nshowDefault

data EdgeSide = Source | Sink
  deriving (Eq, Ord, Generic, NFData, Show)

outConnect :: Edge' n t -> Connect' n t
outConnect (Edge _ l t) = Connect l t

inConnect :: Edge' n t -> Connect' n t
inConnect (Edge s l _) = Connect l s

outgoingEdge :: n -> Connect' n t -> Edge' n t
outgoingEdge s (Connect l t) = Edge s l t

incomingEdge :: Connect' n t -> n -> Edge' n t
incomingEdge (Connect l s) = Edge s l

dualizeEdge :: Edge' n t -> Edge' n t
dualizeEdge (Edge i t o) = Edge o t i
