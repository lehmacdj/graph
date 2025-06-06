module Models.Edge where

import Models.Connect
import Models.NID
import MyPrelude

-- | unbiased representation of an edge
data Edge t = Edge
  { source :: NID,
    transition :: t,
    sink :: NID
  }
  deriving (Eq, Ord, Generic, NFData, Show)

data EdgeSide = Source | Sink
  deriving (Eq, Ord, Generic, NFData, Show)

outConnect :: Edge t -> Connect t
outConnect (Edge _ l t) = Connect l t

inConnect :: Edge t -> Connect t
inConnect (Edge s l _) = Connect l s

outgoingEdge :: NID -> Connect t -> Edge t
outgoingEdge s (Connect l t) = Edge s l t

incomingEdge :: Connect t -> NID -> Edge t
incomingEdge (Connect l s) = Edge s l

dualizeEdge :: Edge t -> Edge t
dualizeEdge (Edge i t o) = Edge o t i
