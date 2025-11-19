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
  deriving (Eq, Ord, Generic, NFData)

instance (Show t) => CompactNodeShow (Edge t) where
  type Augmentation (Edge t) = Void
  minimumNidLength settings c =
    min
      (minimumNidLength settings c.source)
      (minimumNidLength settings c.sink)
  nshowSettings settings Edge {..} =
    nshowSettings settings source
      ++ (">" ++ tshow transition ++ ">")
      ++ nshowSettings settings sink

instance (Show t) => Show (Edge t) where
  show = unpack . nshowDefault

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
