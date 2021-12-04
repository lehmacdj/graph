module Graph.Edge where

import Control.Lens
import Graph.Types

outConnect :: Edge t -> Connect t
outConnect (Edge _ l t) = Connect l t

inConnect :: Edge t -> Connect t
inConnect (Edge s l _) = Connect l s

source :: Edge t -> NID
source = view #_edgeSource

sink :: Edge t -> NID
sink = view #_edgeSink

label :: Edge t -> t
label = view #_edgeTransition

outgoingEdge :: NID -> Connect t -> Edge t
outgoingEdge s (Connect l t) = Edge s l t

incomingEdge :: Connect t -> NID -> Edge t
incomingEdge (Connect l s) = Edge s l

edgeBetween :: Node t -> t -> Node t -> Edge t
edgeBetween s l t = Edge (_nodeId s) l (_nodeId t)

dualizeEdge :: Edge t -> Edge t
dualizeEdge (Edge i t o) = Edge o t i
