module Graph.Edge where

import Control.Lens
import Graph.Types

outConnect :: Edge t -> Connect t
outConnect (Edge _ l t) = Connect l t

inConnect :: Edge t -> Connect t
inConnect (Edge s l _) = Connect l s

source :: Edge t -> Id
source = view edgeSource

sink :: Edge t -> Id
sink = view edgeSink

label :: Edge t -> t
label = view edgeTransition

outgoingEdge :: Id -> Connect t -> Edge t
outgoingEdge s (Connect l t) = Edge s l t

incomingEdge :: Connect t -> Id -> Edge t
incomingEdge (Connect l s) = Edge s l

edgeBetween :: Node t -> t -> Node t -> Edge t
edgeBetween s l t = Edge (_nodeId s) l (_nodeId t)

dualizeEdge :: Edge t -> Edge t
dualizeEdge (Edge i t o) = Edge o t i
