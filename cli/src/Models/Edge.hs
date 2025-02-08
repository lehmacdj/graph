module Models.Edge where

import Control.Lens
import Models.Types

outConnect :: Edge t -> Connect t
outConnect (Edge _ l t) = Connect l t

inConnect :: Edge t -> Connect t
inConnect (Edge s l _) = Connect l s

outgoingEdge :: NID -> Connect t -> Edge t
outgoingEdge s (Connect l t) = Edge s l t

incomingEdge :: Connect t -> NID -> Edge t
incomingEdge (Connect l s) = Edge s l

edgeBetween :: Node t a -> t -> Node t a -> Edge t
edgeBetween s l t = Edge (s ^. #nid) l (t ^. #nid)

dualizeEdge :: Edge t -> Edge t
dualizeEdge (Edge i t o) = Edge o t i
