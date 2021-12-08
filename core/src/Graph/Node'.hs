module Graph.Node' where

import Control.Lens
import qualified Data.Set as Set
import Graph.Types
import Graph.Types.New
import MyPrelude

indegreeOf :: Node' -> Int
indegreeOf = Set.size . view #incoming'

outdegreeOf :: Node' -> Int
outdegreeOf = Set.size . view #outgoing'

nidOf :: Node' -> NID
nidOf = nodeId'

incomingConnectsOf :: Node' -> Set (Connect NID)
incomingConnectsOf = incoming'

outgoingConnectsOf :: Node' -> Set (Connect NID)
outgoingConnectsOf = outgoing'

incomingNeighborsOf :: Node' -> Set NID
incomingNeighborsOf = Set.map node . incomingConnectsOf

incomingTransitionsOf :: Node' -> Set NID
incomingTransitionsOf = Set.map (view #transition) . incomingConnectsOf

outgoingNeighborsOf :: Node' -> Set NID
outgoingNeighborsOf = Set.map (view #node) . incomingConnectsOf

outgoingTransitionsOf :: Node' -> Set NID
outgoingTransitionsOf = Set.map (view #transition) . outgoingConnectsOf

dataOf :: Node' -> Maybe ByteString
dataOf = view #associatedData'

-- | Warning! It is up to the user of the graph to ensure that node ids are
-- unique within the graph
emptyNode :: NID -> Node'
emptyNode i = Node' i mempty mempty mempty Nothing

dualizeNode :: Node' -> Node'
dualizeNode (Node' nid i o r x) = Node' nid o i r x
