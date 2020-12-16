
module Graph.Node' where

import Control.Lens
import qualified Data.Set as Set
import Graph.Types
import Graph.Types.New
import MyPrelude

indegreeOf :: Node' -> Int
indegreeOf = Set.size . view nodeIncoming'

outdegreeOf :: Node' -> Int
outdegreeOf = Set.size . view nodeOutgoing'

nidOf :: Node' -> NID
nidOf = _nodeId'

incomingConnectsOf :: Node' -> Set (Connect NID)
incomingConnectsOf = _nodeIncoming'

outgoingConnectsOf :: Node' -> Set (Connect NID)
outgoingConnectsOf = _nodeOutgoing'

incomingNeighborsOf :: Node' -> Set NID
incomingNeighborsOf = Set.map _connectNode . incomingConnectsOf

incomingTransitionsOf :: Node' -> Set NID
incomingTransitionsOf = Set.map _connectTransition . incomingConnectsOf

outgoingNeighborsOf :: Node' -> Set NID
outgoingNeighborsOf = Set.map _connectNode . incomingConnectsOf

outgoingTransitionsOf :: Node' -> Set NID
outgoingTransitionsOf = Set.map _connectTransition . outgoingConnectsOf

dataOf :: Node' -> Maybe LByteString
dataOf = view nodeData'

-- | Warning! It is up to the user of the graph to ensure that node ids are
-- unique within the graph
emptyNode :: NID -> Node'
emptyNode i = Node' i mempty mempty mempty Nothing

dualizeNode :: Node' -> Node'
dualizeNode (Node' nid i o r x) = Node' nid o i r x
