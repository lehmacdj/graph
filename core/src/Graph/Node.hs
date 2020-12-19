module Graph.Node where

import Control.Lens
import qualified Data.Set as Set
import Graph.Types
import MyPrelude

indegreeOf :: Node t -> Int
indegreeOf = Set.size . view nodeIncoming

outdegreeOf :: Node t -> Int
outdegreeOf = Set.size . view nodeOutgoing

nidOf :: Node t -> NID
nidOf = _nodeId

incomingConnectsOf ::
  TransitionValid t =>
  Node t ->
  Set (Connect t)
incomingConnectsOf = _nodeIncoming

outgoingConnectsOf ::
  TransitionValid t =>
  Node t ->
  Set (Connect t)
outgoingConnectsOf = _nodeOutgoing

incomingNeighborsOf ::
  TransitionValid t =>
  Node t ->
  Set NID
incomingNeighborsOf = Set.map _connectNode . incomingConnectsOf

incomingTransitionsOf ::
  TransitionValid t =>
  Node t ->
  Set t
incomingTransitionsOf = Set.map _connectTransition . incomingConnectsOf

outgoingNeighborsOf ::
  TransitionValid t =>
  Node t ->
  Set NID
outgoingNeighborsOf = Set.map _connectNode . incomingConnectsOf

outgoingTransitionsOf ::
  TransitionValid t =>
  Node t ->
  Set t
outgoingTransitionsOf = Set.map _connectTransition . outgoingConnectsOf

dataOf ::
  TransitionValid t =>
  Node t ->
  Maybe LByteString
dataOf = view nodeData

-- | Warning! It is up to the user of the graph to ensure that node ids are
-- unique within the graph
emptyNode :: Ord t => NID -> Node t
emptyNode i = Node i mempty mempty Nothing

dualizeNode :: Node t -> Node t
dualizeNode (Node nid i o x) = Node nid o i x
