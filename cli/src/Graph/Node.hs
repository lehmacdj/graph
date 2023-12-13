module Graph.Node where

import Control.Lens
import qualified Data.Set as Set
import Graph.Types
import MyPrelude

indegreeOf :: Node t -> Int
indegreeOf = Set.size . view #incoming

outdegreeOf :: Node t -> Int
outdegreeOf = Set.size . view #outgoing

nidOf :: Node t -> NID
nidOf = view #nodeId

incomingConnectsOf ::
  TransitionValid t =>
  Node t ->
  Set (Connect t)
incomingConnectsOf = view #incoming

outgoingConnectsOf ::
  TransitionValid t =>
  Node t ->
  Set (Connect t)
outgoingConnectsOf = view #outgoing

incomingNeighborsOf ::
  TransitionValid t =>
  Node t ->
  Set NID
incomingNeighborsOf = Set.map (view #node) . incomingConnectsOf

incomingTransitionsOf ::
  TransitionValid t =>
  Node t ->
  Set t
incomingTransitionsOf = Set.map (view #transition) . incomingConnectsOf

outgoingNeighborsOf ::
  TransitionValid t =>
  Node t ->
  Set NID
outgoingNeighborsOf = Set.map (view #node) . incomingConnectsOf

outgoingTransitionsOf ::
  TransitionValid t =>
  Node t ->
  Set t
outgoingTransitionsOf = Set.map (view #transition) . outgoingConnectsOf

dataOf ::
  TransitionValid t =>
  Node t ->
  Maybe ByteString
dataOf = view #associatedData

-- | Warning! It is up to the user of the graph to ensure that node ids are
-- unique within the graph
emptyNode :: Ord t => NID -> Node t
emptyNode i = Node i mempty mempty Nothing

dualizeNode :: Node t -> Node t
dualizeNode (Node nid i o x) = Node nid o i x
