{-# LANGUAGE FlexibleContexts #-}

module Data.Graph
  ( module Data.Graph
  , module Data.Graph.Types
  , module Data.Graph.Edge
  ) where

import Control.Lens

import Data.Foldable
import qualified Data.IntMap as IM
import qualified Data.IntMap.Internal.Debug as IMD
import Data.Maybe
import Data.Set (Set)
import Data.Set.Lens
import qualified Data.Set as Set

import Control.Monad.State

import qualified Debug.Trace as Debug

import Data.Graph.Types
import Data.Graph.Edge

indegreeOf :: Node t -> Int
indegreeOf = Set.size . view nodeIncoming

outdegreeOf :: Node t -> Int
outdegreeOf = Set.size . view nodeOutgoing

-- | Utility function for converting lookups into actual node values with error
-- reporting.
assertNodeInGraph :: Id -> Maybe a -> a
assertNodeInGraph _ (Just n) = n
assertNodeInGraph i Nothing =
  error $ "expected " ++ show i ++ " to be in the graph"

lookupNode :: TransitionValid t
           => Graph t -> Id -> Node t
lookupNode = flip nodeLookup
{-# INLINE lookupNode #-}

nodeLookup :: TransitionValid t
           => Id -> Graph t -> Node t
nodeLookup i g = fromMaybe err . IM.lookup i . nodeMap $ g where
  err = error $ "expected to find " ++ show i ++ " in the Graph\n"
             ++ IMD.showTree (nodeMap g)

-- | Utility function for constructing a primed version of a function operating on ids instead of
primed
  :: TransitionValid t
  => (Node t -> Graph t -> Graph t)
  -> (Id -> Graph t -> Graph t)
primed f i ig = f (lookupNode ig i) ig

listify
  :: (a -> Graph t -> Graph t)
  -> ([a] -> Graph t -> Graph t)
listify f nodes ig = foldl' (flip f) ig nodes

primeds
  :: TransitionValid t
  => ([Node t] -> Graph t -> Graph t)
  -> ([Id] -> Graph t -> Graph t)
primeds f i ig = f (pure nodeLookup <*> i <*> pure ig) ig

delEdge :: TransitionValid t => Edge t -> Graph t -> Graph t
delEdge e g = withNodeMap g $
  IM.adjust (over nodeOutgoing (Set.delete (outConnect e))) (source e)
  . IM.adjust (over nodeIncoming (Set.delete (inConnect e))) (sink e)

delEdges
  :: TransitionValid t
  => [Edge t] -> Graph t -> Graph t
delEdges = listify delEdge

-- | Remove a node from the graph; updating the cached data in the neighbors
-- nodes as well.
delNode :: Node t -> Graph t -> Graph t
delNode n g = withNodeMap g $
  IM.map deleteIncoming
  . IM.map deleteIncoming
  . IM.delete nid where
    nid = _nodeId n
    del = Set.filter ((/=nid) . view connectNode)
    deleteIncoming = over nodeIncoming del
    deleteOutgoing = over nodeOutgoing del

delNode'
  :: TransitionValid t
  => Id -> Graph t -> Graph t
delNode' = primed delNode

delNodes
  :: [Node t] -> Graph t -> Graph t
delNodes = listify delNode

delNodes'
  :: TransitionValid t
  => [Id] -> Graph t -> Graph t
delNodes' = primeds delNodes

insertEdge
  :: TransitionValid t
  => Edge t -> Graph t -> Graph t
insertEdge e g = withNodeMap g $
  IM.adjust (over nodeOutgoing (Set.insert (outConnect e))) (source e)
  . IM.adjust (over nodeIncoming (Set.insert (inConnect e))) (sink e)

insertEdges
  :: TransitionValid t
  => [Edge t] -> Graph t -> Graph t
insertEdges = listify insertEdge


-- | Add a node, and all the edges it is associated with to the Graph.
insertNode
  :: TransitionValid t
  => Node t -> Graph t -> Graph t
insertNode n g =
  insertEdges (incomingEs ++ outgoingEs)
  $ withNodeMap g (IM.insert nid n) where
    nid = _nodeId n
    incomingEs = map (`incomingEdge` nid) (toList (_nodeIncoming n))
    outgoingEs = map (outgoingEdge nid) (toList (_nodeOutgoing n))

insertNodes
  :: TransitionValid t
  => [Node t] -> Graph t -> Graph t
insertNodes = listify insertNode

nodesOf :: Graph t -> [Node t]
nodesOf = IM.elems . nodeMap

emptyGraph :: Graph t
emptyGraph = Graph 0 IM.empty

isEmptyGraph :: Graph t -> Bool
isEmptyGraph = IM.null . nodeMap

nidOf :: Node t -> Id
nidOf = _nodeId

incomingConnectsOf
  :: TransitionValid t
  => Node t -> Set (Connect t)
incomingConnectsOf = _nodeIncoming

outgoingConnectsOf
  :: TransitionValid t
  => Node t -> Set (Connect t)
outgoingConnectsOf = _nodeOutgoing

incomingNeighborsOf
  :: TransitionValid t
  => Node t -> Set Id
incomingNeighborsOf = Set.map _connectNode . incomingConnectsOf

incomingTransitionsOf
  :: TransitionValid t
  => Node t -> Set t
incomingTransitionsOf = Set.map _connectTransition . incomingConnectsOf

outgoingNeighborsOf
  :: TransitionValid t
  => Node t -> Set Id
outgoingNeighborsOf = Set.map _connectNode . incomingConnectsOf

outgoingTransitionsOf
  :: TransitionValid t
  => Node t -> Set t
outgoingTransitionsOf = Set.map _connectTransition . outgoingConnectsOf

maybeLookupNode :: Graph t -> Id -> Maybe (Node t)
maybeLookupNode = flip IM.lookup . nodeMap

traceGraph :: TransitionValid t => Graph t -> Graph t
traceGraph g = withNodeMap g $ \nm -> Debug.trace (showDebug (Debug.trace "graph is:" g)) nm

showDebug :: TransitionValid t => Graph t -> String
showDebug = unlines . map show . IM.elems . nodeMap

-- | Warning! Using this to create a node and inserting it into a graph can
-- leave the graph with an invalid state, for future node allocation using
-- freshNode.
emptyNode :: Id -> Node t
emptyNode i = Node i Set.empty Set.empty

-- | Create a node with a unique id not yet in the graph
-- intended for use in larger monad states using zoom from Control.Lens.Zoom
-- Every node should be created using this method, this guarantees that every
-- new node has a unique id.
freshNode :: MonadState (Graph t) m => m (Node t)
freshNode = do
  nid <- use graphAllocator
  graphAllocator += 1
  pure (emptyNode nid)

filterGraph
  :: (Node t -> Bool)
  -> Graph t
  -> Graph t
filterGraph f g = IM.foldr maybeDelNode g (nodeMap g) where
  maybeDelNode x ig'
    | not $ f x = delNode x ig'
    | otherwise = ig'

mapGraph
  :: (Node t -> Node t)
  -> Graph t
  -> Graph t
mapGraph f g = withNodeMap g $ \nm -> IM.map f nm

dualizeGraph :: Graph t -> Graph t
dualizeGraph = mapGraph dualizeNode where
  dualizeNode (Node nid i o) = Node nid o i

-- | Return the id of an arbitrary node in the graph.
arbitraryId :: Graph t -> Id
arbitraryId = nidOf . head' . IM.elems . nodeMap where
  head' [] = error "expected there to be a node in the graph but there were none"
  head' (x:_) = x
