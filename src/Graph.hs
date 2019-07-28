{-# LANGUAGE FlexibleContexts #-}

module Graph
  ( module Graph
  , module Graph.Types
  , module Graph.Edge
  ) where

import Control.Lens

import Data.ByteString.Lazy (ByteString)
import Data.Foldable
import qualified Data.Map as M
import qualified Data.Map.Internal.Debug as MD
import Data.Maybe
import Data.Set (Set)
import qualified Data.Set as Set


import qualified Debug.Trace as Debug

import Graph.Types
import Graph.Edge

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
nodeLookup i g = fromMaybe err . M.lookup i . nodeMap $ g where
  err = error $ "expected to find " ++ show i ++ " in the Graph\n"
             ++ MD.showTree (nodeMap g)

-- | Utility function for constructing a primed version of a function operating on ids instead of
primed
  :: TransitionValid t
  => (Node t -> Graph t -> a)
  -> (Id -> Graph t -> a)
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
  M.adjust (over nodeOutgoing (Set.delete (outConnect e))) (source e)
  . M.adjust (over nodeIncoming (Set.delete (inConnect e))) (sink e)

delEdges
  :: TransitionValid t
  => [Edge t] -> Graph t -> Graph t
delEdges = listify delEdge

-- | Remove a node from the graph; updating the cached data in the neighbors
-- nodes as well.
delNode :: Node t -> Graph t -> Graph t
delNode n g = withNodeMap g $
  M.map deleteIncoming
  . M.map deleteOutgoing
  . M.delete nid where
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
  M.adjust (over nodeOutgoing (Set.insert (outConnect e))) (source e)
  . M.adjust (over nodeIncoming (Set.insert (inConnect e))) (sink e)

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
  $ withNodeMap g (M.insert nid n) where
    nid = _nodeId n
    incomingEs = map (`incomingEdge` nid) (toList (_nodeIncoming n))
    outgoingEs = map (outgoingEdge nid) (toList (_nodeOutgoing n))

insertNodes
  :: TransitionValid t
  => [Node t] -> Graph t -> Graph t
insertNodes = listify insertNode

nodesOf :: Graph t -> [Node t]
nodesOf = M.elems . nodeMap

emptyGraph :: Graph t
emptyGraph = Graph M.empty

isEmptyGraph :: Graph t -> Bool
isEmptyGraph = M.null . nodeMap

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

-- | sets the data, setting to nothing is equivalent to deleting the data
setData
  :: TransitionValid t
  => Maybe ByteString -> Node t -> Graph t -> Graph t
setData d n g = insertNode (set nodeData d (nodeConsistentWithGraph g n)) g

dataOf
  :: TransitionValid t
  => Node t -> Maybe ByteString
dataOf = view nodeData

maybeLookupNode :: Graph t -> Id -> Maybe (Node t)
maybeLookupNode = flip M.lookup . nodeMap

nodeConsistentWithGraph
  :: TransitionValid t
  => Graph t -> Node t -> Node t
nodeConsistentWithGraph g n
  | lookupNode g (nidOf n) == n = n
  | otherwise = error $ "node " ++ show n ++ " is inconsistent with the state of the graph"

traceGraph :: TransitionValid t => Graph t -> Graph t
traceGraph g = withNodeMap g $ \nm -> Debug.trace (showDebug (Debug.trace "graph is:" g)) nm

showDebug :: TransitionValid t => Graph t -> String
showDebug = unlines . map show . M.elems . nodeMap

-- | Warning! It is up to the user of the graph to ensure that node ids are
-- unique within the graph
emptyNode :: Id -> Node t
emptyNode i = Node i Set.empty Set.empty Nothing

filterGraph
  :: (Node t -> Bool)
  -> Graph t
  -> Graph t
filterGraph f g = M.foldr maybeDelNode g (nodeMap g) where
  maybeDelNode x ig'
    | not $ f x = delNode x ig'
    | otherwise = ig'

mapGraph
  :: (Node t -> Node t)
  -> Graph t
  -> Graph t
mapGraph f g = withNodeMap g $ \nm -> M.map f nm

dualizeGraph :: Graph t -> Graph t
dualizeGraph = mapGraph dualizeNode where
  dualizeNode (Node nid i o x) = Node nid o i x

-- | Return the id of an arbitrary node in the graph.
arbitraryId :: Graph t -> Id
arbitraryId = nidOf . head' . M.elems . nodeMap where
  head' [] = error "expected there to be a node in the graph but there were none"
  head' (x:_) = x
