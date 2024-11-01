{-# LANGUAGE FlexibleContexts #-}

module Models.Graph
  ( module Models.Graph,
    module Models.Types,
    module Models.Edge,
    module Models.Node,
  )
where

import Control.Lens
import qualified Data.Map.Internal.Debug as MD
import qualified Debug.Trace as Debug
import GHC.Stack
import Models.Edge
import Models.Node
import Models.Types
import MyPrelude

-- | Utility function for converting lookups into actual node values with error
-- reporting.
assertNodeInGraph :: NID -> Maybe a -> a
assertNodeInGraph _ (Just n) = n
assertNodeInGraph i Nothing =
  error $ "expected " ++ show i ++ " to be in the graph"

lookupNode ::
  TransitionValid t =>
  Graph t ->
  NID ->
  Node t
lookupNode = flip nodeLookup
{-# INLINE lookupNode #-}

-- | Gets the most up to date version of the node from the graph.
-- This does not imply that graphs have version control, it simply means that
-- the original node might be out of date otherwise.
refreshNode ::
  TransitionValid t =>
  Graph t ->
  Node t ->
  Node t
refreshNode g = lookupNode g . nidOf

nodeLookup ::
  TransitionValid t =>
  NID ->
  Graph t ->
  Node t
nodeLookup i g = fromMaybe err . lookup i . nodeMap $ g
  where
    err =
      error $
        "expected to find " ++ show i ++ " in the Graph\n"
          ++ MD.showTree (nodeMap g)

-- | Utility function for constructing a primed version of a function operating
-- on ids instead of nodes
primed ::
  TransitionValid t =>
  (Node t -> Graph t -> a) ->
  (NID -> Graph t -> a)
primed f i ig = f (lookupNode ig i) ig

listify ::
  (a -> Graph t -> Graph t) ->
  ([a] -> Graph t -> Graph t)
listify f nodes ig = foldl' (flip f) ig nodes

primeds ::
  TransitionValid t =>
  ([Node t] -> Graph t -> Graph t) ->
  ([NID] -> Graph t -> Graph t)
primeds f i ig = f (nodeLookup <$> i <*> pure ig) ig

delEdge :: TransitionValid t => Edge t -> Graph t -> Graph t
delEdge e g =
  withNodeMap g $
    adjustMap (over #outgoing (deleteSet (outConnect e))) (source e)
      . adjustMap (over #incoming (deleteSet (inConnect e))) (sink e)

delEdges ::
  TransitionValid t =>
  [Edge t] ->
  Graph t ->
  Graph t
delEdges = listify delEdge

-- | Remove a node from the graph; updating the cached data in the neighbors
-- nodes as well.
delNode :: Ord t => Node t -> Graph t -> Graph t
delNode n g =
  withNodeMap g $
    omap deleteIncoming
      . omap deleteOutgoing
      . deleteMap nid
  where
    nid = view #nodeId n
    del = filterSet ((/= nid) . view #node)
    deleteIncoming = over #incoming del
    deleteOutgoing = over #outgoing del

delNode' ::
  TransitionValid t =>
  NID ->
  Graph t ->
  Graph t
delNode' = primed delNode

delNodes ::
  Ord t => [Node t] -> Graph t -> Graph t
delNodes = listify delNode

delNodes' ::
  TransitionValid t =>
  [NID] ->
  Graph t ->
  Graph t
delNodes' = primeds delNodes

insertEdge ::
  TransitionValid t =>
  Edge t ->
  Graph t ->
  Graph t
insertEdge e g =
  withNodeMap g $
    adjustMap (over #outgoing (insertSet (outConnect e))) (source e)
      . adjustMap (over #incoming (insertSet (inConnect e))) (sink e)

insertEdges ::
  TransitionValid t =>
  [Edge t] ->
  Graph t ->
  Graph t
insertEdges = listify insertEdge

-- | Add a node, and all the edges it is associated with to the Graph.
insertNode ::
  TransitionValid t =>
  Node t ->
  Graph t ->
  Graph t
insertNode n g =
  insertEdges (incomingEs ++ outgoingEs) $
    withNodeMap g (insertMap nid n)
  where
    nid = view #nodeId n
    incomingEs = map (`incomingEdge` nid) (toList (view #incoming n))
    outgoingEs = map (outgoingEdge nid) (toList (view #outgoing n))

insertNodes ::
  TransitionValid t =>
  [Node t] ->
  Graph t ->
  Graph t
insertNodes = listify insertNode

nodesOf :: Graph t -> [Node t]
nodesOf = toList . nodeMap

emptyGraph :: Graph t
emptyGraph = Graph mempty

isEmptyGraph :: Graph t -> Bool
isEmptyGraph = null . nodeMap

-- | sets the data, setting to nothing is equivalent to deleting the data
-- this is a terrible function that should probably not be used
setData ::
  TransitionValid t =>
  Maybe ByteString ->
  Node t ->
  Graph t ->
  Graph t
setData d n g = insertNode (set #associatedData d (nodeConsistentWithGraph g n)) g

setData' ::
  TransitionValid t =>
  Maybe ByteString ->
  NID ->
  Graph t ->
  Graph t
setData' d = primed (setData d)

maybeLookupNode :: Graph t -> NID -> Maybe (Node t)
maybeLookupNode = flip lookup . nodeMap

nodeConsistentWithGraph ::
  (HasCallStack, TransitionValid t) =>
  Graph t ->
  Node t ->
  Node t
nodeConsistentWithGraph g n
  | lookupNode g (nidOf n) == n = n
  | otherwise = error $ "node " ++ show n ++ " is inconsistent with the state of the graph " ++ show g

traceGraph :: TransitionValid t => Graph t -> Graph t
traceGraph g = withNodeMap g $ \nm -> Debug.trace (showDebug (Debug.trace "graph is:" g)) nm

showDebug :: TransitionValid t => Graph t -> String
showDebug = unlines . map show . toList . nodeMap

filterGraph ::
  Ord t =>
  (Node t -> Bool) ->
  Graph t ->
  Graph t
filterGraph f g = ofoldr maybeDelNode g (nodeMap g)
  where
    maybeDelNode x ig'
      | not $ f x = delNode x ig'
      | otherwise = ig'

mapGraph ::
  (Node t -> Node t) ->
  Graph t ->
  Graph t
mapGraph f g = withNodeMap g $ \nm -> omap f nm

dualizeGraph :: Graph t -> Graph t
dualizeGraph = mapGraph dualizeNode
