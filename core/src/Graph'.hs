{-# LANGUAGE FlexibleContexts #-}

-- | manipulate graphs where edges are themselves nodes in the graph.
module Graph'
  ( -- * re-exported types necessary for talking about graphs
    NID,
    Graph' (..),
    Node' (..),
    Edge (..),
    UnlabledEdge (..),

    -- * querying
    lookupNode,
    lookupNodeEx,
    isEmptyGraph,
    nodesOf,

    -- * updating
    emptyGraph,

    -- ** insert
    insertNode,
    insertNodes,
    insertEmptyNode,
    insertEmptyNodes,
    insertEdge,
    insertEdges,

    -- ** delete
    delNode,
    delNodes,
    delEdge,
    delEdges,

    -- ** data
    setData,

    -- ** bulk manipulation
    filterGraph,
    mapGraph,
    dualizeGraph,

    -- * debug
    traceGraph,
    assertNodeInGraph,

    -- * misc
    nextFreeNodeId,
  )
where

import Control.Lens
import qualified Data.Map.Internal.Debug as MD
import Data.Monoid (First (..))
import qualified Debug.Trace as Debug
import GHC.Stack
import Graph.Edge
import Graph.Node'
import Graph.Types
import Graph.Types.New
import MyPrelude

-- | Utility function for converting lookups into actual node values with error
-- reporting.
assertNodeInGraph :: NID -> Maybe a -> a
assertNodeInGraph _ (Just n) = n
assertNodeInGraph i Nothing =
  error $ "expected " ++ show i ++ " to be in the graph"

lookupNodeEx :: HasCallStack => Graph' -> NID -> Node'
lookupNodeEx = flip nodeLookup
{-# INLINE lookupNodeEx #-}

nodeLookup :: NID -> Graph' -> Node'
nodeLookup i g = fromMaybe err . lookup i . nodeMap' $ g
  where
    err =
      error $
        "expected to find " ++ show i ++ " in the Graph'\n"
          ++ MD.showTree (nodeMap' g)

-- | Utility function for constructing a primed version of a function operating on ids instead of
primed ::
  (Node' -> Graph' -> a) ->
  (NID -> Graph' -> a)
primed f i ig = f (lookupNodeEx ig i) ig

listify ::
  (a -> Graph' -> Graph') ->
  ([a] -> Graph' -> Graph')
listify f nodes ig = foldl' (flip f) ig nodes

primeds ::
  ([Node'] -> Graph' -> Graph') ->
  ([NID] -> Graph' -> Graph')
primeds f i ig = f (nodeLookup <$> i <*> pure ig) ig

delEdge :: Edge NID -> Graph' -> Graph'
delEdge e g =
  withNodeMap' g $
    adjustMap (over #outgoing' (deleteSet (outConnect e))) (view #source e)
      . adjustMap (over #incoming' (deleteSet (inConnect e))) (view #sink e)
      . adjustMap (over #referents (deleteSet (referentEdge e))) (view #transition e)

delEdges :: [Edge NID] -> Graph' -> Graph'
delEdges = listify delEdge

-- | Remove a node from the graph; updating the cached data in the neighbors
-- nodes as well.
-- This relies upon the fact that the node is up to date with respect to the
-- graph and thus must be used with caution.
internalDelNode :: Node' -> Graph' -> Graph'
internalDelNode n g =
  withNodeMap' g $
    omap deleteIncoming
      . omap deleteOutgoing
      . omap deleteLabling
      . deleteMap nid
  where
    nid = n ^. #nodeId'
    del =
      filterSet ((/= nid) . view #node)
        . filterSet ((/= nid) . view #transition)
    delByLabel =
      filterSet ((/= nid) . view #_unlabledEdgeSource)
        . filterSet ((/= nid) . view #_unlabledEdgeSink)
    deleteIncoming = over #incoming' del
    deleteOutgoing = over #outgoing' del
    deleteLabling = over #referents delByLabel

delNode :: NID -> Graph' -> Graph'
delNode = primed internalDelNode

internalDelNodes :: [Node'] -> Graph' -> Graph'
internalDelNodes = listify internalDelNode

delNodes :: [NID] -> Graph' -> Graph'
delNodes = primeds internalDelNodes

internalInsertEdge :: Edge NID -> Graph' -> Graph'
internalInsertEdge e g =
  withNodeMap' g $
    adjustMap (over #outgoing' (insertSet (outConnect e))) (view #source e)
      . adjustMap (over #incoming' (insertSet (inConnect e))) (view #sink e)
      . adjustMap (over #referents (insertSet (referentEdge e))) (view #transition e)

internalInsertEdges :: [Edge NID] -> Graph' -> Graph'
internalInsertEdges = listify internalInsertEdge

-- | Safe operation for inserting an edge. Inserts all of the nodes itself to
-- ensure that there can't be any dangling edges
insertEdge :: Edge NID -> Graph' -> Graph'
insertEdge e@(Edge i l o) = internalInsertEdge e . insertEmptyNodes [i, l, o]

insertEdges :: [Edge NID] -> Graph' -> Graph'
insertEdges = listify insertEdge

-- | merge two nodes with the same NID. If the NID of the nodes doesn't match
-- an exception is thrown.
-- Behavior of merging is as follows:
-- * edges are the union of the edges in the two nodes
-- * data is merged via the behavior of 'First'. i.e. the data of the second node
-- is kept or if it is none, then the data of the first node is retained
mergeNodesEx :: HasCallStack => Node' -> Node' -> Node'
mergeNodesEx (Node' nid1 in1 out1 refs1 data1) (Node' nid2 in2 out2 refs2 data2)
  | nid1 /= nid2 = error "node ids don't match!"
  | otherwise =
    Node'
      nid1
      (in1 <> in2)
      (out1 <> out2)
      (refs1 <> refs2)
      (getFirst (First data1 <> First data2))

-- | lookup an existing node in a graph and merge it with the presented node
mergeWithExisting :: Node' -> Graph' -> Node'
mergeWithExisting n g = maybe n (mergeNodesEx n) $ lookupNode g (nidOf n)

-- | Add a node, and all the edges it is associated with to the Graph'.
-- Inserting a node that is already in the graph merges the node with the
-- node that already is in the graph in an intelligent fashion. The resulting
-- edges are always a superset of the previous edges in the graph though,
-- it isn't possible to remove edges by inserting a node. See 'mergeNodesEx'
-- for details on how the merging is done. To remove edges use 'delEdge'.
insertNode :: Node' -> Graph' -> Graph'
insertNode n g =
  internalInsertEdges (incomingEs ++ outgoingEs ++ referentEs) $
    withNodeMap' g (insertMap nid n')
  where
    n' = mergeWithExisting n g
    nid = n' ^. #nodeId'
    incomingEs = map (`incomingEdge` nid) (toList (incoming' n'))
    outgoingEs = map (outgoingEdge nid) (toList (outgoing' n'))
    referentEs = map (labledWith nid) (toList (referents n'))

insertNodes :: [Node'] -> Graph' -> Graph'
insertNodes = listify insertNode

insertEmptyNode :: NID -> Graph' -> Graph'
insertEmptyNode = insertNode . emptyNode

insertEmptyNodes :: [NID] -> Graph' -> Graph'
insertEmptyNodes = listify insertEmptyNode

nodesOf :: Graph' -> [Node']
nodesOf = toList . nodeMap'

nextFreeNodeId :: Graph' -> NID
nextFreeNodeId g = case maximum (Nothing `ncons` fmap Just (nodesOf g)) of
  Nothing -> 0
  Just nid -> nidOf nid + 1

emptyGraph :: Graph'
emptyGraph = Graph' mempty

isEmptyGraph :: Graph' -> Bool
isEmptyGraph = null . nodeMap'

-- | sets the data of a node in the graph
setData :: Maybe ByteString -> NID -> Graph' -> Graph'
setData d nid = set (#nodeMap' . ix nid . #associatedData') d

lookupNode :: Graph' -> NID -> Maybe Node'
lookupNode = flip lookup . nodeMap'

traceGraph :: Graph' -> Graph'
traceGraph g = withNodeMap' g $ \nm -> Debug.trace (showDebug (Debug.trace "graph is:" g)) nm

showDebug :: Graph' -> String
showDebug = unlines . map show . toList . nodeMap'

filterGraph :: (Node' -> Bool) -> Graph' -> Graph'
filterGraph f g = ofoldr maybeDelNode g (nodeMap' g)
  where
    maybeDelNode x ig'
      | not $ f x = internalDelNode x ig'
      | otherwise = ig'

mapGraph :: (Node' -> Node') -> Graph' -> Graph'
mapGraph f g = withNodeMap' g $ \nm -> omap f nm

dualizeGraph :: Graph' -> Graph'
dualizeGraph = mapGraph dualizeNode
