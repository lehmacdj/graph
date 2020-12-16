{-# LANGUAGE FlexibleContexts #-}

-- | manipulate graphs where edges are themselves nodes in the graph.
--
-- = Naming conventions
-- For functions that refer to nodes in the graph there are usually 4 variants.
-- For example:
-- * 'delNode'
-- * 'delNode''
-- * 'delNodes'
-- * 'delNodes''
-- When this occurs the unprimed versions operate on Nodes and are probably the
-- most performant. Care should be taken however with these functions to avoid
-- using an old version of a node that has been updated since it was fetched
-- from the graph. If this occurs behavior is undefined. Primed versions are
-- also available that will automatically query the necessary nodes from the
-- graph. These are safe by default, because the node will definitely be up to
-- date. There are also plural versions that operate on a bunch of Nodes/NIDs
-- at the same time.
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
    delNode',
    delNodes,
    delNodes',
    delEdge,
    delEdges,

    -- ** data
    setData,
    setData',

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
    adjustMap (over nodeOutgoing' (deleteSet (outConnect e))) (source e)
      . adjustMap (over nodeIncoming' (deleteSet (inConnect e))) (sink e)
      . adjustMap (over nodeReferents (deleteSet (referentEdge e))) (label e)

delEdges :: [Edge NID] -> Graph' -> Graph'
delEdges = listify delEdge

-- | Remove a node from the graph; updating the cached data in the neighbors
-- nodes as well.
delNode :: Node' -> Graph' -> Graph'
delNode n g =
  withNodeMap' g $
    omap deleteIncoming
      . omap deleteOutgoing
      . omap deleteLabling
      . deleteMap nid
  where
    nid = _nodeId' n
    del =
      filterSet ((/= nid) . view connectNode)
        . filterSet ((/= nid) . view connectTransition)
    delByLabel =
      filterSet ((/= nid) . view unlabledEdgeSource)
        . filterSet ((/= nid) . view unlabledEdgeSink)
    deleteIncoming = over nodeIncoming' del
    deleteOutgoing = over nodeOutgoing' del
    deleteLabling = over nodeReferents delByLabel

delNode' :: NID -> Graph' -> Graph'
delNode' = primed delNode

delNodes :: [Node'] -> Graph' -> Graph'
delNodes = listify delNode

delNodes' :: [NID] -> Graph' -> Graph'
delNodes' = primeds delNodes

insertEdge :: Edge NID -> Graph' -> Graph'
insertEdge e g =
  withNodeMap' g $
    adjustMap (over nodeOutgoing' (insertSet (outConnect e))) (source e)
      . adjustMap (over nodeIncoming' (insertSet (inConnect e))) (sink e)
      . adjustMap (over nodeReferents (insertSet (referentEdge e))) (label e)

insertEdges :: [Edge NID] -> Graph' -> Graph'
insertEdges = listify insertEdge

-- | Add a node, and all the edges it is associated with to the Graph'.
-- If the graph already has the node it replaces the previous node, but no
-- effort is made to remove any edges, edges may only be added. Note that this
-- can yield an inconsistent graph, so duplicate nodes should be inserted with
-- care.
insertNode :: Node' -> Graph' -> Graph'
insertNode n g =
  insertEdges (incomingEs ++ outgoingEs ++ referentEs) $
    withNodeMap' g (insertMap nid n)
  where
    nid = _nodeId' n
    incomingEs = map (`incomingEdge` nid) (toList (_nodeIncoming' n))
    outgoingEs = map (outgoingEdge nid) (toList (_nodeOutgoing' n))
    referentEs = map (labledWith nid) (toList (_nodeReferents n))

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

-- | sets the data, setting to nothing is equivalent to deleting the data
-- this is a terrible function that should probably not be used
setData :: Maybe LByteString -> Node' -> Graph' -> Graph'
setData d n g = insertNode (set nodeData' d (nodeConsistentWithGraph g n)) g

setData' :: Maybe LByteString -> NID -> Graph' -> Graph'
setData' d = primed (setData d)

lookupNode :: Graph' -> NID -> Maybe Node'
lookupNode = flip lookup . nodeMap'

nodeConsistentWithGraph :: HasCallStack => Graph' -> Node' -> Node'
nodeConsistentWithGraph g n
  | lookupNodeEx g (nidOf n) == n = n
  | otherwise = error $ "node " ++ show n ++ " is inconsistent with the state of the graph " ++ show g

traceGraph :: Graph' -> Graph'
traceGraph g = withNodeMap' g $ \nm -> Debug.trace (showDebug (Debug.trace "graph is:" g)) nm

showDebug :: Graph' -> String
showDebug = unlines . map show . toList . nodeMap'

filterGraph :: (Node' -> Bool) -> Graph' -> Graph'
filterGraph f g = ofoldr maybeDelNode g (nodeMap' g)
  where
    maybeDelNode x ig'
      | not $ f x = delNode x ig'
      | otherwise = ig'

mapGraph :: (Node' -> Node') -> Graph' -> Graph'
mapGraph f g = withNodeMap' g $ \nm -> omap f nm

dualizeGraph :: Graph' -> Graph'
dualizeGraph = mapGraph dualizeNode
