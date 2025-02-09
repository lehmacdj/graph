{-# LANGUAGE FlexibleContexts #-}

module Models.Graph
  ( module Models.Graph,
  )
where

import Control.Lens
import qualified Data.Map.Internal.Debug as MD
import qualified Debug.Trace as Debug
import GHC.Stack
import Models.Edge
import Models.NID
import Models.Node
import MyPrelude

newtype Graph t a = Graph
  { nodeMap :: Map NID (Node t a)
  }
  deriving stock (Eq, Ord, Generic)
  deriving anyclass (NFData)

instance (Show t, Ord t) => Show (Graph t a) where
  show = unlines' . map show . toList . (.nodeMap)
    where
      unlines' = intercalate "\n"

withNodeMap :: Graph t a -> (Map NID (Node t a) -> Map NID (Node t a)) -> Graph t a
withNodeMap = flip (over #nodeMap)

-- | Utility function for converting lookups into actual node values with error
-- reporting.
assertNodeInGraph :: NID -> Maybe a -> a
assertNodeInGraph _ (Just n) = n
assertNodeInGraph i Nothing =
  error $ "expected " ++ show i ++ " to be in the graph"

lookupNode ::
  ValidNode t a =>
  Graph t a ->
  NID ->
  Node t a
lookupNode = flip nodeLookup
{-# INLINE lookupNode #-}

-- | Gets the most up to date version of the node from the graph.
-- This does not imply that graphs have version control, it simply means that
-- the original node might be out of date otherwise.
refreshNode ::
  ValidNode t a =>
  Graph t a ->
  Node t a ->
  Node t a
refreshNode g = lookupNode g . view #nid

nodeLookup ::
  ValidNode t a =>
  NID ->
  Graph t a ->
  Node t a
nodeLookup i g = fromMaybe err . lookup i . (^. #nodeMap) $ g
  where
    err =
      error $
        "expected to find " ++ show i ++ " in the Graph\n"
          ++ MD.showTree ((^. #nodeMap) g)

-- | Utility function for constructing a primed version of a function operating
-- on ids instead of nodes
primed ::
  ValidNode t a =>
  (Node t a -> Graph t a -> x) ->
  (NID -> Graph t a -> x)
primed f i ig = f (lookupNode ig i) ig

listify ::
  (x -> Graph t a -> Graph t a) ->
  ([x] -> Graph t a -> Graph t a)
listify f nodes ig = foldl' (flip f) ig nodes

primeds ::
  ValidNode t a =>
  ([Node t a] -> Graph t a -> x) ->
  ([NID] -> Graph t a -> x)
primeds f i ig = f (nodeLookup <$> i <*> pure ig) ig

delEdge :: ValidNode t a => Edge t -> Graph t a -> Graph t a
delEdge e g =
  withNodeMap g $
    adjustMap (over #outgoing (deleteSet (outConnect e))) (e ^. #source)
      . adjustMap (over #incoming (deleteSet (inConnect e))) (e ^. #sink)

delEdges ::
  ValidNode t a =>
  [Edge t] ->
  Graph t a ->
  Graph t a
delEdges = listify delEdge

-- | Remove a node from the graph; updating the cached data in the neighbors
-- nodes as well.
delNode :: Ord t => Node t a -> Graph t a -> Graph t a
delNode n g =
  withNodeMap g $
    omap deleteIncoming
      . omap deleteOutgoing
      . deleteMap nid
  where
    nid = n ^. #nid
    del = filterSet ((/= nid) . view #node)
    deleteIncoming = over #incoming del
    deleteOutgoing = over #outgoing del

delNode' ::
  ValidNode t a =>
  NID ->
  Graph t a ->
  Graph t a
delNode' = primed delNode

delNodes ::
  Ord t => [Node t a] -> Graph t a -> Graph t a
delNodes = listify delNode

delNodes' ::
  ValidNode t a =>
  [NID] ->
  Graph t a ->
  Graph t a
delNodes' = primeds delNodes

insertEdge ::
  ValidNode t a =>
  Edge t ->
  Graph t a ->
  Graph t a
insertEdge e g =
  withNodeMap g $
    adjustMap (over #outgoing (insertSet (outConnect e))) (e ^. #source)
      . adjustMap (over #incoming (insertSet (inConnect e))) (e ^. #sink)

insertEdges ::
  ValidNode t a =>
  [Edge t] ->
  Graph t a ->
  Graph t a
insertEdges = listify insertEdge

-- | Add a node, and all the edges it is associated with to the Graph.
insertNode ::
  ValidNode t a =>
  Node t a ->
  Graph t a ->
  Graph t a
insertNode n g =
  insertEdges (incomingEs ++ outgoingEs) $
    withNodeMap g (insertMap nid n)
  where
    nid = n ^. #nid
    incomingEs = map (`incomingEdge` nid) (toList (view #incoming n))
    outgoingEs = map (outgoingEdge nid) (toList (view #outgoing n))

insertNodes ::
  ValidNode t a =>
  [Node t a] ->
  Graph t a ->
  Graph t a
insertNodes = listify insertNode

nodesOf :: Graph t a -> [Node t a]
nodesOf = (^.. #nodeMap . folded)

emptyGraph :: Graph t a
emptyGraph = Graph mempty

isEmptyGraph :: Graph t a -> Bool
isEmptyGraph = null . view #nodeMap

-- | sets the data, setting to nothing is equivalent to deleting the data
-- this is a terrible function that should probably not be used
setData ::
  (ValidNode t a, Eq a) =>
  a ->
  Node t a ->
  Graph t a ->
  Graph t a
setData d n g = insertNode (set #associatedData d (nodeConsistentWithGraph g n)) g

setData' ::
  ValidNode t a =>
  a ->
  NID ->
  Graph t a ->
  Graph t a
setData' d = primed (setData d)

maybeLookupNode :: Graph t a -> NID -> Maybe (Node t a)
maybeLookupNode = flip lookup . view #nodeMap

nodeConsistentWithGraph ::
  (HasCallStack, ValidNode t a, Eq a) =>
  Graph t a ->
  Node t a ->
  Node t a
nodeConsistentWithGraph g n
  | lookupNode g (n ^. #nid) == n = n
  | otherwise = error $ "node " ++ show n ++ " is inconsistent with the state of the graph " ++ show g

traceGraph :: ValidNode t a => Graph t a -> Graph t a
traceGraph g = withNodeMap g $ \nm -> Debug.trace (showDebug (Debug.trace "graph is:" g)) nm

showDebug :: ValidNode t a => Graph t a -> String
showDebug = unlines . map show . toList . view #nodeMap

filterGraph ::
  Ord t =>
  (Node t a -> Bool) ->
  Graph t a ->
  Graph t a
filterGraph f g = ofoldr maybeDelNode g (view #nodeMap g)
  where
    maybeDelNode x ig'
      | not $ f x = delNode x ig'
      | otherwise = ig'

mapGraph ::
  (Node t a -> Node t a) ->
  Graph t a ->
  Graph t a
mapGraph f g = withNodeMap g $ \nm -> omap f nm

dualizeGraph :: Graph t a -> Graph t a
dualizeGraph = mapGraph dualizeNode
