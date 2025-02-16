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

type instance Control.Lens.Index (Graph t a) = NID
type instance Control.Lens.IxValue (Graph t a) = (Node t a)

instance ValidNode t a => Ixed (Graph t a) where
  ix nid f g = case maybeNodeLookup nid g of
    Just n -> f n <&> \newNode -> updateNode g newNode
    Nothing -> pure g

instance ValidNode t a => At (Graph t a) where
  at nid = lens (maybeNodeLookup nid) setter where
    setter g Nothing = delNode' nid g
    setter g (Just n) = case maybeNodeLookup nid g of
      Just _ -> updateNode g n
      Nothing -> insertNode n g

instance ValidNode t a => Semigroup (Graph t a) where
  g1 <> g2 = foldl' (flip insertNode) g1 (nodesOf g2)

instance ValidNode t a => Monoid (Graph t a) where
  mempty = emptyGraph
  mappend = (<>)

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

maybeNodeLookup :: NID -> Graph t a -> Maybe (Node t a)
maybeNodeLookup i = view $ #nodeMap . at i

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

deleteEdge :: ValidNode t a => Edge t -> Graph t a -> Graph t a
deleteEdge e g =
  withNodeMap g $
    adjustMap (over #outgoing (deleteSet (outConnect e))) (e ^. #source)
      . adjustMap (over #incoming (deleteSet (inConnect e))) (e ^. #sink)

deleteEdges ::
  ValidNode t a =>
  [Edge t] ->
  Graph t a ->
  Graph t a
deleteEdges = listify deleteEdge

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

containsEdge ::
  (HasCallStack, ValidNode t a) =>
  Graph t a ->
  Edge t ->
  Bool
g `containsEdge` e = withEarlyReturn_ do
  source <- unwrapReturningDefault False $ g ^. at e.source
  sink <- unwrapReturningDefault False $ g ^. at e.sink
  let outConnectExists = outConnect e `member` source.outgoing
  let inConnectExists = inConnect e `member` sink.incoming
  when (outConnectExists /= inConnectExists) $
    error $ "graph inconsistent, containsEdge " ++ show g ++ " " ++ show e
  pure outConnectExists

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
    incomingEs = map (`incomingEdge` nid) (toList n.incoming)
    outgoingEs = map (outgoingEdge nid) (toList n.outgoing)

-- | Update a node in the graph so that it is consistent with the graph.
-- This adds any edges that are missing and removes any edges that were in the
-- graph before but are no longer in the node.
-- If the node is not in the graph, the input graph is returned as is.
updateNode :: ValidNode t a => Graph t a -> Node t a ->  Graph t a
updateNode g n = withEarlyReturn_ do
  let nid = n.nid
  original <- unwrapReturningDefault g $ maybeNodeLookup nid g
  let outgoingAdded = n.outgoing \\ original.outgoing
      outgoingRemoved = original.outgoing \\ n.outgoing
      incomingAdded = n.incoming \\ original.incoming
      incomingRemoved = original.incoming \\ n.incoming
      addedEdges =
        mapSet (outgoingEdge nid) outgoingAdded
        ++ mapSet (`incomingEdge` nid) incomingAdded
      removedEdges =
        mapSet (outgoingEdge nid) outgoingRemoved
        ++ mapSet (`incomingEdge` nid) incomingRemoved
  pure $ deleteEdges (toList removedEdges) $ insertEdges (toList addedEdges) g

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
setData d n g = insertNode (set #augmentation d (nodeConsistentWithGraph g n)) g

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
