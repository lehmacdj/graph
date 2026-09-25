{-# LANGUAGE CPP #-}

module Models.Graph
  ( module Models.Graph,
  )
where

import Control.Lens
import Data.Monoid (First)
import Data.Semigroup (Last (Last, getLast))
import GHC.Stack
import Models.Edge
import Models.NID
import Models.Node
import MyPrelude

-- * Type definition

-- | A graph is a collection of nodes, each of which has a unique identifier.
-- The type of node identifiers is a parameter; in general we only use the type
-- alias 'Graph' that fixes it to 'NID'.
-- - See 'checkedGraphInvariant' for details on the invariant we maintain about
--   graphs.
-- - Functions ending in @'@ in this module expect a node is consistent with
--   the graph it is being passed to. See 'nodeConsistentWithGraph' for details.
--   Generally it is most likely preferrable to use the unprimed version of the
--   function though the primed version is slightly more efficient as it is
--   able to skip a node lookup in some cases
newtype Graph' n t a = Graph
  { nodeMap :: Map n (Node' n t t a)
  }
  deriving stock (Eq, Ord, Generic)
  deriving anyclass (NFData)

-- | A graph whose nodes are identified by 'NID'. See 'Graph'' for more
-- information.
type Graph t a = Graph' NID t a

-- | Functions that operate on graphs maintain the invariant that any node to
-- which there is a connection is also in the graph.
-- i.e. if you do @insertEdge e g@, then @maybeLookupNode g e.source == Just _@
-- regardless of whether @e.source@ was in the graph before.
checkedGraphInvariant ::
  (ValidNode' n t a) =>
  Graph' n t a ->
  Graph' n t a
#ifdef DEBUG
checkedGraphInvariant g = g
#else
checkedGraphInvariant g = g
#endif
{-# INLINE checkedGraphInvariant #-}

-- | Functions in this module that expect a node as an argument instead of an
-- NID, expect that the node is identical to the node in the graph. They are
-- generally only expected to be used internally, but it is okay to use them
-- externally.
nodeConsistentWithGraph ::
  forall n t a.
  (ValidNode' n t a) =>
  Graph' n t a ->
  Node' n t t a ->
  Node' n t t a
#ifdef DEBUG
nodeConsistentWithGraph g n
  | lookupNodeEx g n.nid == n = n
  | otherwise = error $
    "node " ++ snshow n ++ " is inconsistent with the state of the graph:\n"
    ++ snshow g
#else
nodeConsistentWithGraph _ n = n
#endif
{-# INLINE nodeConsistentWithGraph #-}

-- * Instances

instance
  (ValidNodeNCS' n t a, CompactNIDShow n, Show t) =>
  CompactNodeShow (Graph' n t a)
  where
  type Augmentation (Graph' n t a) = a
  minimumNidLength settings g =
    fromMaybe maxBound . maximumMay $ minimumNidLength settings <$> nodesOf g
  nshowSettings settings (checkedGraphInvariant -> g) =
    intercalate "\n" $ nshowSettings settings <$> nodesOf g

instance
  (Show t, ValidNodeNCS' n t a, CompactNIDShow n, ShowableAugmentation a) =>
  Show (Graph' n t a)
  where
  show = unpack . nshowDefault

type instance Control.Lens.Index (Graph' n t a) = n

type instance Control.Lens.IxValue (Graph' n t a) = (Node' n t t a)

instance
  (ValidNodeNCS' n t a, DefaultAugmentation a) =>
  Ixed (Graph' n t a)

instance
  (ValidNodeNCS' n t a, DefaultAugmentation a) =>
  At (Graph' n t a)
  where
  at nid = lens (maybeNodeLookup nid) setter
    where
      setter g Nothing = delNode nid g
      setter g (Just n) = case maybeNodeLookup nid g of
        Just _ ->
          g
            & mapGraph (Last . (.augmentation))
            & updateNode (fmap Last n)
            & mapGraph (getLast . (.augmentation))
        Nothing ->
          g
            & mapGraph (Last . (.augmentation))
            & insertNode (fmap Last n)
            & mapGraph (getLast . (.augmentation))

instance
  (ValidNodeNCS' n t a, MonoidAugmentation a) =>
  Semigroup (Graph' n t a)
  where
  g1 <> g2 = foldl' (flip mergeNodeInto) g1 (nodesOf g2)

instance
  (ValidNodeNCS' n t a, MonoidAugmentation a) =>
  Monoid (Graph' n t a)
  where
  mempty = emptyGraph
  mappend = (<>)

type instance Element (Graph' n t a) = Node' n t t a

instance MonoFoldable (Graph' n t a) where
  ofoldMap f = ofoldMap f . view #nodeMap
  ofoldr f z = ofoldr f z . view #nodeMap
  ofoldl' f z = ofoldl' f z . view #nodeMap
  ofoldr1Ex f = ofoldr1Ex f . view #nodeMap
  ofoldl1Ex' f = ofoldl1Ex' f . view #nodeMap

instance MonoFunctor (Graph' n t a) where
  omap f = over #nodeMap (omap f)

-- TODO: it's quite janky for Functor (Graph' n t) to map over augmentations but
-- MonoFunctor (Graph' n t a) to map over nodes, maybe we should remove the
-- Functor instance
instance Functor (Graph' n t) where
  fmap f = Graph . fmap (fmap f) . (.nodeMap)

instance MonoTraversable (Graph' n t a) where
  otraverse f = fmap Graph . otraverse f . view #nodeMap

-- * Traversals/Lenses

nodesMatching ::
  (Node' n t t a -> Bool) ->
  Traversal' (Graph' n t a) (Node' n t t a)
nodesMatching f = #nodeMap . traversed . filtered f

nodesMatchedBy ::
  (Indexable i p, Applicative f) =>
  Getting (First i) (Node' n t t a) i ->
  p (Node' n t t a) (f (Node' n t t a)) ->
  Graph' n t a ->
  f (Graph' n t a)
nodesMatchedBy g = #nodeMap . traversed . filteredBy g

-- * Graph scale operations

-- map/filter functions that offer a little more flexibility than relevant type
-- classes

emptyGraph :: forall t a n. (Ord n) => Graph' n t a
emptyGraph = Graph mempty

singletonGraph :: (Ord n) => Node' n t t a -> Graph' n t a
singletonGraph n = Graph $ singletonMap n.nid n

isEmptyGraph :: Graph' n t a -> Bool
isEmptyGraph = null . (.nodeMap)

mapGraph ::
  (ValidNode' n t a) =>
  (Node' n t t a -> b) ->
  Graph' n t a ->
  Graph' n t b
mapGraph f = Graph . fmap (extend f) . (.nodeMap) . checkedGraphInvariant

-- | Semantically removes nodes from the graph which don't meet the predicate.
-- It is guaranteed that the resulting graph doesn't contain the nodes for which
-- the predicate returns false.
subtractiveFilterGraph ::
  (ValidNode' n t a) =>
  (Node' n t t a -> Bool) ->
  Graph' n t a ->
  Graph' n t a
subtractiveFilterGraph f (checkedGraphInvariant -> g) =
  foldl' maybeDelNode g g.nodeMap
  where
    maybeDelNode wg x
      | not $ f x = delNode' x wg
      | otherwise = wg

-- | Like subtractiveFilterGraph but also maps the nodes.
subtractiveFilterMapGraph ::
  (ValidNode' n t a, ValidNode' n t b) =>
  (Node' n t t a -> Maybe b) ->
  Graph' n t a ->
  Graph' n t b
subtractiveFilterMapGraph f =
  map (unwrapEx "all just by filter")
    . subtractiveFilterGraph (isJust . (.augmentation))
    . mapGraph f

-- | Semantically builds a new graph by inserting the nodes that meet the
-- predicate. This removes fewer edges than subtractiveFilterGraph, however
-- the resulting graph may contain nodes that are not connected to the rest of
-- the graph.
additiveFilterGraph ::
  (ValidNode' n t a, MonoidAugmentation a) =>
  (Node' n t t a -> Bool) ->
  Graph' n t a ->
  Graph' n t a
additiveFilterGraph f =
  foldl' maybeInsertNode emptyGraph
  where
    maybeInsertNode wg x
      | f x = insertNode x wg
      | otherwise = wg

dualizeGraph :: Graph' n t a -> Graph' n t a
dualizeGraph = omap dualizeNode

-- | Looking up nodes
maybeNodeLookup :: (Ord n) => n -> Graph' n t a -> Maybe (Node' n t t a)
maybeNodeLookup i = view $ #nodeMap . at i

maybeLookupNode :: (Ord n) => Graph' n t a -> n -> Maybe (Node' n t t a)
maybeLookupNode = flip maybeNodeLookup

-- | Utility function for converting lookups into actual node values with error
-- reporting.
assertNodeInGraph :: (HasCallStack, Show n) => n -> Maybe a -> a
assertNodeInGraph _ (Just n) = n
assertNodeInGraph i Nothing =
  error $ "expected " ++ show i ++ " to be in the graph"

nodeLookupEx :: (ValidNode' n t a) => n -> Graph' n t a -> Node' n t t a
nodeLookupEx i = assertNodeInGraph i . maybeNodeLookup i

lookupNodeEx :: (ValidNode' n t a) => Graph' n t a -> n -> Node' n t t a
lookupNodeEx = flip nodeLookupEx

-- * Utility functions

-- | Utility function for constructing a primed version of a function operating
-- on ids instead of nodes
unprimed ::
  (ValidNode' n t a) =>
  (Node' n t t a -> Graph' n t a -> x) ->
  (n -> Graph' n t a -> x)
unprimed f i ig = f (lookupNodeEx ig i) ig

listify ::
  (HasCallStack) =>
  (x -> Graph' n t a -> Graph' n t a) ->
  ([x] -> Graph' n t a -> Graph' n t a)
listify f nodes ig = foldl' (flip f) ig nodes

unprimeds ::
  (ValidNode' n t a) =>
  ([Node' n t t a] -> Graph' n t a -> x) ->
  ([n] -> Graph' n t a -> x)
unprimeds f i ig = f (nodeLookupEx <$> i <*> pure ig) ig

-- * Operations on edges

containsEdge ::
  (ValidNode' n t a, HasCallStack) =>
  Graph' n t a ->
  Edge' n t ->
  Bool
(checkedGraphInvariant -> g) `containsEdge` e = withEarlyReturn_ do
  source <- unwrapReturningDefault False $ maybeLookupNode g e.source
  sink <- unwrapReturningDefault False $ maybeLookupNode g e.sink
  let outConnectExists = outConnect e `member` source.outgoing
  let inConnectExists = inConnect e `member` sink.incoming
  when (outConnectExists /= inConnectExists) $
    error $
      "graph inconsistent, containsEdge"
        ++ show e.source
        ++ " "
        ++ show e.sink
  pure outConnectExists

insertEdge ::
  (ValidNode' n t a, DefaultAugmentation a) =>
  Edge' n t ->
  Graph' n t a ->
  Graph' n t a
insertEdge e (checkedGraphInvariant -> g) =
  g
    & #nodeMap
      %~ ( (at e.source %~ Just . maybe (outStubNode e) (withEdge e))
             . (at e.sink %~ Just . maybe (inStubNode e) (withEdge e))
         )

insertEdges ::
  (ValidNode' n t a, DefaultAugmentation a) =>
  [Edge' n t] ->
  Graph' n t a ->
  Graph' n t a
insertEdges = listify insertEdge

deleteEdge ::
  (ValidNode' n t a) =>
  Edge' n t ->
  Graph' n t a ->
  Graph' n t a
deleteEdge e (checkedGraphInvariant -> g) =
  g
    & #nodeMap
      %~ adjustMap (over #outgoing (deleteSet (outConnect e))) e.source
        . adjustMap (over #incoming (deleteSet (inConnect e))) e.sink

deleteEdges ::
  (ValidNode' n t a) =>
  [Edge' n t] ->
  Graph' n t a ->
  Graph' n t a
deleteEdges = listify deleteEdge

-- * Operations on nodes

nodesOf :: Graph' n t a -> [Node' n t t a]
nodesOf = (^.. #nodeMap . folded)

-- | Add a node to the graph. The resulting graph is made consistent with the
-- provided node. Edges are added/removed to maintain the invariant.
insertNode ::
  (ValidNode' n t a, MonoidAugmentation a) =>
  Node' n t t a ->
  Graph' n t a ->
  Graph' n t a
insertNode n (checkedGraphInvariant -> g) =
  g
    & #nodeMap . at n.nid %~ ensureNodeExists
    & updateNode n
    & checkedGraphInvariant
  where
    ensureNodeExists = \case
      Nothing -> Just (emptyNode n.nid)
      Just original -> Just original

insertNodes ::
  (ValidNode' n t a, MonoidAugmentation a) =>
  [Node' n t t a] ->
  Graph' n t a ->
  Graph' n t a
insertNodes = listify insertNode

-- | Update a node in the graph so that it is consistent with the graph.
-- This adds any edges that are missing and removes any edges that were in the
-- graph before but are no longer in the node.
-- If the node is not in the graph, the input graph is returned as is.
updateNode ::
  (ValidNode' n t a, MonoidAugmentation a) =>
  Node' n t t a ->
  Graph' n t a ->
  Graph' n t a
updateNode n g = withEarlyReturn_ do
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
  pure $
    g
      & insertEdges (toList addedEdges)
      & deleteEdges (toList removedEdges)
      & #nodeMap . ix n.nid . #augmentation %~ (<> n.augmentation)

-- | Somewhat bespoke operation that merges a node into the graph. This is
-- used by the semigroup operation on Graphs. This is like @insertNode@ but
-- includes all edges in the graph / the provided node instead of deleting edges
-- in the graph that aren't in the newly inserted node.
mergeNodeInto ::
  (ValidNode' n t a, HasCallStack, MonoidAugmentation a) =>
  Node' n t t a ->
  Graph' n t a ->
  Graph' n t a
mergeNodeInto n g =
  g
    & at n.nid %~ \case
      Just original -> Just $ mergeNodesEx original n
      Nothing -> Just n

-- | Remove a node from the graph; updating the cached data in the neighbors
-- nodes as well.
delNode' ::
  (ValidNode' n t a) =>
  Node' n t t a ->
  Graph' n t a ->
  Graph' n t a
delNode' n g =
  withNodeMap g $
    omap deleteIncoming
      . omap deleteOutgoing
      . deleteMap nid
  where
    nid = n ^. #nid
    del = filterSet ((/= nid) . view #node)
    deleteIncoming = over #incoming del
    deleteOutgoing = over #outgoing del

delNode ::
  (ValidNode' n t a) =>
  n ->
  Graph' n t a ->
  Graph' n t a
-- this needs special treatment because unprimeds requires the node to be in the
-- graph
delNode nid g = case g ^. #nodeMap . at nid of
  Just n -> delNode' n g
  Nothing -> g

delNodes' ::
  (ValidNode' n t a) => [Node' n t t a] -> Graph' n t a -> Graph' n t a
delNodes' = listify delNode'

delNodes ::
  (ValidNode' n t a) =>
  [n] ->
  Graph' n t a ->
  Graph' n t a
delNodes = listify delNode

nidSet :: (Ord n) => Graph' n t a -> Set n
nidSet = keysSet . (.nodeMap)

edgeSet ::
  forall n t a.
  (ValidTransition t, ValidNID n) =>
  Graph' n t a ->
  Set (Edge' n t)
edgeSet = foldMap getEdges . (.nodeMap)
  where
    getEdges :: Node' n t t a -> Set (Edge' n t)
    getEdges n =
      mapSet (`incomingEdge` n.nid) n.incoming
        ++ mapSet (outgoingEdge n.nid) n.outgoing

compactGraphRepresentation ::
  (ValidTransition t, ValidNID n, CompactNIDShow n) =>
  Graph' n t a ->
  Text
compactGraphRepresentation g =
  ("[" ++) . (++ "]") . intercalate "," $
    map tshow nids ++ map tshow (toList es)
  where
    es = edgeSet g
    edgeNids = mapSet (.sink) es ++ mapSet (.source) es
    nids = toList $ nidSet g \\ edgeNids

-- * Operations on augmentations (formerly referred to as data)

-- | sets the data, setting to nothing is equivalent to deleting the data
-- this is a terrible function that should probably not be used
setData' ::
  (ValidNode' n t a, Eq a) =>
  a ->
  Node' n t t a ->
  Graph' n t a ->
  Graph' n t a
setData' d n g = g & #nodeMap %~ (at n.nid ?~ n {augmentation = d})

setData ::
  (ValidNode' n t a) =>
  a ->
  n ->
  Graph' n t a ->
  Graph' n t a
setData d = unprimed (setData' d)

-- * Legacy utils

-- Generally these are functions that I'd prefer to avoid using but are used
-- in sufficiently many places that it's not worth it to refactor them out.

withNodeMap ::
  Graph' n t a ->
  (Map n (Node' n t t a) -> Map n (Node' n t t a)) ->
  Graph' n t a
withNodeMap = flip (over #nodeMap)
