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
-- - See 'checkedGraphInvariant' for details on the invariant we maintain about
--   graphs.
-- - Functions ending in @'@ in this module expect a node is consistent with
--   the graph it is being passed to. See 'nodeConsistentWithGraph' for details.
--   Generally it is most likely preferrable to use the unprimed version of the
--   function though the primed version is slightly more efficient as it is
--   able to skip a node lookup in some cases
newtype Graph t a = Graph
  { nodeMap :: Map NID (Node t a)
  }
  deriving stock (Eq, Ord, Generic)
  deriving anyclass (NFData)

-- | Functions that operate on graphs maintain the invariant that any node to
-- which there is a connection is also in the graph.
-- i.e. if you do @insertEdge e g@, then @maybeLookupNode g e.source == Just _@
-- regardless of whether @e.source@ was in the graph before.
checkedGraphInvariant ::
  (ValidNode t a) =>
  Graph t a ->
  Graph t a
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
  forall t a.
  (ValidNode t a) =>
  Graph t a ->
  Node t a ->
  Node t a
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
  (ValidNodeNCS t a, Show t) =>
  CompactNodeShow (Graph t a)
  where
  type Augmentation (Graph t a) = a
  minimumNidLength settings g =
    fromMaybe maxBound . maximumMay $ minimumNidLength settings <$> nodesOf g
  nshowSettings settings (checkedGraphInvariant -> g) =
    intercalate "\n" $ nshowSettings settings <$> nodesOf g

instance
  (Show t, ValidNodeNCS t a, ShowableAugmentation a) =>
  Show (Graph t a)
  where
  show = unpack . nshowDefault

type instance Control.Lens.Index (Graph t a) = NID

type instance Control.Lens.IxValue (Graph t a) = (Node t a)

instance (ValidNodeNCS t a, DefaultAugmentation a) => Ixed (Graph t a)

instance (ValidNodeNCS t a, DefaultAugmentation a) => At (Graph t a) where
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
  (ValidNodeNCS t a, MonoidAugmentation a) =>
  Semigroup (Graph t a)
  where
  g1 <> g2 = foldl' (flip mergeNodeInto) g1 (nodesOf g2)

instance
  (ValidNodeNCS t a, MonoidAugmentation a) =>
  Monoid (Graph t a)
  where
  mempty = emptyGraph
  mappend = (<>)

type instance Element (Graph t a) = Node t a

instance MonoFoldable (Graph t a) where
  ofoldMap f = ofoldMap f . view #nodeMap
  ofoldr f z = ofoldr f z . view #nodeMap
  ofoldl' f z = ofoldl' f z . view #nodeMap
  ofoldr1Ex f = ofoldr1Ex f . view #nodeMap
  ofoldl1Ex' f = ofoldl1Ex' f . view #nodeMap

instance MonoFunctor (Graph t a) where
  omap f = over #nodeMap (omap f)

-- TODO: it's quite janky for Functor (Graph t) to map over augmentations but
-- MonoFunctor (Graph t a) to map over nodes, maybe we should remove the
-- Functor instance
instance Functor (Graph t) where
  fmap f = Graph . fmap (fmap f) . (.nodeMap)

instance MonoTraversable (Graph t a) where
  otraverse f = fmap Graph . otraverse f . view #nodeMap

-- * Traversals/Lenses

nodesMatching ::
  (Node t a -> Bool) ->
  Traversal' (Graph t a) (Node t a)
nodesMatching f = #nodeMap . traversed . filtered f

nodesMatchedBy ::
  (Indexable i p, Applicative f) =>
  Getting (First i) (Node t a) i ->
  p (Node t a) (f (Node t a)) ->
  Graph t a ->
  f (Graph t a)
nodesMatchedBy g = #nodeMap . traversed . filteredBy g

-- * Graph scale operations

-- map/filter functions that offer a little more flexibility than relevant type
-- classes

emptyGraph :: Graph t a
emptyGraph = Graph mempty

singletonGraph :: Node t a -> Graph t a
singletonGraph n = Graph $ singletonMap n.nid n

isEmptyGraph :: Graph t a -> Bool
isEmptyGraph = null . (.nodeMap)

mapGraph ::
  (ValidNode t a) =>
  (Node t a -> b) ->
  Graph t a ->
  Graph t b
mapGraph f = Graph . fmap (extend f) . (.nodeMap) . checkedGraphInvariant

-- | Semantically removes nodes from the graph which don't meet the predicate.
-- It is guaranteed that the resulting graph doesn't contain the nodes for which
-- the predicate returns false.
subtractiveFilterGraph ::
  (ValidNode t a) =>
  (Node t a -> Bool) ->
  Graph t a ->
  Graph t a
subtractiveFilterGraph f (checkedGraphInvariant -> g) =
  foldl' maybeDelNode g g.nodeMap
  where
    maybeDelNode wg x
      | not $ f x = delNode' x wg
      | otherwise = wg

-- | Like subtractiveFilterGraph but also maps the nodes.
subtractiveFilterMapGraph ::
  (ValidNode t a, ValidNode t b) =>
  (Node t a -> Maybe b) ->
  Graph t a ->
  Graph t b
subtractiveFilterMapGraph f =
  map (unwrapEx "all just by filter")
    . subtractiveFilterGraph (isJust . (.augmentation))
    . mapGraph f

-- | Semantically builds a new graph by inserting the nodes that meet the
-- predicate. This removes fewer edges than subtractiveFilterGraph, however
-- the resulting graph may contain nodes that are not connected to the rest of
-- the graph.
additiveFilterGraph ::
  (ValidNode t a, MonoidAugmentation a) =>
  (Node t a -> Bool) ->
  Graph t a ->
  Graph t a
additiveFilterGraph f =
  foldl' maybeInsertNode emptyGraph
  where
    maybeInsertNode wg x
      | f x = insertNode x wg
      | otherwise = wg

dualizeGraph :: Graph t a -> Graph t a
dualizeGraph = omap dualizeNode

-- | Looking up nodes
maybeNodeLookup :: NID -> Graph t a -> Maybe (Node t a)
maybeNodeLookup i = view $ #nodeMap . at i

maybeLookupNode :: Graph t a -> NID -> Maybe (Node t a)
maybeLookupNode = flip maybeNodeLookup

-- | Utility function for converting lookups into actual node values with error
-- reporting.
assertNodeInGraph :: (HasCallStack) => NID -> Maybe a -> a
assertNodeInGraph _ (Just n) = n
assertNodeInGraph i Nothing =
  error $ "expected " ++ show i ++ " to be in the graph"

nodeLookupEx :: (ValidNode t a) => NID -> Graph t a -> Node t a
nodeLookupEx i = assertNodeInGraph i . maybeNodeLookup i

lookupNodeEx :: (ValidNode t a) => Graph t a -> NID -> Node t a
lookupNodeEx = flip nodeLookupEx

-- * Utility functions

-- | Utility function for constructing a primed version of a function operating
-- on ids instead of nodes
unprimed ::
  (ValidNode t a) =>
  (Node t a -> Graph t a -> x) ->
  (NID -> Graph t a -> x)
unprimed f i ig = f (lookupNodeEx ig i) ig

listify ::
  (HasCallStack) =>
  (x -> Graph t a -> Graph t a) ->
  ([x] -> Graph t a -> Graph t a)
listify f nodes ig = foldl' (flip f) ig nodes

unprimeds ::
  (ValidNode t a) =>
  ([Node t a] -> Graph t a -> x) ->
  ([NID] -> Graph t a -> x)
unprimeds f i ig = f (nodeLookupEx <$> i <*> pure ig) ig

-- * Operations on edges

containsEdge ::
  (ValidNode t a, HasCallStack) =>
  Graph t a ->
  Edge t ->
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
  (ValidNode t a, DefaultAugmentation a) =>
  Edge t ->
  Graph t a ->
  Graph t a
insertEdge e (checkedGraphInvariant -> g) =
  g
    & #nodeMap
      %~ ( (at e.source %~ Just . maybe (outStubNode e) (withEdge e))
             . (at e.sink %~ Just . maybe (inStubNode e) (withEdge e))
         )

insertEdges ::
  (ValidNode t a, DefaultAugmentation a) =>
  [Edge t] ->
  Graph t a ->
  Graph t a
insertEdges = listify insertEdge

deleteEdge :: (ValidNode t a) => Edge t -> Graph t a -> Graph t a
deleteEdge e (checkedGraphInvariant -> g) =
  g
    & #nodeMap
      %~ adjustMap (over #outgoing (deleteSet (outConnect e))) e.source
        . adjustMap (over #incoming (deleteSet (inConnect e))) e.sink

deleteEdges ::
  (ValidNode t a) =>
  [Edge t] ->
  Graph t a ->
  Graph t a
deleteEdges = listify deleteEdge

-- * Operations on nodes

nodesOf :: Graph t a -> [Node t a]
nodesOf = (^.. #nodeMap . folded)

-- | Add a node to the graph. The resulting graph is made consistent with the
-- provided node. Edges are added/removed to maintain the invariant.
insertNode ::
  (ValidNode t a, MonoidAugmentation a) =>
  Node t a ->
  Graph t a ->
  Graph t a
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
  (ValidNode t a, MonoidAugmentation a) =>
  [Node t a] ->
  Graph t a ->
  Graph t a
insertNodes = listify insertNode

-- | Update a node in the graph so that it is consistent with the graph.
-- This adds any edges that are missing and removes any edges that were in the
-- graph before but are no longer in the node.
-- If the node is not in the graph, the input graph is returned as is.
updateNode ::
  (ValidNode t a, MonoidAugmentation a) =>
  Node t a ->
  Graph t a ->
  Graph t a
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
  (ValidNode t a, HasCallStack, MonoidAugmentation a) =>
  Node t a ->
  Graph t a ->
  Graph t a
mergeNodeInto n g =
  g
    & at n.nid %~ \case
      Just original -> Just $ mergeNodesEx original n
      Nothing -> Just n

-- | Remove a node from the graph; updating the cached data in the neighbors
-- nodes as well.
delNode' :: (ValidNode t a) => Node t a -> Graph t a -> Graph t a
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
  (ValidNode t a) =>
  NID ->
  Graph t a ->
  Graph t a
-- this needs special treatment because unprimeds requires the node to be in the
-- graph
delNode nid g = case g ^. #nodeMap . at nid of
  Just n -> delNode' n g
  Nothing -> g

delNodes' ::
  (ValidNode t a) => [Node t a] -> Graph t a -> Graph t a
delNodes' = listify delNode'

delNodes ::
  (ValidNode t a) =>
  [NID] ->
  Graph t a ->
  Graph t a
delNodes = listify delNode

-- * Operations on augmentations (formerly referred to as data)

-- | sets the data, setting to nothing is equivalent to deleting the data
-- this is a terrible function that should probably not be used
setData' ::
  (ValidNode t a, Eq a) =>
  a ->
  Node t a ->
  Graph t a ->
  Graph t a
setData' d n g = g & #nodeMap %~ (at n.nid ?~ n {augmentation = d})

setData ::
  (ValidNode t a) =>
  a ->
  NID ->
  Graph t a ->
  Graph t a
setData d = unprimed (setData' d)

-- * Legacy utils

-- Generally these are functions that I'd prefer to avoid using but are used
-- in sufficiently many places that it's not worth it to refactor them out.

withNodeMap :: Graph t a -> (Map NID (Node t a) -> Map NID (Node t a)) -> Graph t a
withNodeMap = flip (over #nodeMap)
