{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-|
   Advanced graph functionality that depends on more modules.
 -}
module Graph.Advanced where

import Control.Monad
import Data.Foldable
import Control.Lens

import Graph
import Graph.Connect
import Control.Monad.Unique
import qualified Data.Set as Set

import Lang.Path hiding (mkPath)

-- | Create an edge from the specified node to a new node if the edge does not
-- exist, or return the edge that does exist otherwise.
-- The argument node is expected to have up to date information about the graph.
-- Could potentially use a stateful version at some point.
-- The monad unique should generate ids that aren't in the graph.
followMkEdgeFrom
  :: (TransitionValid t, MonadUnique Id m)
  => t -> Node t -> Graph t -> m (Node t, Graph t)
followMkEdgeFrom e n g = case matchConnect e (outgoingConnectsOf n) of
  nid:_ -> pure (nodeLookup nid g, g)
  [] -> mkEdgeFrom e n g

mkEdgeFrom
  :: (TransitionValid t, MonadUnique Id m)
  => t -> Node t -> Graph t -> m (Node t, Graph t)
mkEdgeFrom e n g = do
  nnid <- fresh
  let newNode = Node nnid (Set.singleton (Connect e (nidOf n))) Set.empty Nothing
      g' = insertNode newNode g
  pure (newNode, g')

mkEdgeFrom'
  :: (TransitionValid t, MonadUnique Id m)
  => t -> Id -> Graph t -> m (Node t, Graph t)
mkEdgeFrom' = primed . mkEdgeFrom

followMkEdgeFrom'
  :: (TransitionValid t, MonadUnique Id m)
  => t -> Id -> Graph t -> m (Node t, Graph t)
followMkEdgeFrom' = primed . followMkEdgeFrom

mkNewPath
  :: (TransitionValid t, MonadUnique Id m)
  => [t] -> Node t -> Graph t -> m (Graph t)
mkNewPath [] _ g = pure g
mkNewPath (x:xs) n g =  do
  (n', g') <- followMkEdgeFrom x n g
  mkNewPath xs n' g'

mkPath
  :: (TransitionValid t, MonadUnique Id m)
  => Path t -> Node t -> Graph t -> m (Graph t)
mkPath p n g = foldM go g xs where
  xs = map (\(DPath _ nid ts) -> (nid, ts)) . toList $ resolvePath p n g
  go g' (n', ts) = primed (mkNewPath ts) n' g'

mkPath'
  :: (TransitionValid t, MonadUnique Id m)
  => Path t -> Id -> Graph t -> m (Graph t)
mkPath' = primed . mkPath

-- | Merge all the connects from the first node into connects of the
-- second node. The node chosen to be the final node is arbitrary.
-- Data in the nodes is completely ignored.
-- TODO: implement node data more as a link so that it can be saved when
-- merging two nodes with data
mergeNodes
  :: forall t. TransitionValid t
  => Node t -> Node t -> Graph t -> (Node t, Graph t)
mergeNodes n1_ n2_ g = (nNew, insertNode nNew $ delNodes [n1, n2] g)
  where
    n1 = nodeConsistentWithGraph g n1_
    n2 = nodeConsistentWithGraph g n2_
    nNew = cin . cout $ n1
    fixSelfLoops = selfLoopify (nidOf n2) (nidOf n1)
    cin :: Node t -> Node t
    cin = nodeIncoming %~ fixSelfLoops . (`Set.union` incomingConnectsOf n2)
    cout :: Node t -> Node t
    cout = nodeOutgoing %~ fixSelfLoops . (`Set.union` outgoingConnectsOf n2)

mgPath
  :: forall t. TransitionValid t
  => Path t -> Node t -> Graph t -> Graph t
mgPath p n g = go g $ lookupNode g <$> resolveSuccesses p n g where
  go g' [] = g'
  go g' [_] = g'
  go g' (x:x':xs) = case mergeNodes x x' g' of
    (xNew, g'') -> go g'' (xNew:xs)

-- | Caution: all node ids are expected to be valid for this function to
-- be well defined.
mergeNodeIds :: TransitionValid t => Graph t -> [Id] -> Graph t
mergeNodeIds g = snd . mergeNodeIds' g

-- | mergeNodeIds but also return the nid that the new node has if it exists
mergeNodeIds' :: TransitionValid t => Graph t -> [Id] -> (Maybe Id, Graph t)
mergeNodeIds' g = go g . map (lookupNode g) where
  go g' [] = (Nothing, g')
  go g' [x] = (Just (nidOf x), g')
  go g' (x:x':xs) = case mergeNodes x x' g' of
    (xNew, g'') -> go g'' (xNew:xs)

-- | Creates an exact copy of a node returning it
cloneNode
  :: (TransitionValid t, MonadUnique Id m)
  => Node t -> Graph t -> m (Node t, Graph t)
cloneNode n g = do
  nnid <- fresh
  let n' = Node nnid (incomingConnectsOf n) (outgoingConnectsOf n) (dataOf n)
  pure (n', insertNode n' g)

-- | Deletes the last edge along each successful path.
deletePath
  :: TransitionValid t
  => Path t -> Node t -> Graph t -> Graph t
deletePath p n g = foldl' delDPath g . toList $ resolvePath p n g where
  delDPath g' (DPath xs@(_:_) nid' []) = case last xs of -- safe because list nonempty
    FromVia nid t -> delEdge (Edge nid t nid') g'
  delDPath g' _ = g'
