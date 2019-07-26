{-# LANGUAGE FlexibleContexts #-}
{-|
   Advanced graph functionality that depends on more modules.
 -}
module Graph.Advanced where

import Control.Monad
import Data.Foldable
import Data.Set.Lens (setmapped)
import Data.Set (Set)
import Control.Lens

import Graph
import Graph.Connect
import Control.Monad.Unique
import qualified Data.Set as Set

import Lang.Path

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
  [] -> do
    nnid <- fresh
    let newNode = Node nnid (Set.singleton (Connect e (nidOf n))) Set.empty Nothing
        g' = insertNode newNode g
    pure (newNode, g')

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

-- TODO: make this interpret links currently in the graph nondeterministcally
-- if there are multiple
mkPath
  :: (TransitionValid t, MonadUnique Id m)
  => Path t -> Node t -> Graph t -> m (Graph t)
mkPath p n g = foldM go g . toList . listifyNewPath $ p where
  go g' dpath = mkNewPath dpath n g'

mergeNodes :: Node t -> Node t -> Graph t -> Graph t
mergeNodes = undefined

mgPath
  :: (TransitionValid t, MonadUnique Id m)
  => Path t -> Node t -> Graph t -> m (Graph t)
mgPath = undefined

selfLoopify :: Id -> Id -> Set (Connect String) -> Set (Connect String)
selfLoopify nid nid' = (setmapped . connectNode . filtered (==nid)) .~ nid'

cloneNode
  :: (TransitionValid t, MonadUnique Id m)
  => Node t -> Graph t -> m (Node t, Graph t)
cloneNode = undefined
