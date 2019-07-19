{-# LANGUAGE FlexibleContexts #-}
{-|
   Advanced graph functionality that depends on more modules.
 -}
module Graph.Advanced where

import Graph
import Graph.Connect
import Control.Monad.Unique
import qualified Data.Set as Set

-- | Create an edge from the specified node to a new node if the edge does not
-- exist, or return the edge that does exist otherwise.
-- The argument node is expected to have up to date information about the graph.
-- Could potentially use a stateful version at some point.
-- The monad unique should generate ids that aren't in the graph.
followMkEdgeFrom
  :: (TransitionValid t, MonadUnique Id m)
  => t -> Node t -> Graph t -> m (Node t, Graph t)
followMkEdgeFrom e n g = case matchConnect e (outgoingConnectsOf n) of
  Just nid -> pure (nodeLookup nid g, g)
  Nothing -> do
    nnid <- fresh
    let newNode = Node nnid (Set.singleton (Connect e (nidOf n))) Set.empty Nothing
        g' = insertNode newNode g
    pure (newNode, g')

followMkEdgeFrom'
  :: (TransitionValid t, MonadUnique Id m)
  => t -> Id -> Graph t -> m (Node t, Graph t)
followMkEdgeFrom' = primed . followMkEdgeFrom
