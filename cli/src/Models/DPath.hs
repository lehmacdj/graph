module Models.DPath
  ( DPathComponent (..),
    DPath (..),
    projPath,
    splitLast,
    endPoint,
  )
where

import Models.NID
import MyPrelude

-- breadcrumb in a trail in the graph
-- each piece denotes an edge from the specified via the transition
data DPathComponent t = FromVia NID t
  deriving (Show, Eq, Ord)

-- | a deterministic path is a list of path components
-- along with a start node, from which those path components are constructed
data DPath t
  = DPath
      NID
      [DPathComponent t] -- the nids/transitions that are in the graph from start
      NID -- the nid of the last node in the graph
      [t] -- transitions that could not be realized within the graph
  deriving (Show, Eq, Ord)

projPath :: (Show t) => [DPathComponent t] -> String
projPath [] = "#"
projPath [FromVia _ x] = show x
projPath (FromVia _ x : xs) = show x ++ "/" ++ projPath xs

-- | Take a path and split off the last edge transition;
-- if the path has components that could not be realized in the graph;
-- then this returns nothing.
splitLast :: DPath t -> Maybe (DPath t, t, NID)
splitLast = \case
  DPath start (unsnoc -> Just (xs, pnid `FromVia` t)) nid [] ->
    Just (DPath start xs pnid [], t, nid)
  _ -> Nothing

endPoint :: DPath t -> NID
endPoint (DPath _ _ x _) = x
