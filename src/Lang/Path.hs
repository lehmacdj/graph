{-|
   Describe sets of nodes using path specifications.
   Given a graph and a node, a path denotes a set of nodes relative to the
   starting node.
   Alternatively a path can be used as an action on a graph to add or equalize
   nodes, contingent on a source for new nodes potentially.
 -}
module Lang.Path where

data Path t
  = Zero
  | One
  | Literal t
  | Path t :. Path t -- ^ sequence
  | Path t :+ Path t -- ^ union
  | Path t :& Path t -- ^ intersection
  deriving (Show, Eq, Ord)

-- TODO: implement these
-- follow: find set of nodes reachable in the graph
-- make: create nodes so that every path can be followed
-- consider trying to fold these two operations together into a single one
-- perhaps one that interprets paths as lists of traces with a marking difference
-- between nodes that exist and nodes that don't
