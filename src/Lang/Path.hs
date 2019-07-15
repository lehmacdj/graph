{-|
   Describe sets of nodes using path specifications.
   Given a graph and a node, a path denotes a set of nodes relative to the
   starting node.
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
