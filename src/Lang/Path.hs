{-# LANGUAGE LambdaCase #-}

{-|
   Describe sets of nodes using path specifications.
   Given a graph and a node, a path denotes a set of nodes relative to the
   starting node.
   Alternatively a path can be used as an action on a graph to add or equalize
   nodes, contingent on a source for new nodes potentially.
 -}
module Lang.Path where

import Data.Set (Set)
import qualified Data.Set as Set
import Data.Foldable (toList)

import Graph
import Graph.Connect

-- | a deterministic path is a list of path components
type DPath t = [t]

data Path t
  = One
--  | Zero (it might be useful to have a additive identity eventually)
--          at least for algebraic reasons
--  | Dual -- ^ a transition that dualizes the view of the graph
--  | Wild -- ^ a transition matched by anything (top in the algebra)
--  | Path t :\ Path t -- ^ set minus (useful with wild to restrict)
--  | Negate (Path t) -- ^ negate a path, if included obsolesces other operators
--  | Star (Path t) -- ^ kleene iteration: technically top in algebra is top^*
  | Literal t
  | Path t :/ Path t -- ^ sequence
  | Path t :+ Path t -- ^ union
  | Path t :& Path t -- ^ intersection
  deriving (Show, Eq, Ord)

-- | Turn a path into a set of DPaths where the set denotes disjunction.
-- Similar to converting to a DNF for paths.
determinizePath
  :: TransitionValid t
  => Path t -> Set (DPath t)
determinizePath = \case
  One -> Set.singleton []
  Literal t -> Set.singleton [t]
  p1 :/ p2 -> Set.fromList $ do
    p1' <- toList $ determinizePath p1
    p2' <- toList $ determinizePath p2
    pure $ p1' ++ p2'
  p1 :+ p2 -> determinizePath p1 `Set.union` determinizePath p2
  p1 :& p2 -> determinizePath p1 `Set.intersection` determinizePath p2

-- | the semantics of a path at a specific node in the graph is
-- the set of deterministic path segments up to the last node still in the graph
-- for all possible deterministic paths, the node that reaches, and then the
-- unfinished DPath beyond that
--
-- For example in a graph consisting of only a single node 0, the path
-- a/b + c resolves to [(#, 0, a/b), (#, 0, c)]
--
-- In a graph with 0 -a> 1 the same path a/b + c resolves at 0 to
-- [(a, 1, b), (#, 0, c)]
resolvePath
  :: TransitionValid t
  => Path t -> Node t -> Graph t -> Set (DPath t, Node t, DPath t)
resolvePath p n g = nodeConsistentWithGraph g n `seq` case p of
  One -> Set.singleton ([], n, [])
  Literal t -> case matchConnect t (outgoingConnectsOf n) >>= maybeLookupNode g of
    Just m -> Set.singleton ([t], m, [])
    Nothing -> Set.singleton ([], n, [t])
  p1 :/ p2 -> Set.fromList $ do
    (pre, n', post) <- toList $ resolvePath p1 n g
    if null post
       then do
         (pre', n'', post') <- toList $ resolvePath p2 n' g
         pure (pre ++ pre', n'', post')
       else do
         post' <- toList $ determinizePath p2
         pure (pre, n', post ++ post')
  p1 :+ p2 -> resolvePath p1 n g `Set.union` resolvePath p2 n g
  p1 :& p2 -> resolvePath p1 n g `Set.intersection` resolvePath p2 n g
