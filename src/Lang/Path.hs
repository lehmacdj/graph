{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NoImplicitPrelude #-}

{-|
   Describe sets of nodes using path specifications.
   Given a graph and a node, a path denotes a set of nodes relative to the
   starting node.
   Alternatively a path can be used as an action on a graph to add or equalize
   nodes, contingent on a source for new nodes potentially.
 -}
module Lang.Path where

import MyPrelude

import Data.Set (Set)
import qualified Data.Set as Set
import Data.Maybe (mapMaybe)
import Data.List (intersectBy)
import Data.Function (on)
import Data.Functor ((<&>))
import Control.Lens hiding (pre)

import Graph
import Graph.Connect

import Control.Monad.Freer
import Control.Monad.Freer.Fresh
import Effect.Graph
import Effect.Throw
import Effect.Graph.Advanced

-- breadcrumb in a trail in the graph
-- each piece denotes an edge from the specified via the transition
data DPathComponent t = FromVia Id t
  deriving (Show, Eq, Ord)

-- | a deterministic path is a list of path components
-- along with a start node, from which those path components are constructed
--
data DPath t = DPath
  [DPathComponent t] -- the nids/transitions that are in the graph from start
  Id -- the nid of the last node in the graph
  [t] -- transitions that could not be realized within the graph
  deriving (Show, Eq, Ord)

projPath :: Show t => [DPathComponent t] -> String
projPath [] = "#"
projPath [FromVia _ x] = show x
projPath (FromVia _ x:xs) = show x ++ "/" ++ projPath xs

endPoint :: DPath t -> Id
endPoint (DPath _ x _) = x

data Path t
  = One
--  | Zero (it might be useful to have a additive identity eventually)
--          at least for algebraic reasons
--  | Dual -- ^ a transition that dualizes the view of the graph
  | Wild -- ^ a transition matched by anything (top in the algebra)
--  | Path t :\ Path t -- ^ set minus (useful with wild to restrict)
--  | Negate (Path t) -- ^ negate a path, if included obsolesces other operators
--  | Star (Path t) -- ^ kleene iteration: technically top in algebra is top^*
  | Literal t
  | Path t :/ Path t -- ^ sequence
  | Path t :+ Path t -- ^ union
  | Path t :& Path t -- ^ intersection
  deriving (Show, Eq, Ord)

resolvePathSuccesses
  :: forall t effs. (Members [ReadGraph t, ThrowMissing] effs, TransitionValid t)
  => Id -> Path t -> Eff effs (Set Id)
resolvePathSuccesses nid = \case
  One -> pure $ singleton nid
  Wild -> do
    n <- getNode' nid
    pure $ toSetOf (folded . connectNode) (outgoingConnectsOf @t n)
  Literal x -> do
    n <- getNode' nid
    pure . setFromList $ matchConnect x (outgoingConnectsOf @t n)
  p :/ q -> do
    pResolved <- toList <$> resolvePathSuccesses nid p
    mconcat <$> (traverse (`resolvePathSuccesses` q) pResolved)
  p :+ q -> union <$> resolvePathSuccesses nid p <*> resolvePathSuccesses nid q
  p :& q -> intersect <$> resolvePathSuccesses nid p <*> resolvePathSuccesses nid q

resolvePathSuccessesDetail
  :: forall t effs. (Members [ReadGraph t, ThrowMissing] effs, TransitionValid t)
  => Id -> Path t -> Eff effs (Set (DPath t))
resolvePathSuccessesDetail nid = \case
  One -> pure $ Set.singleton (DPath [] nid [])
  Wild -> getNode' nid <&> \n -> Set.fromList $ do
    Connect t nid' <- toList $ outgoingConnectsOf n
    pure $ DPath [nidOf n `FromVia` t] nid' []
  Literal t -> getNode' nid <&> \n ->
    case matchConnect t (outgoingConnectsOf n) of
      [] -> Set.singleton (DPath [] (nidOf n) [t])
      ms -> Set.fromList (DPath [nidOf n `FromVia` t] <$> ms <*> pure [])
  p1 :/ p2 -> do
    r1 <- toList <$> resolvePathSuccessesDetail nid p1
    fmap setFromList . (`concatMapM` r1) $ \(DPath pre nid' post) ->
      if null post
         then do
           r2 <- toList <$> resolvePathSuccessesDetail nid' p2
           forM r2 $ \(DPath pre' nid'' post') -> pure $ DPath (pre ++ pre') nid'' post'
         else do
           let r2 = toList $ listifyNewPath p2
           forM r2 $ \post' -> pure $ DPath pre nid' (post ++ post')
  p1 :+ p2 -> union <$> p1r <*> p2r where
    p1r = resolvePathSuccessesDetail nid p1
    p2r = resolvePathSuccessesDetail nid p2
  p1 :& p2 -> setFromList <$> (intersect' <$> p1r <*> p2r) where
    intersect' = intersectBy ((==) `on` endPoint)
    p1r = toList <$> resolvePathSuccessesDetail nid p1
    p2r = toList <$> resolvePathSuccessesDetail nid p2

-- | Create a path such that the end points of the path are all new nodes
-- intersection and union are both interpreted as a splitting point
-- where both paths should be created. This is perhaps a bad implementation...
mkPath
  :: forall t effs.
  ( Members [Fresh, ThrowMissing] effs
  , HasGraph t effs
  )
  => Id -> Path t -> Eff effs (Set Id)
mkPath nid p = fmap setFromList . forM (toList (listifyNewPath p)) $
  \x -> transitionsViaManyFresh nid x

-- | Construct a graph that contains only the edges passed through
-- by the literal edges in the path.
-- nids are the same as in the original graph
tracePath
  :: forall t effs. Members [Fresh, ThrowMissing, ReadGraph t] effs
  => Id -> Path t -> Eff effs (Graph t)
tracePath _ _ = error "unimplemented"

delPath
  :: forall t effs.
  ( Members [Fresh, ThrowMissing] effs
  , HasGraph t effs
  )
  => Id -> Path t -> Eff effs ()
delPath nid p = resolvePathSuccessesDetail nid p >>= mapM_ delDPath where
  delDPath (DPath xs@(_:_) nid' []) = case lastEx xs of -- safe because list nonempty
    FromVia nid2 t -> deleteEdge (Edge nid2 t nid')
  delDPath _ = pure ()

-- | Turn a path into a set of DPaths where the set denotes disjunction.
-- Similar to converting to a DNF for paths.
-- Transitions are asumed to behave deterministically in this transition.
listifyNewPath
  :: TransitionValid t
  => Path t -> Set [t]
listifyNewPath = \case
  One -> Set.singleton []
  Wild -> Set.empty -- we have to ignore paths that contain wild because we
                    -- don't have the graph and can't add all possible
                    -- transitions
  Literal t -> Set.singleton [t]
  p1 :/ p2 -> Set.fromList $ do
    p1' <- toList $ listifyNewPath p1
    p2' <- toList $ listifyNewPath p2
    pure $ p1' ++ p2'
  p1 :+ p2 -> listifyNewPath p1 `Set.union` listifyNewPath p2
  p1 :& p2 -> listifyNewPath p1 `Set.intersection` listifyNewPath p2

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
  => Path t -> Node t -> Graph t -> Set (DPath t)
resolvePath p n g = nodeConsistentWithGraph g n `seq` case p of
  One -> Set.singleton (DPath [] (nidOf n) [])
  Wild -> Set.fromList $ do
    Connect t nid <- toList $ outgoingConnectsOf n
    pure $ DPath [nidOf n `FromVia` t] nid []
  Literal t -> case matchConnect t (outgoingConnectsOf n) of
    [] -> Set.singleton (DPath [] (nidOf n) [t])
    ms -> Set.fromList (DPath [nidOf n `FromVia` t] <$> ms <*> pure [])
  p1 :/ p2 -> Set.fromList $ do
    DPath pre n' post <- toList $ resolvePath p1 n g
    if null post
       then do
         DPath pre' n'' post' <- toList $ primed (resolvePath p2) n' g
         pure $ DPath (pre ++ pre') n'' post'
       else do
         post' <- toList $ listifyNewPath p2
         pure $ DPath pre n' (post ++ post')
  p1 :+ p2 -> resolvePath p1 n g `Set.union` resolvePath p2 n g
  p1 :& p2 -> Set.fromList $ intersectBy ((==) `on` endPoint) p1r p2r where
    p1r = toList $ resolvePath p1 n g
    p2r = toList $ resolvePath p2 n g

-- | Like resolvePath, but fails if there is more than one result and if
-- there is unresolved path remaining after it.
resolveSingle
  :: TransitionValid t
  => Path t -> Node t -> Graph t -> Maybe Id
resolveSingle p n g = case toList $ resolvePath p n g of
  [DPath _ nid []] -> Just nid
  _ -> Nothing

-- | Get all paths that terminate in a node. i.e. there are no transitions that
-- move outside the bounds of the graph.
-- This returns just the ids of the nodes where these paths terminate.
resolveSuccesses
  :: TransitionValid t
  => Path t -> Node t -> Graph t -> [Id]
resolveSuccesses p n g = mapMaybe projSuccess $ toList $ resolvePath p n g where
  projSuccess (DPath _ i []) = Just i
  projSuccess _ = Nothing

-- | Get all paths that terminate in a node. i.e. there are no transitions that
-- move outside the bounds of the graph.
-- This returns just the ids of the nodes where these paths terminate.
-- Also returns a string which identifies the path that was taken to each id
resolveSuccesses'
  :: TransitionValid t
  => Path t -> Node t -> Graph t -> [([DPathComponent t], Id)]
resolveSuccesses' p n g = mapMaybe projSuccess $ toList $ resolvePath p n g where
  projSuccess (DPath xs i []) = Just (xs, i)
  projSuccess _ = Nothing
