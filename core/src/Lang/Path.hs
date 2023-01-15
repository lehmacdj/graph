{-# LANGUAGE StrictData #-}
{-# LANGUAGE ViewPatterns #-}
{-# OPTIONS_GHC -Wno-name-shadowing #-}

-- |
--   Describe sets of nodes using path specifications.
--   Given a graph and a node, a path denotes a set of nodes relative to the
--   starting node.
--   Alternatively a path can be used as an action on a graph to add or equalize
--   nodes, contingent on a source for new nodes potentially.
module Lang.Path where

import Control.Lens hiding (pre, unsnoc)
import Data.List (intersectBy)
import qualified Data.Set as Set
import Effect.FreshNID
import Effect.Graph
import Effect.Graph.Advanced
import Graph hiding (insertEdge)
import Graph.Connect
import MyPrelude
import Polysemy.State (evalState)
import UserError

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

projPath :: Show t => [DPathComponent t] -> String
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

data Path t
  = One
  | Zero
  | --  | Dual -- ^ a transition that dualizes the view of the graph
    -- The correct way to implement Dual is simply make backlinks part of the
    -- graph structure, as opposed to intrinsic. i.e. for each normal node have
    -- a slightly special node that stores backlinks for that node.

    --  | Path t :\ Path t -- ^ set minus (useful with wild to restrict)
    --  | Negate (Path t) -- ^ negate a path, if included obsolesces other operators
    --  | Star (Path t) -- ^ kleene iteration: technically top in algebra is top^*

    -- | a transition matched by anything (top in the algebra)
    Wild
  | Literal t
  | -- | this must not be before @:/@ in a @:+@. @:&@ acts as a scope that
    -- allows another absolute node as long as at least one sibling is not
    -- absolute. e.g.:
    -- * @#10 + a/b@ is fine
    -- * as is @a/(b & #10)@
    -- * but @a/#10@ is not ok
    --
    -- 'isValidPath' checks this condition
    --
    -- This is in order to ensure that there aren't any jumps in the
    -- deterministic paths that are created when resolving a path. We could
    -- probably loosen this condition, but better to start stricter and then
    -- loosen up later
    Absolute NID
  | -- | sequence
    Path t :/ Path t
  | -- | union
    Path t :+ Path t
  | -- | intersection
    Path t :& Path t
  deriving (Show, Eq, Ord)

newtype RelativePath t = UnsafeRelativePath (Path t)
  deriving (Show, Eq, Ord)

isValidPath :: Path t -> Bool
isValidPath = \case
  Absolute _ -> True
  Literal _ -> True
  One -> True
  Wild -> True
  Zero -> True
  p1 :& p2 -> isValidPath p1 && isValidPath p2
  p1 :+ p2 -> isValidPath p1 && isValidPath p2
  p1 :/ p2 -> isValidPath p1 && isValidChildPath p2
    where
      isValidChildPath = \case
        Absolute _ -> False
        Literal _ -> True
        One -> True
        Wild -> True
        Zero -> True
        p1 :+ p2 -> isValidChildPath p1 && isValidChildPath p2
        p1 :/ p2 -> isValidChildPath p1 && isValidChildPath p2
        p1 :& p2 ->
          not (all isAbsolute (andSiblings p1 ++ andSiblings p2))
            && isValidPath p1
            && isValidPath p2
      andSiblings = \case
        p1 :& p2 -> andSiblings p1 ++ andSiblings p2
        x -> [x]
      isAbsolute = \case
        Absolute _ -> True
        Literal _ -> False
        One -> False
        Wild -> False
        Zero -> False
        _ :+ _ -> False
        _ :& _ -> False
        _ :/ _ -> False

mkRelativePath :: TransitionValid t => Path t -> Maybe (RelativePath t)
mkRelativePath p
  | isFullyRelativePath p = Just (UnsafeRelativePath p)
  | otherwise = Nothing

isFullyRelativePath :: TransitionValid t => Path t -> Bool
isFullyRelativePath = all (isNothing . fst) . setToList . listifyNewPath

resolvePathSuccesses ::
  forall t effs.
  (Members [ReadGraph t, Error Missing] effs, TransitionValid t) =>
  NID ->
  Path t ->
  Sem effs (Set NID)
resolvePathSuccesses nid = \case
  Zero -> pure mempty
  One -> pure $ singleton nid
  Wild -> do
    n <- getNodeSem nid
    pure $ toSetOf (folded . #node) (outgoingConnectsOf @t n)
  Literal x -> do
    n <- getNodeSem nid
    pure . setFromList $ matchConnect x (outgoingConnectsOf @t n)
  Absolute nid -> pure $ singleton nid
  p :/ q -> do
    pResolved <- toList <$> resolvePathSuccesses nid p
    mconcat <$> traverse (`resolvePathSuccesses` q) pResolved
  p :+ q -> union <$> resolvePathSuccesses nid p <*> resolvePathSuccesses nid q
  p :& q -> intersect <$> resolvePathSuccesses nid p <*> resolvePathSuccesses nid q

-- | TODO: refactor so I don't need Error Missing. Currently it is used only
-- for the initial node in case it does not exist
resolvePathSuccessesDetail ::
  forall t effs.
  (Members [ReadGraph t, Error Missing] effs, TransitionValid t) =>
  NID ->
  Path t ->
  Sem effs (Set (DPath t))
resolvePathSuccessesDetail nid = \case
  One -> pure $ Set.singleton (DPath nid [] nid [])
  Zero -> pure Set.empty
  Absolute nid' ->
    getNode nid' <&> \case
      Nothing -> Set.empty
      Just _ -> Set.singleton $ DPath nid' [] nid' []
  Wild ->
    getNodeSem nid <&> \n -> Set.fromList $ do
      Connect t nid' <- toList $ outgoingConnectsOf n
      pure $ DPath nid [nidOf n `FromVia` t] nid' []
  Literal t ->
    getNodeSem nid <&> \n ->
      case matchConnect t (outgoingConnectsOf n) of
        [] -> Set.singleton (DPath nid [] nid [t])
        ms -> Set.fromList (DPath nid [nidOf n `FromVia` t] <$> ms <*> pure [])
  p1 :/ p2 -> do
    r1 <- toList <$> resolvePathSuccessesDetail nid p1
    fmap setFromList . (`concatMapM` r1) $ \case
      DPath start pre nid' [] -> do
        r2 <- toList <$> resolvePathSuccessesDetail nid' p2
        forM r2 $ \case
          DPath mid pre' nid'' post'
            | mid /= nid' -> error "midpoint of `/` path is wrong"
            | otherwise -> pure $ DPath start (pre ++ pre') nid'' post'
      DPath _ pre nid' post -> do
        -- when a path component of the first path fails, all of the subsequent
        -- path components will also fail, thus we treat them as such
        let r2 = toList . assertListifiedRelativePath $ listifyNewPath p2
        forM r2 $ \post' -> pure $ DPath nid pre nid' (post ++ post')
  p1 :+ p2 -> union <$> p1r <*> p2r
    where
      p1r = resolvePathSuccessesDetail nid p1
      p2r = resolvePathSuccessesDetail nid p2
  p1 :& p2 -> setFromList <$> (intersect' <$> p1r <*> p2r)
    where
      intersect' = intersectBy ((==) `on` endPoint)
      p1r = toList <$> resolvePathSuccessesDetail nid p1
      p2r = toList <$> resolvePathSuccessesDetail nid p2

-- | Assert that the result of listifyNewPath is relative. This should be the
-- case if the input path was relative.
assertListifiedRelativePath :: (HasCallStack, Ord t) => Set (Maybe NID, [t]) -> Set [t]
assertListifiedRelativePath = Set.map \case
  (Nothing, ts) -> ts
  (Just _, _) -> error "listified path is absolute not relative"

-- | Create a path such that the end points of the path are all new nodes
-- intersection and union are both interpreted as a splitting point
-- where both paths should be created. This is perhaps a bad convention, but I
-- can't think of a better one to use without just banning one or the other
mkPath ::
  forall t effs.
  ( Members [FreshNID, Error Missing] effs,
    HasGraph t effs
  ) =>
  NID ->
  RelativePath t ->
  Sem effs (Set NID)
mkPath nid (UnsafeRelativePath p) =
  fmap setFromList . forM (setToList (assertListifiedRelativePath (listifyNewPath p))) $
    \x -> transitionsViaManyFresh nid x

-- | Construct a graph that contains only the edges passed through
-- by the literal edges in the path.
-- nids are the same as in the original graph
tracePath ::
  forall t effs.
  Members [FreshNID, Error Missing, ReadGraph t] effs =>
  NID ->
  Path t ->
  Sem effs (Graph t)
tracePath _ _ = error "unimplemented"

delPath ::
  forall t effs.
  ( Members [FreshNID, Error Missing] effs,
    HasGraph t effs
  ) =>
  NID ->
  Path t ->
  Sem effs ()
delPath nid p = resolvePathSuccessesDetail nid p >>= mapM_ delDPath
  where
    delDPath (DPath _ xs@(_ : _) nid' []) = case lastEx xs of -- safe because list nonempty
      FromVia nid2 t -> deleteEdge @t (Edge nid2 t nid')
    delDPath _ = pure ()

mvPath ::
  forall t effs.
  ( Members [FreshNID, Error Missing] effs,
    HasGraph t effs
  ) =>
  NID ->
  Path t ->
  NID ->
  Sem effs ()
mvPath s p target =
  resolvePathSuccessesDetail s p >>= mapM_ (mvDPathTo target)

mvDPathTo ::
  forall t effs.
  ( Members [FreshNID, Error Missing] effs,
    HasGraph t effs
  ) =>
  NID ->
  DPath t ->
  Sem effs ()
mvDPathTo target (DPath _ xs@(_ : _) nid []) = case lastEx xs of
  FromVia nid2 t -> do
    deleteEdge (Edge nid2 t nid)
    insertEdge (Edge target t nid)
mvDPathTo _ _ = pure ()

-- | rename a one path to another path
-- the renaming only operates on the last / separated path segment
renameDPath ::
  forall t effs.
  ( Members [FreshNID, Error Missing] effs,
    HasGraph t effs
  ) =>
  DPath t ->
  NID ->
  -- | this path must either be of the form DPath _ nid [t]
  -- or of the form (splitLast -> Just (DPath _ nid _, t, _))
  Path t ->
  Sem effs ()
renameDPath dpathFrom nidPathStart pathTo =
  withJust (splitLast dpathFrom) $ \(DPath _ _ oldRoot _, t, nid) -> do
    successes <- resolvePathSuccessesDetail nidPathStart pathTo
    forM_ successes $ \case
      (DPath _ _ newRoot [t']) -> do
        deleteEdge (Edge oldRoot t nid)
        insertEdge (Edge newRoot t' nid)
      (splitLast -> Just (DPath _ _ newRoot _, t', _)) -> do
        deleteEdge (Edge oldRoot t nid)
        insertEdge (Edge newRoot t' nid)
      _ -> pure ()

-- | Turn a path into a set of DPaths where the set denotes disjunction.
-- Similar to converting to a NFA for paths. Each path also has a start point
-- or Nothing to represent that the path is relative.
-- Transitions are asumed to behave deterministically in this translation.
listifyNewPath ::
  forall t.
  TransitionValid t =>
  Path t ->
  Set (Maybe NID, [t])
listifyNewPath = \case
  Zero -> Set.empty
  One -> Set.singleton (Nothing, [])
  Wild -> Set.empty -- we have to ignore paths that contain wild because we
  -- don't have the graph and can't add all possible
  -- transitions
  Literal t -> Set.singleton (Nothing, [t])
  Absolute nid -> Set.singleton (Just nid, [])
  p1 :/ p2 -> Set.fromList $ do
    (nid, p1') <- toList $ listifyNewPath p1
    p2' <- toList $ assertListifiedRelativePath $ listifyNewPath p2
    pure (nid, p1' ++ p2')
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
resolvePath ::
  TransitionValid t =>
  Path t ->
  Node t ->
  Graph t ->
  Set (DPath t)
resolvePath p n g =
  nodeConsistentWithGraph g n `seq` fromMaybe (error "node is from graph") (resolvePathInConcreteGraph (nidOf n) p g)

resolvePathInConcreteGraph ::
  forall t.
  TransitionValid t =>
  NID ->
  Path t ->
  Graph t ->
  Maybe (Set (DPath t))
resolvePathInConcreteGraph nid p g =
  resolvePathSuccessesDetail nid p
    & runReadGraphState @t
    & evalState g
    & runError
    & run
    & either (const Nothing) Just
