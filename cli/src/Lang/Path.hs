{-# LANGUAGE StrictData #-}
{-# LANGUAGE ViewPatterns #-}
{-# OPTIONS_GHC -Wno-name-shadowing #-}

-- |
--   Describe sets of nodes using path specifications.
--   Given a graph and a node, a path denotes a set of nodes relative to the
--   starting node.
--   Alternatively a path can be used as an action on a graph to add or equalize
--   nodes, contingent on a source for new nodes potentially.
module Lang.Path
  ( module Lang.Path,
    module X,
  )
where

import Control.Lens hiding (pre, unsnoc)
import Data.List (intersectBy)
import qualified Data.Set as Set
import Data.Set.Ordered (OSet)
import qualified Data.Set.Ordered as OSet
import Effect.FreshNID
import Error.UserError
import Graph.Effect
import Graph.Utils
import Models.Connect
import Models.Edge
import Models.Graph hiding (insertEdge)
import MyPrelude
import Polysemy.State (evalState)
import Models.Node
import Error.Missing
import Models.DPath as X
import Models.Path as X

isFullyRelativePath :: ValidTransition t => Path t -> Bool
isFullyRelativePath = all (isNothing . fst) . setToList . listifyNewPath

-- | A deterministic path is successful if it ends at a node in the graph
successfulDPathEndpoint :: DPath t -> Maybe NID
successfulDPathEndpoint (DPath _ _ nid []) = Just nid
successfulDPathEndpoint _ = Nothing

resolvePathSuccesses ::
  forall t effs.
  (Members [ReadGraph t (Maybe ByteString), Error Missing] effs, ValidTransition t) =>
  NID ->
  Path t ->
  Sem effs (Set NID)
resolvePathSuccesses nid = \case
  Zero -> pure mempty
  One -> pure $ singleton nid
  Wild -> do
    n <- getNodeSem nid
    pure $ toSetOf (folded . #node) n.outgoing
  Literal x -> do
    n <- getNodeSem nid
    pure . setFromList $ matchConnect x n.outgoing
  Absolute nid -> pure $ singleton nid
  p :/ q -> do
    pResolved <- toList <$> resolvePathSuccesses nid p
    mconcat <$> traverse (`resolvePathSuccesses` q) pResolved
  p :+ q -> union <$> resolvePathSuccesses nid p <*> resolvePathSuccesses nid q
  p :& q -> intersect <$> resolvePathSuccesses nid p <*> resolvePathSuccesses nid q

-- | General reducer over graph directed by a path.
-- * Traverses the graph in an order based on the syntactic structure of the
--   path.
-- * Interprets missing nodes as empty sets.
-- TODO: refactor so I don't need Error Missing. Currently it is used only
-- for the initial node in case it does not exist
resolvePathSuccessesDetail' ::
  forall t a effs.
  (Members [ReadGraph t a, Error Missing] effs, ValidNode t a) =>
  NID ->
  Path t ->
  Sem effs (OSet (DPath t))
resolvePathSuccessesDetail' nid = \case
  One -> pure $ OSet.singleton (DPath nid [] nid [])
  Zero -> pure OSet.empty
  Absolute nid' ->
    getNode nid' <&> \case
      Nothing -> OSet.empty
      Just _ -> OSet.singleton $ DPath nid' [] nid' []
  Wild ->
    getNodeSem nid <&> \n -> OSet.fromList $ do
      Connect t nid' <- n ^.. #outgoing . folded
      pure $ DPath nid [view #nid n `FromVia` t] nid' []
  Literal t ->
    getNodeSem nid <&> \n ->
      case matchConnect t n.outgoing of
        [] -> OSet.singleton (DPath nid [] nid [t])
        ms -> OSet.fromList (DPath nid [view #nid n `FromVia` t] <$> ms <*> pure [])
  p1 :/ p2 -> do
    r1 <- toList <$> resolvePathSuccessesDetail' nid p1
    fmap OSet.fromList . (`concatMapM` r1) $ \case
      DPath start pre nid' [] -> do
        r2 <- toList <$> resolvePathSuccessesDetail' nid' p2
        forM r2 $ \case
          DPath mid pre' nid'' post'
            | mid /= nid' -> error "midpoint of `/` path is wrong"
            | otherwise -> pure $ DPath start (pre ++ pre') nid'' post'
      DPath _ pre nid' post -> do
        -- when a path component of the first path fails, all of the subsequent
        -- path components will also fail, thus we treat them as such
        let r2 = toList . assertListifiedRelativePath $ listifyNewPath p2
        forM r2 $ \post' -> pure $ DPath nid pre nid' (post ++ post')
  p1 :+ p2 -> (OSet.|<>) <$> p1r <*> p2r
    where
      p1r = resolvePathSuccessesDetail' nid p1
      p2r = resolvePathSuccessesDetail' nid p2
  p1 :& p2 -> OSet.fromList <$> (intersect' <$> p1r <*> p2r)
    where
      intersect' = intersectBy ((==) `on` endPoint)
      p1r = toList <$> resolvePathSuccessesDetail' nid p1
      p2r = toList <$> resolvePathSuccessesDetail' nid p2

resolvePathSuccessesDetail ::
  forall t effs.
  (Members [ReadGraph t (Maybe ByteString), Error Missing] effs, ValidTransition t) =>
  NID ->
  Path t ->
  Sem effs (Set (DPath t))
resolvePathSuccessesDetail nid p = resolvePathSuccessesDetail' nid p <&> OSet.toSet

-- | Assert that the result of listifyNewPath is relative. This should be the
-- case if the input path was relative.
assertListifiedRelativePath :: (HasCallStack, Ord t) => Set (Maybe NID, [t]) -> Set [t]
assertListifiedRelativePath = mapSet \case
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
  Path t ->
  Sem effs (Set NID)
mkPath nid p =
  fmap setFromList . forM (setToList (listifyNewPath p)) $
    \(m_nid, x) -> transitionsViaManyFresh (fromMaybe nid m_nid) x

-- | Construct a graph that contains only the edges passed through
-- by the literal edges in the path.
-- nids are the same as in the original graph
tracePath ::
  forall t a effs.
  Members [FreshNID, Error Missing, ReadGraph t a] effs =>
  NID ->
  Path t ->
  Sem effs (Graph t a)
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
      FromVia nid2 t -> Graph.Effect.deleteEdge @t (Edge nid2 t nid')
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
    Graph.Effect.deleteEdge (Edge nid2 t nid)
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
        Graph.Effect.deleteEdge (Edge oldRoot t nid)
        insertEdge (Edge newRoot t' nid)
      (splitLast -> Just (DPath _ _ newRoot _, t', _)) -> do
        Graph.Effect.deleteEdge (Edge oldRoot t nid)
        insertEdge (Edge newRoot t' nid)
      _ -> pure ()

-- | alias a one path to another path
aliasDPath ::
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
aliasDPath dpathFrom nidPathStart pathTo =
  withJust (splitLast dpathFrom) $ \(DPath {}, _, nid) -> do
    successes <- resolvePathSuccessesDetail nidPathStart pathTo
    forM_ successes $ \case
      (DPath _ _ newRoot [t']) -> do
        insertEdge (Edge newRoot t' nid)
      (splitLast -> Just (DPath _ _ newRoot _, t', _)) -> do
        insertEdge (Edge newRoot t' nid)
      _ -> pure ()

-- | Turn a path into a set of DPaths where the set denotes disjunction.
-- Similar to converting to a NFA for paths. Each path also has a start point
-- or Nothing to represent that the path is relative.
-- Transitions are asumed to behave deterministically in this translation.
listifyNewPath ::
  forall t.
  ValidTransition t =>
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
  ValidNode t a =>
  Path t ->
  Node t a ->
  Graph t a ->
  OSet (DPath t)
resolvePath p n g =
  nodeConsistentWithGraph g n
    `seq` fromMaybe
      (error "node is from graph")
      (resolvePathInConcreteGraph (view #nid n) p g)

resolvePathInConcreteGraph ::
  forall t a.
  ValidNode t a =>
  NID ->
  Path t ->
  Graph t a ->
  Maybe (OSet (DPath t))
resolvePathInConcreteGraph nid p g =
  resolvePathSuccessesDetail' nid p
    & runReadGraphState @t @a
    & evalState g
    & runError
    & run
    & either (const Nothing) Just
