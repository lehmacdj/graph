{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE TemplateHaskell #-}

module Graph.Effect
  ( module Graph.Effect,
    NID,
    Node,
  )
where

import Control.Lens hiding (transform)
import DAL.Serialization qualified as S2
import Data.Set qualified as Set
import Effectful
import Effectful.Dispatch.Dynamic
import Effectful.Error.Static
import Effectful.Reader.Static
import Effectful.State.Static.Local
import Effectful.TH
import Error.UserError
import Error.Warn
import Models.Connect
import Models.Edge
import Models.Graph (Graph)
import Models.Graph qualified as G
import Models.NID
import Models.Node
import MyPrelude
import Utils.Polysemy

data ReadGraph t d :: Effect where
  GetNode :: NID -> ReadGraph t d m (Maybe (Node t d))
  NodeManifest :: ReadGraph t d m [NID]

data ReadGraphDataless t :: Effect where
  GetNodeDataless :: NID -> ReadGraphDataless t m (Maybe (Node t (Maybe ByteString)))

makeEffect ''ReadGraph
makeEffect ''ReadGraphDataless

newtype IsDual = IsDual {isDual :: Bool}
  deriving (Show, Eq, Ord, Generic)

type instance Element IsDual = Bool

instance MonoFunctor IsDual where
  omap f (IsDual x) = IsDual (f x)

-- | Intended for use in view patters to process edges when the might be
-- dualized. Also acts generally on edges to process them relative to a
-- dualization parameter.
ifDualized :: IsDual -> (a -> a) -> a -> a
ifDualized dual f
  | dual ^. #isDual = f
  | otherwise = id

type Dualizeable = State IsDual

dualize :: (Dualizeable :> es) => Eff es ()
dualize = modify @IsDual (omap not)

data WriteGraph t :: Effect where
  -- | make the node with that id exist
  TouchNode ::
    NID ->
    WriteGraph t m ()
  -- | delete a node and all edges to/from it
  DeleteNode ::
    NID ->
    WriteGraph t m ()
  -- | insert edge adding empty nodes to graph if non-existent
  InsertEdge ::
    Edge t ->
    WriteGraph t m ()
  DeleteEdge ::
    Edge t ->
    WriteGraph t m ()
  SetData ::
    NID ->
    -- | delete if Nothing
    Maybe ByteString ->
    WriteGraph t m ()

makeEffect ''WriteGraph

type HasGraph t es =
  ( ReadGraph t (Maybe ByteString) :> es,
    ReadGraphDataless t :> es,
    WriteGraph t :> es,
    ValidTransition t
  )

-- | Run a graph computation in the io monad, using a directory in the
-- serialization format to access the graph
runReadGraphIO ::
  ( IOE :> es,
    FromJSON t,
    ToJSON t,
    ValidTransition t
  ) =>
  FilePath ->
  Eff (ReadGraph t (Maybe ByteString) ': es) a ->
  Eff es a
runReadGraphIO dir = runReader (IsDual False) . runReadGraphIODualizeable dir

-- | Run a graph computation in the io monad, using a directory in the
-- serialization format to access the graph, while allowing for the
-- possibility of the graph being dualized.
runReadGraphIODualizeable ::
  forall t es.
  ( IOE :> es,
    FromJSON t,
    ToJSON t,
    ValidTransition t
  ) =>
  FilePath ->
  Eff (ReadGraph t (Maybe ByteString) ': es) a ->
  Eff (Reader IsDual : es) a
runReadGraphIODualizeable dir = reinterpret $ \_ -> \case
  GetNode nid -> do
    maybeN <-
      errorToNothing $
        -- writing type level lists in your code is not fun :(
        -- kids: be careful when you choose haskell
        S2.deserializeNodeF @t @(Error UserError : Reader IsDual : es) dir nid
    dual <- ask
    pure $ maybeN <&> ifDualized dual dualizeNode
  NodeManifest -> S2.getAllNodeIds dir

runReadGraphDatalessIODualizeable ::
  forall t es.
  ( IOE :> es,
    FromJSON t,
    ToJSON t,
    ValidTransition t
  ) =>
  FilePath ->
  Eff (ReadGraphDataless t ': es) a ->
  Eff (Reader IsDual : es) a
runReadGraphDatalessIODualizeable dir = reinterpret $ \_ -> \case
  GetNodeDataless nid -> do
    maybeN <-
      errorToNothing $
        -- writing type level lists in your code is not fun :(
        -- kids: be careful when you choose haskell
        S2.deserializeNodeWithoutDataF @t @(Error UserError : Reader IsDual : es) dir nid
    dual <- ask
    pure $ maybeN <&> ifDualized dual dualizeNode

-- | Run a graph in IO with the ambient ability for the graph to be
-- dualizeable.
runReadGraphDualizeableIO ::
  forall t es.
  ( IOE :> es,
    Dualizeable :> es,
    FromJSON t,
    ToJSON t,
    ValidTransition t
  ) =>
  FilePath ->
  Eff (ReadGraph t (Maybe ByteString) ': es) a ->
  Eff es a
runReadGraphDualizeableIO dir =
  runReaderAsState
    . runReadGraphIODualizeable dir

-- | Run a graph in IO with the ambient ability for the graph to be
-- dualizeable.
runReadGraphDatalessDualizeableIO ::
  forall t es.
  ( IOE :> es,
    Dualizeable :> es,
    FromJSON t,
    ToJSON t,
    ValidTransition t
  ) =>
  FilePath ->
  Eff (ReadGraphDataless t ': es) a ->
  Eff es a
runReadGraphDatalessDualizeableIO dir =
  runReaderAsState
    . runReadGraphDatalessIODualizeable dir

-- run Dualizeable as a state that also flips the graph in a state to
-- reflect its value
-- runDualizeableState
--   :: State (Graph t) :> es
--   => Eff (Dualizeable : es) a -> Eff es a
-- runDualizeableState = interpret $ modify dualize

runDualizeable :: Eff (Dualizeable : es) a -> Eff es a
runDualizeable = fmap snd . runState (IsDual False)

runReadGraphState ::
  forall t d es a.
  (State (Graph t d) :> es) =>
  Eff (ReadGraph t d ': es) a ->
  Eff es a
runReadGraphState = interpret $ \_ -> \case
  GetNode nid -> G.maybeLookupNode <$> get <*> pure nid
  NodeManifest -> keys . view #nodeMap <$> get @(Graph t d)

runWriteGraphState ::
  forall t es.
  (State (Graph t (Maybe ByteString)) :> es, ValidTransition t) =>
  Eff (WriteGraph t ': es) a ->
  Eff es a
runWriteGraphState = interpret $ \_ -> \case
  TouchNode nid -> modify (G.insertNode (emptyNode nid) :: Graph t (Maybe ByteString) -> Graph t (Maybe ByteString))
  DeleteNode nid -> modify (G.delNode nid :: Graph t (Maybe ByteString) -> Graph t (Maybe ByteString))
  InsertEdge e -> modify (G.insertEdge e)
  DeleteEdge e -> modify (G.deleteEdge e)
  SetData nid d -> modify (G.setData d nid :: Graph t (Maybe ByteString) -> Graph t (Maybe ByteString))

-- | Run a graph computation in the io monad, using a directory in the
-- serialization format to access the graph
runWriteGraphIO ::
  forall t es.
  ( IOE :> es,
    Error UserError :> es,
    Warn UserError :> es,
    FromJSON t,
    ToJSON t,
    ValidTransition t
  ) =>
  FilePath ->
  Eff (WriteGraph t ': es) a ->
  Eff es a
runWriteGraphIO dir = runReader (IsDual False) . runWriteGraphIODualizeable dir

-- | Implements a graph where edges are labeled by strings via system
-- extra strings that are generated are not cleaned up automatically by this,
-- the strings stick around until they are gc-ed
runWriteStringGraph ::
  forall es a.
  ( WriteGraph NID :> es,
    Error UserError :> es,
    Warn UserError :> es,
    Reader IsDual :> es
  ) =>
  Eff (WriteGraph Text : es) a ->
  Eff es a
runWriteStringGraph = interpret $ \_ -> \case
  TouchNode nid -> touchNode nid
  DeleteNode nid -> deleteNode nid
  InsertEdge (Edge _i _label _o) -> do
    -- find an already existing string node if it exists
    _ <- error "not implemented"
    -- insert that edge
    error "not implemented"
  DeleteEdge _e -> error "not implemented"
  SetData nid d -> setData nid d

-- | General handler for WriteGraph parameterized so that it is possible
-- to specify if the computation is dualized or not
-- If the computation is dualized, all operations on edges will be inverted.
-- That is InsertEdge (Edge 0 "x" 1) will result in the same effect as
-- InsertEdge (Edge 1 "x" 0) if the IsDual parameter is true.
-- We obtain whether the compuation is dualized through a reader parameter
runWriteGraphIODualizeable ::
  forall t es.
  ( IOE :> es,
    Error UserError :> es,
    Warn UserError :> es,
    FromJSON t,
    ToJSON t,
    ValidTransition t
  ) =>
  FilePath ->
  Eff (WriteGraph t : es) a ->
  Eff (Reader IsDual : es) a
runWriteGraphIODualizeable dir = reinterpret $ \_ -> \case
  TouchNode nid ->
    whenM (not <$> S2.doesNodeExist dir nid) $
      embedCatchingErrors $
        S2.serializeNodeEx (emptyNode nid :: Node t (Maybe ByteString)) dir
  DeleteNode nid -> do
    n <- S2.deserializeNodeF @t dir nid
    let del = Set.filter ((/= nid) . view #node) :: Set (Connect t) -> Set (Connect t)
        delIn = over #incoming del
        delOut = over #outgoing del
        neighborsIn = toListOf (#incoming . folded . #node) n
        neighborsOut = toListOf (#outgoing . folded . #node) n
        neighbors = ordNub (neighborsIn ++ neighborsOut)
    liftIO $ forM_ neighbors $ S2.withSerializedNode (delIn . delOut) dir
    convertError @UserError . embedCatchingErrors $ S2.removeNode dir nid
  InsertEdge e -> do
    dual <- ask @IsDual
    let (Edge i t o) = ifDualized dual dualizeEdge e
    runWriteGraphIODualizeable @t dir (touchNode @t i)
    runWriteGraphIODualizeable @t dir (touchNode @t o)
    liftIO $ S2.withSerializedNode (#outgoing %~ insertSet (Connect t o)) dir i
    liftIO $ S2.withSerializedNode (#incoming %~ insertSet (Connect t i)) dir o
  DeleteEdge e -> do
    dual <- ask @IsDual
    let (Edge i t o) = ifDualized dual dualizeEdge e
    liftIO $ S2.withSerializedNode (#outgoing %~ deleteSet (Connect t o)) dir i
    liftIO $ S2.withSerializedNode (#incoming %~ deleteSet (Connect t i)) dir o
  SetData nid d -> liftIO $ S2.withSerializedNode (#augmentation .~ d :: Node t (Maybe ByteString) -> Node t (Maybe ByteString)) dir nid

-- | Run both the Dualizeable effect and the WriteGraph in IO
-- The default state of all graphs stored on disk is that they are not dual
-- thus we simply set IsDual False as our initial state parameter for
-- Dualizeable.
runWriteGraphDualizeableIO ::
  forall t es.
  ( IOE :> es,
    Dualizeable :> es,
    Error UserError :> es,
    Warn UserError :> es,
    FromJSON t,
    ToJSON t,
    ValidTransition t
  ) =>
  FilePath ->
  Eff (WriteGraph t : es) a ->
  Eff es a
runWriteGraphDualizeableIO dir = runReaderAsState . runWriteGraphIODualizeable dir
