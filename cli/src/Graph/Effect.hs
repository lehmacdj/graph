{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE ViewPatterns #-}

module Graph.Effect
  ( module Graph.Effect,
    NID,
    Node,
  )
where

import Control.Lens hiding (transform)
import qualified DAL.Serialization as S2
import Data.Aeson (FromJSON (..), ToJSON (..))
import qualified Data.Set as Set
import Error.UserError
import Effect.Util
import Effect.Warn
import Models.Connect
import Models.Edge
import Models.Graph (Graph)
import qualified Models.Graph as G
import Models.NID
import Models.Node
import MyPrelude
import Polysemy.Input
import Polysemy.State

data ReadGraph t d m a where
  GetNode :: NID -> ReadGraph t d m (Maybe (Node t d))
  NodeManifest :: ReadGraph t d m [NID]

data ReadGraphDataless t m a where
  GetNodeDataless :: NID -> ReadGraphDataless t m (Maybe (Node t (Maybe ByteString)))

makeSem ''ReadGraph
makeSem ''ReadGraphDataless

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

dualize :: Member Dualizeable effs => Sem effs ()
dualize = modify @IsDual (omap not)

data WriteGraph t m a where
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

makeSem ''WriteGraph

type HasGraph t effs =
  ( Member (ReadGraph t (Maybe ByteString)) effs,
    Member (ReadGraphDataless t) effs,
    Member (WriteGraph t) effs,
    ValidTransition t
  )

-- | Run a graph computation in the io monad, using a directory in the
-- serialization format to access the graph
runReadGraphIO ::
  ( Member (Embed IO) effs,
    FromJSON t,
    ToJSON t,
    ValidTransition t
  ) =>
  FilePath ->
  Sem (ReadGraph t (Maybe ByteString) ': effs) ~> Sem effs
runReadGraphIO dir = runInputConst (IsDual False) . runReadGraphIODualizeable dir

-- | Run a graph computation in the io monad, using a directory in the
-- serialization format to access the graph, while allowing for the
-- possibility of the graph being dualized.
runReadGraphIODualizeable ::
  forall t effs.
  ( Member (Embed IO) effs,
    FromJSON t,
    ToJSON t,
    ValidTransition t
  ) =>
  FilePath ->
  Sem (ReadGraph t (Maybe ByteString) ': effs) ~> Sem (Input IsDual : effs)
runReadGraphIODualizeable dir = reinterpret $ \case
  GetNode nid -> do
    maybeN <-
      errorToNothing $
        -- writing type level lists in your code is not fun :(
        -- kids: be careful when you choose haskell
        S2.deserializeNodeF @t @(Error UserError : Input IsDual : effs) dir nid
    dual <- input
    pure $ maybeN <&> ifDualized dual dualizeNode
  NodeManifest -> S2.getAllNodeIds dir

runReadGraphDatalessIODualizeable ::
  forall t effs.
  ( Member (Embed IO) effs,
    FromJSON t,
    ToJSON t,
    ValidTransition t
  ) =>
  FilePath ->
  Sem (ReadGraphDataless t ': effs) ~> Sem (Input IsDual : effs)
runReadGraphDatalessIODualizeable dir = reinterpret $ \case
  GetNodeDataless nid -> do
    maybeN <-
      errorToNothing $
        -- writing type level lists in your code is not fun :(
        -- kids: be careful when you choose haskell
        S2.deserializeNodeWithoutDataF @t @(Error UserError : Input IsDual : effs) dir nid
    dual <- input
    pure $ maybeN <&> ifDualized dual dualizeNode

-- | Run a graph in IO with the ambient ability for the graph to be
-- dualizeable.
runReadGraphDualizeableIO ::
  forall t effs.
  ( Member (Embed IO) effs,
    Member Dualizeable effs,
    FromJSON t,
    ToJSON t,
    ValidTransition t
  ) =>
  FilePath ->
  Sem (ReadGraph t (Maybe ByteString) ': effs) ~> Sem effs
runReadGraphDualizeableIO dir =
  runReaderAsState
    . runReadGraphIODualizeable dir

-- | Run a graph in IO with the ambient ability for the graph to be
-- dualizeable.
runReadGraphDatalessDualizeableIO ::
  forall t effs.
  ( Member (Embed IO) effs,
    Member Dualizeable effs,
    FromJSON t,
    ToJSON t,
    ValidTransition t
  ) =>
  FilePath ->
  Sem (ReadGraphDataless t ': effs) ~> Sem effs
runReadGraphDatalessDualizeableIO dir =
  runReaderAsState
    . runReadGraphDatalessIODualizeable dir

-- run Dualizeable as a state that also flips the graph in a state to
-- reflect its value
-- runDualizeableState
--   :: Member (State (Graph t)) effs
--   => Sem (Dualizeable : effs) ~> Sem effs
-- runDualizeableState = interpret $ modify dualize

runDualizeable :: Sem (Dualizeable : effs) ~> Sem effs
runDualizeable = map snd . runState (IsDual False)

runReadGraphState ::
  forall t d effs a.
  (Member (State (Graph t d)) effs) =>
  Sem (ReadGraph t d ': effs) a ->
  Sem effs a
runReadGraphState = interpret $ \case
  GetNode nid -> G.maybeLookupNode <$> get <*> pure nid
  NodeManifest -> keys . view #nodeMap <$> get @(Graph t d)

runWriteGraphState ::
  forall t effs.
  (Member (State (Graph t (Maybe ByteString))) effs, ValidTransition t) =>
  Sem (WriteGraph t ': effs) ~> Sem effs
runWriteGraphState = interpret $ \case
  TouchNode nid -> modify (G.insertNode (emptyNode' nid) :: Graph t (Maybe ByteString) -> Graph t (Maybe ByteString))
  DeleteNode nid -> modify (G.delNode' nid :: Graph t (Maybe ByteString) -> Graph t (Maybe ByteString))
  InsertEdge e -> modify (G.insertEdge e)
  DeleteEdge e -> modify (G.deleteEdge e)
  SetData nid d -> modify (G.setData' d nid :: Graph t (Maybe ByteString) -> Graph t (Maybe ByteString))

-- | Run a graph computation in the io monad, using a directory in the
-- serialization format to access the graph
runWriteGraphIO ::
  forall t effs.
  ( Member (Embed IO) effs,
    Members [Error UserError, Warn UserError] effs,
    FromJSON t,
    ToJSON t,
    ValidTransition t
  ) =>
  FilePath ->
  Sem (WriteGraph t ': effs) ~> Sem effs
runWriteGraphIO dir = runInputConst (IsDual False) . runWriteGraphIODualizeable dir

-- | Implements a graph where edges are labeled by strings via system
-- extra strings that are generated are not cleaned up automatically by this,
-- the strings stick around until they are gc-ed
runWriteStringGraph ::
  forall r a.
  Members
    [ WriteGraph NID,
      Error UserError,
      Warn UserError,
      Input IsDual
    ]
    r =>
  Sem (WriteGraph String : r) a ->
  Sem r a
runWriteStringGraph = interpret $ \case
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
  forall t effs.
  ( Member (Embed IO) effs,
    Member (Error UserError) effs,
    Member (Warn UserError) effs,
    FromJSON t,
    ToJSON t,
    ValidTransition t
  ) =>
  FilePath ->
  Sem (WriteGraph t : effs) ~> Sem (Input IsDual : effs)
runWriteGraphIODualizeable dir = reinterpret $ \case
  TouchNode nid ->
    whenM (not <$> S2.doesNodeExist dir nid) $
      embedCatchingErrors $ S2.serializeNodeEx (emptyNode' nid :: Node t (Maybe ByteString)) dir
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
    dual <- input @IsDual
    let (Edge i t o) = ifDualized dual dualizeEdge e
    runWriteGraphIODualizeable @t dir (touchNode @t i)
    runWriteGraphIODualizeable @t dir (touchNode @t o)
    liftIO $ S2.withSerializedNode (#outgoing %~ insertSet (Connect t o)) dir i
    liftIO $ S2.withSerializedNode (#incoming %~ insertSet (Connect t i)) dir o
  DeleteEdge e -> do
    dual <- input @IsDual
    let (Edge i t o) = ifDualized dual dualizeEdge e
    liftIO $ S2.withSerializedNode (#outgoing %~ deleteSet (Connect t o)) dir i
    liftIO $ S2.withSerializedNode (#incoming %~ deleteSet (Connect t i)) dir o
  SetData nid d -> liftIO $ S2.withSerializedNode (#augmentation .~ d :: Node t (Maybe ByteString) -> Node t (Maybe ByteString)) dir nid

-- | Run both the Dualizeable effect and the WriteGraph in IO
-- The default state of all graphs stored on disk is that they are not dual
-- thus we simply set IsDual False as our initial state parameter for
-- Dualizeable.
runWriteGraphDualizeableIO ::
  forall t effs.
  ( Member (Embed IO) effs,
    Members [Dualizeable, Error UserError, Warn UserError] effs,
    FromJSON t,
    ToJSON t,
    ValidTransition t
  ) =>
  FilePath ->
  Sem (WriteGraph t : effs) ~> Sem effs
runWriteGraphDualizeableIO dir = runReaderAsState . runWriteGraphIODualizeable dir
