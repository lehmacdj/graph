{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Effect.Graph
  ( module Effect.Graph,
    NID,
    Node,
  )
where

import Control.Lens
import Data.Aeson (FromJSON (..), ToJSON (..))
import qualified Data.Set as Set
import Effect.Util
import Effect.Warn
import qualified Graph as G
import Graph.Serialize2
import Graph.Types
import MyPrelude
import Polysemy.Input
import Polysemy.State
import System.Directory (doesFileExist, listDirectory, removeFile)
import System.FilePath (dropExtension)
import Text.Read (readMaybe)
import UserError

data ReadGraph t m a where
  GetNode :: NID -> ReadGraph t m (Maybe (Node t))
  NodeManifest :: ReadGraph t m [NID]

makeSem ''ReadGraph

newtype IsDual = IsDual {isDual :: Bool}
  deriving (Show, Eq, Ord)

type instance Element IsDual = Bool

instance MonoFunctor IsDual where
  omap f (IsDual x) = IsDual (f x)

-- | Intended for use in view patters to process edges when the might be
-- dualized. Also acts generally on edges to process them relative to a
-- dualization parameter.
ifDualized :: IsDual -> (a -> a) -> a -> a
ifDualized dual f
  | isDual dual = f
  | otherwise = id

type Dualizeable = State IsDual

dualize :: Member Dualizeable effs => Sem effs ()
dualize = modify @IsDual (omap not)

data WriteGraph t m a where
  TouchNode ::
    NID ->
    -- | make the node with that id exist
    WriteGraph t m ()
  DeleteNode ::
    NID ->
    -- | delete a node and all edges to/from it
    WriteGraph t m ()
  InsertEdge ::
    Edge t ->
    -- | insert edge if both nodes in graph
    WriteGraph t m ()
  DeleteEdge ::
    Edge t ->
    -- | delete edge
    WriteGraph t m ()
  SetData ::
    NID ->
    Maybe LByteString ->
    -- | delete if Nothing
    WriteGraph t m ()

makeSem ''WriteGraph

type HasGraph t effs =
  ( Member (ReadGraph t) effs,
    Member (WriteGraph t) effs,
    TransitionValid t
  )

-- | Run a graph computation in the io monad, using a directory in the
-- serialization format to access the graph
runReadGraphIO ::
  ( Member (Embed IO) effs,
    FromJSON (Node t),
    ToJSON (Node t),
    TransitionValid t
  ) =>
  FilePath ->
  Sem (ReadGraph t ': effs) ~> Sem effs
runReadGraphIO dir = runInputConst (IsDual False) . runReadGraphIODualizeable dir

-- | Run a graph computation in the io monad, using a directory in the
-- serialization format to access the graph, while allowing for the
-- possibility of the graph being dualized.
runReadGraphIODualizeable ::
  forall t effs.
  ( Member (Embed IO) effs,
    FromJSON (Node t),
    ToJSON (Node t),
    TransitionValid t
  ) =>
  FilePath ->
  Sem (ReadGraph t ': effs) ~> Sem (Input IsDual : effs)
runReadGraphIODualizeable dir = reinterpret $ \case
  GetNode nid -> do
    maybeN <-
      errorToNothing $
        -- writing type level lists in your code is not fun :(
        -- kids: be careful when you choose haskell
        deserializeNodeF @t @(ThrowUserError : Input IsDual : effs) dir nid
    dual <- input
    pure $ maybeN <&> ifDualized dual dualizeNode
  NodeManifest -> do
    cs <- liftIO $ listDirectory dir
    let linkFiles = filter (".json" `isSuffixOf`) cs
        nodes = mapMaybe (readMaybe . dropExtension) linkFiles
    pure nodes

-- | Run a graph in IO with the ambient ability for the graph to be
-- dualizeable.
runReadGraphDualizeableIO ::
  forall t effs.
  ( Member (Embed IO) effs,
    Member Dualizeable effs,
    FromJSON (Node t),
    ToJSON (Node t),
    TransitionValid t
  ) =>
  FilePath ->
  Sem (ReadGraph t ': effs) ~> Sem effs
runReadGraphDualizeableIO dir =
  runReaderAsState
    . runReadGraphIODualizeable dir

-- run Dualizeable as a state that also flips the graph in a state to
-- reflect its value
-- runDualizeableState
--   :: Member (State (Graph t)) effs
--   => Sem (Dualizeable : effs) ~> Sem effs
-- runDualizeableState = interpret $ modify dualize

runDualizeable :: Sem (Dualizeable : effs) ~> Sem effs
runDualizeable = map snd . runState (IsDual False)

runReadGraphState ::
  forall t effs a.
  (Member (State (Graph t)) effs) =>
  Sem (ReadGraph t ': effs) a ->
  Sem effs a
runReadGraphState = interpret $ \case
  GetNode nid -> G.maybeLookupNode <$> get <*> pure nid
  NodeManifest -> keys . G.nodeMap <$> get @(Graph t)

runWriteGraphState ::
  forall t effs.
  (Member (State (Graph t)) effs, TransitionValid t) =>
  Sem (WriteGraph t ': effs) ~> Sem effs
runWriteGraphState = interpret $ \case
  TouchNode nid -> modify (G.insertNode (G.emptyNode nid) :: Graph t -> Graph t)
  DeleteNode nid -> modify (G.delNode' nid :: Graph t -> Graph t)
  InsertEdge e -> modify (G.insertEdge e)
  DeleteEdge e -> modify (G.delEdge e)
  SetData nid d -> modify (G.setData' d nid :: Graph t -> Graph t)

-- | Run a graph computation in the io monad, using a directory in the
-- serialization format to access the graph
runWriteGraphIO ::
  forall t effs.
  ( Member (Embed IO) effs,
    Members [ThrowUserError, Warn UserErrors] effs,
    FromJSON (Node t),
    ToJSON (Node t),
    TransitionValid t
  ) =>
  FilePath ->
  Sem (WriteGraph t ': effs) ~> Sem effs
runWriteGraphIO dir = runInputConst (IsDual False) . runWriteGraphIODualizeable dir

-- | General handler for WriteGraph parameterized so that it is possible
-- to specify if the computation is dualized or not
-- If the computation is dualized, all operations on edges will be inverted.
-- That is InsertEdge (Edge 0 "x" 1) will result in the same effect as
-- InsertEdge (Edge 1 "x" 0) if the IsDual parameter is true.
-- We obtain whether the compuation is dualized through a reader parameter
runWriteGraphIODualizeable ::
  forall t effs.
  ( Member (Embed IO) effs,
    Member ThrowUserError effs,
    Member (Warn UserErrors) effs,
    FromJSON (Node t),
    ToJSON (Node t),
    TransitionValid t
  ) =>
  FilePath ->
  Sem (WriteGraph t : effs) ~> Sem (Input IsDual : effs)
runWriteGraphIODualizeable dir = reinterpret $ \case
  TouchNode nid -> do
    dfe <- liftIO (doesFileExist (linksFile dir nid))
    if not dfe
      then trapIOError' $ serializeNodeEx (G.emptyNode nid :: Node t) dir
      else pure ()
  DeleteNode nid -> do
    n <- deserializeNodeF @t dir nid
    let del = Set.filter ((/= nid) . view connectNode) :: Set (Connect t) -> Set (Connect t)
        delIn = over nodeIncoming del
        delOut = over nodeOutgoing del
        neighborsIn = toListOf (nodeIncoming . folded . connectNode) n
        neighborsOut = toListOf (nodeOutgoing . folded . connectNode) n
        neighbors = ordNub (neighborsIn ++ neighborsOut)
    liftIO $ forM_ neighbors $ withSerializedNode (delIn . delOut) dir
    convertError @UserErrors . trapIOError' . removeFile $ linksFile dir nid
    let ignoreErrors = (`handleError` \(_ :: UserErrors) -> pure ())
    ignoreErrors . trapIOError' . removeFile $ nodeDataFile dir nid
  InsertEdge e -> do
    dual <- input @IsDual
    let (Edge i t o) = ifDualized dual G.dualizeEdge e
    runWriteGraphIODualizeable @t dir (touchNode @t i)
    runWriteGraphIODualizeable @t dir (touchNode @t o)
    liftIO $ withSerializedNode (nodeOutgoing %~ insertSet (Connect t o)) dir i
    liftIO $ withSerializedNode (nodeIncoming %~ insertSet (Connect t i)) dir o
  DeleteEdge e -> do
    dual <- input @IsDual
    let (Edge i t o) = ifDualized dual G.dualizeEdge e
    liftIO $ withSerializedNode (nodeOutgoing %~ deleteSet (Connect t o)) dir i
    liftIO $ withSerializedNode (nodeIncoming %~ deleteSet (Connect t i)) dir o
  SetData nid d -> liftIO $ withSerializedNode (nodeData .~ d :: Node t -> Node t) dir nid

-- | Run both the Dualizeable effect and the WriteGraph in IO
-- The default state of all graphs stored on disk is that they are not dual
-- thus we simply set IsDual False as our initial state parameter for
-- Dualizeable.
runWriteGraphDualizeableIO ::
  forall t effs.
  ( Member (Embed IO) effs,
    Members [Dualizeable, ThrowUserError, Warn UserErrors] effs,
    FromJSON (Node t),
    ToJSON (Node t),
    TransitionValid t
  ) =>
  FilePath ->
  Sem (WriteGraph t : effs) ~> Sem effs
runWriteGraphDualizeableIO dir = runReaderAsState . runWriteGraphIODualizeable dir