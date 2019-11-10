{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE AllowAmbiguousTypes #-}

module Effect.Graph
  ( module Effect.Graph
  , NID
  , Node
  ) where

import ClassyPrelude hiding (Reader, ask)

import Control.Lens

import Control.Monad.Freer
import Control.Monad.Freer.State
import Control.Monad.Freer.Error
import Control.Monad.Freer.Reader
import Effect.Util
import Effect.Warn
import qualified Data.Set as Set
import Text.Read (readMaybe)

import UserError

import System.Directory (doesFileExist, listDirectory, removeFile)
import System.FilePath (dropExtension)

import Graph.Types

import Graph.Serialize2
import qualified Graph as G

import Data.Aeson (FromJSON(..), ToJSON(..))

data ReadGraph t a where
  GetNode :: NID -> ReadGraph t (Maybe (Node t))
  NodeManifest :: ReadGraph t [NID]

newtype IsDual = IsDual { isDual :: Bool }
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

dualize :: Member Dualizeable effs => Eff effs ()
dualize = modify @IsDual (omap not)

data WriteGraph t a where
  TouchNode :: NID -> WriteGraph t () -- ^ make the node with that id exist
  DeleteNode :: NID -> WriteGraph t () -- ^ delete a node and all edges to/from it
  InsertEdge :: Edge t -> WriteGraph t () -- ^ insert edge if both nodes in graph
  DeleteEdge :: Edge t -> WriteGraph t () -- ^ delete edge
  SetData :: NID -> Maybe LByteString -> WriteGraph t () -- ^ delete if Nothing

type HasGraph t effs =
  ( Member (ReadGraph t) effs
  , Member (WriteGraph t) effs
  , TransitionValid t
  )

getNode :: Member (ReadGraph t) effs => NID -> Eff effs (Maybe (Node t))
getNode nid = send (GetNode nid)

nodeManifest :: forall t effs. Member (ReadGraph t) effs => Eff effs [NID]
nodeManifest = send (NodeManifest @t)

touchNode :: forall t effs. Member (WriteGraph t) effs => NID -> Eff effs ()
touchNode n = send @(WriteGraph t) @effs (TouchNode @t n)

deleteNode :: forall t effs. Member (WriteGraph t) effs => NID -> Eff effs ()
deleteNode nid = send (DeleteNode @t nid)

insertEdge :: Member (WriteGraph t) effs => Edge t -> Eff effs ()
insertEdge e = send (InsertEdge e)

deleteEdge :: Member (WriteGraph t) effs => Edge t -> Eff effs ()
deleteEdge e = send (DeleteEdge e)

setData
  :: forall t effs. Member (WriteGraph t) effs
  => NID -> Maybe LByteString -> Eff effs ()
setData nid d = send (SetData @t nid d)

-- | Run a graph computation in the io monad, using a directory in the
-- serialization format to access the graph
runReadGraphIO
  :: (MonadIO m, LastMember m (Reader IsDual : effs), LastMember m effs
     , FromJSON (Node t), ToJSON (Node t), TransitionValid t)
  => FilePath -> Eff (ReadGraph t ': effs) ~> Eff effs
runReadGraphIO dir = runReader (IsDual False) . runReadGraphIODualizeable dir

-- | Run a graph computation in the io monad, using a directory in the
-- serialization format to access the graph, while allowing for the
-- possibility of the graph being dualized.
runReadGraphIODualizeable
  :: forall t m effs. (MonadIO m, LastMember m (Reader IsDual : effs)
     , FromJSON (Node t), ToJSON (Node t), TransitionValid t)
  => FilePath -> Eff (ReadGraph t ': effs) ~> Eff (Reader IsDual : effs)
runReadGraphIODualizeable dir = reinterpret $ \case
  GetNode nid -> do
    maybeN <- errorToNothing $
      -- writing type level lists in your code is not fun :(
      -- kids: be careful when you choose haskell
      deserializeNodeF @t @m @(ThrowUserError : Reader IsDual : effs) dir nid
    dual <- ask
    pure $ maybeN <&> ifDualized dual dualizeNode
  NodeManifest -> do
    cs <- liftIO $ listDirectory dir
    let linkFiles = filter (".json" `isSuffixOf`) cs
        nodes = mapMaybe (readMaybe . dropExtension) linkFiles
    pure nodes

-- | Run a graph in IO with the ambient ability for the graph to be
-- dualizeable.
runReadGraphDualizeableIO
  :: forall t m effs. (MonadIO m, LastMember m (Reader IsDual : effs)
     , Member Dualizeable effs
     , FromJSON (Node t), ToJSON (Node t), TransitionValid t)
  => FilePath -> Eff (ReadGraph t ': effs) ~> Eff effs
runReadGraphDualizeableIO dir =
  runReaderAsState
  . runReadGraphIODualizeable dir

-- run Dualizeable as a state that also flips the graph in a state to
-- reflect its value
-- runDualizeableState
--   :: Member (State (Graph t)) effs
--   => Eff (Dualizeable : effs) ~> Eff effs
-- runDualizeableState = interpret $ modify dualize

runDualizeable :: Eff (Dualizeable : effs) ~> Eff effs
runDualizeable = map fst . runState (IsDual False)

runReadGraphState
  :: forall t effs a. (Member (State (Graph t)) effs)
  => Eff (ReadGraph t ': effs) a -> Eff effs a
runReadGraphState = interpret $ \case
  GetNode nid -> G.maybeLookupNode <$> get <*> pure nid
  NodeManifest -> keys . G.nodeMap <$> get @(Graph t)

runWriteGraphState
  :: forall t effs. (Member (State (Graph t)) effs, TransitionValid t)
  => Eff (WriteGraph t ': effs) ~> Eff effs
runWriteGraphState = interpret $ \case
  TouchNode nid -> modify (G.insertNode (G.emptyNode nid) :: Graph t -> Graph t)
  DeleteNode nid -> modify (G.delNode' nid :: Graph t -> Graph t)
  InsertEdge e -> modify (G.insertEdge e)
  DeleteEdge e -> modify (G.delEdge e)
  SetData nid d -> modify (G.setData' d nid :: Graph t -> Graph t)

-- | Run a graph computation in the io monad, using a directory in the
-- serialization format to access the graph
runWriteGraphIO
  :: forall t m effs.
    ( MonadIO m, LastMember m (Reader IsDual : effs)
    , Members [ThrowUserError, Warn UserErrors] effs
    , FromJSON (Node t), ToJSON (Node t), TransitionValid t)
  => FilePath -> Eff (WriteGraph t ': effs) ~> Eff effs
runWriteGraphIO dir = runReader (IsDual False) . runWriteGraphIODualizeable dir

-- | General handler for WriteGraph parameterized so that it is possible
-- to specify if the computation is dualized or not
-- If the computation is dualized, all operations on edges will be inverted.
-- That is InsertEdge (Edge 0 "x" 1) will result in the same effect as
-- InsertEdge (Edge 1 "x" 0) if the IsDual parameter is true.
-- We obtain whether the compuation is dualized through a reader parameter
runWriteGraphIODualizeable
  :: forall t m effs.
    ( MonadIO m, LastMember m (Reader IsDual : effs)
    , Member ThrowUserError effs
    , Member (Warn UserErrors) effs
    , FromJSON (Node t)
    , ToJSON (Node t)
    , TransitionValid t
    )
  => FilePath -> Eff (WriteGraph t : effs) ~> Eff (Reader IsDual : effs)
runWriteGraphIODualizeable dir = reinterpret $ \case
  TouchNode nid -> do
    dfe <- liftIO (doesFileExist (linksFile dir nid))
    if not dfe
       then trapIOError' $ serializeNodeEx (G.emptyNode nid :: Node t) dir
       else pure ()
  DeleteNode nid -> do
    n <- deserializeNodeF @t dir nid
    let del = Set.filter ((/=nid) . view connectNode) :: Set (Connect t) -> Set (Connect t)
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
    dual <- ask
    let (Edge i t o) = ifDualized dual G.dualizeEdge e
    runWriteGraphIODualizeable @t dir (touchNode @t i)
    runWriteGraphIODualizeable @t dir (touchNode @t o)
    liftIO $ withSerializedNode (nodeOutgoing %~ insertSet (Connect t o)) dir i
    liftIO $ withSerializedNode (nodeIncoming %~ insertSet (Connect t i)) dir o
  DeleteEdge e -> do
    dual <- ask
    let (Edge i t o) = ifDualized dual G.dualizeEdge e
    liftIO $ withSerializedNode (nodeOutgoing %~ deleteSet (Connect t o)) dir i
    liftIO $ withSerializedNode (nodeIncoming %~ deleteSet (Connect t i)) dir o
  SetData nid d -> liftIO $ withSerializedNode (nodeData .~ d :: Node t -> Node t) dir nid

-- | Run both the Dualizeable effect and the WriteGraph in IO
-- The default state of all graphs stored on disk is that they are not dual
-- thus we simply set IsDual False as our initial state parameter for
-- Dualizeable.
runWriteGraphDualizeableIO
  :: forall t m effs.
    ( MonadIO m, LastMember m (Reader IsDual : effs), LastMember m effs
    , Members [Dualizeable, ThrowUserError, Warn UserErrors] effs
    , FromJSON (Node t), ToJSON (Node t), TransitionValid t)
  => FilePath -> Eff (WriteGraph t : effs) ~> Eff effs
runWriteGraphDualizeableIO dir =
  runReaderAsState
  . runWriteGraphIODualizeable dir
