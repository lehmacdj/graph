{-# LANGUAGE LambdaCase #-}

module Effect.Graph where

import Control.Monad.Freer
import Control.Monad.Freer.State
import Control.Monad.Freer.Error

import Graph.Types

import Control.Monad.IO.Class

import Graph.Serialize2
import qualified Graph as G

import Error
import Data.Aeson (FromJSON(..), ToJSON(..))

data ReadGraph t a where
  GetNode :: Id -> ReadGraph t (Maybe (Node t))

data WriteGraph t a where
  SetNode :: Node t -> WriteGraph t ()
  DeleteNode :: Id -> WriteGraph t ()
  InsertEdge :: Edge t -> WriteGraph t ()
  DeleteEdge :: Edge t -> WriteGraph t ()

getNode :: Member (ReadGraph t) effs => Id -> Eff effs (Maybe (Node t))
getNode nid = send (GetNode nid)

setNode :: Member (WriteGraph t) effs => Node t -> Eff effs ()
setNode n = send (SetNode n)

deleteNode :: forall t effs. Member (WriteGraph t) effs => Id -> Eff effs ()
deleteNode nid = send (DeleteNode nid :: WriteGraph t ())

insertEdge :: Member (WriteGraph t) effs => Edge t -> Eff effs ()
insertEdge e = send (InsertEdge e)

deleteEdge :: Member (WriteGraph t) effs => Edge t -> Eff effs ()
deleteEdge e = send (DeleteEdge e)

-- | Run a graph computation in the io monad, using a directory in the
-- serialization format to access the graph
runReadGraphIO
  :: (MonadIO m, LastMember m effs
     , FromJSON (Node t), ToJSON (Node t), TransitionValid t)
  => FilePath -> Eff (ReadGraph t ': effs) a -> Eff effs a
runReadGraphIO dir = interpret $ \case
  GetNode nid -> do
    n <- liftIO $ deserializeNode dir nid
    pure (eToMaybe n)

runReadGraphState
  :: (Member (State (Graph t)) effs)
  => Eff (ReadGraph t ': effs) a -> Eff effs a
runReadGraphState = interpret $ \case
  GetNode nid -> G.maybeLookupNode <$> get <*> pure nid

runWriteGraphState
  :: forall t effs. (Member (State (Graph t)) effs, TransitionValid t)
  => Eff (WriteGraph t ': effs) ~> Eff effs
runWriteGraphState = interpret $ \case
  SetNode n -> modify (G.insertNode n)
  DeleteNode nid -> modify (G.delNode' nid :: Graph t -> Graph t)
  InsertEdge e -> modify (G.insertEdge e)
  DeleteEdge e -> modify (G.delEdge e)

-- | Run a graph computation in the io monad, using a directory in the
-- serialization format to access the graph
runWriteGraphIO
  :: (MonadIO m, LastMember m effs, Member (Error Errors) effs
     , FromJSON (Node t), ToJSON (Node t), TransitionValid t)
  => FilePath -> Eff (WriteGraph t ': effs) a -> Eff effs a
runWriteGraphIO dir = interpret $ \case
  SetNode node -> do
    -- | TODO: match semantics on modifying/adding edges based on nodes
    -- data, so that we don't end up with huge problems there
    -- This will probably require deserializing a bunch of nodes
    r <- liftIO $ serializeNode node dir
    rethrowE r
  DeleteNode _ ->
    -- | TODO: remove the node from all its neighbors sets and delete the
    -- data files from the directory
    pure ()
  InsertEdge _ -> undefined
  DeleteEdge _ -> undefined
