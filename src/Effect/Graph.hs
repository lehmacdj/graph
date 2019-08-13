{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE ConstraintKinds #-}

module Effect.Graph
  ( module Effect.Graph
  , Id
  , Node
  ) where

import ClassyPrelude

import Control.Lens

import Control.Monad.Freer
import Control.Monad.Freer.State
import Control.Monad.Freer.Error
import qualified Data.Set as Set

import System.Directory (doesFileExist)

import Graph.Types

import Graph.Serialize2
import qualified Graph as G

import Error
import Data.Aeson (FromJSON(..), ToJSON(..))

data ReadGraph t a where
  GetNode :: Id -> ReadGraph t (Maybe (Node t))

data WriteGraph t a where
  TouchNode :: Id -> WriteGraph t () -- ^ make the node with that id exist
  DeleteNode :: Id -> WriteGraph t () -- ^ delete a node and all edges to/from it
  InsertEdge :: Edge t -> WriteGraph t () -- ^ insert edge if both nodes in graph
  DeleteEdge :: Edge t -> WriteGraph t () -- ^ delete edge
  SetData :: Id -> Maybe LByteString -> WriteGraph t () -- ^ delete if Nothing

type HasGraph t effs = (Member (ReadGraph t) effs, Member (WriteGraph t) effs)

getNode :: Member (ReadGraph t) effs => Id -> Eff effs (Maybe (Node t))
getNode nid = send (GetNode nid)

touchNode :: forall t effs. Member (WriteGraph t) effs => Id -> Eff effs ()
touchNode n = send (TouchNode n :: WriteGraph t ())

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
  TouchNode nid -> modify (G.insertNode (G.emptyNode nid) :: Graph t -> Graph t)
  DeleteNode nid -> modify (G.delNode' nid :: Graph t -> Graph t)
  InsertEdge e -> modify (G.insertEdge e)
  DeleteEdge e -> modify (G.delEdge e)
  SetData nid d -> modify (G.setData' d nid :: Graph t -> Graph t)

-- | Run a graph computation in the io monad, using a directory in the
-- serialization format to access the graph
runWriteGraphIO
  :: forall t m effs.
    ( MonadIO m, LastMember m effs, Member (Error Errors) effs
    , FromJSON (Node t), ToJSON (Node t), TransitionValid t)
  => FilePath -> Eff (WriteGraph t ': effs) ~> Eff effs
runWriteGraphIO dir = interpret $ \case
  TouchNode nid -> do
    dfe <- liftIO (doesFileExist (linksFile dir nid))
    if not dfe
       then do
         r <- liftIO $ serializeNode (G.emptyNode nid :: Node t) dir
         rethrowE r
       else pure ()
  DeleteNode nid -> do
    r <- liftIO $ deserializeNode dir nid
    case eToMaybe r of
      Nothing -> pure ()
      Just (n :: Node t) -> do
        let del = Set.filter ((/=nid) . view connectNode) :: Set (Connect t) -> Set (Connect t)
            delIn = over nodeIncoming del
            delOut = over nodeOutgoing del
            neighborsIn = toListOf (nodeIncoming . folded . connectNode) n
            neighborsOut = toListOf (nodeOutgoing . folded . connectNode) n
            neighbors = ordNub (neighborsIn ++ neighborsOut)
        liftIO $ forM_ neighbors $ withSerializedNode (delIn . delOut) dir
  InsertEdge _ -> undefined
  DeleteEdge _ -> undefined
  SetData nid d -> undefined
