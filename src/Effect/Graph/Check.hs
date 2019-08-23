{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE AllowAmbiguousTypes #-}

module Effect.Graph.Check where

import MyPrelude

import Control.Monad.Freer

import Control.Lens

import Graph hiding (insertEdge, insertNode, setData)

import Effect.Graph
import Effect.Console
import Effect.Throw
import Effect.Graph.Advanced

data Direction = In | Out
  deriving (Eq, Ord)

dirToLens :: Direction -> Lens' (Node t) (Set (Connect t))
dirToLens = \case
  In -> nodeIncoming
  Out -> nodeOutgoing

data ReportMissing t r where
  NodeMissing :: Id -> ReportMissing t ()
  ConnectMissing :: Direction -> Id -> Connect t -> ReportMissing t ()
deriving instance Eq t => Eq (ReportMissing t r)
deriving instance Ord t => Ord (ReportMissing t r)

instance Show t => Show (ReportMissing t r) where
  show = \case
    NodeMissing nid -> "node " ++ show nid ++ " is missing from the graph"
    ConnectMissing d nid' (Connect t nid) ->
      show nid ++ " has a" ++ dir ++ " edge " ++ show t
      ++ " to " ++ show nid'
      ++ " but the opposite edge doesn't exist"
        where
          dir
            | d == In = "n incoming"
            | otherwise = " outgoing"

reportToConsole
  :: forall t effs. (Member Console effs, Show t)
  => Eff (ReportMissing t : effs) ~> Eff effs
reportToConsole = interpret $ \case
  c@(NodeMissing{}) -> echo $ show c
  c@(ConnectMissing{}) -> echo $ show c

fixErrors
  :: forall t effs. (Member (WriteGraph t) effs, TransitionValid t)
  => Eff (ReportMissing t : effs) ~> Eff effs
fixErrors = interpret $ \case
  NodeMissing nid -> touchNode @t nid
  ConnectMissing _ nid (Connect t nid') -> do
    touchNode @t nid
    touchNode @t nid'
    insertEdge (Edge nid t nid')
    insertEdge (Edge nid' t nid)

nodeMissing
  :: forall t effs. Member (ReportMissing t) effs
  => Id -> Eff effs ()
nodeMissing nid = send (NodeMissing @t nid)

connectMissing
  :: forall t effs. Member (ReportMissing t) effs
  => Direction -> Id -> Connect t -> Eff effs ()
connectMissing d nid c = send (ConnectMissing d nid c)

fsck
  :: forall t effs. (Member (ReportMissing t) effs, HasGraph t effs)
  => Eff effs ()
fsck = do
  ids <- nodeManifest @t
  forM_ ids $ checkNode @t

reportMissingNode
  :: forall t effs. Member (ReportMissing t) effs
  => Eff (ThrowMissing : effs) () -> Eff effs ()
reportMissingNode e = handleError e $ \case
  Missing nid -> nodeMissing @t nid

checkConnectExists
  :: forall t effs. (Member (ReportMissing t) effs, HasGraph t effs)
  => Direction
  -> Id -- ^ nid of the node to check
  -> Connect t -- ^ connect to ensure existence of
  -> Eff effs ()
checkConnectExists dir nid c = reportMissingNode @t $ do
  n <- getNode' @t nid
  if has ((dirToLens dir) . folded . only c) n
     then pure ()
     else connectMissing dir nid c

swapConnect :: Id -> Connect t -> (Id, Connect t)
swapConnect nid (Connect t nid') = (nid', Connect t nid)

checkNode
  :: forall t effs. (Member (ReportMissing t) effs, HasGraph t effs)
  => Id -> Eff effs ()
checkNode nid = reportMissingNode @t $ do
  -- every node needs to exist (error handled above)
  n <- getNode' @t nid
  -- every outgoing edge needs to be present in the corresponding node as incoming
  traverseOf_
    (nodeOutgoing . folded . to (swapConnect nid))
    (uncurry (checkConnectExists In))
    n
  -- every incoming edge needs to be present in the corresponding node as outgoing
  traverseOf_
    (nodeIncoming . folded . to (swapConnect nid))
    (uncurry (checkConnectExists Out))
    n
