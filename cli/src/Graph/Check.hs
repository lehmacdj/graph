{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE ViewPatterns #-}

module Graph.Check where

import Control.Lens
import Effect.Console
import Graph.Effect
import Graph.Utils
import Models.Graph hiding (insertEdge, insertNode, setData)
import MyPrelude
import UserError

data Direction = In | Out
  deriving (Eq, Ord)

dirToLens :: Direction -> Lens' (Node t) (Set (Connect t))
dirToLens = \case
  In -> #incoming
  Out -> #outgoing

data ReportMissing t m r where
  NodeMissing :: NID -> ReportMissing t m ()
  ConnectMissing :: Direction -> NID -> Connect t -> ReportMissing t m ()

makeSem ''ReportMissing

deriving instance Eq t => Eq (ReportMissing t m r)

deriving instance Ord t => Ord (ReportMissing t m r)

instance Show t => Show (ReportMissing t m r) where
  show = \case
    NodeMissing nid -> "node " ++ show nid ++ " is missing from the graph"
    ConnectMissing d nid' (Connect t nid) ->
      show nid ++ " has a" ++ dir1 ++ " edge " ++ show t
        ++ " "
        ++ dir2
        ++ " "
        ++ show nid'
        ++ " but the corresponding "
        ++ dir3
        ++ " one doesn't exist"
      where
        (dir1, dir2, dir3)
          | d == In = (" outgoing", "to", "incoming")
          | otherwise = ("n incoming", "from", "outgoing")

reportToConsole ::
  forall t effs.
  (Member Console effs, Show t) =>
  Sem (ReportMissing t : effs) ~> Sem effs
reportToConsole = interpret $ \case
  c@NodeMissing {} -> echo $ show c
  c@ConnectMissing {} -> echo $ show c

dirToCombineEdges :: Direction -> NID -> Connect t -> Edge t
dirToCombineEdges d nid c
  | d == In = incomingEdge c nid
  | otherwise = outgoingEdge nid c

fixErrors ::
  forall t effs.
  (Member (WriteGraph t) effs, TransitionValid t) =>
  Sem (ReportMissing t : effs) ~> Sem effs
fixErrors = interpret $ \case
  NodeMissing nid -> touchNode @t nid
  ConnectMissing d nid c@(Connect _ nid') -> do
    touchNode @t nid
    touchNode @t nid'
    insertEdge (dirToCombineEdges d nid c)

fsck ::
  forall t effs.
  (Member (ReportMissing t) effs, HasGraph t effs) =>
  Sem effs ()
fsck = do
  ids <- nodeManifest @t
  forM_ ids $ checkNode @t

reportMissingNode ::
  forall t effs.
  Member (ReportMissing t) effs =>
  Sem (Error Missing : effs) () ->
  Sem effs ()
reportMissingNode e = handleError e $ \case
  Missing nid -> nodeMissing @t nid

checkConnectExists ::
  forall t effs.
  (Member (ReportMissing t) effs, HasGraph t effs) =>
  Direction ->
  -- | nid of the node to check
  NID ->
  -- | connect to ensure existence of
  Connect t ->
  Sem effs ()
checkConnectExists dir nid c = reportMissingNode @t $ do
  n <- getNodeDatalessSem @t nid
  if has (dirToLens dir . folded . only c) n
    then pure ()
    else connectMissing dir nid c

swapConnect :: NID -> Connect t -> (NID, Connect t)
swapConnect nid (Connect t nid') = (nid', Connect t nid)

checkNode ::
  forall t effs.
  (Member (ReportMissing t) effs, HasGraph t effs) =>
  NID ->
  Sem effs ()
checkNode nid = reportMissingNode @t $ do
  -- every node needs to exist (error handled above)
  n <- getNodeDatalessSem @t nid
  -- every outgoing edge needs to be present in the corresponding node as incoming
  traverseOf_
    (#outgoing . folded . to (swapConnect nid))
    (uncurry (checkConnectExists In))
    n
  -- every incoming edge needs to be present in the corresponding node as outgoing
  traverseOf_
    (#incoming . folded . to (swapConnect nid))
    (uncurry (checkConnectExists Out))
    n
