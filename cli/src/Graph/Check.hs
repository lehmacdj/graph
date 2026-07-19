{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE TemplateHaskell #-}

module Graph.Check where

import Control.Lens
import Effect.IOWrapper.Echo
import Error.Missing
import Graph.Effect
import Graph.Utils
import Models.Connect
import Models.Edge
import MyPrelude

data Direction = In | Out
  deriving (Eq, Ord)

dirToLens :: Direction -> Lens' (Node t a) (Set (Connect t))
dirToLens = \case
  In -> #incoming
  Out -> #outgoing

data ReportMissing t :: Effect where
  NodeMissing :: NID -> ReportMissing t m ()
  ConnectMissing :: Direction -> NID -> Connect t -> ReportMissing t m ()

makeEffect ''ReportMissing

deriving instance (Eq t) => Eq (ReportMissing t m r)

deriving instance (Ord t) => Ord (ReportMissing t m r)

instance (Show t) => Show (ReportMissing t m r) where
  show = \case
    NodeMissing nid -> "node " ++ show nid ++ " is missing from the graph"
    ConnectMissing d nid' (Connect t nid) ->
      show nid
        ++ " has a"
        ++ dir1
        ++ " edge "
        ++ show t
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
  (Echo :> effs, Show t) =>
  Eff (ReportMissing t : effs) ~> Eff effs
reportToConsole = interpret \_ -> \case
  c@NodeMissing {} -> echo $ show c
  c@ConnectMissing {} -> echo $ show c

dirToCombineEdges :: Direction -> NID -> Connect t -> Edge t
dirToCombineEdges d nid c
  | d == In = incomingEdge c nid
  | otherwise = outgoingEdge nid c

fixErrors ::
  forall t effs.
  (WriteGraph t :> effs, ValidTransition t) =>
  Eff (ReportMissing t : effs) ~> Eff effs
fixErrors = interpret \_ -> \case
  NodeMissing nid -> touchNode @t nid
  ConnectMissing d nid c@(Connect _ nid') -> do
    touchNode @t nid
    touchNode @t nid'
    insertEdge (dirToCombineEdges d nid c)

fsck ::
  forall t effs.
  (ReportMissing t :> effs, HasGraph t effs) =>
  Eff effs ()
fsck = do
  ids <- nodeManifest @t
  forM_ ids $ checkNode @t

reportMissingNode ::
  forall t effs.
  (ReportMissing t :> effs) =>
  Eff (Error Missing : effs) () ->
  Eff effs ()
reportMissingNode e = handleError e $ \case
  Missing nid _ -> nodeMissing @t nid

checkConnectExists ::
  forall t effs.
  (ReportMissing t :> effs, HasGraph t effs) =>
  Direction ->
  -- | nid of the node to check
  NID ->
  -- | connect to ensure existence of
  Connect t ->
  Eff effs ()
checkConnectExists dir nid c = reportMissingNode @t $ do
  n <- getNodeDatalessSem @t nid
  if has (dirToLens dir . folded . only c) n
    then pure ()
    else connectMissing dir nid c

swapConnect :: NID -> Connect t -> (NID, Connect t)
swapConnect nid (Connect t nid') = (nid', Connect t nid)

checkNode ::
  forall t effs.
  (ReportMissing t :> effs, HasGraph t effs) =>
  NID ->
  Eff effs ()
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
