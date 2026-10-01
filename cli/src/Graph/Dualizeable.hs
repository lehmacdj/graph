-- | Viewing the graph with every edge reversed.
module Graph.Dualizeable
  ( IsDual (..),
    ifDualized,
    Dualizeable,
    dualize,
    runDualizeable,
    dualizingGraphMetadataEditing,
  )
where

import Control.Lens
import Effectful.State.Dynamic
import Graph.GraphMetadataEditing
import Models.Edge
import Models.Node
import MyPrelude

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

dualize :: (Dualizeable :> effs) => Eff effs ()
dualize = modify @IsDual (omap not)

runDualizeable :: Eff (Dualizeable : effs) ~> Eff effs
runDualizeable = map fst . runStateLocal (IsDual False)

-- | While the graph is dualized, present every node and edge reversed: nodes
-- read have their incoming and outgoing edges swapped, and edges written are
-- flipped before they reach the graph. Whether the graph is dualized is
-- checked on every operation, so dualizing part way through takes effect
-- immediately.
dualizingGraphMetadataEditing ::
  forall t r a.
  (Members [GraphMetadataEditing' t, Dualizeable] r) =>
  Eff r a ->
  Eff r a
dualizingGraphMetadataEditing = interpose @(GraphMetadataEditing' t) \_ -> \case
  GetNodeMetadata nid -> do
    dual <- get @IsDual
    map (ifDualized dual dualizeNode) <$> getNodeMetadata @t nid
  TouchNode nid -> touchNode @t nid
  DeleteNode nid -> deleteNode @t nid
  InsertEdge e -> do
    dual <- get @IsDual
    insertEdge (ifDualized dual dualizeEdge e)
  DeleteEdge e -> do
    dual <- get @IsDual
    deleteEdge (ifDualized dual dualizeEdge e)
  ListNodes -> listNodes @t
