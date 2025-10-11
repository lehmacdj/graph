module Models.Augmentation.NodeEdits where

import Models.Common
import Models.Connect
import Models.Node
import MyPrelude

newtype NodeEdits t = NodeEdits {edits :: Seq (NodeEdit t)}
  deriving stock (Eq, Show, Ord, Generic)
  deriving anyclass (NFData)
  deriving newtype (MonoFoldable)

type instance Element (NodeEdits t) = NodeEdit t

appendEdit :: NodeEdit t -> NodeEdits t -> NodeEdits t
appendEdit e (NodeEdits es) = NodeEdits (es `snoc` e)

data NodeEdit t
  = Touch
  | Delete
  | DeleteIncoming (Connect t)
  | DeleteOutgoing (Connect t)
  | InsertIncoming (Connect t)
  | InsertOutgoing (Connect t)
  deriving stock (Eq, Show, Ord, Generic)
  deriving anyclass (NFData)

instance (Show t) => ShowableAugmentation (NodeEdits t) where
  augmentationProperties x = [(Just "edits", tshow (toList x.edits))]

instance DefaultAugmentation (NodeEdits t) where
  defaultAugmentation = NodeEdits mempty

applyEdits ::
  forall t a.
  (ValidTransition t, DefaultAugmentation a) =>
  Seq (NodeEdit t) ->
  Node t a ->
  Node t a
applyEdits edits node = foldl' (flip applyEdit) node edits

applyEdit ::
  forall a t.
  (DefaultAugmentation a, ValidTransition t) =>
  NodeEdit t ->
  Node t a ->
  Node t a
applyEdit Touch = id
applyEdit Delete =
  (#incoming .~ mempty @(Set (Connect t)))
    . (#outgoing .~ mempty @(Set (Connect t)))
applyEdit (DeleteIncoming c) = #incoming %~ deleteSet c
applyEdit (DeleteOutgoing c) = #outgoing %~ deleteSet c
applyEdit (InsertIncoming c) = #incoming %~ insertSet c
applyEdit (InsertOutgoing c) = #outgoing %~ insertSet c
