module Models.Augmentation.NodeEdits where

import Models.Common
import Models.Connect
import Models.NID
import Models.Node
import MyPrelude

newtype NodeEdits t = NodeEdits {edits :: Seq (NodeEdit t)}
  deriving stock (Eq, Show, Ord, Generic)
  deriving anyclass (NFData)
  deriving newtype (MonoFoldable)

type instance Element (NodeEdits t) = NodeEdit t

appendEdit :: NodeEdit t -> NodeEdits t -> NodeEdits t
-- we can tombstone when deleting until another operation comes along to add
-- more edits afterwards
appendEdit Delete _ = NodeEdits $ singleton Delete
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
  Maybe (Node t a)
applyEdits edits node = foldM (flip applyEdit) node edits

applyEditsCreatingNonExistent ::
  forall t a.
  (ValidTransition t, DefaultAugmentation a) =>
  Seq (NodeEdit t) ->
  NID ->
  Maybe (Node t a) ->
  Maybe (Node t a)
applyEditsCreatingNonExistent edits nid m_node =
  applyEdits edits (fromMaybe (emptyNode' nid defaultAugmentation) m_node)

applyEditsPreservingDeleted ::
  forall t a.
  (ValidTransition t, DefaultAugmentation a) =>
  Seq (NodeEdit t) ->
  Node t a ->
  Node t a
applyEditsPreservingDeleted edits node =
  fromMaybe
    (emptyNode' node.nid node.augmentation)
    (applyEdits edits node)

applyEdit ::
  forall a t.
  (DefaultAugmentation a, ValidTransition t) =>
  NodeEdit t ->
  Node t a ->
  Maybe (Node t a)
applyEdit Touch = Just
applyEdit Delete = const Nothing
applyEdit (DeleteIncoming c) = Just . (#incoming %~ deleteSet c)
applyEdit (DeleteOutgoing c) = Just . (#outgoing %~ deleteSet c)
applyEdit (InsertIncoming c) = Just . (#incoming %~ insertSet c)
applyEdit (InsertOutgoing c) = Just . (#outgoing %~ insertSet c)
