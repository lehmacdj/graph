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

-- | Apply edits to a node that may not exist: 'Touch' and inserting an edge
-- create it (with the given augmentation), 'Delete' deletes it, and deleting
-- an edge of a node that doesn't exist does nothing. A node can be deleted
-- and then created again.
applyEditsFrom ::
  forall t a.
  (ValidTransition t) =>
  NID ->
  -- | Augmentation for a created node
  a ->
  Seq (NodeEdit t) ->
  Maybe (Node t a) ->
  Maybe (Node t a)
applyEditsFrom nid a edits m_node = foldl' (flip applyEdit) m_node edits
  where
    created = Just . fromMaybe (emptyNode' nid a)
    applyEdit :: NodeEdit t -> Maybe (Node t a) -> Maybe (Node t a)
    applyEdit = \case
      Touch -> created
      Delete -> const Nothing
      DeleteIncoming c -> fmap (#incoming %~ deleteSet c)
      DeleteOutgoing c -> fmap (#outgoing %~ deleteSet c)
      InsertIncoming c -> fmap (#incoming %~ insertSet c) . created
      InsertOutgoing c -> fmap (#outgoing %~ insertSet c) . created

applyEdits ::
  forall t a.
  (ValidTransition t) =>
  Seq (NodeEdit t) ->
  Node t a ->
  Maybe (Node t a)
applyEdits edits node =
  applyEditsFrom node.nid node.augmentation edits (Just node)

applyEditsCreatingNonExistent ::
  forall t a.
  (ValidTransition t, DefaultAugmentation a) =>
  Seq (NodeEdit t) ->
  NID ->
  Maybe (Node t a) ->
  Maybe (Node t a)
applyEditsCreatingNonExistent edits nid =
  applyEditsFrom nid defaultAugmentation edits

applyEditsPreservingDeleted ::
  forall t a.
  (ValidTransition t) =>
  Seq (NodeEdit t) ->
  Node t a ->
  Node t a
applyEditsPreservingDeleted edits node =
  fromMaybe
    (emptyNode' node.nid node.augmentation)
    (applyEdits edits node)
