module Graph.ResolvePath where

import Models.Graph (Graph, emptyGraph)
import Models.NID
import Models.Path
import MyPrelude
import Graph.GraphMetadataEditing
import Models.Node
import GHC.Records

data MaterializedPathInfo t = MaterializedPathInfo
  { -- | Path that wasn't possible to materialize further from this node
    leftoverPath :: Maybe (Path t),
    -- | NIDs that were "jumped" to by the path
    -- (this happens when Absolute is materialized at this node)
    jumps :: Set NID
  }
  deriving (Show, Eq, Ord, Generic)

leftoverPath ::
  Lens
    (Node' ti to (MaterializedPathInfo t))
    (Node' ti to (MaterializedPathInfo t'))
    (Maybe (Path t))
    (Maybe (Path t'))
leftoverPath = #augmentation . #leftoverPath

jumps :: Lens' (Node' ti to (MaterializedPathInfo t)) (Set NID)
jumps = #augmentation . #jumps

emptyMaterializedPathInfo :: MaterializedPathInfo t
emptyMaterializedPathInfo = MaterializedPathInfo Nothing mempty

instance HasField "leftoverPath" (Node' ti to (MaterializedPathInfo t)) (Maybe (Path t)) where
  getField = (.augmentation.leftoverPath)

instance HasField "jumps" (Node' ti to (MaterializedPathInfo t)) (Set NID) where
  getField = (.augmentation.jumps)

materializedPathTargets :: Node (Either t t) (MaterializedPathInfo t) -> Set NID
materializedPathTargets n = n.jumps <> incomingTargets <> outgoingTargets where
  incomingTargets = toSetOf (#incoming . targeted) n
  outgoingTargets = toSetOf (#outgoing . targeted) n
  -- only NIDs that are "targeted"
  targeted = folded . filtered (has (#transition . _Right)) . #node

-- | Resolve a path to a node into a graph. The resulting graph encodes which
-- transitions were travers_ed by the path + which transitions were not possible
-- to traverse.
materializePathAsGraph ::
  forall t r.
  (Member (GraphMetadataReading t) r,
  ValidTransition t
  ) =>
  NID ->
  Path t ->
  -- | Nothing if passed in node isn't in the graph or Just a graph containing
  -- the node where the value on the node is a path that wasn't possible to
  -- resolve further starting from that node
  Sem r (Maybe (Graph (Either t t) (MaterializedPathInfo t)))
materializePathAsGraph nid path = withEarlyReturn do
  n <- unwrapM $
    getNodeMetadata nid <&> (_Just . #augmentation .~ emptyMaterializedPathInfo)
  let markIncoming ::
        forall ti ti' to a. Ord ti' =>
        (ti -> ti') -> Node' ti to a -> Node' ti' to a
      markIncoming = over $ #incoming . setmapped . #transition
  let markOutgoing ::
        forall ti to to' a. Ord to' =>
        (to -> to') -> Node' ti to a -> Node' ti to' a
      markOutgoing = over $ #outgoing . setmapped . #transition
  case path of
    One -> do
      let n' = n
            & markIncoming Left
            & markOutgoing Left
      pure $ Just $ emptyGraph & at nid ?~ n'
    Zero -> pure Nothing
    Wild -> do
      let n' = n
            & markIncoming Left
            & markOutgoing Right
      pure $ Just $ emptyGraph & at nid ?~ n'
    Literal t -> do
      let n' = n
            & markIncoming Left
            & markOutgoing \t' -> if t == t' then Right t else Left t'
      pure $ Just $ emptyGraph & at nid ?~ n'
    Absolute nid2 -> do
      let n' = n
            & markIncoming Left
            & markOutgoing Left
      n2 <- unwrapMaybeM (getNodeMetadata nid2) do
        returnEarly $ Just $
          emptyGraph & at nid ?~ (n' & leftoverPath ?~ Absolute nid2)
      let n2' = n2
            & markIncoming Left
            & markOutgoing Left
      pure $ Just $
        emptyGraph
          & at nid ?~ (n' & jumps %~ insertSet nid2)
          & at nid2 ?~ (n2' & #augmentation .~ emptyMaterializedPathInfo)
    p1 :/ p2 -> undefined
    p1 :+ p2 -> undefined
    p1 :& p2 -> undefined
