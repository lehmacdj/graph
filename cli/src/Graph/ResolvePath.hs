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
    jumps :: Set NID,
    -- | Whether this node is a target of the path. We may need to extend this
    -- to include the exact path(s) taken to support more advanced stuff in the
    -- future but I think this is good enough for our purposes for now
    isTarget :: Bool
  }
  deriving (Show, Eq, Ord, Generic)

instance HasField "leftoverPath" (Node' ti to (MaterializedPathInfo t)) (Maybe (Path t)) where
  getField = (.augmentation.leftoverPath)

instance HasField "jumps" (Node' ti to (MaterializedPathInfo t)) (Set NID) where
  getField = (.augmentation.jumps)

instance HasField "isTarget" (Node' ti to (MaterializedPathInfo t)) Bool where
  getField = (.augmentation.isTarget)

leftoverPath ::
  Lens
    (Node' ti to (MaterializedPathInfo t))
    (Node' ti to (MaterializedPathInfo t'))
    (Maybe (Path t))
    (Maybe (Path t'))
leftoverPath = #augmentation . #leftoverPath

jumps :: Lens' (Node' ti to (MaterializedPathInfo t)) (Set NID)
jumps = #augmentation . #jumps

isTarget :: Lens' (Node' ti to (MaterializedPathInfo t)) Bool
isTarget = #augmentation . #isTarget

emptyMaterializedPathInfo :: MaterializedPathInfo t
emptyMaterializedPathInfo = MaterializedPathInfo Nothing mempty False

targetsOfNode :: Fold (Node (Either t t) (MaterializedPathInfo t)) NID
targetsOfNode = jumps . folded <> incomingTargets <> outgoingTargets where
  incomingTargets = #incoming . targeted
  outgoingTargets = #outgoing . targeted
  -- only NIDs that are "targeted"
  targeted = folded . filtered (has (#transition . _Right)) . #node

targetsInGraph :: Fold (Graph (Either t t) (MaterializedPathInfo t)) NID
targetsInGraph = #nodeMap . traverse .  filtered (view isTarget) . #nid

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
    p1 :/ p2 -> do
      g1 <- unwrapM $ materializePathAsGraph nid p1
      let g1Targets = toSetOf targetsInGraph g1
      g2s <- forM (toList g1Targets) \nid1 ->
        materialized <- materializePathAsGraph nid1 p2
        g2 <- unwrapEx "impossible: already fetched targets" materialized
        -- TODO: merge g1 with g2s as well; concatenating the results from a
        -- node onto the end of the node that they were visited from
        pure g2
      pure $ mconcat g2s
    p1 :+ p2 ->
      -- definining this seems fairly obvious; though maybe not even that obvious
      undefined
    p1 :& p2 ->
      -- defining this seems pretty hard; how do we figure out if a transition was "visited" in the final graph
      -- it seems like we might need to keep track of the complete history over which the path was resolved
      -- so that we don't lose information we need to produce the appropriate intersection here
      undefined
