module Graph.ResolvePath where

import GHC.Records
import Graph.GraphMetadataEditing
import Models.Graph (Graph, emptyGraph, nodesMatchedBy, singletonGraph)
import Models.NID
import Models.Node
import Models.Path
import MyPrelude

data MaterializedPathInfo t = MaterializedPathInfo
  { -- | Path that wasn't possible to materialize further from this node
    leftoverPath :: Maybe (Path t),
    -- | NIDs that were "jumped" to by the path
    -- (this happens when Absolute is materialized at this node)
    jumps :: Set NID,
    -- | Whether this node is a target of the path. We may need to extend this
    -- to include the exact path(s) taken to support more advanced stuff in the
    -- future but I think this is good enough for our purposes for now
    isTarget :: Bool,
    -- | Whether this node is thin. Thin nodes are nodes that were not fetched
    -- from the underlying graph but rather were created due to an edge leading
    -- to them existing
    isThin :: Bool
  }
  deriving (Show, Eq, Ord, Generic)

instance (Show t) => ShowableAugmentation (MaterializedPathInfo t) where
  defaultShowAugmentation = Just ("materializedPathInfo", tshow)

instance HasField "leftoverPath" (Node' ti to (MaterializedPathInfo t)) (Maybe (Path t)) where
  getField = (.augmentation.leftoverPath)

instance HasField "jumps" (Node' ti to (MaterializedPathInfo t)) (Set NID) where
  getField = (.augmentation.jumps)

instance HasField "isTarget" (Node' ti to (MaterializedPathInfo t)) Bool where
  getField = (.augmentation.isTarget)

instance HasField "isThin" (Node' ti to (MaterializedPathInfo t)) Bool where
  getField = (.augmentation.isThin)

instance Semigroup (MaterializedPathInfo t) where
  MaterializedPathInfo p1 j1 ta1 th1 <> MaterializedPathInfo p2 j2 ta2 th2 =
    MaterializedPathInfo (liftA2 (:+) p1 p2) (j1 <> j2) (ta1 || ta2) (th1 || th2)

instance DefaultAugmentation (MaterializedPathInfo t) where
  defaultAugmentation = MaterializedPathInfo Nothing mempty False True

instance Monoid (MaterializedPathInfo t) where
  mempty = defaultAugmentation

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

targetsInGraph :: Fold (Graph t1 (MaterializedPathInfo t2)) NID
targetsInGraph = #nodeMap . traverse . filtered (view isTarget) . #nid

-- transitions were travers_ed by the path + which transitions were not possible
-- to traverse.
materializePathAsGraph ::
  forall t r.
  ( Member (GraphMetadataReading t) r,
    ValidTransition t
  ) =>
  NID ->
  Path t ->
  -- | Nothing if passed in node isn't in the graph or Just a graph containing
  -- the node where the value on the node is a path that wasn't possible to
  -- resolve further starting from that node
  Sem r (Graph t (MaterializedPathInfo t))
materializePathAsGraph nid path = do
  case path of
    One -> pure $ singletonGraph (emptyNode nid & isTarget .~ True)
    Zero -> pure emptyGraph
    Wild -> withEarlyReturn do
      n <- onNothingM (getNodeMetadata nid) $ returnEarly emptyGraph
      let n' :: Node t (MaterializedPathInfo t)
          n' = n & #augmentation .~ ((mempty :: MaterializedPathInfo t) & #isThin .~ False)
          g = emptyGraph & at nid ?~ n'
          outgoingNids = toSetOf (#outgoing . folded . #node) n
          g' = g & nodesMatchedBy (#nid . filtered (`member` outgoingNids)) . isTarget .~ True
      pure g'
    Literal t -> withEarlyReturn do
      n <- onNothingM (getNodeMetadata nid) $ returnEarly emptyGraph
      let n' :: Node t (MaterializedPathInfo t)
          n' = n & #augmentation .~ traceShowId ((mempty :: MaterializedPathInfo t) & #isThin .~ False)
          g = traceShowId (emptyGraph & at nid ?~ traceShowId n')
      traceM $ "literal: " <> show g
      let matchedNids = toSetOf (#outgoing . folded . filteredBy (#transition . only t) . #node) n'
      traceM $ "matched nids: " <> show matchedNids
      let g' = g & nodesMatchedBy (#nid . filtered (`member` matchedNids)) . isTarget .~ True
      pure g'
    Absolute targetNid -> pure $ singletonGraph (emptyNode targetNid & isTarget .~ True)

-- p1 :/ p2 -> do
--   g1 <- unwrapM $ materializePathAsGraph nid p1
--   let g1Targets = toList $ toSetOf targetsInGraph g1
--       g1Targetless = g1 & otraverse . isTarget .~ False
--   g2s <- forM (toList g1Targets) \nid1 -> do
--     materialized <- materializePathAsGraph nid1 p2
--     pure $ unwrapEx "impossible: already fetched targets" materialized
--   pure $ Just $ g1Targetless <> mconcat g2s
-- p1 :+ p2 -> do
--   g1 <- materializePathAsGraph nid p1
--   g2 <- materializePathAsGraph nid p2
--   pure $ g1 <> g2
-- p1 :& p2 -> do
--   g1 <- materializePathAsGraph nid p1
--   g2 <- materializePathAsGraph nid p2
--   let g1Targets = toSetOf (_Just . targetsInGraph) g1
--       g2Targets = toSetOf (_Just . targetsInGraph) g2
--       targets = g1Targets `intersection` g2Targets
--   pure
--     $ (g1 <> g2)
--     & (_Just . otraverse . isTarget .~ False)
--     & (_Just . nodesMatchedBy (#nid . filtered (`member` targets)) . isTarget .~ True)
