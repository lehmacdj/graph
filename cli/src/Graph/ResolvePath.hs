module Graph.ResolvePath where

import GHC.Records
import Graph.FreshNID
import Graph.GraphMetadataEditing
import Models.Connect (Connect (..))
import Models.Graph (Graph, emptyGraph, insertNode, nodesMatchedBy, nodesMatching, singletonGraph)
import Models.NID
import Models.Node
import Models.Path
import MyPrelude hiding ((\\))

data MaterializedPathInfo t = MaterializedPathInfo
  { -- | NIDs that were "jumped" to by the path from this node
    -- (this happens when Absolute is materialized at this node)
    jumps :: Set NID,
    -- | Whether this node is a target of the path. We may need to extend this
    -- to include the exact path(s) taken to support more advanced stuff in the
    -- future but I think this is good enough for our purposes for now
    isTarget :: Bool,
    -- | Whether this node is thin. Thin nodes are nodes that were not fetched
    -- from the underlying graph but rather were created due to an edge leading
    -- to them existing
    -- Invariant: a node may not be new & thin at the same time.
    isThin :: Bool,
    -- | Whether this node is new. New nodes are nodes that are added to the
    -- graph while resolving the path to represent parts of the path that
    -- don't exist in the underlying graph.
    -- Invariant: a node may not be new & thin at the same time.
    isNew :: Bool
  }
  deriving (Show, Eq, Ord, Generic)

instance (Show t) => ShowableAugmentation (MaterializedPathInfo t) where
  augmentationLabel = "materializedPathInfo"
  defaultShowAugmentation = tshow
  shouldShowStandaloneAugmentation = True

instance HasField "jumps" (Node' ti to (MaterializedPathInfo t)) (Set NID) where
  getField = (.augmentation.jumps)

instance HasField "isTarget" (Node' ti to (MaterializedPathInfo t)) Bool where
  getField = (.augmentation.isTarget)

instance HasField "isThin" (Node' ti to (MaterializedPathInfo t)) Bool where
  getField = (.augmentation.isThin)

instance HasField "isNew" (Node' ti to (MaterializedPathInfo t)) Bool where
  getField = (.augmentation.isNew)

instance DefaultAugmentation (MaterializedPathInfo t) where
  defaultAugmentation = MaterializedPathInfo mempty False True False

errorIfMergingNew :: Bool -> Bool -> Bool
errorIfMergingNew n1 n2
  | n1 || n2 = error "Cannot merge nodes that are new"
  | otherwise = False

instance Semigroup (MaterializedPathInfo t) where
  MaterializedPathInfo j1 ta1 th1 n1 <> MaterializedPathInfo j2 ta2 th2 n2 =
    MaterializedPathInfo (j1 <> j2) (ta1 || ta2) (th1 && th2) (errorIfMergingNew n1 n2)

instance Monoid (MaterializedPathInfo t) where
  mempty = defaultAugmentation

instance MonoidAugmentation (MaterializedPathInfo t)

-- | Version of 'MaterializedPathInfo' that intersects targets instead of
-- unioning them
newtype IntersectingTargets t = IntersectingTargets
  { underlying :: MaterializedPathInfo t
  }
  deriving (Show, Eq, Ord, Generic)

instance forall t. DefaultAugmentation (IntersectingTargets t) where
  defaultAugmentation =
    IntersectingTargets
      ((defaultAugmentation @(MaterializedPathInfo t)) & #isTarget .~ True)

instance Semigroup (IntersectingTargets t) where
  IntersectingTargets (MaterializedPathInfo j1 ta1 th1 n1)
    <> IntersectingTargets (MaterializedPathInfo j2 ta2 th2 n2) =
      IntersectingTargets
        (MaterializedPathInfo (j1 <> j2) (ta1 && ta2) (th1 && th2) (errorIfMergingNew n1 n2))

instance Monoid (IntersectingTargets t) where
  mempty = defaultAugmentation

instance MonoidAugmentation (IntersectingTargets t)

jumps :: Lens' (Node' ti to (MaterializedPathInfo t)) (Set NID)
jumps = #augmentation . #jumps

isTarget :: Lens' (Node' ti to (MaterializedPathInfo t)) Bool
isTarget = #augmentation . #isTarget

isThin :: Lens' (Node' ti to (MaterializedPathInfo t)) Bool
isThin = #augmentation . #isThin

isNew :: Lens' (Node' ti to (MaterializedPathInfo t)) Bool
isNew = #augmentation . #isNew

targetsInGraph :: Fold (Graph t1 (MaterializedPathInfo t2)) NID
targetsInGraph = #nodeMap . traverse . filtered (view isTarget) . #nid

-- Transitions were traversed by the path + which transitions were not possible
-- to traverse. See 'MaterializedPathInfo' for details on the info returned by
-- this function.
-- This minimally accesses the underlying graph to get the metadata necessary
-- to materialize the path. The resulting graph may require some additional
-- accesses to the underlying graph to access full metadata for the nodes
-- returned (such nodes are marked as thin in the resulting graph).
materializePathAsGraph ::
  forall t r.
  ( Members [GraphMetadataReading t, FreshNID] r,
    ValidTransition t
  ) =>
  NID ->
  Path t ->
  -- | TODO bring back (Either t t) as the type of the transition to be able to
  -- mark whether edges are new or were part of the original graph
  Sem r (Graph t (MaterializedPathInfo t))
materializePathAsGraph nid path = do
  case path of
    One -> pure $ singletonGraph (emptyNode nid & isTarget .~ True)
    Zero -> pure emptyGraph
    Wild -> withEarlyReturn do
      n <- onNothingM (getNodeMetadata nid) $ returnEarly emptyGraph
      let n' = n & #augmentation .~ (mempty @(MaterializedPathInfo t) & #isThin .~ False)
          g = emptyGraph & at nid ?~ n'
          outgoingNids = toSetOf (#outgoing . folded . #node) n
          g' = g & nodesMatchedBy (#nid . filtered (`member` outgoingNids)) . isTarget .~ True
      pure g'
    Literal t -> withEarlyReturn do
      n <- onNothingM (getNodeMetadata nid) $ returnEarly emptyGraph
      let n' = n & #augmentation .~ (mempty @(MaterializedPathInfo t) & #isThin .~ False)
          g = emptyGraph & at nid ?~ n'
      let matchedNids = toSetOf (#outgoing . folded . filteredBy (#transition . only t) . #node) n'
          noMatches = null matchedNids
      unless noMatches . returnEarly $
        g & nodesMatchedBy (#nid . filtered (`member` matchedNids)) . isTarget .~ True
      newNID <- freshNID
      let newNode =
            emptyNode newNID
              & isNew .~ True
              & isTarget .~ True
              -- TODO: make edge left-ed
              & #incoming .~ singletonSet (Connect t nid)
      pure $ insertNode newNode g
    Absolute targetNid ->
      pure $
        singletonGraph (emptyNode nid & jumps .~ singletonSet targetNid)
          <> singletonGraph (emptyNode targetNid & isTarget .~ True)
    p1 :/ p2 -> do
      g1 <- materializePathAsGraph nid p1
      let g1Targets = toList $ toSetOf targetsInGraph g1
          g1Targetless = g1 & otraverse . isTarget .~ False

      -- TODO: run this materializePathAsGraph in an environment where
      -- we use the already materialized graph as a cache to avoid duplicating
      -- things + be able to add on to the underlying graph
      -- I guess this might take rearchitecting this function to take the
      -- graph as an accumulator + have a version that starts with an empty
      -- accumulator as the entry point from outside this module?
      -- it is not ok to generate the same node twice, because we need to avoid creating duplicated paths
      -- so we don't accidentally create too many nodes
      g2s <- forM (toList g1Targets) (`materializePathAsGraph` p2)

      pure $ g1Targetless <> mconcat g2s
    p1 :+ p2 -> do
      g1 <- materializePathAsGraph nid p1
      -- it is not ok to generate the same node twice, because we need to avoid creating duplicated paths
      -- so we don't accidentally create too many nodes
      g2 <- materializePathAsGraph nid p2
      pure $ g1 <> g2
    p1 :& p2 -> do
      g1 <- materializePathAsGraph nid p1
      -- it is not ok to generate the same node twice, because we need to avoid creating duplicated paths
      -- so we don't accidentally create too many nodes
      g2 <- materializePathAsGraph nid p2
      let g1NonNewTargets = g1 ^.. nodesMatching (\x -> not x.isNew && x.isTarget)
          g2NonNewTargets = g2 ^.. nodesMatching (\x -> not x.isNew && x.isTarget)
      let g1NewTargets = g1 ^.. nodesMatching (\x -> x.isNew && x.isTarget)
          g2NewTargets = g2 ^.. nodesMatching (\x -> x.isNew && x.isTarget)
      let g1NewTargetEdges = asSet $ setFromList $ g1NewTargets >>= toList . (.incoming)
          g2NewTargetEdges = asSet $ setFromList $ g2NewTargets >>= toList . (.incoming)
      let commonNonNewTargetNids = undefined
          g1NonNewTargetsWithNewTargetEdges = undefined
          g2NonNewTargetsWithNewTargetEdges = undefined
          mergedNewTargetNode = undefined
      undefined
