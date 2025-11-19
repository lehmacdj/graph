module Models.GraphSpec where

import Models.Connect
import Models.Graph
import Models.NID
import Models.Node
import MyPrelude
import Utils.Testing

newtype TestTextAugmentation = TestTextAugmentation Text
  deriving stock (Eq, Show)
  deriving newtype (Semigroup, Monoid)

instance IsString TestTextAugmentation where
  fromString = TestTextAugmentation . fromString

instance DefaultAugmentation TestTextAugmentation where
  defaultAugmentation = TestTextAugmentation ""

instance MonoidAugmentation TestTextAugmentation

instance ShowableAugmentation TestTextAugmentation where
  augmentationProperties (TestTextAugmentation txt) =
    [(Just "testText", txt)]

spec_at :: Spec
spec_at = do
  let n0 = node 0
  let minimalGraph :: Graph Text ()
      minimalGraph = insertNode n0 emptyGraph
  let t_n0 = emptyNode (smallNID 0)
  let t_minimalGraph :: Graph Text TestTextAugmentation
      t_minimalGraph = insertNode t_n0 emptyGraph
  it "fails to fetch a node not in the graph" do
    emptyGraph @Text @() ^. at (smallNID 0) `shouldBe` Nothing
  it "fetches a node" do
    minimalGraph ^. at (smallNID 0) `shouldBe` Just n0
  it "inserts" do
    (emptyGraph & at (smallNID 0) ?~ n0) `shouldBe` minimalGraph
  it "deletes" do
    (minimalGraph & at (smallNID 0) .~ Nothing) `shouldBe` emptyGraph
  let n0' = n0 & #outgoing .~ setFromList [Connect "0->1" (smallNID 1)]
  let n1 :: Node Text ()
      n1 =
        emptyNode (smallNID 1)
          & #incoming
            .~ setFromList [Connect "0->1" (smallNID 0)]
  it "adds outgoing edges" do
    (minimalGraph & at (smallNID 0) ?~ n0') `shouldBe` insertNode n1 minimalGraph
  it "adds incoming edges" do
    (minimalGraph & at (smallNID 0) ?~ dualizeNode n0')
      `shouldBe` insertNode (dualizeNode n1) minimalGraph
  let oneEdgeGraph = minimalGraph & at (smallNID 0) ?~ n0'
  let noEdgeGraph = minimalGraph & at (smallNID 1) ?~ emptyNode (smallNID 1)
  it "removes outgoing edges" do
    (oneEdgeGraph & at (smallNID 0) ?~ n0) `shouldBe` noEdgeGraph
  it "removes incoming edges" do
    (dualizeGraph oneEdgeGraph & at (smallNID 0) ?~ dualizeNode n0)
      `shouldBe` noEdgeGraph
  let foobarGraph :: Graph Text TestTextAugmentation
      foobarGraph = singletonGraph (Node (smallNID 0) mempty mempty "foobar")
  it "replaces augmentation" do
    (t_minimalGraph & at (smallNID 0) ?~ t_n0 & ix (smallNID 0) . #augmentation .~ "foobar")
      `shouldBe` foobarGraph
  it "modifies augmentation" do
    ( t_minimalGraph
        & (at (smallNID 0) ?~ t_n0)
        & (ix (smallNID 0) . #augmentation .~ "foo")
        & (ix (smallNID 0) . #augmentation %~ (++ "bar"))
      )
      `shouldBe` foobarGraph
  it "overwrites augmentation" do
    ( t_minimalGraph
        & (at (smallNID 0) ?~ t_n0)
        & (ix (smallNID 0) . #augmentation .~ "Hello world!")
        & (ix (smallNID 0) . #augmentation .~ "foobar")
      )
      `shouldBe` foobarGraph

spec_subtractiveFilterGraph :: Spec
spec_subtractiveFilterGraph = do
  it "filters out nodes" do
    subtractiveFilterGraph (const False) (disconnectedGraph [0, 1, 2])
      `shouldBe` (emptyGraph :: Graph Text ())
  it "filters out edges" do
    subtractiveFilterGraph ((== smallNID 0) . (.nid)) (stronglyConnectedGraph [0, 1, 2])
      `shouldBe` stronglyConnectedGraph [0]

spec_additiveFilterGraph :: Spec
spec_additiveFilterGraph = do
  it "filters out nodes" do
    additiveFilterGraph (const False) (disconnectedGraph [0, 1, 2])
      `shouldBe` (emptyGraph :: Graph Text ())
  let k3 = stronglyConnectedGraph [0, 1, 2]
  it "filters out irrelevant edges" do
    let expected =
          emptyGraph
            & insertNode (k3 ^. at (smallNID 0) . non (error "impossible"))
    additiveFilterGraph ((== smallNID 0) . (.nid)) k3 `shouldBe` expected
  it "filters out only irrelevant edges" do
    additiveFilterGraph ((/= smallNID 2) . (.nid)) k3 `shouldBe` k3
