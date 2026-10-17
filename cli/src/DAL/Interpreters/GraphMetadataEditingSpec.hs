module DAL.Interpreters.GraphMetadataEditingSpec where

import DAL.FileSystemOperations.Metadata
import DAL.FileSystemOperations.MetadataWriteDiff
import DAL.Interpreters.GraphMetadataEditing
import Data.List.NonEmpty qualified as NonEmpty
import Effectful.State.Dynamic
import Error.UserError
import Graph.GraphMetadataEditing
import Models.Augmentation.NodeEdits
import Models.Connect
import Models.Damage
import Models.Edge
import Models.Graph (Graph, emptyGraph, insertEdges, insertNode, nodesOf)
import Models.NID
import Models.Node
import MyPrelude
import Utils.Testing

-- | The node files of a graph, by the name of the file
type NodeFiles = Map NID (Node Text ())

-- | Node files kept in memory
runNodeFilesInMemory ::
  (State NodeFiles :> r) =>
  Eff (GraphMetadataFilesystemOperations : r) a ->
  Eff r a
runNodeFilesInMemory = interpret \_ -> \case
  ReadNodeMetadata nid -> gets @NodeFiles (lookup nid)
  WriteNodeMetadata n -> modify @NodeFiles (insertMap n.nid n)
  DeleteNodeMetadata nid -> modify @NodeFiles (deleteMap nid)
  -- only node files are kept here
  KeepNodeMetadataCopy _ _ -> pure ()
  RemoveKeptNodeMetadata _ -> pure ()
  ListNodeMetadata -> gets @NodeFiles keysSet
  OnlyDescribesWrites -> pure False

-- | Writes a transaction's changes to node files kept in memory, with the
-- checks that 'writeGraphDiff_' makes before it writes.
runWriteDiffInMemory ::
  (Members [State NodeFiles, Error UserError] r) =>
  Eff (GraphMetadataFilesystemOperationsWriteDiff : r) a ->
  Eff r a
runWriteDiffInMemory = interpret \_ -> \case
  WriteGraphDiff asLoaded changes changedNodes _ -> do
    files <- get @NodeFiles
    let changed =
          [nid | (nid, loaded) <- mapToList asLoaded, lookup nid files /= loaded]
    unless (null changed) $
      throwText ("changed since they were loaded: " <> tshow changed)
    for_ (NonEmpty.nonEmpty (toList (damageAmong (map Just files)))) $
      throwError . DamagedGraph
    let planned = flip imap changes \nid nodeEdits ->
          applyEditsCreatingNonExistent nodeEdits.edits nid (lookup nid files)
        mismatches =
          concat
            [ recordMismatches nid changedNode expected
            | (nid, changedNode) <- mapToList planned,
              expected <- toList (lookup nid changedNodes)
            ]
    for_ (NonEmpty.nonEmpty mismatches) (throwError . InconsistentNodeRecords)
    for_ (mapToList planned) \(nid, n) ->
      modify @NodeFiles (maybe (deleteMap nid) (insertMap nid) n)
    pure planned

-- | One transaction on node files kept in memory, with a choice of how the
-- transaction keeps the nodes it read and changed.
transactionStoringIn ::
  (Members [State NodeFiles, Error UserError] r) =>
  NodeSlot ->
  Eff (GraphMetadataEditing : r) a ->
  Eff r a
transactionStoringIn slot action = do
  (((result, graph), asLoaded), nonExistent) <-
    action
      & inject
      & cachingGraphMetadataEditingStoringIn slot
      & runStateLocal @GraphWithEdits emptyGraph
      & runStateLocal @(Map NID (Maybe (Node Text ()))) mempty
      & runStateLocal @(Set NID) mempty
      & runNodeFilesInMemory
  let (edits, changedNodes) = transactionChanges graph asLoaded nonExistent
  unless (null edits) . void . runWriteDiffInMemory $
    writeGraphDiff asLoaded edits changedNodes mempty
  pure result

-- | What transactions did until they kept every node as its own file says:
-- storing a node rewrote the nodes stored before it so that they agreed with
-- it about the edges between them ("the node loaded later wins"). Kept as
-- the reference that 'ownRecords' must agree with on an undamaged graph.
reconcilingRecords :: NodeSlot
reconcilingRecords = NodeSlot at

data Operation
  = Read NID
  | Touch' NID
  | DeleteNode' NID
  | InsertEdge' (Edge Text)
  | DeleteEdge' (Edge Text)
  | List
  deriving stock (Eq, Show)

-- | What an operation answered
data Answer
  = GotNode (Maybe (Node Text ()))
  | Listed (Set NID)
  | Done
  deriving stock (Eq, Show)

perform :: (GraphMetadataEditing :> r) => Operation -> Eff r Answer
perform = \case
  Read nid -> GotNode <$> getNodeMetadata nid
  Touch' nid -> touchNode @Text nid $> Done
  DeleteNode' nid -> deleteNode @Text nid $> Done
  InsertEdge' e -> insertEdge e $> Done
  DeleteEdge' e -> deleteEdge e $> Done
  List -> Listed <$> listNodes @Text

-- | Run transactions one after the other: their answers and the node files
-- they leave, or the error of the first one that failed.
runTransactions ::
  NodeSlot ->
  NodeFiles ->
  [[Operation]] ->
  Either String ([[Answer]], NodeFiles)
runTransactions slot files transactions =
  first show
    . runPureEff
    . runError @UserError
    . runStateLocal files
    $ traverse (transactionStoringIn slot . traverse perform) transactions

genNID :: Gen NID
genNID = smallNID <$> chooseInt (0, 5)

genEdge :: Gen (Edge Text)
genEdge = Edge <$> genNID <*> elements ["a", "b"] <*> genNID

-- | Node files in which both nodes of every edge record it
genConsistentFiles :: Gen NodeFiles
genConsistentFiles = do
  nodes <- listOf genNID
  edges <- listOf genEdge
  let graph :: Graph Text ()
      graph =
        insertEdges edges (foldl' (flip (insertNode . emptyNode)) emptyGraph nodes)
  pure $ mapFromList [(n.nid, n) | n <- nodesOf graph]

genOperation :: Gen Operation
genOperation =
  frequency
    [ (4, Read <$> genNID),
      (1, Touch' <$> genNID),
      (2, DeleteNode' <$> genNID),
      (3, InsertEdge' <$> genEdge),
      (3, DeleteEdge' <$> genEdge),
      (1, pure List)
    ]

shortListOf :: Int -> Gen a -> Gen [a]
shortListOf longest gen = chooseInt (0, longest) >>= (`vectorOf` gen)

-- | Whether both nodes of every edge record it and exist
consistent :: NodeFiles -> Bool
consistent files =
  and
    [ maybe False (member (Connect c.transition nid) . mirror) (lookup c.node files)
    | (nid, n) <- mapToList files,
      (recorded, mirror) <-
        [ (n.outgoing, (.incoming)),
          (n.incoming, (.outgoing))
        ],
      c <- toList recorded
    ]
    && and [nid == n.nid | (nid, n) <- mapToList files]

test_ownRecords :: TestTree
test_ownRecords =
  testGroup
    "a transaction keeping each node as its own file says"
    [ testProperty "generates graphs whose edges both nodes record" $
        forAll genConsistentFiles consistent,
      testProperty
        "answers and writes as the reconciling one on an undamaged graph"
        . withMaxSuccess 2000
        $ forAll genConsistentFiles \files ->
          forAllShrink
            (shortListOf 5 (shortListOf 12 genOperation))
            (shrinkList (shrinkList (const [])))
            \transactions ->
              let reference = runTransactions reconcilingRecords files transactions
                  actual = runTransactions ownRecords files transactions
               in counterexample ("reference: " <> show reference) $
                    actual
                      === reference
                      .&&. counterexample
                        "a transaction failed"
                        (isRight actual)
                      .&&. counterexample
                        "the graph it leaves is damaged"
                        (either (const True) (consistent . snd) actual),
      testCase "deletes an edge of a node it read and found missing" $
        runTransactions
          ownRecords
          mempty
          [[Read (smallNID 1), DeleteEdge' (Edge (smallNID 1) "a" (smallNID 2))]]
          @?= Right ([[GotNode Nothing, Done]], mempty),
      testCase "doesn't depend on the order in which it reads a damaged edge" do
        -- only the node 1 records the edge 0 -b-> 1
        let files =
              mapFromList
                [ (n0, emptyNode n0),
                  (n1, emptyNode n1 & #incoming .~ singletonSet (Connect "b" n0))
                ]
            n0 = smallNID 0
            n1 = smallNID 1
            answers order =
              sortOn (\case GotNode n -> fmap (.nid) n; _ -> Nothing) . concat . fst
                <$> runTransactions ownRecords files [map Read order]
        answers [n0, n1] @?= answers [n1, n0]
        answers [n0, n1]
          @?= Right (map (GotNode . (`lookup` files)) [n0, n1])
        -- reading the node 0 used to take the edge out of what the
        -- transaction had read of the node 1
        let rereading slot =
              concat . fst <$> runTransactions slot files [map Read [n1, n0, n1]]
            withTheEdge = GotNode (lookup n1 files)
            withoutTheEdge = GotNode (Just (emptyNode n1))
        rereading ownRecords
          @?= Right [withTheEdge, GotNode (lookup n0 files), withTheEdge]
        rereading reconcilingRecords
          @?= Right [withTheEdge, GotNode (lookup n0 files), withoutTheEdge]
    ]
