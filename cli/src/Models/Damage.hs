-- | Damage to a graph: node files that contradict each other or that cannot
-- be used. @ge@ never writes such files, but a write that was interrupted, a
-- sync that brought only some of the files, or an edit by hand can leave them.
module Models.Damage where

import Data.Map.Strict qualified as Map
import GHC.IO.Exception (IOErrorType (InappropriateType), IOException (ioe_type))
import Models.Connect
import Models.Edge
import Models.NID
import Models.Node
import MyPrelude
import Utils.Testing.External

-- | Which of a node's two lists of edges
data RecordSide = IncomingRecord | OutgoingRecord
  deriving stock (Eq, Ord, Show, Generic)
  deriving anyclass (NFData)

data Damage
  = -- | Only one of the two nodes of the edge records it: its source among
    -- its outgoing edges ('OutgoingRecord') or its sink among its incoming
    -- edges ('IncomingRecord')
    OneSidedEdge RecordSide (Edge Text)
  | -- | A node records the edge (on the given side, as for 'OneSidedEdge')
    -- and the other node of the edge has no file
    EdgeToMissingNode RecordSide (Edge Text)
  | -- | The file named after the first node says it is the second node
    FileSaysOtherId NID NID
  | -- | The file of the node cannot be read as a node; with the reason
    UnparsableNodeFile NID String
  | -- | The origin, the node that every graph has, has no file
    MissingOriginFile
  deriving stock (Eq, Ord, Show, Generic)
  deriving anyclass (NFData)

-- | The name of the file that holds a node's edges
nodeFileName :: NID -> String
nodeFileName nid = unpack (nidRepresentation nid) <> ".json"

-- | An edge as a message shows it. The label is between quotes as it is,
-- not as Haskell would escape it: a label is read by people, and @ls@ shows
-- it the same way.
describeEdge :: Edge Text -> String
describeEdge e =
  show e.source <> " -\"" <> unpack e.transition <> "\"-> " <> show e.sink

-- | The node whose record of the edge is on that side, then the other node
recordingNode :: RecordSide -> Edge Text -> (NID, NID)
recordingNode side e = case side of
  OutgoingRecord -> (e.source, e.sink)
  IncomingRecord -> (e.sink, e.source)

-- | One line saying what is wrong, naming the files involved.
describeDamage :: Damage -> String
describeDamage = \case
  OneSidedEdge side e ->
    let (recording, other) = recordingNode side e
     in "the edge "
          <> describeEdge e
          <> " is among the "
          <> sideName side
          <> " edges in "
          <> nodeFileName recording
          <> " but not among the "
          <> sideName (otherSide side)
          <> " edges in "
          <> nodeFileName other
  EdgeToMissingNode side e ->
    let (recording, other) = recordingNode side e
     in "the edge "
          <> describeEdge e
          <> " is among the "
          <> sideName side
          <> " edges in "
          <> nodeFileName recording
          <> " but there is no "
          <> nodeFileName other
  FileSaysOtherId nid claimed ->
    nodeFileName nid <> " says it is the node " <> show claimed
  UnparsableNodeFile nid reason ->
    nodeFileName nid <> " cannot be read as a node: " <> reason
  MissingOriginFile ->
    "there is no " <> nodeFileName nilNID <> ", the file of the origin node"
  where
    sideName = \case
      IncomingRecord -> "incoming"
      OutgoingRecord -> "outgoing"

otherSide :: RecordSide -> RecordSide
otherSide = \case
  IncomingRecord -> OutgoingRecord
  OutgoingRecord -> IncomingRecord

-- | Why a node file that is there could not be read, as the reason of an
-- 'UnparsableNodeFile': first in words, then once as the system said it
-- (which has the whole path of the file).
readFailure :: IOError -> String
readFailure e = inWords <> " (" <> show e <> ")"
  where
    inWords
      | isPermissionError e = "there is no permission to read it"
      | isDoesNotExistError e =
          "the directory lists it but it can't be opened, as a symbolic link"
            <> " to a file that isn't there can't"
      | ioe_type e == InappropriateType = "it is not a file but a directory"
      | otherwise = "the system could not read it"

-- | What follows a description of damage to say what to do about it.
repairHint :: String
repairHint =
  "fsck lists all such damage; fix repairs it, or says why it can't"

-- | The edges that a node's file records, each with the side it is on.
recordedEdges :: NID -> Node Text a -> [(RecordSide, Edge Text)]
recordedEdges = recordedEdgesWith (const True)

-- | The edges that a node's file records with the nodes that the predicate
-- holds for.
recordedEdgesWith ::
  (NID -> Bool) -> NID -> Node Text a -> [(RecordSide, Edge Text)]
recordedEdgesWith with nid node =
  [(OutgoingRecord, outgoingEdge nid c) | c <- toList node.outgoing, with c.node]
    <> [ (IncomingRecord, incomingEdge c nid)
       | c <- toList node.incoming,
         with c.node
       ]

-- | Whether a node's file records the edge on that side.
records :: RecordSide -> Edge Text -> Node Text a -> Bool
records side e node = case side of
  OutgoingRecord -> outConnect e `member` node.outgoing
  IncomingRecord -> inConnect e `member` node.incoming

-- | The damage that the files of some nodes show among themselves. The key
-- is the name of a file (edges find a node by the name of its file, whatever
-- id the file says) and 'Nothing' means that there is no such file. Nothing
-- is said about an edge whose other node is not in the map: its file was not
-- looked at.
damageAmong :: Map NID (Maybe (Node Text a)) -> Set Damage
damageAmong files = damageNear (keysSet files) files

-- | The part of 'damageAmong' that the files of the given nodes have a part
-- in: what such a file says of itself, the edges that it records, and the
-- edges with its node that the other files record. Together with the damage
-- among the other files alone, that is all of it.
--
-- For a few files that were read next to many that have been compared with
-- each other before: this goes through the records of the many once, and
-- looks up only the ones that lead to the few.
damageNear :: Set NID -> Map NID (Maybe (Node Text a)) -> Set Damage
damageNear near files
  | null near = mempty
  | otherwise =
      setFromList . concat $
        [ [FileSaysOtherId nid node.nid | node.nid /= nid, isNear]
            <> concatMap (uncurry edgeDamage) (recordedEdgesWith with nid node)
        | (nid, Just node) <- mapToList files,
          let isNear = nid `member` near
              with other = isNear || other `member` near
        ]
  where
    edgeDamage side e =
      let other = snd (recordingNode side e)
       in case lookup other files of
            Nothing -> []
            Just Nothing -> [EdgeToMissingNode side e]
            Just (Just otherNode)
              -- a loop is recorded twice in the one file
              | records (otherSide side) e otherNode -> []
              | otherwise -> [OneSidedEdge side e]

spec_damageAmong :: Spec
spec_damageAmong = do
  let n0 = smallNID 0
      n1 = smallNID 1
      n2 = smallNID 2
      node :: NID -> [Connect Text] -> [Connect Text] -> Node Text ()
      node nid incoming outgoing =
        Node nid (setFromList incoming) (setFromList outgoing) ()
      among = toList . damageAmong . mapFromList
      b = Edge n0 "b" n1
  it "finds nothing when both nodes record an edge" $
    among
      [ (n0, Just (node n0 [] [Connect "b" n1])),
        (n1, Just (node n1 [Connect "b" n0] []))
      ]
      `shouldBe` []
  it "finds an edge that only its source records" $
    among [(n0, Just (node n0 [] [Connect "b" n1])), (n1, Just (node n1 [] []))]
      `shouldBe` [OneSidedEdge OutgoingRecord b]
  it "finds an edge that only its sink records" $
    among [(n0, Just (node n0 [] [])), (n1, Just (node n1 [Connect "b" n0] []))]
      `shouldBe` [OneSidedEdge IncomingRecord b]
  it "finds an edge to a node that has no file" $
    among [(n0, Just (node n0 [] [Connect "b" n1])), (n1, Nothing)]
      `shouldBe` [EdgeToMissingNode OutgoingRecord b]
  it "finds an edge from a node that has no file" $
    among [(n0, Nothing), (n1, Just (node n1 [Connect "b" n0] []))]
      `shouldBe` [EdgeToMissingNode IncomingRecord b]
  it "says nothing about an edge whose other node was not looked at" $
    among [(n0, Just (node n0 [Connect "z" n2] [Connect "b" n1]))]
      `shouldBe` []
  it "finds a file that says it is another node, and uses it by its name" $
    among
      [ (n0, Just (node n0 [] [Connect "b" n1])),
        (n1, Just (node n2 [Connect "b" n0] []))
      ]
      `shouldBe` [FileSaysOtherId n1 n2]
  it "finds a loop that is recorded on one side only" $
    among [(n0, Just (node n0 [] [Connect "l" n0]))]
      `shouldBe` [OneSidedEdge OutgoingRecord (Edge n0 "l" n0)]
  it "accepts a loop that is recorded on both sides" $
    among [(n0, Just (node n0 [Connect "l" n0] [Connect "l" n0]))]
      `shouldBe` []

-- | The files among the second ones that are not among the first ones, or
-- not as they are there: of what was just read, the files that whoever has
-- the first ones has not read before, or not as they are now.
filesNewTo ::
  (Eq a) =>
  Map NID (Maybe (Node Text a)) ->
  Map NID (Maybe (Node Text a)) ->
  Set NID
filesNewTo before now =
  keysSet $
    Map.differenceWith (\new old -> justIfTrue (new /= old) new) now before

test_damageNear :: TestTree
test_damageNear =
  testProperty "damageNear and the damage among the other files are damageAmong" $
    forAll genFiles \files -> forAll (sublistOf (keys files)) \near ->
      let nearSet = setFromList near
       in damageNear nearSet files
            <> damageAmong (files `Map.withoutKeys` nearSet)
              === damageAmong files
  where
    -- mostly damaged: any file may record any edge
    genFiles :: Gen (Map NID (Maybe (Node Text ())))
    genFiles = mapFromList . catMaybes <$> traverse genFile nids
    nids = map smallNID [0 .. 4]
    genFile nid =
      frequency
        [ (1, pure Nothing),
          (1, pure (Just (nid, Nothing))),
          (6, Just . (nid,) . Just <$> genNode nid)
        ]
    genNode nid = do
      said <- frequency [(9, pure nid), (1, elements nids)]
      Node said <$> genConnects <*> genConnects <*> pure ()
    genConnects =
      setFromList
        <$> resize 4 (listOf (Connect <$> elements ["a", "b"] <*> elements nids))

spec_describeDamage :: Spec
spec_describeDamage = do
  let b = Edge (smallNID 0) "b" (smallNID 2)
      describes damage expected =
        it expected $ describeDamage damage `shouldBe` expected
  OneSidedEdge OutgoingRecord b
    `describes` ( "the edge @000000000000 -\"b\"-> @000000000002 is among the"
                    <> " outgoing edges in 000000000000.json but not among the"
                    <> " incoming edges in 000000000002.json"
                )
  OneSidedEdge IncomingRecord b
    `describes` ( "the edge @000000000000 -\"b\"-> @000000000002 is among the"
                    <> " incoming edges in 000000000002.json but not among the"
                    <> " outgoing edges in 000000000000.json"
                )
  EdgeToMissingNode OutgoingRecord b
    `describes` ( "the edge @000000000000 -\"b\"-> @000000000002 is among the"
                    <> " outgoing edges in 000000000000.json but there is no"
                    <> " 000000000002.json"
                )
  FileSaysOtherId (smallNID 2) (smallNID 3)
    `describes` "000000000002.json says it is the node @000000000003"
  UnparsableNodeFile (smallNID 2) "not enough input"
    `describes` "000000000002.json cannot be read as a node: not enough input"
  -- a file that could not be read: in words, then what the system said
  UnparsableNodeFile
    (smallNID 2)
    ( readFailure
        (mkIOError permissionErrorType "openBinaryFile" Nothing (Just "/g/2.json"))
    )
    `describes` ( "000000000002.json cannot be read as a node: there is no"
                    <> " permission to read it (/g/2.json: openBinaryFile:"
                    <> " permission denied)"
                )
  -- a label is shown as it is written
  OneSidedEdge OutgoingRecord (Edge (smallNID 0) "été 日本" (smallNID 2))
    `describes` ( "the edge @000000000000 -\"été 日本\"->"
                    <> " @000000000002 is among the outgoing edges in"
                    <> " 000000000000.json but not among the incoming edges in"
                    <> " 000000000002.json"
                )
