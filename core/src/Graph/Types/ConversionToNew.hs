module Graph.Types.ConversionToNew where

import qualified Data.ByteString.Lazy.Char8 as BS
import qualified Data.Map as Map
import Effect.FreshNID
import Effect.Util
import qualified Graph
import qualified Graph'
import Graph.Edge
import Graph.Types (Edge (..), Graph, NID, Node (..))
import Graph.Types.New
import MyPrelude
import Polysemy.Input
import Polysemy.State

internString ::
  Members [State (Map String NID), FreshNID] r =>
  String ->
  Sem r NID
internString str = do
  m <- get
  case lookup str m of
    Nothing -> do
      nid <- freshNID
      modify (insertMap str nid)
      pure nid
    Just nid -> pure nid

data SpecialNodes = SpecialNodes
  { _origin :: NID,
    _strings :: NID,
    _systemNode :: NID,
    _name :: NID,
    _dependsOn :: NID
  }

getStringsNID,
  getNameNID,
  getSystemNodeNID,
  getDependsOnNID ::
    Member (Input SpecialNodes) r => Sem r NID
getStringsNID = _strings <$> input
getNameNID = _name <$> input
getSystemNodeNID = _systemNode <$> input
getDependsOnNID = _dependsOn <$> input

initSpecialNodes :: Member FreshNID r => Sem r SpecialNodes
initSpecialNodes =
  SpecialNodes 0
    <$> freshNID
    <*> freshNID
    <*> freshNID
    <*> freshNID

listSpecialNodes :: SpecialNodes -> [(LByteString, NID)]
listSpecialNodes (SpecialNodes origin strings systemNode name dependsOn) =
  [ (BS.pack "origin", origin),
    (BS.pack "strings", strings),
    (BS.pack "system-node", systemNode),
    (BS.pack "name", name),
    (BS.pack "depends-on", dependsOn)
  ]

insertConvertedNode ::
  Members [State (Map String NID), FreshNID, State Graph', Input SpecialNodes] r =>
  Node String ->
  Sem r ()
insertConvertedNode (Node nid _ o x) = do
  let outgoingEs = outgoingEdge nid <$> toList o
  newOutgoingEs <- traverse convertEdge outgoingEs
  modify $ Graph'.insertNode $ Node' nid mempty mempty mempty x
  modify $ Graph'.insertEdges newOutgoingEs

convertEdge ::
  Members [Input SpecialNodes, FreshNID, State (Map String NID), State Graph'] r =>
  Edge String ->
  Sem r (Edge NID)
convertEdge (Edge i l o) = do
  edgeNID <- freshNID
  edgeNameNID <- internString l
  nameNID <- getNameNID
  modify $ Graph'.insertEdge $ Edge edgeNID nameNID edgeNameNID
  pure $ Edge i edgeNID o

-- | precondition: system nodes have already been created
insertInternedStringsNodes ::
  forall r.
  Members [Input SpecialNodes, State Graph', FreshNID] r =>
  Map String NID ->
  Sem r ()
insertInternedStringsNodes internedStrings = do
  let insertStringNode :: (String, NID) -> Sem r ()
      insertStringNode (s, nid) = do
        stringsNID <- getStringsNID
        nameNID <- getNameNID
        edgeNID <- freshNID
        modify $ Graph'.insertNode $ Node' nid mempty mempty mempty (Just (BS.pack s))
        -- we need two edges to the strings node, one from the strings node and
        -- one to mark the name of the edge to the strings node
        modify $ Graph'.insertEdges [Edge edgeNID nameNID nid, Edge stringsNID edgeNID nid]
  traverse_ insertStringNode $ Map.assocs internedStrings

insertSpecialNodes ::
  forall r.
  Members [State Graph', FreshNID, State (Map String NID)] r =>
  SpecialNodes ->
  Sem r ()
insertSpecialNodes specialNodes = do
  let insertSpecialNodeEdges :: (LByteString, NID) -> Sem r ()
      insertSpecialNodeEdges (name, nid) = runInputConst specialNodes $ do
        edgeNameNID <- internString (BS.unpack name)
        systemNodeNID <- getSystemNodeNID
        dependsOnNID <- getDependsOnNID
        nameNID <- getNameNID
        modify . Graph'.insertEdges $
          [Edge systemNodeNID dependsOnNID nid, Edge nid nameNID edgeNameNID]
  traverse_ insertSpecialNodeEdges (listSpecialNodes specialNodes)

-- returns Nothing if the input graph doesn't have a node with NID 0
convertGraph :: Graph String -> Graph'
convertGraph g =
  let runEffs =
        run
          . execState Graph'.emptyGraph
          . evalState (Graph.nextFreeNodeId g)
          . runFreshNIDState
          . evalState @(Map String NID) mempty
          . runInputConstSem initSpecialNodes
   in runEffs $ do
        traverse_ insertConvertedNode (Graph.nodesOf g)
        input @SpecialNodes >>= insertSpecialNodes
        get @(Map String NID) >>= insertInternedStringsNodes
