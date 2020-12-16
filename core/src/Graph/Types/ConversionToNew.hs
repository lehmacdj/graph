
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
  { _strings :: NID,
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
  SpecialNodes
    <$> freshNID
    <*> freshNID
    <*> freshNID
    <*> freshNID

listSpecialNodes :: SpecialNodes -> [(LByteString, NID)]
listSpecialNodes (SpecialNodes strings systemNode name dependsOn) =
  [ (BS.pack "strings", strings),
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
  modify $ Graph'.insertNode $ Node' edgeNameNID mempty mempty mempty (Just (BS.pack l))
  modify $ Graph'.insertEmptyNode edgeNID
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
        modify $ Graph'.insertEmptyNode edgeNID
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
  -- first create the nodes so we can create edges between them
  modify $ Graph'.insertEmptyNodes (map snd (listSpecialNodes specialNodes))
  let insertSpecialNodeEdges :: (LByteString, NID) -> Sem r ()
      insertSpecialNodeEdges (name, nid) = runInputConst specialNodes $ do
        systemNodeNID <- getSystemNodeNID
        edge <- convertEdge $ Edge systemNodeNID (BS.unpack name) nid
        modify $ Graph'.insertEdge edge
  traverse_ insertSpecialNodeEdges (listSpecialNodes specialNodes)

-- TODO: write test for this because it is pretty involved
convertGraph :: Graph String -> Graph'
convertGraph g =
  let runEffs =
        run
          . execState Graph'.emptyGraph
          . evalState (Graph.nextFreeNodeId g)
          . runFreshNIDState
          . evalState @(Map String NID) mempty
          . runInputConstSem initSpecialNodes
   in runEffs $
        do
          traverse_ insertConvertedNode (Graph.nodesOf g)
          input @SpecialNodes >>= insertSpecialNodes
          get @(Map String NID) >>= insertInternedStringsNodes
