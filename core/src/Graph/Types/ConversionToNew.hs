module Graph.Types.ConversionToNew where

import qualified Data.ByteString.Lazy.Char8 as BS
import qualified Data.Map as Map
import Effect.FreshNID
import qualified Graph
import qualified Graph'
import Graph.Edge
import Graph.Types (Edge (..), Graph, NID, Node (..))
import Graph.Types.New
import MyPrelude
import Polysemy.Input
import Polysemy.State
import SpecialNodes

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
  Members [State Graph', FreshNID, State (Map String NID), Input SpecialNodes] r =>
  Sem r ()
insertSpecialNodes = do
  let insertSpecialNodeEdges :: SnInfo -> Sem r ()
      insertSpecialNodeEdges (SnInfo nid name) = do
        edgeNameNID <- internString (BS.unpack name)
        systemNodeNID <- getSystemNodeNID
        dependsOnNID <- getDependsOnNID
        nameNID <- getNameNID
        modify . Graph'.insertEdges $
          [Edge systemNodeNID dependsOnNID nid, Edge nid nameNID edgeNameNID]
  specialNodeList <- listSpecialNodes
  traverse_ insertSpecialNodeEdges specialNodeList

-- returns Nothing if the input graph doesn't have a node with NID 0
convertGraph :: Graph String -> Graph'
convertGraph g =
  let runEffs =
        run
          . execState Graph'.emptyGraph
          . evalState (Graph.nextFreeNodeId g)
          . runFreshNIDState
          . evalState @(Map String NID) mempty
          . runInputSpecialNodes
   in runEffs $ do
        traverse_ insertConvertedNode (Graph.nodesOf g)
        insertSpecialNodes
        get @(Map String NID) >>= insertInternedStringsNodes
