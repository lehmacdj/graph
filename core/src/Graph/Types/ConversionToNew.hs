{-# LANGUAGE NoImplicitPrelude #-}

module Graph.Types.ConversionToNew where

import qualified Data.ByteString.Lazy.Char8 as BS
import Effect.FreshNID
import Effect.Util
import qualified Graph
import qualified Graph'
import Graph.Edge
import Graph.Types (Connect (..), Edge (..), Graph, NID, Node (..))
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
  { strings :: NID,
    systemNodes :: NID,
    name :: NID
  }

initSpecialNodes :: Member FreshNID r => Sem r SpecialNodes
initSpecialNodes = SpecialNodes <$> freshNID <*> freshNID <*> freshNID

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
  labelNID <- freshNID
  labelNameNID <- internString l
  nameNID <- name <$> input @SpecialNodes
  modify $
    Graph'.insertNode $
      Node' labelNID (singleton (Connect nameNID o)) mempty mempty Nothing
  modify $
    Graph'.insertNode $
      Node' labelNameNID mempty mempty mempty (Just (BS.pack l))
  pure $ Edge i labelNID o

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
   in runEffs $ traverse insertConvertedNode (Graph.nodesOf g)
