{-# LANGUAGE NoImplicitPrelude #-}

module Graph.Types.ConversionToNew where

import Effect.FreshNID
import qualified Graph
import qualified Graph'
import Graph.Types (Graph, NID)
import Graph.Types.New
import MyPrelude
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

-- TODO: write test for this because it is pretty involved
convertGraph :: Graph String -> Graph'
convertGraph g =
  let runEffs =
        run
          . execState Graph'.emptyGraph
          . evalState (Graph.nextFreeNodeId g)
          . runFreshNIDState
          . evalState @(Map String NID) mempty
   in runEffs $ do
        -- TODO: build a map of strings, then assign each of them a new node in the
        -- new graph, linking everything together using a folder of node labels called
        -- string
        -- TODO: will need to keep track of references of strings too while
        -- processing the conversion
        undefined
