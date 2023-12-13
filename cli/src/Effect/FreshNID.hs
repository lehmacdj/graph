{-# LANGUAGE TemplateHaskell #-}

-- |
-- Effect for generating unique identifiers. To be implemented by random
-- generation of UUIDs or sequencial indicies for example.
module Effect.FreshNID
  ( runFreshNIDState,
    runFreshNIDStateOriginNode,
    freshNID,
    FreshNID (..),
  )
where

import Effect.Graph (WriteGraph, setData)
import Graph (NID)
import MyPrelude
import Polysemy.State

data FreshNID m r where
  FreshNID :: FreshNID m NID

makeSem ''FreshNID

-- | Run FreshNID as a computation with a state representing the next value to
-- use for a Fresh NID.
runFreshNIDState :: Member (State NID) r => Sem (FreshNID : r) a -> Sem r a
runFreshNIDState = interpret $ \FreshNID -> do
  oldValue <- get @NID
  modify @NID (+ 1)
  pure oldValue

-- | Run FreshNID as a computation with a state representing the next value to
-- use for a Fresh NID. Also write the next NID to the origin node in the graph.
runFreshNIDStateOriginNode :: Members [State NID, WriteGraph t] r => Sem (FreshNID : r) a -> Sem r a
runFreshNIDStateOriginNode = interpret $ \FreshNID -> do
  oldValue <- get @NID
  modify @NID (+ 1)
  -- we maintain the invariant that the root node has the next NID to use
  setData 0 (Just . encodeUtf8 . tshow $ oldValue + 1)
  pure oldValue
