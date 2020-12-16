{-# LANGUAGE TemplateHaskell #-}

-- |
-- Effect for generating unique identifiers. To be implemented by random
-- generation of UUIDs or sequencial indicies for example.
module Effect.FreshNID
  ( runFreshNIDState,
    freshNID,
    FreshNID (..),
  )
where

import Effect.Util
import Graph (NID)
import MyPrelude
import Polysemy.State

data FreshNID m r where
  FreshNID :: FreshNID m NID

makeSem ''FreshNID

-- | Run FreshNID as a computation with a state representing the next value to
-- use for a Fresh NID.
runFreshNIDState :: Member (State NID) r => Sem (FreshNID : r) a -> Sem r a
runFreshNIDState = interpret $ \FreshNID -> modifying @NID (+ 1)
