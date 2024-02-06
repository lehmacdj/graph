{-# LANGUAGE TemplateHaskell #-}

-- |
-- Effect for generating unique identifiers. To be implemented by random
-- generation of UUIDs or sequencial indicies for example.
module Effect.FreshNID
  ( runFreshNIDRandom,
    freshNID,
    FreshNID (..),
  )
where

import Graph (NID)
import MyPrelude
import Polysemy.State
import System.Random

data FreshNID m r where
  FreshNID :: FreshNID m NID

makeSem ''FreshNID

-- | Run FreshNID as a computation with a state representing the next value to
-- use for a Fresh NID.
runFreshNIDRandom ::
  forall r a.
  Member (State StdGen) r =>
  Sem (FreshNID : r) a ->
  Sem r a
runFreshNIDRandom = interpret $ \FreshNID -> embedStateful uniform
