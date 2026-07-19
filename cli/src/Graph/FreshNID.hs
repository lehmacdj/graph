{-# LANGUAGE TemplateHaskell #-}

-- |
-- Effect for generating unique identifiers. To be implemented by random
-- generation of UUIDs or sequencial indicies for example.
module Graph.FreshNID
  ( runFreshNIDRandom,
    freshNID,
    FreshNID (..),
  )
where

import Effectful.State.Dynamic
import Models.NID (NID)
import MyPrelude
import System.Random

data FreshNID :: Effect where
  FreshNID :: FreshNID m NID

makeEffect ''FreshNID

-- | Run FreshNID as a computation with a state representing the next value to
-- use for a Fresh NID.
runFreshNIDRandom ::
  forall r a.
  (State StdGen :> r) =>
  Eff (FreshNID : r) a ->
  Eff r a
runFreshNIDRandom = interpret \_ -> \case
  FreshNID -> state uniform

-- PickNID nids -> do
--   i <- state $ uniformR (0, length nids - 1)
--   pure $ nids `indexEx` i
