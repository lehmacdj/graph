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

import Effectful
import Effectful.Dispatch.Dynamic
import Effectful.State.Static.Local
import Effectful.TH
import Models.NID (NID)
import MyPrelude
import System.Random

data FreshNID :: Effect where
  FreshNID :: FreshNID m NID

-- -- | Pick a single NID from a list of NIDs in a case where we generated too
-- -- many and need to pick one to use arbitrarily.
-- --
-- -- Doing this instead of just generating a new NID marginally reduces the
-- -- chances of an NID collision, though argably this entire approach is just
-- -- premature optimization.
-- --
-- -- e.g. consider the use in `materializePathAsGraph`'s `:&` case
-- -- where we want to merge all new targets into a single new target
-- -- (because `:&` semantically should produce only nodes such that both paths
-- -- lead to the same node)
-- PickNID :: [NID] -> FreshNID m NID

makeEffect ''FreshNID

-- | Run FreshNID as a computation with a state representing the next value to
-- use for a Fresh NID.
runFreshNIDRandom ::
  forall es a.
  (State StdGen :> es) =>
  Eff (FreshNID : es) a ->
  Eff es a
runFreshNIDRandom = interpret $ \_ -> \case
  FreshNID -> embedStateful uniform

-- PickNID nids -> do
--   i <- embedStateful $ uniformR (0, length nids - 1)
--   pure $ nids `indexEx` i
