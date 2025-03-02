module Polysemy.UtilSpec where

import Polysemy.Input
import Polysemy.State
import Polysemy.Util
import TestPrelude

-- | even after modifying the state variable used to initialize the Input
-- the input should be the original value; i.e. the initialization action
-- should not be called multiple times
unit_runInputConstSem_alwaysSame :: Assertion
unit_runInputConstSem_alwaysSame =
  (run . runState 0 . runInputConstSem initAction) action @=? (0, 0)
  where
    initAction :: (Member (State Int) r) => Sem r Int
    initAction = get @Int
    action :: (Members [State Int, Input Int] r) => Sem r Int
    action = do
      orig <- input @Int
      modify @Int (+ 1)
      newInput <- input @Int
      put @Int newInput
      pure orig
