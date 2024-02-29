{-# LANGUAGE TemplateHaskell #-}

module Effect.Time where

import MyPrelude

data GetTime m r where
  CurrentTime :: GetTime m UTCTime

-- IDEA: Collapse the time to an instant, all calls to CurrentTime within the
-- block will return the same value
-- Would perhaps allow simplifying Graph.Time operations (e.g. taggingWithTime)
-- since you wouldn't need to keep track of a set
-- CollapseToInstant :: m a -> GetTime m a
--
-- this could also be implemented just as an interpreter which caches the time
-- and returns the same value for all calls to CurrentTime

makeSem ''GetTime

interpretTimeAsIO ::
  (Member (Embed IO) effs) =>
  Sem (GetTime : effs) ~> Sem effs
interpretTimeAsIO = interpret $ \case
  CurrentTime -> liftIO getCurrentTime

collapsingTimeToInstant ::
  (Member GetTime effs) =>
  Sem effs a ->
  Sem effs a
collapsingTimeToInstant action = do
  instant <- currentTime
  let interceptor = intercept $ \CurrentTime -> pure instant
  interceptor action
