{-# LANGUAGE TemplateHaskell #-}

module Effect.IOWrapper.GetTime where

import Data.Time
import Data.Time.Clock.POSIX
import MyPrelude
import Polysemy.State

data GetTime m r where
  CurrentTime :: GetTime m UTCTime
  CurrentTimeZone :: GetTime m TimeZone

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
  CurrentTimeZone -> liftIO getCurrentTimeZone

interpretTimeAsMonotonicIncreasingUnixTime ::
  Sem (GetTime : effs) a -> Sem effs a
interpretTimeAsMonotonicIncreasingUnixTime x = evalState (0 :: Int) $
  (`interpret` raiseUnder x) $
    \case
      CurrentTime -> posixSecondsToUTCTime . fromIntegral <$> get <* modify' (+ 1)
      CurrentTimeZone -> pure utc

collapsingTimeToInstant ::
  (Member GetTime effs) =>
  Sem effs a ->
  Sem effs a
collapsingTimeToInstant action = do
  instant <- currentTime
  let interceptor = intercept $ \case
        CurrentTime -> pure instant
        CurrentTimeZone -> currentTimeZone
  interceptor action
