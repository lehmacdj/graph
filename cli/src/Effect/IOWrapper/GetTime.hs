{-# LANGUAGE TemplateHaskell #-}

module Effect.IOWrapper.GetTime where

import Data.Time
import Data.Time.Clock.POSIX
import Effectful.State.Dynamic
import MyPrelude

data GetTime :: Effect where
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

makeEffect ''GetTime

interpretTimeAsIO ::
  (IOE :> effs) =>
  Eff (GetTime : effs) ~> Eff effs
interpretTimeAsIO = interpret \_ -> \case
  CurrentTime -> liftIO getCurrentTime
  CurrentTimeZone -> liftIO getCurrentTimeZone

interpretTimeAsMonotonicIncreasingUnixTime ::
  Eff (GetTime : effs) a -> Eff effs a
interpretTimeAsMonotonicIncreasingUnixTime =
  reinterpret (evalStateLocal (0 :: Int)) \_ -> \case
    CurrentTime ->
      posixSecondsToUTCTime . fromIntegral <$> get @Int <* modify' @Int (+ 1)
    CurrentTimeZone -> pure utc

collapsingTimeToInstant ::
  (GetTime :> effs) =>
  Eff effs a ->
  Eff effs a
collapsingTimeToInstant action = do
  instant <- currentTime
  let interceptor = interpose \_ -> \case
        CurrentTime -> pure instant
        CurrentTimeZone -> currentTimeZone
  interceptor action
