{-# LANGUAGE TemplateHaskell #-}

module Effect.IOWrapper.GetTime where

import Data.Time
import Data.Time.Clock.POSIX
import Effectful
import Effectful.Dispatch.Dynamic
import Effectful.State.Static.Local
import Effectful.TH
import MyPrelude
import System.Random

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
  (IOE :> es) =>
  Eff (GetTime : es) a ->
  Eff es a
interpretTimeAsIO = interpret $ \_ -> \case
  CurrentTime -> liftIO getCurrentTime
  CurrentTimeZone -> liftIO getCurrentTimeZone

interpretTimeAsMonotonicIncreasingUnixTime ::
  Eff (GetTime : es) a -> Eff es a
interpretTimeAsMonotonicIncreasingUnixTime x = evalState (0 :: Int) $
  raiseUnder x & interpret (\_ -> \case
    CurrentTime -> posixSecondsToUTCTime . fromIntegral <$> get <* modify' (+ 1)
    CurrentTimeZone -> pure utc)

collapsingTimeToInstant ::
  (GetTime :> es) =>
  Eff es a ->
  Eff es a
collapsingTimeToInstant action = do
  instant <- currentTime
  let interceptor = interpose $ \_ -> \case
        CurrentTime -> pure instant
        CurrentTimeZone -> currentTimeZone
  interceptor action
