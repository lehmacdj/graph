module MyPrelude.Time
  ( module X,
    module MyPrelude.Time,
  )
where

import ClassyPrelude
import Control.Monad.Fail (MonadFail (..))
import Data.Time as X
  ( Day (..),
    LocalTime (..),
    NominalDiffTime,
    TimeOfDay (..),
    TimeZone (..),
    ZonedTime (..),
    addUTCTime,
    diffUTCTime,
    getCurrentTimeZone,
    localTimeToUTC,
    parseTimeOrError,
    utc,
    utcToLocalTime,
  )
import Utils.Testing.External

getDiffTimeSeconds :: (Integral a) => NominalDiffTime -> a
getDiffTimeSeconds = round

getDiffTimeMinutes :: (Integral a) => NominalDiffTime -> a
getDiffTimeMinutes dt = round (dt / 60)

getDiffTimeHours :: (Integral a) => NominalDiffTime -> a
getDiffTimeHours dt = round (dt / 3600)

-- | Parse a time from its components.
timeFromDateStrings ::
  (MonadFail m) => String -> String -> String -> String -> m UTCTime
timeFromDateStrings year month day time =
  parseTimeM True defaultTimeLocale "%Y%m%d%H:%M:%S.%q" $
    concat [year, month, day, time]

timeToDateStrings :: UTCTime -> NonNull [Text]
timeToDateStrings time =
  impureNonNull
    [ pack (formatTime' "%Y" time),
      pack (formatTime' "%m" time),
      pack (formatTime' "%d" time),
      pack (formatTime' "%H:%M:%S.%q" time)
    ]
  where
    formatTime' = formatTime defaultTimeLocale

formatMaybeRelativeTimestamp :: TimeZone -> UTCTime -> UTCTime -> String
formatMaybeRelativeTimestamp tz currentTime t = case diff of
  _ | diff < 60 && diff > -60 -> "now"
  (getDiffTimeMinutes -> minutes :: Int)
    | minutes == 1 -> "1 min ago"
    | minutes == -1 -> "in 1 min"
    | minutes < 60 && minutes > 0 -> show minutes ++ " mins ago"
    | minutes > -60 && minutes < 0 -> "in " ++ show (negate minutes) ++ " mins"
  (getDiffTimeHours -> hours :: Int)
    | hours == 1 -> "1 hour ago"
    | hours == -1 -> "in 1 hour"
    | hours < 24 && hours > 0 -> show hours ++ " hours ago"
    | hours > -24 && hours < 0 -> "in " ++ show (negate hours) ++ " hours"
  _ | otherwise -> formatTime defaultTimeLocale "%Y-%m-%d %H:%M" localTime
  where
    diff = currentTime `diffUTCTime` t
    localTime = utcToLocalTime tz t

referenceDate :: UTCTime
referenceDate =
  parseTimeOrError
    True
    defaultTimeLocale
    "%Y-%m-%d %H:%M:%S"
    "2024-01-26 20:00:00"

test_formatMaybeRelativeTimestamp :: TestTree
test_formatMaybeRelativeTimestamp =
  testGroup
    "formatMaybeRelativeTimestamp"
    [ 1 `formatsTo` "now",
      (-1 * 60) `formatsTo` "1 min ago",
      (-5 * 60) `formatsTo` "5 mins ago",
      (-65 * 60) `formatsTo` "1 hour ago",
      (-23 * 3600) `formatsTo` "23 hours ago",
      (-48 * 3600) `formatsTo` "2024-01-24 20:00",
      (1 * 60) `formatsTo` "in 1 min",
      (3 * 60) `formatsTo` "in 3 mins",
      (75 * 60) `formatsTo` "in 1 hour",
      (13 * 3600) `formatsTo` "in 13 hours",
      (25 * 3600) `formatsTo` "2024-01-27 21:00"
    ]
  where
    formatsTo :: NominalDiffTime -> String -> TestTree
    formatsTo offset expected =
      testCase (show offset ++ " seconds -> " ++ show expected) $
        formatMaybeRelativeTimestamp
          utc
          referenceDate
          (addUTCTime offset referenceDate)
          @?= expected
