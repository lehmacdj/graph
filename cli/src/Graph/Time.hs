module Graph.Time where

import qualified Data.Set as Set
import Effect.FreshNID
import Graph.Effect
import Graph.Utils
import Effect.Time
import MyPrelude
import Polysemy.Error
import Polysemy.State
import SpecialNodes
import UserError

timeToDateStrings :: UTCTime -> NonNull [String]
timeToDateStrings time =
  impureNonNull
    [ formatTime' "%Y" time,
      formatTime' "%m" time,
      formatTime' "%d" time,
      formatTime' "%H:%M:%S.%q" time
    ]
  where
    formatTime' = formatTime defaultTimeLocale

-- | Tag the current node with a path to it that represents the current time
tagWithTime ::
  (Members [FreshNID, Error Missing, GetTime] effs, HasGraph String effs) =>
  NID ->
  Sem effs ()
tagWithTime targetNID = do
  timeStrings <- timeToDateStrings <$> currentTime
  transitionsViaManyTo importDatesNID timeStrings targetNID

-- | Intercept FreshNID effects & tag all fresh NIDs created with the date
taggingFreshNodesWithTime ::
  forall a effs.
  (Members [FreshNID, Error Missing, GetTime] effs, HasGraph String effs) =>
  Sem effs a ->
  Sem effs a
taggingFreshNodesWithTime action = evalState @(Set NID) mempty $ do
  let interceptor = intercept $ \case
        FreshNID -> do
          newNID <- freshNID
          modify @(Set NID) $ Set.insert newNID
          pure newNID
  result <- interceptor (raise action)
  -- though you could achieve the same effect of all of the nodes having the
  -- same time by using `collapsingTimeToInstant` that would require mutating
  -- the graph during the interceptor & would lead to weird ordering problems
  -- where for example the node created `importData` might be created before
  -- it compares with the hash of the data.
  timeStrings <- timeToDateStrings <$> currentTime
  newNIDs <- get @(Set NID)
  traverse_ (transitionsViaManyTo importDatesNID timeStrings) newNIDs
  pure result
