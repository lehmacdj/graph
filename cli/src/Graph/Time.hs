module Graph.Time where

import Data.Set qualified as Set
import Effect.IOWrapper.GetTime
import Error.Missing
import Graph.Effect
import Graph.FreshNID
import Graph.SystemNodes
import Graph.Utils
import MyPrelude
import Polysemy.State

-- | Tag the current node with a path to it that represents the current time
tagWithTime ::
  (Members [FreshNID, Error Missing, GetTime] effs, HasGraph Text effs) =>
  NID ->
  Sem effs ()
tagWithTime targetNID = do
  timeStrings <- timeToDateStrings <$> currentTime
  transitionsViaManyTo importDatesNID timeStrings targetNID

-- | Intercept FreshNID effects & tag all fresh NIDs created with the date
taggingFreshNodesWithTime ::
  forall a effs.
  (Members [FreshNID, Error Missing, GetTime] effs, HasGraph Text effs) =>
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
