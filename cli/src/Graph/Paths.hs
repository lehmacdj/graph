-- | Common paths used in graph operations for various purposes
module Graph.Paths where

import Graph.SystemNodes
import Models.Path.Simple
import MyPrelude

-- | These system nodes can be fairly large and take a long-ish time to parse /
-- traverse so we explicitly include them from the tag traversal
excludedLargeSystemNodes :: Path Text
excludedLargeSystemNodes =
  ExcludingNIDs $ setFromList [fileHashesNID, importUrlsNID, sequenceIDsNID]
