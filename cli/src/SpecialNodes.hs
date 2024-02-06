{-# LANGUAGE OverloadedStrings #-}

-- | special nodes which the system requires for it to function completely.
-- these are always fixed NIDs, their values are added directly to this file
module SpecialNodes
  ( -- * NID's of special nodes
    originNID,
    systemNodeNID,
    tagsNID,
    importDatesNID,
    fileHashesNID,
    importURLsNID,

    -- * Full info for special nodes
    SpecialNodeInfo (..),
    originInfo,
    systemNodeInfo,
    tagsInfo,
    importDatesInfo,
    fileHashesInfo,
    importURLsInfo,
  )
where

import Graph.Types
import MyPrelude

originNID :: NID
originNID = nilNID

systemNodeNID :: NID
systemNodeNID = unsafeNID "0daCJjMrQe"

tagsNID :: NID
tagsNID = unsafeNID "pbYxBO6fzB"

importURLsNID :: NID
importURLsNID = unsafeNID "a0fVkm0kR7"

importDatesNID :: NID
importDatesNID = unsafeNID "S00KkOYoVp"

fileHashesNID :: NID
fileHashesNID = unsafeNID "AhQufiPzgy"

data SpecialNodeInfo = SpecialNodeInfo
  { specialNodeNID :: NID,
    specialNodeName :: Text
  }

originInfo :: SpecialNodeInfo
originInfo = SpecialNodeInfo originNID "origin"

systemNodeInfo :: SpecialNodeInfo
systemNodeInfo = SpecialNodeInfo systemNodeNID "system-nodes"

tagsInfo :: SpecialNodeInfo
tagsInfo = SpecialNodeInfo tagsNID "tags"

importDatesInfo :: SpecialNodeInfo
importDatesInfo = SpecialNodeInfo importDatesNID "import-dates"

fileHashesInfo :: SpecialNodeInfo
fileHashesInfo = SpecialNodeInfo fileHashesNID "file-hashes"

importURLsInfo :: SpecialNodeInfo
importURLsInfo = SpecialNodeInfo importURLsNID "import-urls"
