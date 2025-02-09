{-# LANGUAGE OverloadedStrings #-}

-- | special nodes which the system requires for it to function completely.
-- these are always fixed NIDs, their values are added directly to this file
module Graph.SystemNodes
  ( -- * NID's of special nodes
    originNID,
    systemNodeNID,
    tagsNID,
    importDatesNID,
    fileHashesNID,
    importUrlsNID,
    historicalSystemNodesNID,
    sequenceIDsNID,
    fileExtensionsNID,
    mimeTypesNID,

    -- * Full info for special nodes
    SystemNodeInfo (..),
    originInfo,
    systemNodeInfo,
    tagsInfo,
    importDatesInfo,
    fileHashesInfo,
    importUrlsInfo,
    historicalSystemNodesInfo,
    sequenceIDsInfo,
    fileExtensionsInfo,
    mimeTypesInfo,
  )
where

import Models.NID
import MyPrelude

originNID :: NID
originNID = nilNID

systemNodeNID :: NID
systemNodeNID = unsafeNID "0daCJjMrQel8"

tagsNID :: NID
tagsNID = unsafeNID "pbYxBO6fzBQV"

importUrlsNID :: NID
importUrlsNID = unsafeNID "a0fVkm0kR7KE"

importDatesNID :: NID
importDatesNID = unsafeNID "S00KkOYoVpFu"

fileHashesNID :: NID
fileHashesNID = unsafeNID "AhQufiPzgyRf"

historicalSystemNodesNID :: NID
historicalSystemNodesNID = unsafeNID "3JJvxFUHGAA1"

sequenceIDsNID :: NID
sequenceIDsNID = unsafeNID "VEfLhuTgZ88Z"

fileExtensionsNID :: NID
fileExtensionsNID = unsafeNID "zsBuPkn5mh8F"

mimeTypesNID :: NID
mimeTypesNID = unsafeNID "927u9Xyky2pC"

data SystemNodeInfo = SystemNodeInfo
  { specialNodeNID :: NID,
    specialNodeName :: Text
  }

originInfo :: SystemNodeInfo
originInfo = SystemNodeInfo originNID "origin"

systemNodeInfo :: SystemNodeInfo
systemNodeInfo = SystemNodeInfo systemNodeNID "system-nodes"

tagsInfo :: SystemNodeInfo
tagsInfo = SystemNodeInfo tagsNID "tags"

importDatesInfo :: SystemNodeInfo
importDatesInfo = SystemNodeInfo importDatesNID "import-dates"

fileHashesInfo :: SystemNodeInfo
fileHashesInfo = SystemNodeInfo fileHashesNID "file-hashes"

importUrlsInfo :: SystemNodeInfo
importUrlsInfo = SystemNodeInfo importUrlsNID "import-urls"

historicalSystemNodesInfo :: SystemNodeInfo
historicalSystemNodesInfo = SystemNodeInfo historicalSystemNodesNID "historical-special-nodes"

sequenceIDsInfo :: SystemNodeInfo
sequenceIDsInfo = SystemNodeInfo sequenceIDsNID "sequence-ids"

fileExtensionsInfo :: SystemNodeInfo
fileExtensionsInfo = SystemNodeInfo fileExtensionsNID "file-extensions"

mimeTypesInfo :: SystemNodeInfo
mimeTypesInfo = SystemNodeInfo mimeTypesNID "mime-types"
