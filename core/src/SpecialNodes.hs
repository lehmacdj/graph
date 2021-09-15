{-# LANGUAGE OverloadedStrings #-}

module SpecialNodes
  ( SpecialNodes,
    SnInfo (..),

    -- * api for accessing special node information
    getOriginNID,
    getNameNID,
    getStringsNID,
    getDependsOnNID,
    getSystemNodeNID,
    listSpecialNodes,
    runInputSpecialNodes,
  )
where

import Graph.Types
import MyPrelude
import Polysemy.Input

-- | special nodes which the system requires for it to function properly.
-- subsystem dependencies should have sub types to allow more granular spec
-- of which dependencies are required for which functions, potentially with
-- aliases to merge several dependencies
--
-- these are always fixed NIDs, their values are added directly to this file
data SpecialNodes = SpecialNodes
  { _origin :: SnInfo,
    _strings :: SnInfo,
    _systemNode :: SnInfo,
    _name :: SnInfo,
    _dependsOn :: SnInfo
  }

data SnInfo = SnInfo
  { specialNodeNID :: NID,
    specialNodeName :: ByteString
  }

-- | this is only exposed internally to encourage acurately tracking
-- usage rather than just accesing it directly from here whenever needed
specialNodes :: SpecialNodes
specialNodes =
  SpecialNodes
    { _origin = SnInfo 0 "origin",
      _strings = SnInfo (-1) "strings",
      _systemNode = SnInfo (-2) "system-node",
      _name = SnInfo (-3) "name",
      _dependsOn = SnInfo (-4) "depends-on"
    }

getOriginNID,
  getStringsNID,
  getNameNID,
  getSystemNodeNID,
  getDependsOnNID ::
    Member (Input SpecialNodes) r => Sem r NID
getOriginNID = specialNodeNID . _origin <$> input
getStringsNID = specialNodeNID . _strings <$> input
getNameNID = specialNodeNID . _name <$> input
getSystemNodeNID = specialNodeNID . _systemNode <$> input
getDependsOnNID = specialNodeNID . _dependsOn <$> input

listSpecialNodes :: Member (Input SpecialNodes) r => Sem r [SnInfo]
listSpecialNodes =
  sequence $
    map fmap [_origin, _strings, _systemNode, _name, _dependsOn]
      <*> singleton input

runInputSpecialNodes :: Sem (Input SpecialNodes : r) a -> Sem r a
runInputSpecialNodes = runInputConst specialNodes
