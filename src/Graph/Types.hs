{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE ConstraintKinds #-}

module Graph.Types
  ( module Graph.Types
  ) where

import Control.DeepSeq
import GHC.Generics

import Data.Aeson

import Control.Lens (makeLenses, view, over)

import Data.Map (Map)

import Data.Set (Set)

type TransitionValid t = (Show t, Eq t, Ord t)

type Id = Int

-- | A transition from/to a node to/from another node.
-- The first node isn't represented here, because this is used only in the node
-- structure where the first node is clear from context.
data Connect t = Connect
  { _connectTransition :: t
  , _connectNode :: Id
  } deriving (Show, Eq, Ord, Generic, NFData)
makeLenses ''Connect

instance (FromJSON t, TransitionValid t) => FromJSON (Connect t)
instance (ToJSON t, TransitionValid t) => ToJSON (Connect t) where
  toEncoding = genericToEncoding defaultOptions

data Node t = Node
  { _nodeId :: Id
  -- ^ unique node id
  , _nodeIncoming :: Set (Connect t)
  , _nodeOutgoing :: Set (Connect t)
  } deriving (Show, Eq, Ord, Generic, NFData)
makeLenses ''Node

instance (FromJSON t, TransitionValid t) => FromJSON (Node t)
instance (ToJSON t, TransitionValid t) => ToJSON (Node t) where
  toEncoding = genericToEncoding defaultOptions

newtype Graph t = Graph
  { _graphNodeMap :: Map Id (Node t)
  } deriving (Show, Eq, Ord, Generic, NFData)
makeLenses ''Graph

instance (FromJSON t, TransitionValid t) => FromJSON (Graph t)
instance (ToJSON t, TransitionValid t) => ToJSON (Graph t) where
  toEncoding = genericToEncoding defaultOptions

nodeMap :: Graph t -> Map Id (Node t)
nodeMap = view graphNodeMap

withNodeMap :: Graph t -> (Map Id (Node t) -> Map Id (Node t)) -> Graph t
withNodeMap = flip (over graphNodeMap)

-- | unbiased representation of an edge
data Edge t = Edge
  { _edgeSource :: Id
  , _edgeTransition :: t
  , _edgeSink :: Id
  }
makeLenses ''Edge
