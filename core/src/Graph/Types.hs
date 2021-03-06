{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}

module Graph.Types
  ( module Graph.Types,
  )
where

import Control.DeepSeq
import Control.Lens
import Data.Aeson
import qualified Data.Char as Char
import qualified Data.Map as Map
import Data.Maybe (fromJust)
import GHC.Generics
import MyPrelude

type NID = Int

nilNID :: NID
nilNID = 0

smallNID :: Int -> NID
smallNID = id

type TransitionValid t = (Show t, Eq t, Ord t)

-- | A transition from/to a node to/from another node.
-- The first node isn't represented here, because this is used only in the node
-- structure where the first node is clear from context.
data Connect t = Connect
  { _connectTransition :: t,
    _connectNode :: NID
  }
  deriving (Eq, Ord, Generic, NFData)

makeLenses ''Connect

instance Show t => Show (Connect t) where
  show (Connect t nid) = show nid ++ " via " ++ show t

instance (FromJSON t, TransitionValid t) => FromJSON (Connect t)

instance (ToJSON t, TransitionValid t) => ToJSON (Connect t) where
  toEncoding = genericToEncoding defaultOptions

data Node t = Node
  { -- | unique node id
    _nodeId :: NID,
    _nodeIncoming :: Set (Connect t),
    _nodeOutgoing :: Set (Connect t),
    _nodeData :: Maybe LByteString
  }
  deriving (Eq, Ord, Generic, NFData)

makeLenses ''Node

instance (Show t, Ord t) => Show (Node t) where
  show n =
    show (_nodeId n) ++ "{"
      ++ "in="
      ++ show (toList . _nodeIncoming $ n)
      ++ ", out="
      ++ show (toList . _nodeOutgoing $ n)
      ++ "}"

-- | For the purpose of implementing from and ToJSON we use Prenode.
data Prenode t = Prenode
  { -- | unique node id
    _prenodeId :: NID,
    _prenodeIncoming :: Set (Connect t),
    _prenodeOutgoing :: Set (Connect t)
  }
  deriving (Show, Eq, Ord, Generic, NFData)

makeLenses ''Prenode

instance (FromJSON t, TransitionValid t) => FromJSON (Prenode t) where
  parseJSON = genericParseJSON (strippedPrefixOptions "_prenode")

instance (ToJSON t, TransitionValid t) => ToJSON (Prenode t) where
  toEncoding = genericToEncoding (strippedPrefixOptions "_prenode")

lowercaseFirst :: String -> String
lowercaseFirst [] = []
lowercaseFirst (x : xs) = Char.toLower x : xs

strippedPrefixOptions :: String -> Options
strippedPrefixOptions prefix =
  defaultOptions
    { fieldLabelModifier = lowercaseFirst . fromJust . stripPrefix prefix
    }

prenodeToNode :: Prenode t -> Node t
prenodeToNode (Prenode nid i o) = Node nid i o Nothing

nodeToPrenode :: Node t -> Prenode t
nodeToPrenode (Node nid i o _) = Prenode nid i o

instance (FromJSON t, TransitionValid t) => FromJSON (Node t) where
  parseJSON = fmap prenodeToNode . parseJSON

instance (ToJSON t, TransitionValid t) => ToJSON (Node t) where
  toJSON = toJSON . nodeToPrenode
  toEncoding = toEncoding . nodeToPrenode

newtype Graph t = Graph
  { _graphNodeMap :: Map NID (Node t)
  }
  deriving (Eq, Ord, Generic, NFData)

makeLenses ''Graph

instance (Show t, Ord t) => Show (Graph t) where
  show = unlines' . map show . Map.elems . _graphNodeMap
    where
      unlines' = intercalate "\n"

instance (FromJSON t, TransitionValid t) => FromJSON (Graph t)

instance (ToJSON t, TransitionValid t) => ToJSON (Graph t) where
  toEncoding = genericToEncoding defaultOptions

nodeMap :: Graph t -> Map NID (Node t)
nodeMap = view graphNodeMap

withNodeMap :: Graph t -> (Map NID (Node t) -> Map NID (Node t)) -> Graph t
withNodeMap = flip (over graphNodeMap)

-- | unbiased representation of an edge
data Edge t = Edge
  { _edgeSource :: NID,
    _edgeTransition :: t,
    _edgeSink :: NID
  }
  deriving (Eq, Ord, Generic, NFData, Show)

makeLenses ''Edge
