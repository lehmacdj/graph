{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Graph.Types
  ( module Graph.Types
  ) where

import Control.DeepSeq
import GHC.Generics
import Data.ByteString.Lazy (ByteString)
import Data.Maybe (fromJust)
import Data.List (stripPrefix, intercalate)
import Data.Char (toLower)
import Data.Foldable (toList)

import Data.Aeson

import Control.Lens

import Data.Map (Map)
import qualified Data.Map as Map

import Data.Set (Set)

type TransitionValid t = (Show t, Eq t, Ord t)

type Id = Int

-- | A transition from/to a node to/from another node.
-- The first node isn't represented here, because this is used only in the node
-- structure where the first node is clear from context.
data Connect t = Connect
  { _connectTransition :: t
  , _connectNode :: Id
  } deriving (Eq, Ord, Generic, NFData)
makeLenses ''Connect

instance Show t => Show (Connect t) where
  show (Connect t nid) = show t ++ " at " ++ show nid

instance (FromJSON t, TransitionValid t) => FromJSON (Connect t)
instance (ToJSON t, TransitionValid t) => ToJSON (Connect t) where
  toEncoding = genericToEncoding defaultOptions

data Node t = Node
  { _nodeId :: Id
  -- ^ unique node id
  , _nodeIncoming :: Set (Connect t)
  , _nodeOutgoing :: Set (Connect t)
  , _nodeData :: Maybe ByteString
  } deriving (Eq, Ord, Generic, NFData)
makeLenses ''Node

instance Show t => Show (Node t) where
  show n =
    show (_nodeId n) ++ "{"
    ++ "in=" ++ show (toList . _nodeIncoming $ n)
    ++ ", out=" ++ show (toList . _nodeOutgoing $ n) ++ "}"

-- | For the purpose of implementing from and ToJSON we use Prenode.
data Prenode t = Prenode
  { _prenodeId :: Id
  -- ^ unique node id
  , _prenodeIncoming :: Set (Connect t)
  , _prenodeOutgoing :: Set (Connect t)
  } deriving (Show, Eq, Ord, Generic, NFData)
makeLenses ''Prenode

lowercaseFirst :: String -> String
lowercaseFirst [] = []
lowercaseFirst (x:xs) = toLower x:xs

prenodeOptions :: Options
prenodeOptions = defaultOptions
  { fieldLabelModifier = lowercaseFirst . fromJust . stripPrefix "_prenode"
  }

prenodeToNode :: Prenode t -> Node t
prenodeToNode (Prenode nid i o) = Node nid i o Nothing

nodeToPrenode :: Node t -> Prenode t
nodeToPrenode (Node nid i o _) = Prenode nid i o

instance (FromJSON t, TransitionValid t) => FromJSON (Prenode t) where
  parseJSON = genericParseJSON prenodeOptions
instance (ToJSON t, TransitionValid t) => ToJSON (Prenode t) where
  toEncoding = genericToEncoding prenodeOptions

instance (FromJSON t, TransitionValid t) => FromJSON (Node t) where
  parseJSON = fmap prenodeToNode . parseJSON
instance (ToJSON t, TransitionValid t) => ToJSON (Node t) where
  toJSON = toJSON . nodeToPrenode
  toEncoding = toEncoding . nodeToPrenode

newtype Graph t = Graph
  { _graphNodeMap :: Map Id (Node t)
  } deriving (Eq, Ord, Generic, NFData)
makeLenses ''Graph

instance Show t => Show (Graph t) where
  show = unlines' . map show . Map.elems . _graphNodeMap where
    unlines' = intercalate "\n"

instance (FromJSON t, TransitionValid t) => FromJSON (Graph t)
instance (ToJSON t, TransitionValid t) => ToJSON (Graph t) where
  toEncoding = genericToEncoding defaultOptions

nodeMap :: Graph t -> Map Id (Node t)
nodeMap = view graphNodeMap

withNodeMap :: Graph t -> (Map Id (Node t) -> Map Id (Node t)) -> Graph t
withNodeMap = flip (over graphNodeMap)

dualizeNode :: Node t -> Node t
dualizeNode (Node nid i o x) = Node nid o i x

-- | unbiased representation of an edge
data Edge t = Edge
  { _edgeSource :: Id
  , _edgeTransition :: t
  , _edgeSink :: Id
  } deriving (Eq, Ord, Generic, NFData)
makeLenses ''Edge
