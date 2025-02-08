{-# LANGUAGE QuantifiedConstraints #-}

module Models.Types
  ( module Models.Types,
    module Models.NID,
  )
where

import Control.DeepSeq
import Control.Lens
import Data.Aeson
import qualified Data.Map as Map
import GHC.Generics
import Models.NID
import MyPrelude

type ValidTransition t = (Show t, Eq t, Ord t)

type ValidNode t a = (Show t, Eq t, Ord t, Eq a)

-- | A transition from/to a node to/from another node.
-- The first node isn't represented here, because this is used only in the node
-- structure where the first node is clear from context.
data Connect t = Connect
  { transition :: t,
    node :: NID
  }
  deriving (Eq, Ord, Generic, NFData)

instance Show t => Show (Connect t) where
  show (Connect t nid) = show nid ++ " via " ++ show t

instance (FromJSON t, ValidTransition t) => FromJSON (Connect t)

instance (ToJSON t, ValidTransition t) => ToJSON (Connect t) where
  toEncoding = genericToEncoding defaultOptions

data Node t a = Node
  { -- | unique node id
    -- TODO once we have NoFieldSelectors use that and name this id
    nodeId :: NID,
    incoming :: Set (Connect t),
    outgoing :: Set (Connect t),
    associatedData :: a
  }
  deriving (Eq, Ord, Generic, NFData, Functor)

instance (Show t, Ord t) => Show (Node t a) where
  show Node {..} =
    show nodeId ++ "{"
      ++ "in="
      ++ show (toList incoming)
      ++ ", out="
      ++ show (toList outgoing)
      ++ "}"

newtype Graph t a = Graph
  { nodeMap :: Map NID (Node t a)
  }
  deriving stock (Eq, Ord, Generic)
  deriving anyclass (NFData)

instance (Show t, Ord t) => Show (Graph t a) where
  show = unlines' . map show . Map.elems . nodeMap
    where
      unlines' = intercalate "\n"

withNodeMap :: Graph t a -> (Map NID (Node t a) -> Map NID (Node t a)) -> Graph t a
withNodeMap = flip (over #nodeMap)

-- | unbiased representation of an edge
data Edge t = Edge
  { source :: NID,
    transition :: t,
    sink :: NID
  }
  deriving (Eq, Ord, Generic, NFData, Show)
