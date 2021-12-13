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
  { transition :: t,
    node :: NID
  }
  deriving (Eq, Ord, Generic, NFData)

instance Show t => Show (Connect t) where
  show (Connect t nid) = show nid ++ " via " ++ show t

instance (FromJSON t, TransitionValid t) => FromJSON (Connect t)

instance (ToJSON t, TransitionValid t) => ToJSON (Connect t) where
  toEncoding = genericToEncoding defaultOptions

data Node t = Node
  { -- | unique node id
    -- TODO once we have NoFieldSelectors use that and name this id
    nodeId :: NID,
    incoming :: Set (Connect t),
    outgoing :: Set (Connect t),
    associatedData :: Maybe ByteString
  }
  deriving (Eq, Ord, Generic, NFData)

instance (Show t, Ord t) => Show (Node t) where
  show Node {..} =
    show nodeId ++ "{"
      ++ "in="
      ++ show (toList incoming)
      ++ ", out="
      ++ show (toList outgoing)
      ++ "}"

lowercaseFirst :: String -> String
lowercaseFirst [] = []
lowercaseFirst (x : xs) = Char.toLower x : xs

strippedPrefixOptions :: String -> Options
strippedPrefixOptions prefix =
  defaultOptions
    { fieldLabelModifier = lowercaseFirst . fromJust . stripPrefix prefix
    }

newtype Graph t = Graph
  { nodeMap :: Map NID (Node t)
  }
  deriving stock (Eq, Ord, Generic)
  deriving anyclass (NFData)

instance (Show t, Ord t) => Show (Graph t) where
  show = unlines' . map show . Map.elems . nodeMap
    where
      unlines' = intercalate "\n"

withNodeMap :: Graph t -> (Map NID (Node t) -> Map NID (Node t)) -> Graph t
withNodeMap = flip (over #nodeMap)

-- | unbiased representation of an edge
data Edge t = Edge
  { source :: NID,
    transition :: t,
    sink :: NID
  }
  deriving (Eq, Ord, Generic, NFData, Show)
