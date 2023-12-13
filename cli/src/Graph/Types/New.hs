module Graph.Types.New where

import Control.DeepSeq
import Data.Aeson
import GHC.Generics
import Graph.Types
import MyPrelude

data UnlabledEdge = UnlabledEdge
  { source :: NID,
    sink :: NID
  }
  deriving (Eq, Ord, Generic, NFData)

instance Show UnlabledEdge where
  show (UnlabledEdge i o) = show i ++ "->" ++ show o

instance FromJSON UnlabledEdge

instance ToJSON UnlabledEdge where
  toEncoding = genericToEncoding defaultOptions

-- | New type of graph node that is inherently labled with NIDs. In this view
-- we also want to be able to access referents of edges efficiently so we
-- also include those in the node's representation
data Node' = Node'
  { -- | unique node id
    -- TODO once we have NoFieldSelectors use that and name this id
    nodeId' :: NID,
    incoming' :: Set (Connect NID),
    outgoing' :: Set (Connect NID),
    referents :: Set UnlabledEdge,
    associatedData' :: Maybe ByteString
  }
  deriving (Eq, Ord, Generic, NFData)

instance Show Node' where
  show Node' {..} =
    show nodeId' ++ "{"
      ++ "in="
      ++ show (toList incoming')
      ++ ", out="
      ++ show (toList outgoing')
      ++ ", refs="
      ++ show (toList referents)
      ++ ", data="
      ++ show associatedData'
      ++ "}"

referentEdge :: Edge NID -> UnlabledEdge
referentEdge (Edge i _ o) = UnlabledEdge i o

labledWith :: NID -> UnlabledEdge -> Edge NID
labledWith l (UnlabledEdge i o) = Edge i l o

newtype Graph' = Graph'
  { nodeMap' :: Map NID Node'
  }
  deriving stock (Eq, Ord, Generic)
  deriving anyclass (NFData)

instance Show Graph' where
  show = unlines' . map show . toList . nodeMap'
    where
      unlines' = intercalate "\n"

withNodeMap' :: Graph' -> (Map NID Node' -> Map NID Node') -> Graph'
withNodeMap' = flip (over #nodeMap')

dualizeUnlabledEdge :: UnlabledEdge -> UnlabledEdge
dualizeUnlabledEdge (UnlabledEdge i o) = UnlabledEdge o i

dualizeNode' :: Node' -> Node'
dualizeNode' (Node' nid i o r x) = Node' nid o i (mapSet dualizeUnlabledEdge r) x
