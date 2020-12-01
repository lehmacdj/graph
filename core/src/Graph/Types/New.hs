{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Graph.Types.New where

import Control.DeepSeq
import Control.Lens (makeLenses)
import Data.Aeson
import Effect.FreshNID
import GHC.Generics
import qualified Graph
import Graph.Types
import MyPrelude
import Polysemy.State

data UnlabledEdge = UnlabledEdge
  { _unlabledEdgeSource :: NID,
    _unlabledEdgeSink :: NID
  }
  deriving (Show, Eq, Ord, Generic, NFData)

instance FromJSON UnlabledEdge

instance ToJSON UnlabledEdge where
  toEncoding = genericToEncoding defaultOptions

-- | New type of graph node that is inherently labled with NIDs. In this view
-- we also want to be able to access referents of edges efficiently so we
-- also include those in the node's representation
data Node' = Node'
  { -- | unique node id
    _nodeId' :: NID,
    _nodeIncoming' :: Set (Connect NID),
    _nodeOutgoing' :: Set (Connect NID),
    _nodeReferents :: Set UnlabledEdge,
    _nodeData' :: Maybe ByteString
  }
  deriving (Eq, Ord, Generic, NFData)

instance Show Node' where
  show n =
    show (_nodeId' n) ++ "{"
      ++ "in="
      ++ show (toList . _nodeIncoming' $ n)
      ++ ", out="
      ++ show (toList . _nodeOutgoing' $ n)
      ++ ", refs="
      ++ show (toList . _nodeReferents $ n)
      ++ "}"

makeLenses ''Node'

-- | For the purpose of implementing from and ToJSON we use Prenode'.
data Prenode' = Prenode'
  { -- | unique node id
    _prenode'Id :: NID,
    _prenode'Incoming :: Set (Connect NID),
    _prenode'Outgoing :: Set (Connect NID),
    _prenode'Referents :: Set UnlabledEdge
  }
  deriving (Show, Eq, Ord, Generic, NFData)

instance FromJSON Prenode' where
  parseJSON = genericParseJSON (strippedPrefixOptions "_prenode'")

instance ToJSON Prenode' where
  toEncoding = genericToEncoding (strippedPrefixOptions "_prenode'")

prenodeToNode' :: Prenode' -> Node'
prenodeToNode' (Prenode' nid i o r) = Node' nid i o r Nothing

nodeToPrenode' :: Node' -> Prenode'
nodeToPrenode' (Node' nid i o r _) = Prenode' nid i o r

instance FromJSON Node' where
  parseJSON = fmap prenodeToNode' . parseJSON

instance ToJSON Node' where
  toJSON = toJSON . nodeToPrenode'
  toEncoding = toEncoding . nodeToPrenode'

newtype Graph' = Graph'
  { _graphNodeMap' :: Map NID Node'
  }
  deriving (Eq, Ord, Generic, NFData)

makeLenses ''Graph'

instance Show Graph' where
  show = unlines' . map show . toList . _graphNodeMap'
    where
      unlines' = intercalate "\n"

internString ::
  Members [State (Map String NID), FreshNID] r =>
  String ->
  Sem r NID
internString str = do
  m <- get
  case lookup str m of
    Nothing -> do
      nid <- freshNID
      modify (insertMap str nid)
      pure nid
    Just nid -> pure nid

convertGraph :: Graph String -> Graph'
convertGraph g =
  let runEffs =
        run
          . evalState (Graph.nextFreeNodeId g)
          . runFreshNIDState
          . evalState @(Map String NID) mempty
   in runEffs $ do
        -- TODO: build a map of strings, then assign each of them a new node in the
        -- new graph, linking everything together using a folder of node labels called
        -- string
        -- TODO: will need to keep track of references of strings too while
        -- processing the conversion
        undefined
