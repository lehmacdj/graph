-- | Data transfer objects representing graph objects and conversions to/from
-- them. Used for (de)serializing from/to JSON.
--
-- CAUTION: Modifying this module, and the associated tests is a breaking
-- change to the way we serialize graphs!
module Graph.DataTransferObjects.V3 where

import Data.Aeson
import Graph.DataTransferObjects (ConnectDTO (..), connectFromDTO, connectToDTO)
import Graph.Types
import Graph.Types.New
import MyPrelude

data NodeDTO = NodeDTO
  { id :: NID,
    incoming :: Set (ConnectDTO NID),
    outgoing :: Set (ConnectDTO NID),
    referents :: Set UnlabledEdgeDTO
  }
  deriving stock (Show, Eq, Ord, Generic)
  deriving (ToJSON, FromJSON) via FastGenericEncoding NodeDTO
  deriving anyclass (NFData)

nodeToDTO :: Node' -> NodeDTO
nodeToDTO Node' {..} =
  NodeDTO
    { id = nodeId',
      incoming = mapSet connectToDTO incoming',
      outgoing = mapSet connectToDTO outgoing',
      referents = mapSet unlabledEdgeToDTO referents
    }

nodeFromDTO :: NodeDTO -> Node'
nodeFromDTO NodeDTO {id = nodeId, ..} =
  Node'
    { nodeId' = nodeId,
      incoming' = mapSet connectFromDTO incoming,
      outgoing' = mapSet connectFromDTO outgoing,
      referents = mapSet unlabledEdgeFromDTO referents,
      associatedData' = Nothing
    }

data UnlabledEdgeDTO = UnlabledEdgeDTO
  { s :: NID,
    t :: NID
  }
  deriving (Show, Eq, Ord, Generic)
  deriving (ToJSON, FromJSON) via FastGenericEncoding UnlabledEdgeDTO
  deriving anyclass (NFData)

unlabledEdgeToDTO :: UnlabledEdge -> UnlabledEdgeDTO
unlabledEdgeToDTO UnlabledEdge {..} = UnlabledEdgeDTO {s = source, t = sink}

unlabledEdgeFromDTO :: UnlabledEdgeDTO -> UnlabledEdge
unlabledEdgeFromDTO UnlabledEdgeDTO {..} = UnlabledEdge {source = s, sink = t}
