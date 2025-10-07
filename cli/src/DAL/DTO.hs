-- | Data transfer objects representing graph objects and conversions to/from
-- them. Used for (de)serializing from/to JSON.
--
-- CAUTION: Modifying this module, and the associated tests is a breaking
-- change to the way we serialize graphs!
module DAL.DTO where

import Data.Aeson
import Error.UserError
import Models.Connect
import Models.NID
import Models.Node
import MyPrelude

data NodeDTO t = NodeDTO
  { id :: NID,
    incoming :: Set (ConnectDTO t),
    outgoing :: Set (ConnectDTO t)
  }
  deriving stock (Show, Eq, Ord, Generic)
  deriving (ToJSON, FromJSON) via FastGenericEncoding (NodeDTO t)
  deriving anyclass (NFData)

nodeToDTO :: (Ord t) => Node t a -> NodeDTO t
nodeToDTO Node {..} =
  NodeDTO
    { id = nid,
      incoming = mapSet connectToDTO incoming,
      outgoing = mapSet connectToDTO outgoing
    }

nodeFromDTO :: (Ord t) => NodeDTO t -> Node t ()
nodeFromDTO NodeDTO {id = nid, ..} =
  Node
    { augmentation = (),
      incoming = mapSet connectFromDTO incoming,
      outgoing = mapSet connectFromDTO outgoing,
      nid
    }

data ConnectDTO t = ConnectDTO
  { t :: t,
    n :: NID
  }
  deriving (Show, Eq, Ord, Generic)
  deriving (ToJSON, FromJSON) via FastGenericEncoding (ConnectDTO t)
  deriving anyclass (NFData)

connectToDTO :: Connect t -> ConnectDTO t
connectToDTO Connect {..} = ConnectDTO {t = transition, n = node}

connectFromDTO :: ConnectDTO t -> Connect t
connectFromDTO ConnectDTO {..} = Connect {transition = t, node = n}

decodeNode ::
  forall t effs.
  (Ord t, FromJSON t, Member (Error UserError) effs) =>
  NID ->
  ByteString ->
  Sem effs (Node t ())
decodeNode nid =
  throwLeft
    . bimap (FailedToDeserializeNode nid) nodeFromDTO
    . (eitherDecodeStrict @(NodeDTO t))
