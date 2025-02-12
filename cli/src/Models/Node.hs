module Models.Node
  ( module Models.Node,
    module Models.Common,
  )
where

import Control.DeepSeq
import Control.Lens
import qualified Data.Set as Set
import Models.Common
import Models.Connect
import Models.NID
import MyPrelude
import GHC.Records

data Node t a = Node
  { nid :: NID,
    incoming :: Set (Connect t),
    outgoing :: Set (Connect t),
    augmentation :: a
  }
  deriving (Eq, Ord, Generic, NFData, Functor)

instance (Show t, Ord t) => Show (Node t a) where
  show Node {..} =
    show nid ++ "{"
      ++ "in="
      ++ show (toList incoming)
      ++ ", out="
      ++ show (toList outgoing)
      ++ "}"

-- | We implement special @HasField@ instances for specific types of
-- augmentation so that we can refer to them in a more intuitive way.
instance HasField "rawData" (Node t (Maybe ByteString)) (Maybe ByteString) where
  getField = (.augmentation)

indegree :: Getter (Node t a) Int
indegree = #incoming . to Set.size

outdegree :: Getter (Node t a) Int
outdegree = #outgoing . to Set.size

-- | Warning! It is up to the user of the graph to ensure that node ids are
-- unique within the graph
emptyNode :: Ord t => NID -> Node t ()
emptyNode i = Node i mempty mempty ()

-- | Warning! It is up to the user of the graph to ensure that node ids are
-- unique within the graph
emptyNode' :: Ord t => NID -> Node t (Maybe ByteString)
emptyNode' i = emptyNode i $> Nothing

dualizeNode :: Node t a -> Node t a
dualizeNode (Node nid i o x) = Node nid o i x

mergeNodes ::
  (Ord t, HasCallStack, Semigroup a) =>
  Node t a -> Node t a -> Maybe (Node t a)
mergeNodes n1 n2 =
  justIfTrue (n1.nid == n2.nid) $
    Node
      { nid = n1.nid,
        incoming = n1.incoming <> n2.incoming,
        outgoing = n1.outgoing <> n2.outgoing,
        augmentation = n1.augmentation <> n2.augmentation
      }

mergeNodesEx ::
  (Ord t, HasCallStack, Semigroup a) =>
  Node t a -> Node t a -> Node t a
mergeNodesEx n1 n2 =
  fromMaybe (error "mergeNodesEx: nodes don't match") $ mergeNodes n1 n2
