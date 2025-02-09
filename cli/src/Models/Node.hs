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

data Node t a = Node
  { nid :: NID,
    incoming :: Set (Connect t),
    outgoing :: Set (Connect t),
    associatedData :: a
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

indegreeOf :: Node t a -> Int
indegreeOf = Set.size . view #incoming

outdegreeOf :: Node t a -> Int
outdegreeOf = Set.size . view #outgoing

incomingConnectsOf ::
  ValidNode t a =>
  Node t a ->
  Set (Connect t)
incomingConnectsOf = view #incoming

outgoingConnectsOf ::
  ValidNode t a =>
  Node t a ->
  Set (Connect t)
outgoingConnectsOf = view #outgoing

incomingNeighborsOf ::
  ValidNode t a =>
  Node t a ->
  Set NID
incomingNeighborsOf = Set.map (view #node) . incomingConnectsOf

incomingTransitionsOf ::
  ValidNode t a =>
  Node t a ->
  Set t
incomingTransitionsOf = Set.map (view #transition) . incomingConnectsOf

outgoingNeighborsOf ::
  ValidNode t a =>
  Node t a ->
  Set NID
outgoingNeighborsOf = Set.map (view #node) . incomingConnectsOf

outgoingTransitionsOf ::
  ValidNode t a =>
  Node t a ->
  Set t
outgoingTransitionsOf = Set.map (view #transition) . outgoingConnectsOf

dataOf ::
  ValidNode t a =>
  Node t a ->
  a
dataOf = view #associatedData

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
