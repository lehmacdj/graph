{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE UndecidableInstances #-}

module Models.Node
  ( module Models.Node,
    module Models.Common,
  )
where

import Control.DeepSeq
import Control.Lens
import Data.Semigroup qualified as Semigroup (Last (Last))
import Data.Set qualified as Set
import GHC.Records
import Models.Common
import Models.Connect
import Models.Edge
import Models.NID
import MyPrelude

type ValidNode t a = (ValidTransition t, Eq a)

-- | variant of ValidNode that doesn't impose HasCallStack
-- to be used in instances/places where we don't want to impose that
type ValidNodeNCS t a = (ValidTransitionNCS t, Eq a)

type ValidNode' ti to a = (ValidTransition ti, ValidTransition to, Eq a)

-- | variant of ValidNode' that doesn't impose HasCallStack
-- to be used in instances/places where we don't want to impose that
type ValidNode'NCS ti to a = (ValidTransitionNCS ti, ValidTransitionNCS to, Eq a)

class DefaultAugmentation a where
  defaultAugmentation :: a

-- | This requires that @mempty == defaultAugmentation@ if @Monoid a@
class (DefaultAugmentation a, Semigroup a) => MonoidAugmentation a

instance DefaultAugmentation (Maybe a) where
  defaultAugmentation = Nothing

instance (Semigroup a) => MonoidAugmentation (Maybe a)

instance DefaultAugmentation () where
  defaultAugmentation = ()

instance MonoidAugmentation ()

instance (DefaultAugmentation a) => DefaultAugmentation (Semigroup.Last a) where
  defaultAugmentation = Semigroup.Last defaultAugmentation

instance (DefaultAugmentation a) => MonoidAugmentation (Semigroup.Last a)

-- | A node in a graph. To allow separately mapping over the incoming and
-- outgoing edges in a type changing way, we have two type parameters.
-- In general we only use the typealias that fixes the type of the transitions
-- to be the same.
data Node' ti to a = Node
  { nid :: NID,
    incoming :: Set (Connect ti),
    outgoing :: Set (Connect to),
    augmentation :: a
  }
  deriving (Eq, Ord, Generic, NFData, Functor)

instance Comonad (Node' ti to) where
  extract = (.augmentation)
  duplicate n = n {augmentation = n}

-- | A node in the graph. See 'Node'' for more information.
type Node t a = Node' t t a

instance
  forall ti to a.
  (Show ti, Ord ti, Show to, Ord to) =>
  CompactNodeShow (Node' ti to a)
  where
  type Augmentation (Node' ti to a) = a
  minimumNidLength settings@CompactNodeShowSettings {..} n =
    maximum . ncons (getMinLen n.nid) $ toList $ incomingLens <> outgoingLens
    where
      getMinLen :: forall x. (CompactNodeShow x) => x -> Int
      getMinLen = minimumNidLength settings
      incomingLens
        | showIncoming = mapSet getMinLen n.incoming
        | otherwise = mempty
      outgoingLens = mapSet getMinLen n.outgoing
  nshowSettings settings@CompactNodeShowSettings {..} Node {..} =
    nshowSettings (unconsumed settings) nid ++ "{" ++ intercalate ", " reprParts ++ "}"
    where
      reprParts = catMaybes [outgoingRepr, incomingRepr, augmentationRepr]
      incomingRepr :: Maybe Text
      incomingRepr =
        justIfTrue showIncoming
          $ "in=["
          ++ intercalate ", " (nshowSettings (unconsumed settings) <$> toList incoming)
          ++ "]"
      outgoingRepr :: Maybe Text
      outgoingRepr =
        Just
          $ "out=["
          ++ intercalate ", " (nshowSettings (unconsumed settings) <$> toList outgoing)
          ++ "]"
      augmentationRepr :: Maybe Text
      augmentationRepr =
        showAugmentation <&> \(name, showAug) ->
          name ++ "=" ++ showAug augmentation

instance
  {-# OVERLAPPABLE #-}
  (Show ti, Ord ti, Show to, Ord to, ShowableAugmentation a) =>
  Show (Node' ti to a)
  where
  show = unpack . nshowDefault

-- | We implement special @HasField@ instances for specific types of
-- augmentation so that we can refer to them in a more intuitive way.
instance HasField "rawData" (Node' ti to (Maybe ByteString)) (Maybe ByteString) where
  getField = (.augmentation)

indegree :: Getter (Node t a) Int
indegree = #incoming . to Set.size

outdegree :: Getter (Node t a) Int
outdegree = #outgoing . to Set.size

-- | Warning! It is up to the user of the graph to ensure that node ids are
-- unique within the graph
emptyNode :: (Ord t, DefaultAugmentation a) => NID -> Node t a
emptyNode i = Node i mempty mempty defaultAugmentation

-- | Warning! It is up to the user of the graph to ensure that node ids are
-- unique within the graph
emptyNode' :: (Ord t) => NID -> Node t (Maybe a)
emptyNode' = emptyNode

inStubNode :: (Ord t, DefaultAugmentation a) => Edge t -> Node t a
inStubNode e = Node e.sink (singleton $ inConnect e) mempty defaultAugmentation

outStubNode :: (Ord t, DefaultAugmentation a) => Edge t -> Node t a
outStubNode e = Node e.source mempty (singleton $ outConnect e) defaultAugmentation

dualizeNode :: Node t a -> Node t a
dualizeNode (Node nid i o x) = Node nid o i x

mergeNodes ::
  (Ord t, HasCallStack, Semigroup a) =>
  Node t a ->
  Node t a ->
  Maybe (Node t a)
mergeNodes n1 n2 =
  justIfTrue (n1.nid == n2.nid)
    $ Node
      { nid = n1.nid,
        incoming = n1.incoming <> n2.incoming,
        outgoing = n1.outgoing <> n2.outgoing,
        augmentation = n1.augmentation <> n2.augmentation
      }

mergeNodesEx ::
  (Ord t, HasCallStack, Semigroup a) =>
  Node t a ->
  Node t a ->
  Node t a
mergeNodesEx n1 n2 =
  fromMaybe (error "mergeNodesEx: nodes don't match") $ mergeNodes n1 n2

withoutEdges :: (ValidTransition t) => Set (Edge t) -> Node t a -> Node t a
withoutEdges deletedEdges n =
  n
    & #incoming %~ (\\ mapSet inConnect deletedEdges)
    & #outgoing %~ (\\ mapSet outConnect deletedEdges)

withEdge :: (ValidTransition t) => Edge t -> Node t a -> Node t a
withEdge e n =
  n
    & #incoming %~ maybe id insertSet (justIfTrue (e.sink == n.nid) (inConnect e))
    & #outgoing %~ maybe id insertSet (justIfTrue (e.source == n.nid) (outConnect e))
