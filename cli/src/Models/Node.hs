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
import Debug.Trace qualified
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

-- | Like 'ValidNode' but generalized over the type of node identifiers.
type ValidNode' n t a = (ValidNID n, ValidTransition t, Eq a)

-- | variant of ValidNode' that doesn't impose HasCallStack
-- to be used in instances/places where we don't want to impose that
type ValidNodeNCS' n t a = (ValidNIDNCS n, ValidTransitionNCS t, Eq a)

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
-- outgoing edges in a type changing way, we have two transition type
-- parameters. The type of node identifiers is a parameter as well.
-- In general we only use the typealiases that fix the type of the identifiers
-- to be 'NID' and the type of the transitions to be the same.
data Node' n ti to a = Node
  { nid :: n,
    incoming :: Set (Connect' n ti),
    outgoing :: Set (Connect' n to),
    augmentation :: a
  }
  deriving (Eq, Ord, Generic, NFData, Functor)

instance Comonad (Node' n ti to) where
  extract = (.augmentation)
  duplicate n = n {augmentation = n}

-- | A node in the graph. See 'Node'' for more information.
type Node t a = Node' NID t t a

instance
  forall n ti to a.
  (Show ti, Ord ti, Show to, Ord to, Ord n, CompactNIDShow n) =>
  CompactNodeShow (Node' n ti to a)
  where
  type Augmentation (Node' n ti to a) = a
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
      reprParts = catMaybes [outgoingRepr, incomingRepr] ++ augmentationReprs
      incomingRepr :: Maybe Text
      incomingRepr =
        justIfTrue showIncoming $
          "in=["
            ++ intercalate ", " (nshowSettings (unconsumed settings) <$> toList incoming)
            ++ "]"
      outgoingRepr :: Maybe Text
      outgoingRepr =
        Just $
          "out=["
            ++ intercalate ", " (nshowSettings (unconsumed settings) <$> toList outgoing)
            ++ "]"
      augmentationReprs :: [Text]
      augmentationReprs =
        showAugmentation & \case
          Nothing -> []
          Just (mLabel, showAug) ->
            let properties = showAug augmentation
                formatted = formatProperties properties
             in case mLabel of
                  Nothing -> [formatted | formatted /= ""]
                  Just label -> [label ++ "=" ++ formatted | formatted /= ""]
      formatProperties :: [(Maybe Text, Text)] -> Text
      formatProperties props
        | null props = ""
        | otherwise = intercalate ", " (formatProperty <$> props)
      formatProperty :: (Maybe Text, Text) -> Text
      formatProperty (Nothing, v) = v
      formatProperty (Just k, v) = k ++ "=" ++ v

instance
  {-# OVERLAPPABLE #-}
  ( Show ti,
    Ord ti,
    Show to,
    Ord to,
    Ord n,
    CompactNIDShow n,
    ShowableAugmentation a
  ) =>
  Show (Node' n ti to a)
  where
  show = unpack . nshowDefault

-- | We implement special @HasField@ instances for specific types of
-- augmentation so that we can refer to them in a more intuitive way.
instance HasField "rawData" (Node' n ti to (Maybe ByteString)) (Maybe ByteString) where
  getField = (.augmentation)

indegree :: Getter (Node' n ti to a) Int
indegree = #incoming . to Set.size

outdegree :: Getter (Node' n ti to a) Int
outdegree = #outgoing . to Set.size

-- | Warning! It is up to the user of the graph to ensure that node ids are
-- unique within the graph
emptyNode ::
  forall a t n. (Ord t, Ord n, DefaultAugmentation a) => n -> Node' n t t a
emptyNode i = emptyNode' i defaultAugmentation

-- | Warning! It is up to the user of the graph to ensure that node ids are
-- unique within the graph
emptyNode' :: forall a t n. (Ord t, Ord n) => n -> a -> Node' n t t a
emptyNode' i = Node i mempty mempty

inStubNode ::
  (Ord t, Ord n, DefaultAugmentation a) => Edge' n t -> Node' n t t a
inStubNode e = Node e.sink (singleton $ inConnect e) mempty defaultAugmentation

inStubNode' :: (Ord t, Ord n) => Edge' n t -> a -> Node' n t t a
inStubNode' e = Node e.sink (singleton $ inConnect e) mempty

outStubNode ::
  (Ord t, Ord n, DefaultAugmentation a) => Edge' n t -> Node' n t t a
outStubNode e =
  Node e.source mempty (singleton $ outConnect e) defaultAugmentation

outStubNode' :: (Ord t, Ord n) => Edge' n t -> a -> Node' n t t a
outStubNode' e = Node e.source mempty (singleton $ outConnect e)

dualizeNode :: Node' n t t a -> Node' n t t a
dualizeNode (Node nid i o x) = Node nid o i x

mergeNodes ::
  (Ord t, Ord n, HasCallStack, Semigroup a) =>
  Node' n t t a ->
  Node' n t t a ->
  Maybe (Node' n t t a)
mergeNodes n1 n2 =
  justIfTrue (n1.nid == n2.nid) $
    Node
      { nid = n1.nid,
        incoming = n1.incoming <> n2.incoming,
        outgoing = n1.outgoing <> n2.outgoing,
        augmentation = n1.augmentation <> n2.augmentation
      }

mergeNodesEx ::
  (Ord t, Ord n, Show n, HasCallStack, Semigroup a) =>
  Node' n t t a ->
  Node' n t t a ->
  Node' n t t a
mergeNodesEx n1 n2 =
  fromMaybe (error msg) $ mergeNodes n1 n2
  where
    msg =
      "mergeNodesEx: nodes don't match " ++ show n1.nid ++ " != " ++ show n2.nid

withoutEdges ::
  (ValidTransition t, ValidNID n) =>
  Set (Edge' n t) ->
  Node' n t t a ->
  Node' n t t a
withoutEdges deletedEdges n =
  n
    & #incoming %~ (\\ mapSet inConnect deletedEdges)
    & #outgoing %~ (\\ mapSet outConnect deletedEdges)

withEdge ::
  (ValidTransition t, ValidNID n) => Edge' n t -> Node' n t t a -> Node' n t t a
withEdge e n =
  n
    & #incoming %~ maybe id insertSet (justIfTrue (e.sink == n.nid) (inConnect e))
    & #outgoing %~ maybe id insertSet (justIfTrue (e.source == n.nid) (outConnect e))

traceNode :: (CompactNodeShow n) => n -> n
traceNode a =
  Debug.Trace.trace (unpack (nshowWith (\x -> x {showIncoming = True}) a)) a
{-# WARNING traceNode "Leaving traces in code" #-}

traceMaybeNode :: (CompactNodeShow x, Show n) => n -> Maybe x -> Maybe x
traceMaybeNode nid =
  maybe
    (Debug.Trace.trace (show nid <> " does not exist") Nothing)
    (Just . traceNode)
{-# WARNING traceMaybeNode "Leaving traces in code" #-}

traceGetNodeMetadata ::
  (CompactNodeShow x, Show n) =>
  (n -> Eff r (Maybe x)) ->
  (n -> Eff r (Maybe x))
traceGetNodeMetadata getNodeMetadata nid =
  traceMaybeNode nid <$> getNodeMetadata nid
{-# WARNING traceGetNodeMetadata "Leaving traces in code" #-}

separatedAugmentation ::
  Iso
    (Node' n t t' a)
    (Node' n t t' b)
    (Node' n t t' (), a)
    (Node' n t t' (), b)
separatedAugmentation =
  iso
    (\n -> (n {augmentation = ()}, n.augmentation))
    (\(n, a) -> n {augmentation = a})
{-# INLINE separatedAugmentation #-}

alongsideMetadata ::
  (Functor f) =>
  LensLike (AlongsideRight f (Node' n t t' ())) s s' a b ->
  LensLike
    f
    (Node' n t t' s)
    (Node' n t t' s')
    (Node' n t t' a)
    (Node' n t t' b)
alongsideMetadata o =
  separatedAugmentation . alongside id o . from separatedAugmentation
{-# INLINE alongsideMetadata #-}

asideMetadata ::
  forall f g a b n t t'.
  APrism f g a b ->
  Prism
    (Node' n t t' f)
    (Node' n t t' g)
    (Node' n t t' a)
    (Node' n t t' b)
asideMetadata p = separatedAugmentation . aside p . from separatedAugmentation
{-# INLINE asideMetadata #-}
