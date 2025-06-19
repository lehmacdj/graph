module MyPrelude.Collections
  ( module X,
    module MyPrelude.Collections,
  )
where

import ClassyPrelude
import Control.Lens hiding (op)
import Control.Lens as X
  ( ALens,
    ALens',
    ASetter,
    ASetter',
    Fold,
    Getter,
    Getting,
    IndexedFold,
    Iso,
    Iso',
    Lens,
    Lens',
    LensLike,
    LensLike',
    Prism,
    Prism',
    Setter,
    Setter',
    Setting,
    Traversal,
    Traversal',
    ala,
    at,
    au,
    filtered,
    filteredBy,
    foldOf,
    folded,
    has,
    hasn't,
    ifiltered,
    ifolded,
    isn't,
    iso,
    ix,
    mapped,
    mapping,
    nearly,
    non,
    non',
    only,
    over,
    preview,
    set,
    to,
    view,
    (%~),
    (&),
    (.~),
    (<&>),
    (?~),
    (^.),
    (^..),
    (^?),
    (^@..),
    _1,
    _2,
    _3,
    _Just,
    _Left,
    _Right,
  )
import Control.Lens.Extras as X (is)
import Data.Generics.Labels as X
import Data.Generics.Wrapped as X (_Unwrapped, _Wrapped)
import Data.IxSet.Typed qualified as IxSet
import Data.List as X (iterate, tails)
import Data.List.NonEmpty as X (NonEmpty (..))
import Data.Map.Strict qualified as Map
import Data.Set qualified as Set
import Data.Set.Lens as X (setmapped)
import GHC.Stack (HasCallStack)
import MyPrelude.Orphans ()

-- | Copied from cabal codebase
toSetOf :: Getting (Set a) s a -> s -> Set a
toSetOf l s = getConst (l (Const . singleton) s)

mapSet :: (Ord b) => (a -> b) -> Set a -> Set b
mapSet = Set.map

mapFromSet :: (Ord k) => Set k -> Map k ()
mapFromSet = Map.fromSet (const ())

mapFromSetBy :: (Ord k) => (k -> v) -> Set k -> Map k v
mapFromSetBy = Map.fromSet

-- | do something when a mono is NonNull
whenNonNull ::
  (MonoFoldable mono, Applicative f) =>
  mono ->
  (NonNull mono -> f ()) ->
  f ()
whenNonNull mono = for_ (fromNullable mono)

type NNMap k v = NonNull (Map k v)

type NNSet a = NonNull (Set a)

singletonNN :: (IsSequence s) => Element s -> NonNull s
singletonNN = impureNonNull . singleton

singletonNNSet :: (IsSet s) => Element s -> NonNull s
singletonNNSet = impureNonNull . singletonSet

singletonNNMap :: (IsMap m) => ContainerKey m -> MapValue m -> NonNull m
singletonNNMap k v = impureNonNull $ singletonMap k v

singletonNNIxSet :: (IxSet.Indexable ixs v) => v -> NonNull (IxSet.IxSet ixs v)
singletonNNIxSet = impureNonNull . IxSet.fromList . singleton

-- | generalized foldl1 monadically
-- i.e. @foldlM1 :: (a -> a -> Maybe a) -> NonNull [a] -> Maybe a@
foldlM1 ::
  (IsSequence mono, Monad m) =>
  (Element mono -> Element mono -> m (Element mono)) ->
  NonNull mono ->
  m (Element mono)
foldlM1 f m = uncurry (foldlM f) (splitFirst m)

-- | Fair 2-way interleaving
interleave :: [a] -> [a] -> [a]
interleave [] ys = ys
interleave (x : xs) ys = x : interleave ys xs

-- | Taken from extras-1.6.17
concatMapM :: (Monad m) => (a -> m [b]) -> [a] -> m [b]
concatMapM op = foldr f (return [])
  where
    f x xs = do x' <- op x; if null x' then xs else do xs' <- xs; return $ x' ++ xs'

-- | A newtype wrapper for a list that contains at least two elements
newtype TwoElemList a = UnsafeTwoElemList {unTwoElemList :: [a]}
  deriving stock (Show, Eq, Ord, Functor, Foldable, Traversable)
  deriving newtype (Semigroup)
  deriving anyclass (MonoFoldable, MonoTraversable, MonoFunctor)

type instance Element (TwoElemList a) = a

twoElemList :: a -> a -> [a] -> TwoElemList a
twoElemList x x' xs = UnsafeTwoElemList (x : x' : xs)

intoElems :: (HasCallStack) => TwoElemList a -> (a, NonEmpty a)
intoElems (UnsafeTwoElemList (x : x' : xs)) = (x, x' :| xs)
intoElems (UnsafeTwoElemList _) = error "intoElems: broken invariant"

chunksOf :: Int -> [a] -> [[a]]
chunksOf _ [] = []
chunksOf n xs
  | length xs < n = [xs]
  | otherwise =
      let (chunk, rest) = splitAt n xs
       in chunk : chunksOf n rest

allPairs :: [a] -> [(a, a)]
allPairs xs = [(x, y) | (x : ys) <- tails xs, y <- ys]

allAnyOrderPairs :: [a] -> [(a, a)]
allAnyOrderPairs xs = allPairs xs ++ map swap (allPairs xs)

cartesianProduct :: [a] -> [b] -> [(a, b)]
cartesianProduct xs ys = [(x, y) | x <- xs, y <- ys]

cartesianProductSet :: (Ord a, Ord b) => Set a -> Set b -> Set (a, b)
cartesianProductSet xs ys =
  setFromList $ cartesianProduct (toList xs) (toList ys)

assertSingleton :: (HasCallStack, Show a) => [a] -> a
assertSingleton [x] = x
assertSingleton xs = error $ "assertSingleton: not a singleton: " <> show xs

assertMaxOne :: (HasCallStack, Show a) => [a] -> Maybe a
assertMaxOne [] = Nothing
assertMaxOne [x] = Just x
assertMaxOne xs = error $ "assertMaxOne: not at most one: " <> show xs

traverseSet ::
  (Applicative f, Ord b, Ord (f b)) => (a -> f b) -> Set a -> f (Set b)
traverseSet f s = setFromList <$> sequenceA (toList (mapSet f s))

ixsetmapped ::
  (IxSet.All Ord jxs, IxSet.Indexable jxs j) =>
  IndexPreservingSetter (IxSet.IxSet ixs i) (IxSet.IxSet jxs j) i j
ixsetmapped = iso IxSet.toSet IxSet.fromSet . setmapped

nnmap :: (HasCallStack, MonoFoldable (f b), Functor f) => (a -> b) -> NonNull (f a) -> NonNull (f b)
nnmap f = impureNonNull . fmap f . toNullable
