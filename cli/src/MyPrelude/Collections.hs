module MyPrelude.Collections
  ( module X,
    module MyPrelude.Collections,
  )
where

import ClassyPrelude
import Control.Lens hiding (op)
import qualified Data.Set as Set
import GHC.Stack (HasCallStack)
import Data.List.NonEmpty as X (NonEmpty (..))
import Data.List as X (iterate)
import Control.Lens.Extras as X (is)
import Data.Set.Lens as X (setmapped)

import Control.Lens as X
    ( over,
      set,
      view,
      (&),
      (.~),
      (<&>),
      (^.),
      at,
      to,
      ix,
      _Just,
      (?~),
      (%~),
      preview,
      (^?),
      has,
      hasn't,
      isn't,
      folded,
      filtered,
      _Right,
      _Left,
      Lens,
      Lens',
      Traversal,
      Traversal',
      Prism,
      Prism',
      Iso,
      Iso',
      Getter,
      Fold,
      Setter,
      Setter',
      ALens,
      ALens',
      ASetter,
      ASetter',
      Getting,
      Setting,
      LensLike,
      LensLike'
    )

-- | Copied from cabal codebase
toSetOf :: Getting (Set a) s a -> s -> Set a
toSetOf l s = getConst (l (Const . singleton) s)

mapSet :: Ord b => (a -> b) -> Set a -> Set b
mapSet = Set.map

-- | do something when a mono is NonNull
whenNonNull ::
  (MonoFoldable mono, Applicative f) =>
  mono ->
  (NonNull mono -> f ()) ->
  f ()
whenNonNull mono = for_ (fromNullable mono)

-- | generalized foldl1 monadically
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
concatMapM :: Monad m => (a -> m [b]) -> [a] -> m [b]
concatMapM op = foldr f (return [])
  where
    f x xs = do x' <- op x; if null x' then xs else do { xs' <- xs; return $ x' ++ xs' }

-- | A newtype wrapper for a list that contains at least two elements
newtype TwoElemList a = UnsafeTwoElemList {unTwoElemList :: [a]}
  deriving stock (Show, Eq, Ord, Functor, Foldable, Traversable)
  deriving newtype (Semigroup)
  deriving anyclass (MonoFoldable, MonoTraversable, MonoFunctor)

type instance Element (TwoElemList a) = a

twoElemList :: a -> a -> [a] -> TwoElemList a
twoElemList x x' xs = UnsafeTwoElemList (x : x' : xs)

intoElems :: HasCallStack => TwoElemList a -> (a, NonEmpty a)
intoElems (UnsafeTwoElemList (x : x' : xs)) = (x, x' :| xs)
intoElems (UnsafeTwoElemList _) = error "intoElems: broken invariant"

chunksOf :: Int -> [a] -> [[a]]
chunksOf _ [] = []
chunksOf n xs
  | length xs < n = [xs]
  | otherwise =
    let (chunk, rest) = splitAt n xs
     in chunk : chunksOf n rest
