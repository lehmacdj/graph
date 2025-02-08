{-# LANGUAGE UndecidableInstances #-}

module MyPrelude
  ( -- * Fundamentally MyPrelude is ClassyPrelude with some extra stuff
    module ClassyPrelude,

    -- * All functions in this module

    -- Although probably I should change this to an explicit export list
    -- eventually.
    module MyPrelude,

    -- * We use Polysemy extensively

    -- Individual effects are still imported separately because it is kind of
    -- useful for tracking which effects are used in which modules.
    module Polysemy,

    -- * Full module re-exports

    -- Most of these should probably be converted to partial imports of just a
    -- few things
    -- TODO: convert these to individual imports using @module X@ or move them
    -- to their own section
    module Control.DeepSeq,
    module Data.Generics.Labels,

    -- * Miscellaneous additional exports
    module X,
  )
where

import ClassyPrelude hiding (throwString)
import Control.DeepSeq
import Control.Lens hiding (op)
import Control.Lens as X (over, set, view, (&), (.~), (<&>), (^.))
import Data.Aeson
import Data.Coerce as X (Coercible, coerce)
import Data.Generics.Labels
import Data.List as X (iterate)
import Data.List.NonEmpty as X (NonEmpty (..))
import qualified Data.Set as Set
import Data.Set.Ordered.Orphans as X ()
import GHC.Generics as X (Generic, Rep)
import GHC.Stack as X (HasCallStack)
import Polysemy
import Polysemy.Error
import Polysemy.State
import System.IO

-- | natural transformation
type (~>) f g = forall x. f x -> g x

-- | execute a computation only if it is Just
withJust :: Monad m => Maybe a -> (a -> m ()) -> m ()
withJust (Just x) f = f x
withJust Nothing _ = pure ()

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

-- | Apply a function n times to a given value
-- implementation taken from protolude
applyN :: forall a. Int -> (a -> a) -> a -> a
applyN n f = foldr (.) id (replicate n f :: [a -> a])

describe :: String -> Maybe a -> Either String a
describe s Nothing = Left s
describe _ (Just x) = Right x

forgetLeft :: Either e a -> Maybe a
forgetLeft (Right x) = Just x
forgetLeft (Left _) = Nothing

-- # IO functions for stderr

eputStr :: String -> IO ()
eputStr = hPutStrLn stderr

eprint :: Show a => a -> IO ()
eprint = eputStr . show

ioErrorToMaybe :: IO a -> IO (Maybe a)
ioErrorToMaybe = (`ClassyPrelude.catch` ioHandler) . (Just <$>)
  where
    ioHandler :: IOError -> IO (Maybe a)
    ioHandler = pure . const Nothing

ignoreIOError :: IO a -> IO ()
ignoreIOError a = ioErrorToMaybe a $> ()

-- # Effect utilities

-- | The identity funciton
withEffects :: forall effs a. Sem effs a -> Sem effs a
withEffects = id

-- | Handle error without allowing error to be present in resulting computation.
-- Added in migration from freer-simple to polysemy, soft deprecated, consider
-- finding a sufficient replacement in the near future
handleError :: Sem (Error e : effs) a -> (e -> Sem effs a) -> Sem effs a
handleError action handler = do
  result <- runError action
  case result of
    Right x -> pure x
    Left e -> handler e

newtype FastGenericEncoding a = GenericEncodingAeson
  {underlying :: a}
  deriving (Generic)

instance
  (Generic a, GToJSON' Encoding Zero (Rep a), GToJSON' Value Zero (Rep a)) =>
  ToJSON (FastGenericEncoding a)
  where
  toJSON = genericToJSON defaultOptions . (^. #underlying)
  toEncoding = genericToEncoding defaultOptions . (^. #underlying)

instance
  (Generic a, GFromJSON Zero (Rep a)) =>
  FromJSON (FastGenericEncoding a)
  where
  parseJSON = fmap GenericEncodingAeson . genericParseJSON defaultOptions

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

embedStateful :: forall s r a. Member (State s) r => (s -> (a, s)) -> Sem r a
embedStateful f = do
  state <- get
  let (result, newState) = f state
  put newState
  pure result

untry :: Sem r (Either e a) -> Sem (Error e : r) a
untry x =
  raise x >>= \case
    Left e -> throw e
    Right v -> pure v
