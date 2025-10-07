module MyPrelude.MaybeEither
  ( module MyPrelude.MaybeEither,
    module X,
  )
where

import ClassyPrelude
import Data.Bifoldable (Bifoldable)
import Data.Bitraversable (Bitraversable (bitraverse))
import Data.Either as X (isLeft, isRight)
import Data.Functor.Alt (Alt ((<!>)))
import Data.Functor.Apply (Apply ((<.>)))
import GHC.Stack (HasCallStack)
import MyPrelude.Collections

-- | execute a computation only if it is Just
withJust :: (Applicative m) => Maybe a -> (a -> m ()) -> m ()
withJust (Just x) f = f x
withJust Nothing _ = pure ()

-- | execute a computation only if it is Just
whenJust :: (Applicative m) => Maybe a -> m () -> m ()
whenJust x = withJust x . const

-- | execute a computation only if it is Just
withJustM :: (Monad m) => m (Maybe a) -> (a -> m ()) -> m ()
withJustM v action = v >>= (`withJust` action)

-- | execute a computation only if it is Just
whenJustM :: (Monad m) => m (Maybe a) -> m () -> m ()
whenJustM v = withJustM v . const

whenNothing :: (Monad m) => Maybe a -> m () -> m ()
whenNothing Nothing action = action
whenNothing (Just _) _ = pure ()

whenNothingM :: (Monad m) => m (Maybe a) -> m () -> m ()
whenNothingM v action = v >>= (`whenNothing` action)

onNothing :: (Monad m) => Maybe a -> m a -> m a
onNothing Nothing action = action
onNothing (Just v) _ = pure v

onNothingM :: (Monad m) => m (Maybe a) -> m a -> m a
onNothingM v action = v >>= (`onNothing` action)

unwrapDefaulting :: (Applicative m) => m a -> Maybe a -> m a
unwrapDefaulting def = maybe def pure

unwrapDefaultingM :: (Monad m) => m a -> m (Maybe a) -> m a
unwrapDefaultingM def = (>>= unwrapDefaulting def)

unwrapEx :: (HasCallStack) => String -> Maybe a -> a
unwrapEx _ (Just x) = x
unwrapEx s Nothing = error s

forgetLeft :: Either e a -> Maybe a
forgetLeft (Right x) = Just x
forgetLeft (Left _) = Nothing

justIfTrue :: Bool -> a -> Maybe a
justIfTrue True x = Just x
justIfTrue False _ = Nothing

attachError :: Maybe a -> e -> Either e a
attachError (Just x) _ = Right x
attachError Nothing e = Left e

fromJustEx :: (HasCallStack) => Maybe a -> a
fromJustEx (Just x) = x
fromJustEx Nothing = error "fromJustEx: Nothing"

injectError :: Maybe a -> e -> Either (NonEmpty e) a
injectError (Just x) _ = Right x
injectError Nothing e = Left (singleton e)

mergeMaybes :: (a -> a -> a) -> Maybe a -> Maybe a -> Maybe a
mergeMaybes _ Nothing x = x
mergeMaybes _ x Nothing = x
mergeMaybes f (Just x) (Just y) = Just (f x y)

codiagonal :: Either a a -> a
codiagonal (Left x) = x
codiagonal (Right x) = x

combineErrors ::
  Either (NonEmpty e) a ->
  Either (NonEmpty e) b ->
  Either (NonEmpty e) (a, b)
combineErrors (Left e1) (Left e2) = Left (e1 <> e2)
combineErrors (Left e1) (Right _) = Left e1
combineErrors (Right _) (Left e2) = Left e2
combineErrors (Right x) (Right y) = Right (x, y)

(<!>) ::
  (Monoid m) =>
  Either (NonEmpty e) m ->
  Either (NonEmpty e) m ->
  Either (NonEmpty e) m
e1 <!> e2 = uncurry (<>) <$> combineErrors e1 e2

-- * Validation

newtype Validation err a = Validation {either :: Either err a}
  deriving stock (Eq, Generic, Ord, Show, Typeable)
  deriving newtype (Functor, Foldable, NFData, MonoFoldable, Bifunctor, Bifoldable)

type instance Element (Validation err a) = a

instance (Semigroup err) => Apply (Validation err) where
  Validation (Left e1) <.> Validation (Left e2) = Validation (Left (e1 <> e2))
  Validation (Left e1) <.> Validation (Right _) = Validation (Left e1)
  Validation (Right _) <.> Validation (Left e2) = Validation (Left e2)
  Validation (Right f) <.> Validation (Right a) = Validation (Right (f a))
  {-# INLINE (<.>) #-}

instance (Semigroup err) => Applicative (Validation err) where
  pure = Validation . Right
  (<*>) = (Data.Functor.Apply.<.>)

-- | For two errors, this instance reports only the last of them.
instance Alt (Validation err) where
  Validation (Left _) <!> x = x
  Validation (Right a) <!> _ = Validation (Right a)
  {-# INLINE (<!>) #-}

instance Traversable (Validation err) where
  traverse f (Validation e) = Validation <$> traverse f e

instance Bitraversable Validation where
  bitraverse f g (Validation e) = Validation <$> bitraverse f g e
