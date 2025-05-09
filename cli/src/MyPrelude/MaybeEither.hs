module MyPrelude.MaybeEither where

import ClassyPrelude
import GHC.Stack (HasCallStack)

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
