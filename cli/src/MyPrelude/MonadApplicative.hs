module MyPrelude.MonadApplicative where

import ClassyPrelude

liftJoin2 :: (Monad m) => (a -> b -> m c) -> m a -> m b -> m c
liftJoin2 f ma mb = join $ liftA2 f ma mb
