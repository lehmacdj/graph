module MyPrelude
  ( module MyPrelude
  , module ClassyPrelude
  ) where

import ClassyPrelude

import Control.Lens

-- | execute a computation only if it is Just
withJust :: Monad m => Maybe a -> (a -> m ()) -> m ()
withJust (Just x) f = f x
withJust Nothing _ = pure ()

-- | Copied from cabal codebase
toSetOf  :: Getting (Set a) s a -> s -> Set a
toSetOf l s = getConst (l (\x -> Const (singleton x)) s)

whenNonNull
  :: (MonoFoldable mono, Applicative f)
  => mono -> (NonNull mono -> f ()) -> f ()
whenNonNull mono f = case fromNullable mono of
  Nothing -> pure ()
  Just xs -> f xs

foldlM1
  :: (IsSequence mono, Monad m)
  => (Element mono -> Element mono -> m (Element mono))
  -> NonNull mono
  -> m (Element mono)
foldlM1 f m = uncurry (foldlM f) (splitFirst m)
