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
