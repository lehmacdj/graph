{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE ViewPatterns #-}

-- | Functions for dealing with stuff that should be singletons
module Singleton where

import MyPrelude

import Control.Monad.Freer
import UserError

the'
  :: (MonoFoldable mono, Member ThrowUserError effs)
  => (mono -> UserError) -> mono -> Eff effs (Element mono)
the' toErr = \case
  (toList -> [x]) -> pure x
  xs -> throw (toErr xs)

the
  :: (MonoFoldable mono, Show mono, Member ThrowUserError effs)
  => mono -> Eff effs (Element mono)
the = the' (\xs -> NotSingleton (show xs))
