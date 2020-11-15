{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE NoImplicitPrelude #-}

-- | Functions for dealing with stuff that should be singletons
module Singleton where

import MyPrelude
import UserError

the' ::
  (MonoFoldable mono, Member ThrowUserError effs) =>
  (mono -> UserError) ->
  mono ->
  Sem effs (Element mono)
the' toErr = \case
  (toList -> [x]) -> pure x
  xs -> throw (toErr xs)

the ::
  (MonoFoldable mono, Show mono, Member ThrowUserError effs) =>
  mono ->
  Sem effs (Element mono)
the = the' (NotSingleton . show)
