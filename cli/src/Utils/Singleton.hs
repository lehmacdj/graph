{-# LANGUAGE ViewPatterns #-}

-- | Functions for dealing with stuff that should be singletons
module Utils.Singleton where

import Error.Utils
import MyPrelude

the' ::
  (MonoFoldable mono, Member (Error UserError) effs) =>
  (mono -> UserError) ->
  mono ->
  Sem effs (Element mono)
the' toErr = \case
  (toList -> [x]) -> pure x
  xs -> throw (toErr xs)

the ::
  (MonoFoldable mono, Show mono, Member (Error UserError) effs) =>
  mono ->
  Sem effs (Element mono)
the = the' (NotSingleton . show)
