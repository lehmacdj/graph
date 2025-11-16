-- | Functions for dealing with stuff that should be singletons
module Utils.Singleton where

import Error.UserError
import MyPrelude

the' ::
  (MonoFoldable mono, Member (Error UserError) effs) =>
  (mono -> UserError) ->
  mono ->
  Eff es (Element mono)
the' toErr = \case
  (toList -> [x]) -> pure x
  xs -> throw (toErr xs)
