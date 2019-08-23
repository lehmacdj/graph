{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE ViewPatterns #-}

module Effect.Throw
  ( module Control.Monad.Freer.Error
  , module Effect.Throw
  , module Error
  ) where

import ClassyPrelude

import Control.Monad.Freer
import Control.Monad.Freer.Error
import Data.List.NonEmpty (NonEmpty(..))

import Error
import Graph

-- | An effect for simple errors
type Throw = Error Errors

newtype Missing = Missing { unMissing :: Id }
  deriving (Show, Eq, Ord)
type ThrowMissing = Error Missing

throwErr :: Member Throw effs => Err -> Eff effs a
throwErr = throwError . (:| [])

throwMissing :: Member ThrowMissing effs => Id -> Eff effs a
throwMissing = throwError . Missing

subsumeMissing :: Member Throw effs => Eff (ThrowMissing ': effs) ~> Eff effs
subsumeMissing = (`handleError` (throwErr . MissingNode . unMissing))

the
  :: (MonoFoldable mono, Show mono, Member Throw effs)
  => mono -> Eff effs (Element mono)
the = the' (\xs -> NotSingleton (show xs))

the'
  :: (MonoFoldable mono, Member Throw effs)
  => (mono -> Err) -> mono -> Eff effs (Element mono)
the' toErr = \case
  (toList -> [x]) -> pure x
  xs -> throwErr (toErr xs)

printErrors :: (MonadIO m, LastMember m effs)
            => Eff (Throw : effs) () -> Eff effs ()
printErrors c = handleError c $ traverse_ (liftIO . putStrErr . show)
