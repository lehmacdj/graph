{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE ViewPatterns #-}

module Effect.Throw
  ( module Control.Monad.Freer.Error
  , module Effect.Throw
  ) where

import ClassyPrelude

import Control.Monad.Freer.Error

import UserError
import Graph.Types

newtype Missing = Missing { unMissing :: NID }
  deriving (Show, Eq, Ord)
type ThrowMissing = Error Missing

throwMissing :: Member ThrowMissing effs => NID -> Eff effs a
throwMissing = throwError . Missing

subsumeMissing :: Member ThrowUserError effs => Eff (ThrowMissing ': effs) ~> Eff effs
subsumeMissing = (`handleError` (throw . MissingNode . unMissing))
