{-# LANGUAGE NoImplicitPrelude #-}

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
