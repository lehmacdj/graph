{-# OPTIONS_GHC -Wno-orphans #-}

module MyPrelude.Orphans where

import ClassyPrelude
import Control.Monad (MonadFail (fail))
import Data.IxSet.Typed
import Language.Haskell.TH.Syntax (Lift (..))
import Text.Megaparsec.Pos (Pos, SourcePos (..), mkPos, unPos)

type instance Element (IxSet ixs a) = a

instance MonoFoldable (IxSet ixs a)

instance GrowingAppend (IxSet ixs a)

deriving instance (Lift a) => Lift (Identity a)

instance Lift Pos where
  lift pos = [|mkPos (unPos pos)|]
  liftTyped pos = [||mkPos (unPos pos)||]

deriving instance Lift SourcePos

-- | This is nice for having a pure way to get the error message from APIs that
-- expose MonadFail as their interface for reporting errors
instance MonadFail (Either String) where
  fail = Left
