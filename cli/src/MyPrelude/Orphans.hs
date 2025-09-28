{-# OPTIONS_GHC -Wno-orphans #-}

module MyPrelude.Orphans where

import Data.Functor.Identity
import Data.IxSet.Typed
import Data.MonoTraversable
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
