{-# OPTIONS_GHC -Wno-orphans #-}

module MyPrelude.Orphans where

import Data.Functor.Identity
import Data.IxSet.Typed
import Data.MonoTraversable
import Language.Haskell.TH.Syntax (Lift)

type instance Element (IxSet ixs a) = a

instance MonoFoldable (IxSet ixs a)

instance GrowingAppend (IxSet ixs a)

deriving instance (Lift a) => Lift (Identity a)
