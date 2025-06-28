{-# OPTIONS_GHC -Wno-orphans #-}

module MyPrelude.Orphans where

import Data.IxSet.Typed
import Data.MonoTraversable

type instance Element (IxSet ixs a) = a

instance MonoFoldable (IxSet ixs a)

instance GrowingAppend (IxSet ixs a)
