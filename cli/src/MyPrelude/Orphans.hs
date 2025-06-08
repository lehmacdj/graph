{-# OPTIONS_GHC -Wno-orphans #-}

module MyPrelude.Orphans where

import Data.IxSet.Typed
import Data.MonoTraversable
import Data.Set.Ordered

type instance Element (OSet a) = a

instance MonoFoldable (OSet a)

type instance Element (IxSet ixs a) = a

instance MonoFoldable (IxSet ixs a)

instance GrowingAppend (IxSet ixs a)
