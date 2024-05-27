{-# OPTIONS_GHC -Wno-orphans #-}

module Data.Set.Ordered.Orphans where

import Data.MonoTraversable
import Data.Set.Ordered

type instance Element (OSet a) = a

instance MonoFoldable (OSet a)
