{-# LANGUAGE CPP #-}

module Models.Common where

import MyPrelude

#ifdef DEBUG
type ValidTransition t = (Eq t, Ord t, Show t, HasCallStack)
#else
type ValidTransition t = (Eq t, Ord t)
#endif
