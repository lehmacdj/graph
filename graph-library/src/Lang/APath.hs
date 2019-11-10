{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NoImplicitPrelude #-}

{-|
   Absolute paths, paths that may be augmented with an nid:path or may be
   relative.
 -}
module Lang.APath
  ( module Lang.APath
  , module Lang.Path
  , NID
  ) where

import ClassyPrelude

import Graph
import Lang.Path

import Effect.NodeLocated

data APath t
  = Relative (Path t)
  | Absolute NID (Path t)
  deriving (Show, Eq, Ord)

mkAPath :: Maybe NID -> Path t -> APath t
mkAPath (Just nid) p = Absolute nid p
mkAPath Nothing p = Relative p

relativizeAPath
  :: Member GetLocation effs
  => APath t -> Eff effs (NID, Path t)
relativizeAPath = \case
  Relative p -> (,) <$> currentLocation <*> pure p
  Absolute nid p -> pure (nid, p)
