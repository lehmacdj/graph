{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Lang.Command2 where

import ClassyPrelude

import Control.Monad.Freer

import Effect.Console
import Effect.Throw
import Effect.NodeLocated
import Effect.Graph
import Effect.Graph.Advanced

import Lang.APath

data Command
  = ChangeNode (APath String)         -- ^ cd
  | Dualize                           -- ^ d
  | Make (APath String)               -- ^ mk
  | Merge (APath String)              -- ^ mg
  | Clone (APath String) String       -- ^ cl
  | ListOut                           -- ^ ls
  | Query (APath String) String       -- ^ q
  | Tag (APath String) (APath String) -- ^ t
  | Remove (APath String)             -- ^ rm
  | At (APath String) Command         -- ^ at
  | Dedup String                      -- ^ dd
  | Dump FilePath
  | Load FilePath
  | NodeId
  | Debug
  | ShowImage
  | Import FilePath
  | ImportUrl String
  deriving (Eq, Show, Ord)

interpretCommand
  :: ( Members [Console, Throw, SetLocation, GetLocation] effs
     , HasGraph String effs
     )
  => Command -> Eff effs ()
interpretCommand = \case
  ChangeNode a -> do
    (nid, p) <- relativizeAPath a
    let err = const (UE "cd needs a path that resolves to a single node as an argument")
    nid' <- the' err =<< subsumeMissing (resolvePathSuccesses nid p)
    changeLocation nid'
