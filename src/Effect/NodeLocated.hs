{-# LANGUAGE NoImplicitPrelude #-}

module Effect.NodeLocated where

import Control.Monad.Freer
import Control.Monad.Freer.Reader
import Control.Monad.Freer.Writer
import Graph (Id)

type GetLocation = Reader Id
type SetLocation = Writer Id

currentLocation :: Member GetLocation effs => Eff effs Id
currentLocation = ask

changeLocation :: Member SetLocation effs => Id -> Eff effs ()
changeLocation = tell
