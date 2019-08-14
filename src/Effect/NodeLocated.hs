{-# LANGUAGE NoImplicitPrelude #-}

module Effect.NodeLocated where

import Control.Monad.Freer
import Control.Monad.Freer.Reader
import Graph (Id)

type NodeLocated = Reader Id

currentLocation :: Member NodeLocated effs => Eff effs Id
currentLocation = ask
