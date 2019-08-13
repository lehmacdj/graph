{-# LANGUAGE NoImplicitPrelude #-}

module Effect.Graph.Advanced where

import Control.Monad.Freer
import Effect.Graph
import Data.Witherable

getNodes :: Member (ReadGraph t) effs => [Id] -> Eff effs [Node t]
getNodes = wither getNode
