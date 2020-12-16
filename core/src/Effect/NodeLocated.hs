
module Effect.NodeLocated
  ( module Effect.NodeLocated,
    -- | for use in overriding location temporarily
    local,
  )
where

import Graph (NID)
import MyPrelude hiding (Reader, ask)
import Polysemy.Output
import Polysemy.Reader

type GetLocation = Reader NID

type SetLocation = Output NID

currentLocation :: Member GetLocation effs => Sem effs NID
currentLocation = ask

changeLocation :: Member SetLocation effs => NID -> Sem effs ()
changeLocation = output
