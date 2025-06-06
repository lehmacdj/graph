module Graph.NodeLocated
  ( module Graph.NodeLocated,
    -- | for use in overriding location temporarily
    local,
  )
where

import Models.History (History, addToHistory)
import Models.NID (NID)
import MyPrelude hiding (Reader, ask)
import Polysemy.Output
import Polysemy.Reader
import Polysemy.State
import Polysemy.Util

type GetLocation = Reader NID

type SetLocation = Output NID

currentLocation :: (Member GetLocation effs) => Sem effs NID
currentLocation = ask

changeLocation :: (Member SetLocation effs) => NID -> Sem effs ()
changeLocation = output

runLocableHistoryState ::
  (Member (State History) effs) =>
  Sem (GetLocation : SetLocation : effs) ~> Sem effs
runLocableHistoryState = subsumeReaderState (view #now) >>> runSetLocationHistoryState

runSetLocationHistoryState ::
  (Member (State History) r) => Sem (SetLocation : r) ~> Sem r
runSetLocationHistoryState = interpret $ \case
  Output nid -> modify @History (addToHistory nid)
