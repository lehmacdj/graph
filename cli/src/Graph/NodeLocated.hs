module Graph.NodeLocated
  ( module Graph.NodeLocated,
    -- | for use in overriding location temporarily
    local,
  )
where

import Effectful.Reader.Dynamic
import Effectful.State.Dynamic
import Models.History (History, pushHistory)
import Models.NID (NID)
import MyPrelude hiding (Reader, ask)
import Utils.Effectful

type GetLocation = Reader NID

data SetLocation :: Effect where
  ChangeLocation :: NID -> SetLocation m ()

type instance DispatchOf SetLocation = Dynamic

currentLocation :: (GetLocation :> effs) => Eff effs NID
currentLocation = ask

changeLocation :: (SetLocation :> effs) => NID -> Eff effs ()
changeLocation = send . ChangeLocation

runLocableHistoryState ::
  (State History :> effs) =>
  Eff (GetLocation : SetLocation : effs) ~> Eff effs
runLocableHistoryState = subsumeReaderState @History (view #now) >>> runSetLocationHistoryState

runSetLocationHistoryState ::
  (State History :> r) => Eff (SetLocation : r) ~> Eff r
runSetLocationHistoryState = interpret \_ -> \case
  ChangeLocation nid -> modify @History (pushHistory nid)
