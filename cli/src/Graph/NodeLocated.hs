{-# LANGUAGE TemplateHaskell #-}

module Graph.NodeLocated
  ( module Graph.NodeLocated,
    -- | for use in overriding location temporarily
    local,
  )
where

import Effectful
import Effectful.Dispatch.Dynamic
import Effectful.Reader.Static
import Effectful.State.Static.Local
import Effectful.TH
import Models.History (History, pushHistory)
import Models.NID (NID)
import MyPrelude hiding (Reader, ask)
import Utils.Polysemy

type GetLocation = Reader NID

-- Output effect for SetLocation
data SetLocation :: Effect where
  Output :: NID -> SetLocation m ()

makeEffect ''SetLocation

currentLocation :: (GetLocation :> es) => Eff es NID
currentLocation = ask

changeLocation :: (SetLocation :> es) => NID -> Eff es ()
changeLocation = output

runLocableHistoryState ::
  (State History :> es) =>
  Eff (GetLocation : SetLocation : es) a ->
  Eff es a
runLocableHistoryState = subsumeReaderState (view #now) . runSetLocationHistoryState . raiseUnder

runSetLocationHistoryState ::
  (State History :> es) => Eff (SetLocation : es) a -> Eff es a
runSetLocationHistoryState = interpret $ \_ -> \case
  Output nid -> modify @History (pushHistory nid)
