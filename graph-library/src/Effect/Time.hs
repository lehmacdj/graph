{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Effect.Time where

import Control.Monad.Freer.TH
import MyPrelude

data GetTime r where
  CurrentTime :: GetTime UTCTime

makeEffect ''GetTime

interpretTimeAsIO ::
  (LastMember m effs, MonadIO m) =>
  Eff (GetTime : effs) ~> Eff effs
interpretTimeAsIO = interpret $ \case
  CurrentTime -> liftIO getCurrentTime
