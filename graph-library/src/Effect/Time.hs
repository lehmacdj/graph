{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE TemplateHaskell #-}

module Effect.Time where

import MyPrelude

import Control.Monad.Freer.TH

data GetTime r where
  CurrentTime :: GetTime UTCTime
makeEffect ''GetTime

interpretTimeAsIO
  :: (LastMember m effs, MonadIO m)
  => Eff (GetTime : effs) ~> Eff effs
interpretTimeAsIO = interpret $ \case
  CurrentTime -> liftIO getCurrentTime
