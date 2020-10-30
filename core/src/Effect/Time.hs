{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Effect.Time where

import MyPrelude

data GetTime m r where
  CurrentTime :: GetTime m UTCTime

makeSem ''GetTime

interpretTimeAsIO ::
  (Member (Embed IO) effs) =>
  Sem (GetTime : effs) ~> Sem effs
interpretTimeAsIO = interpret $ \case
  CurrentTime -> liftIO getCurrentTime
