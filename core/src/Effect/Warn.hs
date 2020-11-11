{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Effect.Warn where

import Effect.Throw
import MyPrelude

data Warn e m r where
  Warn :: e -> Warn e m ()

makeSem ''Warn

-- warn :: Member (Warn e) effs => e -> Sem effs ()
-- warn e = send (Warn e)

convertError :: forall e effs. Member (Warn e) effs => Sem (Error e : effs) () -> Sem effs ()
convertError = (`handleError` warn)

printWarnings ::
  forall e effs.
  (Member (Embed IO) effs, Show e) =>
  Sem (Warn e : effs) () ->
  Sem effs ()
printWarnings = interpret $ \case
  Warn e -> liftIO . eputStr . show $ e
