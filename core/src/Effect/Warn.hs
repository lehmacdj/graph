{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Effect.Warn where

import Effect.Throw
import MyPrelude
import UserError

data Warn e m r where
  Warn :: e -> Warn e m ()

makeSem ''Warn

convertError :: forall e effs. Member (Warn e) effs => Sem (Error e : effs) () -> Sem effs ()
convertError = (`handleError` warn)

warnString :: Member (Warn UserErrors) r => String -> Sem r ()
warnString = warnSingle . OtherError

warnSingle :: Member (Warn UserErrors) r => UserError -> Sem r ()
warnSingle = warn . singleton

printWarnings ::
  forall e effs.
  (Member (Embed IO) effs, Show e) =>
  Sem (Warn e : effs) () ->
  Sem effs ()
printWarnings = interpret $ \case
  Warn e -> liftIO . eputStr . show $ e
