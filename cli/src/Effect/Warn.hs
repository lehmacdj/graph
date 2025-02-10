{-# LANGUAGE TemplateHaskell #-}

module Effect.Warn where

import Error.UserError
import MyPrelude

data Warn e m r where
  Warn :: e -> Warn e m ()

makeSem ''Warn

convertError ::
  forall e effs. Member (Warn e) effs => Sem (Error e : effs) () -> Sem effs ()
convertError = (`handleError` warn)

warnString :: Member (Warn UserError) r => String -> Sem r ()
warnString = warn . OtherError

printWarnings ::
  forall e r a.
  (Member (Embed IO) r, Show e) =>
  Sem (Warn e : r) a ->
  Sem r a
printWarnings = interpret $ \case
  Warn e -> liftIO . eputStr . show $ e

ignoreWarnings :: forall e r a. Sem (Warn e : r) a -> Sem r a
ignoreWarnings = interpret $ \(Warn _) -> pure ()
