{-# LANGUAGE TemplateHaskell #-}

module Error.Warn where

import Error.UserError
import MyPrelude

data Warn e :: Effect where
  Warn :: e -> Warn e m ()

makeEffect ''Warn

convertError ::
  forall e effs. (Warn e :> effs) => Eff (Error e : effs) () -> Eff effs ()
convertError = (`handleError` warn)

warnString :: (Warn UserError :> r) => String -> Eff r ()
warnString = warn . OtherError . pack

warnText :: (Warn UserError :> r) => Text -> Eff r ()
warnText = warn . OtherError

printWarnings ::
  forall e r a.
  (IOE :> r, Show e) =>
  Eff (Warn e : r) a ->
  Eff r a
printWarnings = interpret \_ -> \case
  Warn e -> liftIO . eputStr . show $ e

ignoreWarnings :: forall e r a. Eff (Warn e : r) a -> Eff r a
ignoreWarnings = interpret \_ -> \case Warn _ -> pure ()
