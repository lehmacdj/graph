{-# LANGUAGE TemplateHaskell #-}

module Error.Warn where

import Effectful
import Effectful.Dispatch.Dynamic
import Effectful.Error.Static
import Effectful.TH
import Error.UserError
import MyPrelude

data Warn e :: Effect where
  Warn :: e -> Warn e m ()

makeEffect ''Warn

convertError ::
  forall e es. (Warn e :> es) => Eff (Error e : es) () -> Eff es ()
convertError = (`handleError` warn)

warnString :: (Warn UserError :> es) => String -> Eff es ()
warnString = warn . OtherError . pack

warnText :: (Warn UserError :> es) => Text -> Eff es ()
warnText = warn . OtherError

printWarnings ::
  forall e es a.
  (IOE :> es, Show e) =>
  Eff (Warn e : es) a ->
  Eff es a
printWarnings = interpret $ \_ -> \case
  Warn e -> liftIO . eputStr . show $ e

ignoreWarnings :: forall e es a. Eff (Warn e : es) a -> Eff es a
ignoreWarnings = interpret $ \_ (Warn _) -> pure ()
