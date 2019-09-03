{-# LANGUAGE NoImplicitPrelude #-}

module Effect.Warn where

import ClassyPrelude

import Control.Monad.Freer

import Effect.Throw

data Warn e r where
  Warn :: e -> Warn e ()

warn :: Member (Warn e) effs => e -> Eff effs ()
warn e = send (Warn e)

convertError :: forall e effs. Member (Warn e) effs => Eff (Error e : effs) () -> Eff effs ()
convertError = (`handleError` (\e -> warn e))

printWarnings
  :: forall e m effs. (LastMember m effs, MonadIO m, Show e)
  => Eff (Warn e : effs) () -> Eff effs ()
printWarnings = interpretWith $ \case
  Warn e -> \k -> (liftIO . putStrErr . show $ e) >> k ()
