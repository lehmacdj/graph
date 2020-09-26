{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Effect.Console where

import Control.Monad.Freer.TH
import MyPrelude
import System.IO.Term.Image

data Console a where
  Echo ::
    String ->
    -- | print a string followed by a newline
    Console ()
  DisplayImage ::
    LByteString ->
    -- | print a LBS as an image
    Console ()

makeEffect ''Console

interpretConsoleIO ::
  (MonadIO m, LastMember m effs) =>
  Eff (Console : effs) a ->
  Eff effs a
interpretConsoleIO = interpret $ \case
  Echo s -> liftIO $ putStrLn (pack s)
  DisplayImage i -> liftIO $ printImage i
