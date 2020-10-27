{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Effect.Console where

import Control.Monad.Freer.TH
import MyPrelude
import System.IO.Term.Image

data Console a where
  -- | print a string followed by a newline
  Echo :: String -> Console ()
  -- | print a LBS as an image
  DisplayImage :: LByteString -> Console ()

makeEffect ''Console

interpretConsoleIO ::
  (MonadIO m, LastMember m effs) =>
  Eff (Console : effs) a ->
  Eff effs a
interpretConsoleIO = interpret $ \case
  Echo s -> liftIO $ putStrLn (pack s)
  DisplayImage i -> liftIO $ printImage i
