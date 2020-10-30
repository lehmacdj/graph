{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Effect.Console where

import MyPrelude
import System.IO.Term.Image

data Console m a where
  -- | print a string followed by a newline
  Echo :: String -> Console m ()
  -- | print a LBS as an image
  DisplayImage :: LByteString -> Console m ()

makeSem ''Console

interpretConsoleIO ::
  Member (Embed IO) effs =>
  Sem (Console : effs) a ->
  Sem effs a
interpretConsoleIO = interpret $ \case
  Echo s -> liftIO $ putStrLn (pack s)
  DisplayImage i -> liftIO $ printImage i
