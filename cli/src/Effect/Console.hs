{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TemplateHaskell #-}

module Effect.Console where

import MyPrelude
import Polysemy.Readline
import System.IO.Term.Image

-- | TODO: make this use Console.Readline for thread safe writing with prompt
-- even for DisplayImage. This will require some thought about how to write
-- raw bytestring to the console given only strings
data Console m a where
  -- | print a string followed by a newline
  Echo :: String -> Console m ()
  -- | print a LBS as an image
  DisplayImage :: LByteString -> Console m ()

makeSem ''Console

interpretConsoleIO ::
  (Member Readline effs, Member (Embed IO) effs) =>
  Sem (Console : effs) a ->
  Sem effs a
interpretConsoleIO = interpret $ \case
  Echo s -> outputStrLn (pack s)
  DisplayImage i -> liftIO $ printImage i
