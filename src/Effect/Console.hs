{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE LambdaCase #-}

module Effect.Console where

import ClassyPrelude

import Control.Monad.Freer
import Control.Monad.Freer.TH

import System.IO.Term.Image

data Console a where
  Echo :: String -> Console () -- ^ print a string followed by a newline
  DisplayImage :: LByteString -> Console () -- ^ print a LBS as an image
makeEffect ''Console

interpretConsoleIO
  :: (MonadIO m, LastMember m effs)
  => Eff (Console : effs) a -> Eff effs a
interpretConsoleIO = interpret $ \case
  Echo s -> liftIO $ putStrLn (pack s)
  DisplayImage i -> liftIO $ printImage i
