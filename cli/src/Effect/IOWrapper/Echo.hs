{-# LANGUAGE TemplateHaskell #-}

module Effect.IOWrapper.Echo where

import MyPrelude
import Polysemy.Readline

data Echo m k where
  Echo :: String -> Echo m ()

makeSem ''Echo

runEchoReadline :: (Member Readline effs) => Sem (Echo : effs) ~> Sem effs
runEchoReadline = interpret $ \case
  Echo s -> outputStrLn s

runEchoIO :: (Member (Embed IO) effs) => Sem (Echo : effs) ~> Sem effs
runEchoIO = interpret $ \case
  Echo s -> embed $ putStrLn $ pack $ unpack s
