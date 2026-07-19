{-# LANGUAGE TemplateHaskell #-}

module Effect.IOWrapper.Echo where

import Effectful.Readline
import MyPrelude

data Echo :: Effect where
  Echo :: String -> Echo m ()

makeEffect ''Echo

runEchoReadline :: (Readline :> effs) => Eff (Echo : effs) ~> Eff effs
runEchoReadline = interpret \_ -> \case
  Echo s -> outputStrLn s

runEchoIO :: (IOE :> effs) => Eff (Echo : effs) ~> Eff effs
runEchoIO = interpret \_ -> \case
  Echo s -> liftIO $ putStrLn $ pack $ unpack s
