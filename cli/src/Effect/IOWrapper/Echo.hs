{-# LANGUAGE TemplateHaskell #-}

module Effect.IOWrapper.Echo where

import Effectful
import Effectful.Dispatch.Dynamic
import Effectful.TH
import MyPrelude
import Effect.Readline

data Echo :: Effect where
  Echo :: String -> Echo m ()

makeEffect ''Echo

runEchoReadline :: (Readline :> es) => Eff (Echo : es) a -> Eff es a
runEchoReadline = interpret $ \_ -> \case
  Echo s -> outputStrLn s

runEchoIO :: (IOE :> es) => Eff (Echo : es) a -> Eff es a
runEchoIO = interpret $ \_ -> \case
  Echo s -> liftIO $ putStrLn $ pack $ unpack s
