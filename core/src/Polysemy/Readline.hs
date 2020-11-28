{-# LANGUAGE TemplateHaskell #-}

module Polysemy.Readline where

import Control.Monad.Catch
import Control.Monad.IO.Class
import Polysemy
import Polysemy.Embed
import qualified System.Console.Haskeline as H

data Readline m a where
  GetInputLine :: String -> Readline m (Maybe String)
  GetInputLineWithInitial :: String -> (String, String) -> Readline m (Maybe String)
  GetInputChar :: String -> Readline m (Maybe Char)
  GetPassword :: Maybe Char -> String -> Readline m (Maybe String)
  WaitForAnyKey :: String -> Readline m Bool
  OutputStr :: String -> Readline m ()

makeSem ''Readline

outputStrLn :: Member Readline r => String -> Sem r ()
outputStrLn str = outputStr (str <> "\n")

runReadline ::
  forall m r a.
  (MonadIO m, MonadMask m, Member (Embed m) r) =>
  H.Settings m ->
  Sem (Readline : r) a ->
  Sem r a
runReadline settings =
  runEmbedded (H.runInputT settings)
    . interpretReadline
    . raiseUnder @(Embed (H.InputT m))
  where
    interpretReadline ::
      Sem (Readline : Embed (H.InputT m) : r) a ->
      Sem (Embed (H.InputT m) : r) a
    interpretReadline = interpret $ \case
      GetInputLine prompt -> embed $ H.getInputLine prompt
      GetInputLineWithInitial prompt initial ->
        embed $ H.getInputLineWithInitial prompt initial
      GetInputChar prompt -> embed $ H.getInputChar prompt
      GetPassword maskChar prompt -> embed $ H.getPassword maskChar prompt
      WaitForAnyKey prompt -> embed $ H.waitForAnyKey prompt
      OutputStr str -> embed $ H.outputStr str
