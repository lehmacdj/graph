{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Main where

import App
import Completion
import Control.Lens hiding (index)
import Control.Repl
import Graph.Serialize2 (nextNodeId)
import Lang.Command hiding (printTransitions)
import Lang.Command.Parse
import MyPrelude
import Options

ioExceptionHandler :: IOError -> IO (Maybe a)
ioExceptionHandler _ = pure Nothing

defReplSettings :: Settings App
defReplSettings = setComplete completionFunction defaultSettings

main :: IO ()
main = withOptions $ \options -> do
  let graphDir = view graphLocation options
  nextNid <- nextNodeId graphDir
  env <- initEnv graphDir nextNid defReplSettings
  runAppM env $
    interpretAsApp $
      makeRepl "g" (withDefaultQuitParser parseCommand) interpretCommand
