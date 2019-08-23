{-# LANGUAGE OverloadedStrings #-}

module Main where

import Control.Repl
import Control.Monad.State

import Lang.Command2 hiding (printTransitions)
import Lang.Command2.Parse

import Env
import App
import Completion

-- | Style guide for commands for the future:
-- All commands and paths are interpreted relative to the current location
-- We can reintroduce the ability to execute commands relative to a different
-- location later via an `at` command that changes the location and then
-- changes it back.
-- This means that new nodes created and edges created etc start/end at the
-- current node
-- Commands that act on nodes should also act on at least deterministic
-- paths and if possible nondeterministic paths too
execCommand :: Command -> App ()
execCommand c = Repl . lift $ interpretAsAppBase (interpretCommand c)

ioExceptionHandler :: IOError -> IO (Maybe a)
ioExceptionHandler _ = pure Nothing

replSettings :: Settings AppBase
replSettings = setComplete completionFunction defaultSettings

main :: IO ()
main = do
  env <- emptyEnv
  doRepl' replSettings "g" (withDefaultQuitParser parseCommand) execCommand env
