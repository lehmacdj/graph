{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Main where

import App
import ClassyPrelude
import Completion
import Control.Lens hiding (index)
import Control.Repl
import Env
import Lang.Command hiding (printTransitions)
import Lang.Command.Parse

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
  args :: [String] <- map unpack <$> getArgs
  case index args 0 of
    Nothing -> putStrLn . pack $ "needs one command line argument"
    Just dir -> do
      writeIORef (view filePath env) (Just dir)
      doRepl' replSettings "g" (withDefaultQuitParser parseCommand) execCommand env
