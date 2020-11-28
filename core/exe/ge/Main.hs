{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Main where

import App
import Completion
import Control.Lens hiding (index)
import Control.Repl
import Lang.Command hiding (printTransitions)
import Lang.Command.Parse
import MyPrelude
import System.Directory
import System.FilePath

ioExceptionHandler :: IOError -> IO (Maybe a)
ioExceptionHandler _ = pure Nothing

defReplSettings :: Settings App
defReplSettings = setComplete completionFunction defaultSettings

main :: IO ()
main = do
  env <- emptyEnv defReplSettings
  args :: [String] <- map unpack <$> getArgs
  case index args 0 of
    Nothing -> putStrLn . pack $ "needs one command line argument"
    Just dir -> do
      writeIORef (view filePath env) (Just dir)
      linkFileNames <- filter (".json" `isSuffixOf`) <$> listDirectory dir
      let nids = mapMaybe (readMay . dropExtension) linkFileNames
      writeIORef (view nextId env) (maximum (1 `ncons` nids) + 1)
      runAppM env $
        interpretAsApp $
          makeRepl "g" (withDefaultQuitParser parseCommand) interpretCommand
