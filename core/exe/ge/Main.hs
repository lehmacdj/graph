{-# LANGUAGE OverloadedStrings #-}

module Main where

import App
import Completion
import Control.Lens hiding (index)
import Effect.Console
import Effect.Editor
import Effect.Filesystem
import Effect.FreshNID
import Effect.Graph
import Effect.NodeLocated
import Effect.Time
import Effect.Web
import Graph.Serialize2 (doesNodeExist, initializeGraph, nextNodeId)
import History
import Lang.Command hiding (printTransitions)
import Lang.Command.Parse
import MyPrelude
import Options
import Polysemy.Readline
import Polysemy.State
import qualified System.Console.Haskeline as H
import System.Directory (doesDirectoryExist)

ioExceptionHandler :: IOError -> IO (Maybe a)
ioExceptionHandler _ = pure Nothing

defReplSettings :: Settings App
defReplSettings = H.setComplete completionFunction H.defaultSettings

data C c
  = Quit
  | C c
  deriving (Show, Eq, Ord)

-- | Takes :q and :quit as quit and otherwise defers to the main parser
withDefaultQuitParser ::
  (String -> Either String c) -> String -> Either String (C c)
withDefaultQuitParser p s
  | s == ":q" || s == ":quit" = Right Quit
  | otherwise = C <$> p s

repl ::
  ( Members [Console, SetLocation, GetLocation, Dualizeable, Readline, Embed IO] effs,
    Members [FileSystemTree, Web, FreshNID, GetTime, Editor, State History] effs,
    HasGraph String effs
  ) =>
  Sem effs ()
repl = do
  command <- getInputLine "g> "
  case withDefaultQuitParser parseCommand <$> command of
    Nothing -> outputStrLn "Goodbye!"
    Just (Right Quit) -> outputStrLn "Goodbye!"
    -- TODO: since we now interpret Haskeline as an effect we need to add a
    -- catch block for each individual command execution outside of the effect
    -- interpreter
    Just (Right (C command')) ->
      printingErrorsAndWarnings (interpretCommand command') >> repl
    Just (Left err) -> outputStrLn err >> repl

-- | Ensures that the graph directory exists, emitting an error if it doesn't
-- or creates it if the --new option is passed in
graphDirInitialization :: FilePath -> Options -> IO ()
graphDirInitialization graphDir options = do
  graphDirExists <- doesDirectoryExist graphDir
  if view createNew options
    then
      if graphDirExists
        then
          whenM (not <$> doesNodeExist graphDir 0) $
            initializeGraph graphDir
        else initializeGraph graphDir
    else do
      unless graphDirExists $
        error $ "graph in directory " ++ graphDir ++ " doesn't exist"
      unlessM (doesNodeExist graphDir 0) $
        error "couldn't find origin node in graph "

main :: IO ()
main = withOptions $ \options -> do
  let graphDir = view graphLocation options
  graphDirInitialization graphDir options
  nextNid <- nextNodeId graphDir
  env <- initEnv graphDir nextNid defReplSettings
  runAppM env $
    interpretAsApp $
      maybe repl interpretCommand $ view executeExpression options
