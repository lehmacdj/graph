{-# LANGUAGE OverloadedStrings #-}

module Executable.GraphEditor where

import Completion
import Control.Lens hiding (index)
import Control.Monad.Fix
import Effect.Console
import Effect.Editor
import Effect.FileTypeOracle
import Effect.Filesystem
import Effect.FreshNID
import Effect.Graph
import Effect.NodeLocated
import Effect.Time
import Effect.Web
import DAL.Serialization (doesNodeExist, initializeGraph)
import Models.Types
import History
import Interpreters
import Lang.Command hiding (printTransitions)
import Lang.Command.Parse
import MyPrelude
import Options
import Polysemy.Readline
import Polysemy.State
import SpecialNodes.Init (createSpecialNodes)
import qualified System.Console.Haskeline as H
import System.Directory (createDirectoryIfMissing, doesDirectoryExist)
import System.Environment.XDG.BaseDir (getUserDataDir)

ioExceptionHandler :: IOError -> IO (Maybe a)
ioExceptionHandler _ = pure Nothing

defReplSettings :: Env -> IO (Settings IO)
defReplSettings env = do
  dataDir <- getUserDataDir "ge"
  createDirectoryIfMissing True dataDir
  pure $
    H.Settings
      { H.complete = completionFunction env,
        H.historyFile = Just $ dataDir </> "history.txt",
        H.autoAddHistory = True
      }

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
    Members '[FileTypeOracle] effs,
    HasGraph String effs,
    -- TODO: these effects are bad and shouldn't be exposed; need to rewrite
    -- commands using them as plugins once plugins are available. See also
    -- lengthier comment at interpretCommand
    Members [Embed IO, RawGraph] effs
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
  if view #_createNew options
    then
      if graphDirExists
        then
          whenM (not <$> doesNodeExist graphDir nilNID) $
            initializeGraph graphDir
        else initializeGraph graphDir
    else do
      unless graphDirExists $
        error $ "graph in directory " ++ graphDir ++ " doesn't exist"
      unlessM (doesNodeExist graphDir nilNID) $
        error "couldn't find origin node in graph "

main :: IO ()
main = withOptions $ \options -> do
  let graphDir = view #_graphLocation options
  graphDirInitialization graphDir options
  env <- mfix (initEnv graphDir <=< defReplSettings)
  runMainEffectsIO env do
    createSpecialNodes
    maybe repl interpretCommand (view #_executeExpression options)
