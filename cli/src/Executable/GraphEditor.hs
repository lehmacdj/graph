{-# LANGUAGE OverloadedStrings #-}

module Executable.GraphEditor where

import Control.Lens hiding (index)
import Control.Monad.Fix
import DAL.Serialization (doesNodeExist, initializeGraph)
import Effect.Console
import Effect.Editor
import Effect.FileSystem
import Effect.FileTypeOracle
import Effect.FreshNID
import Effect.Interpreters
import Effect.NodeLocated
import Effect.Time
import Effect.Web
import Executable.GraphEditor.Completion
import Executable.GraphEditor.Options
import Graph.Effect
import Graph.SystemNodes.Init (createSystemNodes)
import Lang.Command hiding (printTransitions)
import Lang.Command.Parse
import Models.History
import Models.Types
import MyPrelude
import Polysemy.Readline
import Polysemy.State
import qualified System.Console.Haskeline as H
import System.Directory (createDirectoryIfMissing, doesDirectoryExist)
import System.Environment.XDG.BaseDir (getUserDataDir)
import System.Random (initStdGen, mkStdGen)

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
    Members [FileSystem, Web, FreshNID, GetTime, Editor, State History] effs,
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
  nidGenerator <-
    maybe initStdGen (pure . mkStdGen) $
      options ^. #_testOnlyNidGenerationSeed
  -- because we need the Env for the repl's completionFunction this is pretty
  -- fancy and uses mfix to tie the knot; it would probably be a better idea to
  -- break the cycle somehow to make this easier to reason about
  env <- mfix (initEnv graphDir nidGenerator <=< defReplSettings)
  let timeBehavior :: Member (Embed IO) effs => Sem (GetTime : effs) ~> Sem effs
      timeBehavior
        | options ^. #_testOnlyMonotonicIncreasingDeterministicTime =
          interpretTimeAsMonotonicIncreasingUnixTime
        | otherwise = interpretTimeAsIO
  runMainEffectsIO printingErrorsAndWarnings timeBehavior env do
    createSystemNodes
    maybe repl interpretCommand (view #_executeExpression options)
