module Executable.GraphEditor where

import Control.Exception qualified as E
import Control.Monad.Fix
import DAL.RawGraph
import DAL.Serialization (doesNodeExist, initializeGraph)
import Effect.IOWrapper.DisplayImage
import Effect.IOWrapper.Echo
import Effect.IOWrapper.Editor
import Effect.IOWrapper.FileSystem
import Effect.IOWrapper.FileTypeOracle
import Effect.IOWrapper.GetTime
import Effect.IOWrapper.Web
import Effectful.Provider.List
import Effectful.Readline
import Effectful.State.Dynamic
import Error.UserError
import Error.Warn
import Executable.GraphEditor.Completion
import Executable.GraphEditor.Options
import Graph.AppInterpreters
import Graph.Command
import Graph.Dualizeable
import Graph.FreshNID
import Graph.GraphEditing
import Graph.NodeLocated
import Graph.SystemNodes.Init (createSystemNodes)
import Models.Command.Parse
import Models.History
import Models.NID
import MyPrelude
import System.Console.Haskeline qualified as H
import System.Directory (createDirectoryIfMissing, doesDirectoryExist)
import System.Environment.XDG.BaseDir (getUserDataDir)
import System.Random (initStdGen, mkStdGen)
import Utils.Testing

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
  (Text -> Either String c) -> Text -> Either String (C c)
withDefaultQuitParser p s
  | s == ":q" || s == ":quit" = Right Quit
  | otherwise = C <$> p s

-- | Run one command of the REPL. Whatever goes wrong in it is reported
-- (with @report@) and the REPL goes on:
--
-- * a 'UserError', whether the command threw it or an interpreter that the
--   command uses did (those are installed outside of the REPL, so a handler
--   installed around the command alone would not see their errors)
-- * any other exception, such as an @error@ call
--
-- An interrupt (Ctrl-C) is left to the REPL's own handler.
reportingFailure ::
  (Members [Error UserError, IOE] effs) =>
  (String -> Eff effs ()) ->
  Eff effs () ->
  Eff effs ()
reportingFailure report command =
  (command `catchError` \_ e -> report (show e))
    `catchAny` \e -> case E.fromException e of
      Just H.Interrupt -> throwIO e
      Nothing ->
        report $
          "the command failed unexpectedly (uncaught-exception): "
            <> displayException e

repl ::
  ( Members [Echo, DisplayImage, SetLocation, GetLocation, Dualizeable, Readline, IOE] effs,
    Members [FileSystem, Web, FreshNID, GetTime, Editor, State History] effs,
    Members '[FileTypeOracle, ProviderList_ GraphEditing ()] effs,
    Members [Error UserError, Warn UserError] effs,
    -- TODO: these effects are bad and shouldn't be exposed; need to rewrite
    -- commands using them as plugins once plugins are available. See also
    -- lengthier comment at interpretCommand
    Members [IOE, RawGraph] effs
  ) =>
  Eff effs ()
repl = handleInterrupt repl $ withInterrupt do
  command <- getInputLine "g> "
  case withDefaultQuitParser parseCommand . pack <$> command of
    Nothing -> outputStrLn "Goodbye!"
    Just (Right Quit) -> outputStrLn "Goodbye!"
    Just (Right (C command')) ->
      reportingFailure (liftIO . eputStr) (interpretCommand command') >> repl
    Just (Left err) -> outputStrLn err >> repl

-- | Ensures that the graph directory exists, emitting an error if it doesn't
-- or creates it if the --new option is passed in
graphDirInitialization :: FilePath -> Options -> IO ()
graphDirInitialization graphDir options = do
  graphDirExists <- doesDirectoryExist graphDir
  if options.createNew
    then
      if graphDirExists
        then
          whenM (not <$> doesNodeExist graphDir nilNID) $
            initializeGraph graphDir
        else initializeGraph graphDir
    else do
      unless graphDirExists $
        error $
          "graph in directory "
            ++ graphDir
            ++ " doesn't exist"
      unlessM (doesNodeExist graphDir nilNID) $
        error "couldn't find origin node in graph "

geMain :: IO ()
geMain = withOptions $ \options -> do
  let graphDir = options.graphLocation
  graphDirInitialization graphDir options
  nidGenerator <-
    maybe
      initStdGen
      (pure . mkStdGen)
      options.testOnlyNidGenerationSeed
  -- because we need the Env for the repl's completionFunction this is pretty
  -- fancy and uses mfix to tie the knot; it would probably be a better idea to
  -- break the cycle somehow to make this easier to reason about
  env <- mfix (initEnv graphDir nidGenerator <=< defReplSettings)
  let timeBehavior :: (IOE :> effs) => Eff (GetTime : effs) ~> Eff effs
      timeBehavior
        | options.testOnlyMonotonicIncreasingDeterministicTime =
            interpretTimeAsMonotonicIncreasingUnixTime
        | otherwise = interpretTimeAsIO
  let filesystemBehavior :: FilesystemOperationsBehavior
      filesystemBehavior =
        if options.dryRun
          then filesystemBehaviorDryRun
          else filesystemBehaviorIO
  bracket (initializeInput env.replSettings) cancelInput \inputState ->
    runAppEffects
      (runReadlineHaskeline inputState)
      printingErrorsAndWarnings
      timeBehavior
      filesystemBehavior
      env
      do
        provideList_ @GraphEditing createSystemNodes
        maybe repl interpretCommand options.executeExpression

spec_reportingFailure :: Spec
spec_reportingFailure = do
  let reports :: Eff [Error UserError, IOE] () -> IO [String]
      reports command = do
        reported <- newIORef []
        result <-
          runEff . runErrorNoCallStack @UserError $
            reportingFailure
              (\message -> modifyIORef reported (message :))
              command
        either (const (fail "the error was not handled")) pure result
        reverse <$> readIORef reported
  it "reports nothing for a command that succeeds" $
    reports (pure ()) `shouldReturn` []
  it "reports a UserError" $
    reports (throwText "no such thing") `shouldReturn` ["no such thing"]
  it "reports an exception" $
    reports (throwIO (userError "disk on fire"))
      `shouldReturn` [ "the command failed unexpectedly (uncaught-exception): "
                         <> "user error (disk on fire)"
                     ]
  it "reports an error call" do
    [message] <- reports (error "broken invariant")
    message `shouldStartWith` "the command failed unexpectedly"
    message `shouldContain` "broken invariant"
  it "leaves an interrupt to the REPL" $
    reports (throwIO H.Interrupt) `shouldThrow` \H.Interrupt -> True
