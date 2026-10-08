module Executable.GraphEditor where

import Control.Exception qualified as E
import Control.Monad.Fix
import DAL.FileSystemOperations.Metadata
  ( GraphMetadataFilesystemOperations,
    onlyDescribesWrites,
  )
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
import Graph.SystemNodes.Init (SystemParts (..), createSystemNodes)
import Models.Command.Parse
import Models.Damage (Damage (MissingOriginFile), describeDamage, describeEdge)
import Models.History
import Models.NID
import MyPrelude
import System.Console.Haskeline qualified as H
import System.Directory (createDirectoryIfMissing, doesDirectoryExist)
import System.Environment.XDG.BaseDir (getUserDataDir)
import System.Exit (ExitCode (..), exitWith)
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

-- | Make the graph ready for commands: its origin must be there, and its
-- system nodes and the edges to them are added if they aren't. Every command
-- but @fsck@ and @fix@ needs this ('needsOpenGraph'); those two must run on a
-- graph where this fails, since they are what finds and repairs the reason.
openGraph ::
  ( Members
      [ RawGraph,
        IOE,
        Error UserError,
        Warn UserError,
        ProviderList_ GraphEditing (),
        GraphMetadataFilesystemOperations
      ]
      effs
  ) =>
  Eff effs ()
openGraph =
  ( do
      graphDir <- getGraphFilePath
      unlessM (doesNodeExist graphDir nilNID) . throwString $
        "the graph is damaged: " <> describeDamage MissingOriginFile
      added <- provideList_ @GraphEditing createSystemNodes
      -- in a dry run nothing was written: say what would be
      dry <- onlyDescribesWrites
      -- not silently, whatever the command is that made the graph open: a
      -- file may have been lost (had another node recorded an edge with it,
      -- creating it would have been refused), and a command that only reads
      -- has written to the graph
      unless (null added.nodes) . warnText $
        ( if dry
            then "warning: would create the system nodes that have no file: "
            else "warning: created the system nodes that had no file: "
        )
          <> intercalate ", " (map tshow added.nodes)
      unless (null added.edges) . warnText $
        ( if dry
            then "warning: would add the system edges that no node records: "
            else "warning: added the system edges that no node recorded: "
        )
          <> intercalate ", " (map (pack . describeEdge) added.edges)
  )
    `catchError` \_ e -> throwError (CannotOpenGraph e)

-- | 'openGraph' when the commands ask for it, unless it has succeeded since
-- they last asked for it to be done again; the reference says whether it has.
openingOnce ::
  ( Members
      [ RawGraph,
        IOE,
        Error UserError,
        Warn UserError,
        ProviderList_ GraphEditing (),
        GraphMetadataFilesystemOperations
      ]
      effs
  ) =>
  IORef Bool ->
  Opening ->
  Eff effs ()
openingOnce opened = \case
  EnsureOpen -> unlessM (readIORef opened) (openGraph >> writeIORef opened True)
  OpenAgain -> writeIORef opened False

-- | The REPL. A command that needs the graph open fails while it can't be
-- opened, with the reason; opening is tried again for each such command, so
-- that the REPL goes on normally once @fix@ has repaired the graph.
repl ::
  ( Members [Echo, DisplayImage, SetLocation, GetLocation, Dualizeable, Readline, IOE] effs,
    Members [FileSystem, Web, FreshNID, GetTime, Editor, State History] effs,
    Members '[FileTypeOracle, ProviderList_ GraphEditing ()] effs,
    Members [Error UserError, Warn UserError] effs,
    -- TODO: these effects are bad and shouldn't be exposed; need to rewrite
    -- commands using them as plugins once plugins are available. See also
    -- lengthier comment at interpretCommand
    Members [IOE, RawGraph, State DamageReadPast] effs,
    GraphMetadataFilesystemOperations :> effs
  ) =>
  -- | whether the graph is open
  IORef Bool ->
  Eff effs ()
repl opened = handleInterrupt (repl opened) $ withInterrupt do
  command <- getInputLine "g> "
  case withDefaultQuitParser parseCommand . pack <$> command of
    Nothing -> outputStrLn "Goodbye!"
    Just (Right Quit) -> outputStrLn "Goodbye!"
    Just (Right (C command')) -> do
      reportingFailure
        (liftIO . eputStr)
        (interpretCommand (openingOnce opened) command')
      repl opened
    Just (Left err) -> outputStrLn err >> repl opened

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
    else
      -- whether its origin is there is for 'openGraph' to say, so that fsck
      -- and fix can run on a graph that has lost it
      unless graphDirExists $
        error $
          "graph in directory "
            ++ graphDir
            ++ " doesn't exist"

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
  exitCode <-
    bracket (initializeInput env.replSettings) cancelInput \inputState ->
      runAppEffects
        (runReadlineHaskeline inputState)
        exitCodeOfErrors
        timeBehavior
        filesystemBehavior
        env
        do
          opened <- newIORef False
          case options.executeExpression of
            -- opening the graph is part of the command line: what it reads
            -- past is warned of once, with what the commands read past
            Just command -> interpretCommand (openingOnce opened) command
            Nothing -> do
              -- before the first prompt, so that it is known from the start
              -- if the graph can't be opened, or is damaged where opening
              -- it reads
              reportingFailure
                (liftIO . eputStr)
                (warningOfDamageReadPast (openingOnce opened EnsureOpen))
              repl opened
          pure ExitSuccess
  exitWith exitCode

-- | Print warnings and errors like 'printingErrorsAndWarnings'; an error
-- makes the exit code 1, so that whoever ran @ge -e@ can tell that the
-- command failed, or that the graph could not be opened for it. Warnings
-- don't change the exit code. (The REPL reports the errors of its commands
-- and of opening the graph itself, and goes on.)
exitCodeOfErrors :: ErrorHandlingBehavior ExitCode
exitCodeOfErrors =
  printWarnings
    >>> (`handleError` \e -> liftIO (eprint e) $> ExitFailure 1)

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

spec_exitCodeOfErrors :: Spec
spec_exitCodeOfErrors = do
  let exitCodeOf :: Eff [Warn UserError, Error UserError, IOE] () -> IO ExitCode
      exitCodeOf command = runEff . exitCodeOfErrors $ command $> ExitSuccess
  it "is 0 for a command that succeeds" $
    exitCodeOf (pure ()) `shouldReturn` ExitSuccess
  it "is 1 for a command that fails" $
    exitCodeOf (throwText "(expected test output) failed")
      `shouldReturn` ExitFailure 1
  it "is 0 for a command that only warns" $
    exitCodeOf (warnText "(expected test output) warned")
      `shouldReturn` ExitSuccess
