module Executable.GraphEditor where

import Control.Concurrent (forkIO, myThreadId, threadDelay, threadWaitRead)
import Control.Exception qualified as E
import Control.Monad.Fix
import DAL.FileSystemOperations.Metadata
  ( GraphMetadataFilesystemOperations,
    onlyDescribesWrites,
  )
import DAL.FileSystemOperations.MetadataWriteDiff
  ( GraphMetadataFilesystemOperationsWriteDiff (..),
    writeGraphDiff,
  )
import DAL.RawGraph
import DAL.Serialization (doesNodeExist, initializeGraph, serializeNodeEx)
import Data.Time (getCurrentTime, getCurrentTimeZone)
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
import Error.Interrupt (EndedBySignal (..))
import Error.UserError
import Error.Warn
import Executable.GraphEditor.Completion
import Executable.GraphEditor.GoldenSpec (geEnvironment)
import Executable.GraphEditor.Options
import Foreign (Ptr, allocaBytes, castPtr, peek)
import Foreign.C.Error (throwErrnoIfMinus1_)
import Foreign.C.Types (CInt (..), CULong (..))
import Foreign.Ptr (FunPtr, castPtrToFunPtr, nullPtr, plusPtr)
import GHC.Clock (getMonotonicTimeNSec)
import GHC.IO.Exception
  ( IOErrorType (ResourceExhausted, ResourceVanished),
    IOException (..),
  )
import Graph.AppInterpreters
import Graph.Command
import Graph.Dualizeable
import Graph.FreshNID
import Graph.GraphEditing
import Graph.NodeLocated
import Graph.SystemNodes.Init (SystemParts (..), createSystemNodes)
import Models.Command.Parse
import Models.Connect
import Models.Damage (Damage (MissingOriginFile), describeDamage, describeEdge)
import Models.History
import Models.NID
import Models.Node
import MyPrelude
import System.Console.Haskeline qualified as H
import System.ControllingTerminal
import System.Directory
  ( createDirectoryIfMissing,
    doesDirectoryExist,
    doesFileExist,
    doesPathExist,
    listDirectory,
  )
import System.Environment (getEnvironment)
import System.Environment.XDG.BaseDir (getUserDataDir)
import System.Exit (ExitCode (..), exitWith)
import System.Posix.Files (ownerModes, setFileMode)
import System.Posix.IO
  ( FdOption (CloseOnExec, NonBlockingRead),
    closeFd,
    createPipe,
    dupTo,
    fdRead,
    fdToHandle,
    fdWrite,
    setFdOption,
  )
import System.Posix.Process
  ( ProcessStatus (Exited, Terminated),
    createSession,
    executeFile,
    forkProcess,
    getProcessStatus,
  )
import System.Posix.Signals qualified as Signals
import System.Posix.Terminal (openPseudoTerminal)
import System.Posix.Types (Fd (..), ProcessID)
import System.Process.Typed
  ( Process,
    byteStringInput,
    byteStringOutput,
    closed,
    getExitCode,
    getPid,
    getStderr,
    nullStream,
    proc,
    setEnv,
    setStderr,
    setStdin,
    setStdout,
    useHandleClose,
    useHandleOpen,
    waitExitCode,
    withProcessTerm,
  )
import System.Random (initStdGen, mkStdGen)
import Utils.Testing
import Utils.Testing.TempGraph

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
-- * output that could not be written ('outputFailure'), as what it is
-- * any other exception, such as an @error@ call
--
-- An interrupt (Ctrl-C) is left to the REPL's own handler, and so is
-- output that could not be written because its reader has gone: that ends
-- the REPL as it ends @ge -e@ ('sayingThatOutputFailed').
reportingFailure ::
  (Members [Error UserError, IOE] effs) =>
  (String -> Eff effs ()) ->
  Eff effs () ->
  Eff effs ()
reportingFailure report command =
  (command `catchError` \_ (e :: UserError) -> report (show e))
    `catchAny` \e -> case (E.fromException e, outputFailure e) of
      (Just H.Interrupt, _) -> throwIO e
      (_, Just failed)
        | readerHasGone failed -> throwIO e
        | otherwise -> report (displayException (OutputFailed failed))
      _ ->
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
--
-- Ctrl-C ends the line that is being typed or the command that is running
-- and the next prompt follows, however often. For that the turns (a prompt
-- and what is done with its line) are a loop and each is run under its own
-- handler: no turn runs inside the handler of an earlier one, and a turn
-- leaves no handler and no stack behind. Between two turns asynchronous
-- exceptions are masked, so an interrupt arrives inside a turn, where it is
-- handled, and not between two, where nothing would catch it.
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
repl opened = withInterrupt . mask $ \restore ->
  let turns = do
        goesOn <- handleInterrupt (pure True) (restore turn)
        when goesOn turns
   in turns
  where
    -- one prompt and what is done with its line; whether the REPL goes on
    turn = do
      command <- getInputLine "g> "
      case withDefaultQuitParser parseCommand . pack <$> command of
        Nothing -> outputStrLn "Goodbye!" $> False
        Just (Right Quit) -> outputStrLn "Goodbye!" $> False
        Just (Right (C command')) -> do
          reportingFailure
            (liftIO . eputStr)
            (interpretCommand (openingOnce opened) command')
          pure True
        Just (Left err) -> outputStrLn err $> True

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
      unless graphDirExists do
        eputStr =<< noGraphDirectory graphDir
        exitWith (ExitFailure 1)

-- | What @ge@ says when the directory of the graph it was given is not
-- there, or is not a directory.
noGraphDirectory :: FilePath -> IO String
noGraphDirectory graphDir = do
  isSomething <- doesPathExist graphDir
  pure $
    "there is no graph at "
      ++ graphDir
      ++ ": "
      ++ ( if isSomething
             then "it is not a directory"
             else "there is no such directory"
         )
      ++ " (no-graph-directory); give the directory of a graph, or add"
      ++ " --new to create a new graph there"

-- | Run the program so that SIGTERM and SIGHUP (the terminal was closed) end
-- it the way an interrupt signal does: as an exception in the main thread
-- ('EndedBySignal'). Everything that is to happen when the program ends does
-- (the terminal is given back, the history is written, damage that was read
-- past is warned of, an interrupted line says what it had changed), nothing
-- is said of the signal, and the process then ends by it, as whoever sent it
-- expects.
--
-- A change to the files of the graph is never cut short by this: the signal
-- by itself left the node files of a large change half written. Whatever
-- ends the process here first waits for the change that is being written
-- and lets no other start ('noMoreGraphChanges').
--
-- All of that is given a limited time, the allowance ('endingAllowance', but
-- for tests) twice over and the time that a change in progress takes. Then
-- the process ends by the signal anyway, and what it had not said by then is
-- lost. Without a limit it could not be ended at all in some states: a
-- terminal whose output is stopped (Ctrl-S), or that nobody reads, takes
-- nothing, so what is written to it waits, in the system, where no exception
-- arrives. Nothing tells such a terminal from one that is a moment late, so
-- there is nothing to wait for but time. (See "System.ControllingTerminal" for what a process that leads
-- its session has to do besides, and for where this was measured.)
--
-- A second SIGTERM or SIGHUP ends the process without waiting for anything
-- but a change in progress. One that the process was started to ignore, as
-- SIGHUP under @nohup@, stays ignored.
endingBySignal :: Int -> GraphWrites -> IO a -> IO a
endingBySignal allowance writes program = do
  main <- myThreadId
  arrived <- newIORef False
  let -- each arrival runs this in a thread of its own
      handler signal = do
        isFirst <- atomicModifyIORef' arrived \was -> (True, not was)
        when isFirst do
          -- by a thread of its own: the throw only returns when the main
          -- thread has got the exception, which it has not while it is in
          -- a call to the system
          _ <- forkIO (E.throwTo main (EndedBySignal signal))
          threadDelay allowance
          noMoreGraphChanges writes
          threadDelay allowance
        unless isFirst (noMoreGraphChanges writes)
        -- what the terminal has not taken by now is lost, and so is what a
        -- thread is still writing to it
        discardTerminalOutput
        endNowBy signal
  -- handled when they arrive, also during a garbage collection, which would
  -- hold them for a main thread that waits for a line to be entered
  for_ [Signals.sigTERM, Signals.sigHUP] \signal ->
    unlessM (startedIgnoring signal) . void $
      installHandlerUnheld signal (Signals.Catch (handler signal))
  program `E.catch` \(EndedBySignal signal) -> do
    -- so that a terminal which takes output shows what was said last (the
    -- limit of time above is what ends this for one that takes none)
    awaitTerminalOutput
    endNowBy signal

-- | For how long each of the two parts of ending at SIGTERM or SIGHUP may
-- take, in microseconds: saying what there is to say before the files of
-- the graph are closed to changes, and after.
endingAllowance :: Int
endingAllowance = 250000

-- | End the process by the signal, now.
endNowBy :: Signals.Signal -> IO a
endNowBy signal = do
  _ <- Signals.installHandler signal Signals.Default Nothing
  Signals.raiseSignal signal
  -- not reached
  exitWith (ExitFailure (128 + fromIntegral signal))

-- | Whether the process ignores the signal, which, before the signal gets a
-- handler here, is whether whoever started the process had it ignore the
-- signal. Asked without changing anything. (An earlier form set the signal
-- to be ignored and then back, and a process started in that moment, by the
-- tests, ignored the signal for good.)
startedIgnoring :: Signals.Signal -> IO Bool
startedIgnoring signal = allocaBytes 1024 \action -> do
  throwErrnoIfMinus1_ "sigaction" (c_sigaction signal nullPtr action)
  -- a struct sigaction begins with the handler
  (== ignoring) <$> peek (castPtr action)
  where
    -- SIG_IGN
    ignoring :: FunPtr (CInt -> IO ())
    ignoring = castPtrToFunPtr (nullPtr `plusPtr` 1)

foreign import ccall unsafe "sigaction"
  c_sigaction :: CInt -> Ptr () -> Ptr () -> IO CInt

-- for the tests: make a terminal the controlling terminal of the session
foreign import capi unsafe "sys/ioctl.h ioctl"
  c_ioctl :: Fd -> CULong -> CInt -> IO CInt

foreign import capi unsafe "sys/ioctl.h value TIOCSCTTY" c_TIOCSCTTY :: CULong

geMain :: IO ()
geMain = withOptions geWith

-- | @ge@ with its options.
geWith :: Options -> IO ()
geWith options = do
  -- Every line written as a whole as soon as it is complete, also into a
  -- pipe or a file: what a command prints there by itself (ls does) is out
  -- when the command has printed it, and not when a buffer is full or at the
  -- end. So a failure to write it stops the line there, and no line is left
  -- unwritten when a signal ends the program, which writes nothing more.
  hSetBuffering stdout LineBuffering
  -- For the tests: the system reaps the processes that ge starts, so that
  -- the stop of one fails, as the stop of the editor of vi does by chance
  -- when an interrupt arrives as the editor ends ('runToEnd'). A wait for
  -- the exit status of one that ends by itself then never returns: the
  -- thread of typed-process that waits for it fails and records no status.
  -- So it is only for an editor that is stopped at an interrupt.
  when options.testOnlyChildrenReapedBySystem . void $
    Signals.installHandler Signals.sigCHLD Signals.Ignore Nothing
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
  let allowance = fromMaybe endingAllowance options.testOnlyEndingAllowance
  exitCode <- endingBySignal allowance env.graphWrites . sayingThatOutputFailed $
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

-- | When what the program has to say cannot be written, it does not go on
-- and does not report success:
--
-- * When whoever read its output has gone (@ge -e … | head@), it ends by
--   SIGPIPE and says nothing, like any program whose reader has gone. (The
--   runtime ignores that signal, so writing fails instead, and it ends a
--   program in which nothing handles that failure with exit status 0: a
--   line that was cut short there had seemed to succeed.)
-- * Otherwise (its output is closed, say) it says so on standard error,
--   with the error of the system as it is, and fails.
--
-- (The REPL ends here as well when the reader has gone, whether a command
-- or the REPL itself was writing; a command's output that fails otherwise
-- it reports itself, as what it is, and goes on: 'reportingFailure'.)
sayingThatOutputFailed :: IO ExitCode -> IO ExitCode
sayingThatOutputFailed program =
  program `E.catch` \e -> maybe (E.throwIO e) failed (outputFailure e)
  where
    failed e
      | readerHasGone e = endNowBy Signals.sigPIPE
      | otherwise = do
          eputStr ("ge: " <> displayException (OutputFailed e))
            `E.catch` \(_ :: IOError) -> pure ()
          pure (ExitFailure 1)

-- | The error of the system when this exception is the program's output
-- failing to be written: through the line editor ('OutputFailed'), or to
-- standard output directly (as @ls@ prints).
outputFailure :: SomeException -> Maybe IOError
outputFailure e
  | Just (OutputFailed failed) <- E.fromException e = Just failed
  | Just failed <- E.fromException e,
    ioe_handle failed == Just stdout =
      Just failed
  | otherwise = Nothing

-- | Whether output failed because whoever read it has gone (a broken pipe)
readerHasGone :: IOError -> Bool
readerHasGone e = ioe_type e == ResourceVanished

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
  -- the error of the system when writing standard output fails so (the
  -- runtime names the handle's file)
  let failedWriting kind =
        IOError
          (Just stdout)
          kind
          "commitBuffer"
          "the reason"
          Nothing
          (Just "<stdout>")
  it "reports output that could not be written as what it is" do
    let said =
          "could not write the output (output-failed): <stdout>:"
            <> " commitBuffer: resource exhausted (the reason)"
    -- through the line editor, and straight to standard output (ls)
    reports (throwIO (OutputFailed (failedWriting ResourceExhausted)))
      `shouldReturn` [said]
    reports (throwIO (failedWriting ResourceExhausted)) `shouldReturn` [said]
  it "leaves output whose reader has gone to the program, which ends" do
    reports (throwIO (OutputFailed (failedWriting ResourceVanished)))
      `shouldThrow` \(OutputFailed _) -> True
    reports (throwIO (failedWriting ResourceVanished))
      `shouldThrow` \(e :: IOError) -> ioe_type e == ResourceVanished

-- | What a prompt of 'spec_repl' is answered with.
data Answer = Typed String | CtrlC

spec_repl :: Spec
spec_repl = do
  -- A process of its own, forked from this one, whose main thread runs the
  -- given action while another thread allocates, so that the runtime
  -- collects garbage all the time. The action is given what tells this
  -- process that the child is ready for a signal; it then sleeps, as the
  -- thread of the REPL does while a line is read. The runtime blocks the
  -- signals that have a Haskell handler during a collection, and one that
  -- arrived then was held for the main thread until it woke: in the REPL a
  -- Ctrl-C until the next line was entered, whose command it then stopped,
  -- and a SIGTERM at the prompt was not acted on at all. Once the child is
  -- ready, the test gets its id and what waits for it to be ready again
  -- (each wait a minute long, for a busy machine); the child is gone
  -- afterwards. A child that is not ready at first is replaced, twice at
  -- most: a process forked from this one inherits what other threads here
  -- held at that moment (the table of signal handlers, which installing a
  -- handler takes) and can wait for it for good.
  let inACollectingChild :: (IO () -> IO ()) -> (ProcessID -> IO Bool -> IO a) -> IO a
      inACollectingChild inChild test = tries (3 :: Int)
        where
          tries n = do
            (readEnd, writeEnd) <- createPipe
            for_ [readEnd, writeEnd] \fd -> setFdOption fd CloseOnExec True
            -- nothing of this process's output for the child to write again
            hFlush stdout >> hFlush stderr
            pid <- forkProcess do
              let churn :: Int -> IO ()
                  churn k = do
                    kept <- newIORef [k .. k + 20000]
                    readIORef kept >>= evaluate . sum . reverse >>= \sum' ->
                      when (sum' < 0) (fail "negative")
                    churn (k + 1)
              _ <- forkIO (churn 0)
              inChild (void (fdWrite writeEnd "r"))
            let ready =
                  timeout 60000000 (threadWaitRead readEnd >> fdRead readEnd 1) <&> \case
                    Just ("r", _) -> True
                    _ -> False
                gone = do
                  -- one that has ended and been waited for is not signalled:
                  -- its number may be another process's by now
                  tryAny (getProcessStatus False False pid) >>= \case
                    Right Nothing -> do
                      void . tryAny $ Signals.signalProcess Signals.sigKILL pid
                      void . tryAny $ getProcessStatus True False pid
                    _ -> pure ()
                  closeFd readEnd >> closeFd writeEnd
            started <- ready `onException` gone
            if started
              then test pid ready `finally` gone
              else do
                gone
                if n > 1
                  then tries (n - 1)
                  else assertFailure "a child was not ready, three times"
      -- the signal 20 to 50 ms after the child is ready, the given number of
      -- times, each when the child is ready again: the number of the first
      -- signal after which it was not, if there is one
      eachHandled :: Signals.Signal -> Int -> ProcessID -> IO Bool -> IO (Maybe Int)
      eachHandled signal times pid ready = go 1
        where
          go n
            | n > times = pure Nothing
            | otherwise = do
                threadDelay (pacing n)
                Signals.signalProcess signal pid
                ready >>= bool (pure (Just n)) (go (n + 1))
      pacing :: Int -> Int
      pacing n = [20000, 35000, 50000] `unsafeIndex` (n `mod` 3)
      asleep :: IO a
      asleep = forever (threadDelay 3600000000)
  it "handles a signal at once also while the runtime collects garbage" $
    -- with installHandlerUnheld, the child saying it is ready again in the
    -- handler
    inACollectingChild
      ( \isReady -> do
          _ <- installHandlerUnheld Signals.sigUSR2 (Signals.Catch isReady)
          isReady >> asleep
      )
      (eachHandled Signals.sigUSR2 40)
      `shouldReturn` Nothing
  it "has Ctrl-C handled at once while a line is read, also while the runtime collects garbage" $
    -- with the interpreter of the REPL: the line is read under its handler
    -- of the interrupt signal, and the child is ready again under a new one
    -- once the interrupt has stopped the reading
    inACollectingChild
      ( \isReady -> forever do
          let noSession :: H.InputT IO x -> IO x
              noSession _ = fail "no session"
          E.try @H.Interrupt . runEff . runReadlineAsking noSession . withInterrupt $
            liftIO (isReady >> asleep)
      )
      (eachHandled Signals.sigINT 40)
      `shouldReturn` Nothing
  it "ends by SIGTERM and SIGHUP at once also while the runtime collects garbage" do
    -- with what ge ends by at these signals, each in a child of its own; it
    -- ends by the signal, or with the exit status that stands for it when
    -- the signal could not be raised at once
    let endsBy signal pid _ = do
          threadDelay (pacing (fromIntegral pid))
          Signals.signalProcess signal pid
          join <$> timeout 60000000 (getProcessStatus True False pid)
        ended signal = \case
          Just (Terminated s _) -> s == signal
          Just (Exited (ExitFailure code)) -> code == 128 + fromIntegral signal
          _ -> False
        -- the first that did not end so, if one did not
        firstNotEnded [] = pure Nothing
        firstNotEnded (signal : signals) = do
          outcome <-
            inACollectingChild
              ( \isReady -> do
                  -- as in a program that a shell started: this process may
                  -- have been started ignoring SIGHUP (nohup), and has
                  -- handlers of its own
                  for_ [Signals.sigTERM, Signals.sigHUP] \s ->
                    Signals.installHandler s Signals.Default Nothing
                  writes <- newGraphWrites
                  endingBySignal endingAllowance writes (isReady >> asleep)
              )
              (endsBy signal)
          if ended signal outcome
            then firstNotEnded signals
            else pure (Just (signal, outcome))
    firstNotEnded [s | _ <- [1 :: Int .. 15], s <- [Signals.sigTERM, Signals.sigHUP]]
      `shouldReturn` Nothing
  it "runs the handler of an interrupt as unmasked as what was interrupted" do
    -- with the interpreter of the REPL: under a handler that ran masked, a
    -- command would not see the next Ctrl-C
    outside <- E.getMaskingState
    inHandler <-
      bracket (initializeInput H.defaultSettings) cancelInput \session ->
        runEff . runReadlineHaskeline session $
          handleInterrupt
            (liftIO E.getMaskingState)
            (liftIO (E.throwIO H.Interrupt))
    inHandler `shouldBe` outside
  it "keeps its session when an interrupt reaches it between two requests" $
    -- as the second of two Ctrl-C in quick succession does, when the first
    -- has ended the line that both were for; a session that this ended
    -- answered nothing any more
    bracket (initializeInput H.defaultSettings) cancelInput \session -> do
      let answers = timeout 60000000 (askSession session (pure 'x'))
      -- the session is waiting for a request once it has answered one
      answers `shouldReturn` Just 'x'
      for_ [1 :: Int .. 3] \_ -> do
        E.throwTo (sessionThread session) H.Interrupt
        answers `shouldReturn` Just 'x'
  -- What asking the session gives within a minute (for a busy machine):
  -- nothing here may wait for good, whatever the session does.
  let asking :: InputState -> H.InputT IO a -> IO (Maybe (Either SomeException a))
      asking session = timeout 60000000 . E.try . askSession session
      threw :: (Exception e) => (e -> Bool) -> Maybe (Either SomeException a) -> Bool
      threw expected = \case
        Just (Left e) -> maybe False expected (E.fromException e)
        _ -> False
  it "passes an interrupt that a request does not handle to whoever asked" $
    bracket (initializeInput H.defaultSettings) cancelInput \session -> do
      outcome <- asking session (liftIO (E.throwIO H.Interrupt) :: H.InputT IO ())
      outcome `shouldSatisfy` threw \H.Interrupt -> True
      timeout 60000000 (askSession session (pure 'x')) `shouldReturn` Just 'x'
  it "passes what a request throws to whoever asked, and goes on" $
    -- as writing to an output that is closed throws; a session that this
    -- ended left whoever had asked waiting for good
    bracket (initializeInput H.defaultSettings) cancelInput \session -> do
      outcome <-
        asking session (liftIO (throwIO (userError "disk on fire")) :: H.InputT IO ())
      outcome `shouldSatisfy` threw (== userError "disk on fire")
      timeout 60000000 (askSession session (pure 'x')) `shouldReturn` Just 'x'
  it "tells whoever asks a session that has ended" do
    session <- initializeInput H.defaultSettings
    cancelInput session
    outcome <- asking session (pure 'x')
    outcome `shouldSatisfy` threw \SessionEnded -> True
  it "tells whoever waits for a session when it ends" $
    bracket (initializeInput H.defaultSettings) cancelInput \session -> do
      started <- newEmptyMVar
      waiting <-
        async . asking session . liftIO $
          putMVar started () >> threadDelay 3600000000
      timeout 60000000 (readMVar started) `shouldReturn` Just ()
      -- as when what the session runs in cannot go on
      _ <- forkIO (E.throwTo (sessionThread session) E.ThreadKilled)
      outcome <- timeout 60000000 (wait waiting)
      join outcome `shouldSatisfy` threw \SessionEnded -> True
  it "does not run a request that was given up before the session took it" $
    bracket (initializeInput H.defaultSettings) cancelInput \session -> do
      -- the session is busy with a first request until the gate opens
      started <- newEmptyMVar
      gate <- newEmptyMVar
      busy <-
        async . askSession session . liftIO $
          putMVar started () >> uninterruptibleMask_ (readMVar gate)
      timeout 60000000 (readMVar started) `shouldReturn` Just ()
      ran <- newIORef False
      givenUp <- async (askSession session (liftIO (writeIORef ran True)))
      -- that one waits for the session to take it, and is given up
      threadDelay 100000
      cancel givenUp
      putMVar gate ()
      timeout 60000000 (wait busy) `shouldReturn` Just ()
      timeout 60000000 (askSession session (pure 'x')) `shouldReturn` Just 'x'
      readIORef ran `shouldReturn` False
  it "takes ctrl-c in the thread that asks, not in the session" do
    -- With the interpreter of the REPL, and the interrupt signal sent to
    -- this process. The session is given the request in another thread,
    -- as in the REPL, and reads a line that never comes. The signal must
    -- interrupt the thread that asks, which then stops the session: a
    -- signal that interrupted the session itself went on doing so for a
    -- moment after a line had been entered, when there was nothing to stop
    -- any more, and was lost to the command that the line had started.
    (readFd, writeFd) <- createPipe
    readEnd <- fdToHandle readFd
    writeEnd <- fdToHandle writeFd
    reading <- newEmptyMVar
    askerInterrupted <- newIORef False
    sessionInterrupted <- newIORef False
    let inAnotherThread :: H.InputT IO x -> IO x
        inAnotherThread asked = do
          session <-
            async $
              H.runInputTBehavior
                (H.useFileHandle readEnd)
                H.defaultSettings
                (liftIO (putMVar reading ()) >> asked)
                `E.catch` \H.Interrupt ->
                  writeIORef sessionInterrupted True >> E.throwIO H.Interrupt
          wait session `E.catch` \H.Interrupt -> do
            alreadyStopped <- readIORef sessionInterrupted
            unless alreadyStopped (writeIORef askerInterrupted True)
            E.throwTo (asyncThreadId session) H.Interrupt
            E.throwIO H.Interrupt
    asker <-
      async . E.try @H.Interrupt . runEff . runReadlineAsking inAnotherThread $
        withInterrupt (getInputLine "")
    timeout 60000000 (readMVar reading) `shouldReturn` Just ()
    -- The handler of the REPL is installed by now (it is before the
    -- session is asked); the moment is for the line to be read, with
    -- whatever that installs.
    threadDelay 200000
    Signals.raiseSignal Signals.sigINT
    outcome <- timeout 60000000 (wait asker)
    hClose writeEnd >> hClose readEnd
    fmap isLeft outcome `shouldBe` Just True
    readIORef askerInterrupted `shouldReturn` True
  it "has the session stop what it does for someone who is stopped waiting" $
    bracket (initializeInput H.defaultSettings) cancelInput \session -> do
      -- a request that takes an hour, as reading a line that nobody enters
      started <- newEmptyMVar
      stopped <-
        async . askSession session . liftIO $
          putMVar started () >> threadDelay 3600000000
      timeout 60000000 (readMVar started) `shouldReturn` Just ()
      cancel stopped
      -- the session is free for the next request
      timeout 60000000 (askSession session (pure 'x')) `shouldReturn` Just 'x'
  it "gets every line from the session, whenever the session answers" do
    -- With the interpreter of the REPL. Between the session and the REPL no
    -- line may be lost, whatever the moment at which it is handed over: a
    -- wait for the answer that woke every 50 ms once lost the lines that
    -- were handed over as a step of it ended, and then waited for good. So
    -- the session here has its lines already (it reads a pipe) and answers
    -- at a chosen time after it was asked, mostly at that mark.
    (readFd, writeFd) <- createPipe
    readEnd <- fdToHandle readFd
    writeEnd <- fdToHandle writeFd
    let delays :: [Int]
        delays = [0, 100000, 150000] <> replicate 40 50000
        typed :: Int -> String
        typed n = "line " <> show n
        answeringAfter :: Int -> H.InputT IO x -> IO x
        answeringAfter delay asked = do
          start <- getMonotonicTimeNSec
          answer <-
            H.runInputTBehavior (H.useFileHandle readEnd) H.defaultSettings asked
          now <- getMonotonicTimeNSec
          threadDelay (delay - fromIntegral ((now - start) `div` 1000))
          pure answer
        -- the first line that did not arrive as it was written, if any; the
        -- minute is for a busy machine, and a lost line is waited for for
        -- all of it
        firstLost = \case
          [] -> pure Nothing
          (n, delay) : rest -> do
            line <-
              timeout 60000000 . runEff . runReadlineAsking (answeringAfter delay) $
                getInputLine ""
            if line == Just (Just (typed n))
              then firstLost rest
              else pure (Just (n, delay, line))
    hPut writeEnd . encodeUtf8 . pack @Text $
      concatMap ((<> "\n") . typed) [0 .. length delays - 1]
    hClose writeEnd
    (firstLost (zip [0 ..] delays) `finally` hClose readEnd)
      `shouldReturn` Nothing
  it "leaves nothing of a turn around the turns after it" $
    withEmptyTempGraph \dir -> do
      -- lines that don't parse, and Ctrl-C at the prompt; then the end of
      -- the input
      let answers = [Typed "(", CtrlC, Typed "(", Typed "(", CtrlC, CtrlC, Typed "("]
      script <- newIORef answers
      handlers <- newIORef (0 :: Int)
      atPrompts <- newIORef ([] :: [(Int, E.MaskingState)])
      betweenTurns <- newIORef ([] :: [E.MaskingState])
      said <- newIORef ([] :: [String])
      let -- answers the prompts from the script, and notes at each how many
          -- interrupt handlers it is under and whether it would see Ctrl-C
          scripted :: ReadlineBehavior
          scripted = interpret \localEnv -> \case
            GetInputLine _ -> liftIO do
              seen <- (,) <$> readIORef handlers <*> E.getMaskingState
              modifyIORef atPrompts (seen :)
              answer <- atomicModifyIORef' script \case
                [] -> ([], Nothing)
                next : rest -> (rest, Just next)
              case answer of
                Nothing -> pure Nothing
                Just CtrlC -> E.throwIO H.Interrupt
                Just (Typed line) -> pure (Just line)
            OutputStr s -> modifyIORef said (s :)
            WithInterrupt m -> localSeqUnliftIO localEnv \unlift -> unlift m
            HandleInterrupt handler m ->
              localSeqUnliftIO localEnv \unlift -> do
                masking <- E.getMaskingState
                modifyIORef betweenTurns (masking :)
                let handled =
                      E.bracket_
                        (modifyIORef handlers (+ 1))
                        (modifyIORef handlers (subtract 1))
                E.try (handled (unlift m))
                  >>= either (\H.Interrupt -> unlift handler) pure
      env <- initEnv dir (mkStdGen 0) H.defaultSettings
      outside <- E.getMaskingState
      runAppEffects
        scripted
        printingErrorsAndWarnings
        interpretTimeAsIO
        filesystemBehaviorDryRun
        env
        (newIORef False >>= repl)
      let turns = length answers + 1
      -- every turn is under its own handler alone, and sees Ctrl-C
      (reverse <$> readIORef atPrompts)
        `shouldReturn` replicate turns (1, outside)
      -- an interrupt can only arrive inside a turn
      readIORef betweenTurns
        `shouldReturn` replicate turns E.MaskedInterruptible
      -- each line was answered and the input ended the REPL
      (reverse <$> readIORef said) >>= \outputs -> do
        length outputs `shouldBe` 5
        lastMay outputs `shouldBe` Just "Goodbye!\n"

-- | The origin -b-> 1 -d-> 2 in a new graph, where the file of 2 does not
-- record the edge d if the graph is to be damaged.
writeChainGraph :: Bool -> FilePath -> IO ()
writeChainGraph damaged dir = do
  let n1 = smallNID 1
      n2 = smallNID 2
      write :: Node Text () -> IO ()
      write n = serializeNodeEx (n $> Nothing) dir
      reached :: Node Text ()
      reached = emptyNode n1 & #incoming .~ singletonSet (Connect "b" nilNID)
  write $ emptyNode nilNID & #outgoing .~ singletonSet (Connect "b" n1)
  write $ reached & #outgoing .~ singletonSet (Connect "d" n2)
  write $
    emptyNode n2
      & #incoming .~ (if damaged then mempty else singletonSet (Connect "d" n1))

-- | Where the interrupt of 'spec_interruptedCommand' arrives.
data InterruptedAt
  = -- | as the line asks for the time (@mk@ does, for the nodes that it
    -- creates, before it writes them) once edges with these labels are in
    -- the graph's files
    OnceMade [Text]
  | -- | as the line is about to say something for this time, from 1
    AtOutput Int
  | -- | while the line writes a change to the graph for this time, from 1:
    -- it is thrown at the thread then and delivered as the write returns,
    -- as an interrupt is that arrives while a change is written
    AsWritten Int

spec_interruptedCommand :: Spec
spec_interruptedCommand = do
  -- A command line on the origin -b-> 1 -d-> 2 and an interrupt (Ctrl-C):
  -- the notes that were printed, whether the interrupt went on to whoever
  -- ran the line, and where the line left the REPL.
  let interruptedAndWhere :: InterruptedAt -> Text -> IO ([String], Bool, NID)
      interruptedAndWhere moment line = withEmptyTempGraph \dir -> do
        writeChainGraph False dir
        outputs <- newIORef (0 :: Int)
        writes <- newIORef (0 :: Int)
        warned <- newIORef ([] :: [String])
        let made :: [Text] -> IO Bool
            made labels = do
              files <- listDirectory dir
              contents :: ByteString <-
                concat <$> traverse (readFile . (dir </>)) files
              let inQuotes label = encodeUtf8 ("\"" <> label <> "\"")
              pure (all ((`isInfixOf` contents) . inQuotes) labels)
            scripted :: ReadlineBehavior
            scripted = interpret \localEnv -> \case
              GetInputLine _ -> pure Nothing
              OutputStr _ -> liftIO do
                nth <- atomicModifyIORef' outputs \n -> (n + 1, n + 1)
                case moment of
                  AtOutput n | n == nth -> E.throwIO H.Interrupt
                  _ -> pure ()
              WithInterrupt m -> localSeqUnliftIO localEnv \unlift -> unlift m
              HandleInterrupt _ m -> localSeqUnliftIO localEnv \unlift -> unlift m
            timed :: TimeBehavior
            timed = interpret \_ -> \case
              CurrentTime -> liftIO do
                case moment of
                  OnceMade labels -> whenM (made labels) (E.throwIO H.Interrupt)
                  _ -> pure ()
                getCurrentTime
              CurrentTimeZone -> liftIO getCurrentTimeZone
            -- Writes like the real thing, but for an interrupt that is
            -- thrown at the thread before the chosen write starts and
            -- cannot arrive until that write has returned.
            writing :: FilesystemOperationsBehavior
            writing =
              interpose
                ( \_ (WriteGraphDiff asLoaded changes changedNodes others) -> do
                    nth <- atomicModifyIORef' writes \n -> (n + 1, n + 1)
                    let write =
                          writeGraphDiff asLoaded changes changedNodes others
                    case moment of
                      -- no exception is delivered inside of the write, as
                      -- none is inside of the call to the system that the
                      -- real one is
                      AsWritten n | n == nth -> uninterruptibleMask_ do
                        me <- liftIO myThreadId
                        thrown <- newEmptyMVar
                        _ <- liftIO . forkIO $ do
                          putMVar thrown ()
                          E.throwTo me H.Interrupt
                        readMVar thrown >> liftIO (threadDelay 20000)
                        write
                      _ -> write
                )
                >>> filesystemBehaviorIO
            note :: (IOE :> r) => UserError -> Eff r ()
            note e = liftIO (modifyIORef warned (show e :))
            recording :: ErrorHandlingBehavior ()
            recording = interpret (\_ (Warn e) -> note e) >>> (`handleError` note)
        command <- either fail pure (parseCommand line)
        env <- initEnv dir (mkStdGen 0) H.defaultSettings
        -- The graph is opened beforehand: that creates its system nodes,
        -- which is a change to its files like any other, and would be the
        -- first thing that every one of these lines had changed.
        runAppEffects scripted recording interpretTimeAsIO filesystemBehaviorIO env do
          opened <- newIORef False
          openingOnce opened EnsureOpen
        writeIORef warned []
        outcome <-
          E.try @H.Interrupt $
            runAppEffects scripted recording timed writing env do
              opened <- newIORef False
              interpretCommand (openingOnce opened) command
        notes <- filter ("Note: " `isPrefixOf`) . reverse <$> readIORef warned
        location <- view #now <$> readIORef env.history
        pure (notes, isLeft outcome, location)
      interrupted moment line =
        interruptedAndWhere moment line <&> \(notes, went, _) -> (notes, went)
      atChangedAt targets =
        "Note: at had already changed the graph at "
          <> intercalate ", " (map ("@00000000000" <>) targets)
          <> "; what it did there stays."
      thisOne =
        "Note: the command that was interrupted had already changed the graph;"
          <> " what it did stays."
      oneBefore =
        "Note: the command before this one in the line had already run; what"
          <> " it did stays."
      twoBefore =
        "Note: the 2 commands before this one in the line had already run;"
          <> " what they did stays."
  it "says at which targets at had changed the graph when it is interrupted" $
    interrupted (OnceMade ["xx"]) "at (b | b/d) mk xx"
      `shouldReturn` ([atChangedAt ["1"]], True)
  it "names the target that at was writing at when the interrupt arrived" do
    interrupted (AsWritten 1) "at (b | b/d) mk xx"
      `shouldReturn` ([atChangedAt ["1"]], True)
    interrupted (AsWritten 2) "at (b | b/d) mk xx"
      `shouldReturn` ([atChangedAt ["1", "2"]], True)
  it "says nothing when at is interrupted before it has written anything" $
    interrupted (OnceMade []) "at (b | b/d) mk xx" `shouldReturn` ([], True)
  it "says nothing when at is interrupted in a command that changes nothing" $
    interrupted (AtOutput 2) "at (b | b/d) nid" `shouldReturn` ([], True)
  it "names only the targets at which something was written" $
    -- the targets are the origin, 1 and 2, in that order; at 1 the edge d
    -- is there already and mk writes nothing, and the interrupt arrives
    -- there, in what nid says: each mk says what it created, so the
    -- outputs are mk and nid at the origin, then mk and nid at 1
    interrupted (AtOutput 4) "at (@ | b | b/d) { mk d ; nid }"
      `shouldReturn` ([atChangedAt ["0"]], True)
  it "leaves the REPL where it was when at is interrupted" do
    (_, went, location) <-
      interruptedAndWhere (OnceMade ["xx"]) "at b { cd d ; mk xx ; mk yy }"
    (went, location) `shouldBe` (True, nilNID)
  it "says how many commands of a line had run when it is interrupted" do
    interrupted (OnceMade ["xx"]) "mk xx ; mk yy ; mk zz"
      `shouldReturn` ([oneBefore], True)
    interrupted (OnceMade ["xx", "yy"]) "mk xx ; mk yy ; mk zz"
      `shouldReturn` ([twoBefore], True)
    interrupted (OnceMade ["xx"]) "nid ; mk xx ; mk yy"
      `shouldReturn` ([twoBefore], True)
  it "says that the interrupted command had written, also the first of a line" do
    interrupted (AsWritten 1) "mk xx ; mk yy" `shouldReturn` ([thisOne], True)
    interrupted (AsWritten 2) "mk xx ; mk yy ; mk zz"
      `shouldReturn` ([thisOne, oneBefore], True)
  it "says that a command by itself had written when it is interrupted" $
    interrupted (AsWritten 1) "mk xx" `shouldReturn` ([thisOne], True)
  it "says nothing when the first command of a line is interrupted before it writes" $
    interrupted (OnceMade []) "mk xx ; mk yy" `shouldReturn` ([], True)
  it "says nothing when the commands that had run changed nothing" do
    interrupted (AtOutput 2) "nid ; nid" `shouldReturn` ([], True)
    interrupted (OnceMade []) "nid ; ls b ; mk xx" `shouldReturn` ([], True)
    -- mk of what is there already writes nothing; it says so (output 1),
    -- and the interrupt arrives in what nid says after it
    interrupted (AtOutput 2) "mk b/d ; nid" `shouldReturn` ([], True)
  it "says both of an at in a line" $
    interrupted (OnceMade ["xx", "yy"]) "mk xx ; at (b | b/d) mk yy"
      `shouldReturn` ([atChangedAt ["1"], oneBefore], True)
  it "names a target once when an at in the command has named it" do
    interrupted (AsWritten 1) "at (b | b/d) at @ mk xx"
      `shouldReturn` ([atChangedAt ["1"]], True)
    interrupted (OnceMade ["xx"]) "at (b | b/d) at @ { mk xx ; mk yy }"
      `shouldReturn` ([oneBefore, atChangedAt ["1"]], True)
  it "says nothing of a line that is not interrupted" $
    interrupted (AtOutput 0) "mk xx ; at (b | b/d) mk yy"
      `shouldReturn` ([], False)

-- | @ge -e@ at a signal, with the @ge@ that the tests run.
spec_interruptSignal :: Spec
spec_interruptSignal = do
  -- a path that never finishes compiling (see 'spec_completionFunction')
  let endless :: String
      endless = "(x | y)" <> concat (replicate 7 "/(a & b & c)" :: [String])
      -- Run @ge -e@ on a line that ends in a command that cannot finish,
      -- send it the signal once the given condition on the graph's directory
      -- holds, and return how it ended and what it wrote to standard output
      -- and error. No moment is too early for the signal: before @ge@
      -- handles signals the system ends it in the same way. The minute is for
      -- a busy machine.
      signalledWhen ::
        Signals.Signal ->
        Bool ->
        (FilePath -> IO Bool) ->
        String ->
        IO (Maybe (ExitCode, LByteString, LByteString))
      signalledWhen signal =
        signalledThus asIs Nothing (Signals.signalProcess signal)
      -- The same, but @ge@ is started as the first argument says, with its
      -- arguments and environment (the program and its arguments, the
      -- environment), its standard error goes to the handle given second if
      -- one is (and nothing of it is returned), and the third argument is
      -- what is done to it once the condition holds.
      --
      -- What @ge@ writes goes into files, which hold all of it once it has
      -- ended. Read from a pipe after the end, it could be cut short: the
      -- pipe ends only when every process that has a copy of its writing end
      -- has closed it, and a process that this one starts at the same moment
      -- gets a copy.
      signalledThus ::
        (([String], [(String, String)]) -> (FilePath, [String], [(String, String)])) ->
        Maybe Handle ->
        (ProcessID -> IO ()) ->
        Bool ->
        (FilePath -> IO Bool) ->
        String ->
        IO (Maybe (ExitCode, LByteString, LByteString))
      signalledThus launch errorsTo signalling damaged ready line =
        withEmptyTempGraph \dir -> withSystemTempDirectory "ge-data" \dataHome ->
          withSystemTempDirectory "ge-output" \outputs -> do
            writeChainGraph damaged dir
            environment <- getEnvironment
            let outFile = outputs </> "stdout"
                errFile = outputs </> "stderr"
                launched arguments =
                  let (program, arguments', environment') =
                        launch (dir : arguments, geEnvironment dataHome environment)
                   in proc program arguments'
                        & setEnv environment'
                        & setStdin nullStream
                -- until the condition holds, or ge has ended without it
                waitingFor :: Process () () () -> Int -> IO Bool
                waitingFor _ 0 = pure False
                waitingFor running n =
                  getExitCode running >>= \case
                    Just _ -> pure False
                    Nothing ->
                      ready dir
                        >>= bool (threadDelay 10000 >> waitingFor running (n - 1)) (pure True)
            -- the system nodes, which opening the graph would warn of creating
            withProcessTerm
              ( proc "ge" [dir, "-e", "nid"]
                  & setEnv (geEnvironment dataHome environment)
                  & setStdin nullStream
                  & setStdout nullStream
                  & setStderr nullStream
              )
              waitExitCode
              `shouldReturn` ExitSuccess
            ended <-
              withBinaryFile outFile WriteMode \out ->
                withBinaryFile errFile WriteMode \err ->
                  withProcessTerm
                    ( launched ["-e", line]
                        & setStdout (useHandleOpen out)
                        & setStderr (maybe (useHandleOpen err) useHandleClose errorsTo)
                    )
                    \running -> do
                      isReady <- waitingFor running 6000
                      when isReady $ getPid running >>= traverse_ signalling
                      ended <- timeout 60000000 (waitExitCode running)
                      -- one that has not ended is not left to be waited for
                      when (isNothing ended) $
                        getPid running >>= traverse_ (Signals.signalProcess Signals.sigKILL)
                      pure ended
            for ended \exitCode ->
              (exitCode,,)
                <$> (fromStrict <$> readFile outFile)
                <*> (fromStrict <$> readFile errFile)
      asIs (arguments, environment) = ("ge", arguments, environment)
      interruptedWhen = signalledWhen Signals.sigINT
      by signal = ExitFailure (negate (fromIntegral signal))
      bySigINT = by Signals.sigINT
      -- whether the directory has a file more than when this was first
      -- asked: a command of the line has created a node
      aFileMore :: IO (FilePath -> IO Bool)
      aFileMore = do
        files <- newIORef (Nothing :: Maybe Int)
        pure \dir -> do
          now <- length <$> listDirectory dir
          before <- atomicModifyIORef' files \case
            Nothing -> (Just now, now)
            Just n -> (Just n, n)
          pure (now > before)
      text :: LByteString -> String
      text = unpack . decodeUtf8 . toStrict
      -- what a line says when a command of it had written: the one that was
      -- interrupted, or the one before it
      hadWritten :: [String]
      hadWritten =
        [ "Note: the command before this one in the line had already run; what"
            <> " it did stays.\n",
          "Note: the command that was interrupted had already changed the"
            <> " graph; what it did stays.\n"
        ]
      -- With an editor that runs the shell commands that the first argument
      -- gives for a directory of the editor's own (in which the editor is),
      -- where $1 is the file that it edits: that directory, and how ge is
      -- started with it.
      withEditorRunning ::
        (FilePath -> ByteString) ->
        ( FilePath ->
          (([String], [(String, String)]) -> (FilePath, [String], [(String, String)])) ->
          IO a
        ) ->
        IO a
      withEditorRunning script act =
        withSystemTempDirectory "ge-editor" \own -> do
          let editor = own </> "editor"
          writeFile editor ("#!/bin/sh\n" <> script own <> "\n")
          setFileMode editor ownerModes
          act own \(arguments, environment) ->
            ( "ge",
              arguments,
              ("EDITOR", editor)
                : filter ((`notElem` ["EDITOR", "VISUAL"]) . fst) environment
            )
  it "ends by the signal and prints nothing" do
    Just (exitCode, out, err) <-
      interruptedWhen False (const (pure True)) ("ls " <> endless)
    (exitCode, out, err) `shouldBe` (bySigINT, "", "")
  it "warns of the damage that the line had read past" do
    -- the second command adds a node file, which shows that the first is
    -- done
    made <- aFileMore
    Just (exitCode, out, err) <-
      interruptedWhen True made ("ls b/d ; mk zz ; ls " <> endless)
    exitCode `shouldBe` bySigINT
    -- what the first command listed
    text out `shouldContain` "@000000000002"
    text err `shouldContain` "damaged where this command read it"
    -- the interrupt is not reported as a failure of the command (a note
    -- may say that the interrupted command had written)
    text err `shouldNotContain` "Interrupt"
    text err `shouldNotContain` "user interrupt"
  -- A ge that the test starts has at a signal what this process has as it
  -- starts it: one ignored here is ignored there, and one caught here is at
  -- its default there. The test runner (tasty) catches SIGTERM and SIGHUP,
  -- and the runtime SIGINT, also when the tests were started ignoring them
  -- (as SIGHUP under nohup), so each of them can be checked. One that this
  -- process ignores when the test runs could not be: the test fails then,
  -- and says so, after checking the others.
  it "says what the line had changed, at SIGTERM and SIGHUP as at an interrupt" do
    unchecked <-
      for
        [ (Signals.sigINT, "SIGINT"),
          (Signals.sigTERM, "SIGTERM"),
          (Signals.sigHUP, "SIGHUP")
        ]
        \(signal, name) ->
          startedIgnoring signal >>= \case
            True -> pure [name]
            False -> do
              made <- aFileMore
              Just (exitCode, _, err) <-
                signalledWhen signal False made ("mk zz ; ls " <> endless)
              -- the signal is sent when the node's file is there, which is
              -- when mk has written and may or may not have returned
              (exitCode, text err)
                `shouldSatisfy` \(code, said) ->
                  code == by signal && said `elem` hadWritten
              pure []
    unless (null (concat unchecked)) . expectationFailure $
      "not checked at "
        <> intercalate " and " (concat unchecked)
        <> ": this process ignores it, and so does every ge that it starts"
  it "ends by SIGTERM in time when nothing takes what it has to say" do
    -- Its standard error is a pipe that is full and that nobody reads, and
    -- the line has a warning and a note to write there when it is ended:
    -- writing them waits for good.
    (readFd, writeFd) <- createPipe
    setFdOption writeFd NonBlockingRead True
    let fill = fdWrite writeFd (replicate 4096 'x') >> fill
    fill `catch` \(_ :: IOError) -> pure ()
    setFdOption writeFd NonBlockingRead False
    full <- fdToHandle writeFd
    made <- aFileMore
    ended <-
      signalledThus
        asIs
        (Just full)
        (Signals.signalProcess Signals.sigTERM)
        True
        made
        ("ls b/d ; mk zz ; ls " <> endless)
        `finally` closeFd readFd
    fmap (\(exitCode, _, _) -> exitCode) ended
      `shouldBe` Just (by Signals.sigTERM)
  it "fails, and says why, when its output cannot be written" $
    -- standard output is closed; this once waited for good
    withEmptyTempGraph \dir -> withSystemTempDirectory "ge-data" \dataHome -> do
      writeChainGraph False dir
      environment <- getEnvironment
      let ge =
            proc "ge" [dir, "-e", "nid"]
              & setEnv (geEnvironment dataHome environment)
              & setStdin nullStream
              & setStdout closed
              & setStderr byteStringOutput
      Just (exitCode, err) <- withProcessTerm ge \running ->
        timeout 60000000 $
          (,) <$> waitExitCode running <*> atomically (getStderr running)
      exitCode `shouldBe` ExitFailure 1
      text err `shouldContain` "could not write the output (output-failed): "
  it "ends by SIGPIPE, not with success, when the reader of its output has gone" $
    -- The reader takes a little and leaves, as head does. The line prints
    -- more than a pipe holds, with a command that writes to standard output
    -- by itself (ls) and with one that goes through the line editor (nid),
    -- and would go on to create a node. The REPL ends the same way, given
    -- those commands a line each, and says nothing either.
    for_ ["ls b", "nid" :: String] \command -> for_ [False, True] \inRepl ->
      withEmptyTempGraph \dir -> withSystemTempDirectory "ge-data" \dataHome -> do
        writeChainGraph False dir
        environment <- getEnvironment
        (readFd, writeFd) <- createPipe
        -- or ge would have the reading end itself, and its reader would
        -- never be gone
        setFdOption readFd CloseOnExec True
        writeEnd <- fdToHandle writeFd
        let commands = replicate 8000 command <> ["mk zz"] :: [String]
            ge arguments =
              proc "ge" (dir : arguments)
                & setEnv (geEnvironment dataHome environment)
                & setStdin nullStream
                & setStderr byteStringOutput
            reading
              | inRepl =
                  ge []
                    & setStdin
                      (byteStringInput (fromString (unlines commands)))
              | otherwise = ge ["-e", intercalate " ; " commands]
        -- the system nodes, which opening the graph would warn of creating
        withProcessTerm (ge ["-e", "nid"] & setStdout byteStringOutput) waitExitCode
          `shouldReturn` ExitSuccess
        outcome <-
          withProcessTerm (reading & setStdout (useHandleClose writeEnd)) \running -> do
            _ <- fdRead readFd 100
            closeFd readFd
            ended <-
              timeout 60000000 $
                (,) <$> waitExitCode running <*> atomically (getStderr running)
            -- one that is still there is not left to wait for a reader
            when (isNothing ended) $
              getPid running >>= traverse_ (Signals.signalProcess Signals.sigKILL)
            pure ended
        (inRepl, fmap (fmap text) outcome)
          `shouldBe` (inRepl, Just (by Signals.sigPIPE, ""))
        files <- listDirectory dir
        contents :: ByteString <- concat <$> traverse (readFile . (dir </>)) files
        ("\"zz\"" `isInfixOf` contents) `shouldBe` False

  it "fails at once when its standard output and error are both closed" $
    -- the runtime's own descriptors took their numbers, and what ge wrote
    -- there made it wait for good
    withEmptyTempGraph \dir -> withSystemTempDirectory "ge-data" \dataHome -> do
      writeChainGraph False dir
      environment <- getEnvironment
      let ge =
            proc "ge" [dir, "-e", "nid"]
              & setEnv (geEnvironment dataHome environment)
              & setStdin nullStream
              & setStdout closed
              & setStderr closed
      ended <- withProcessTerm ge \running -> do
        ended <- timeout 60000000 (waitExitCode running)
        when (isNothing ended) $
          getPid running >>= traverse_ (Signals.signalProcess Signals.sigKILL)
        pure ended
      ended `shouldBe` Just (ExitFailure 1)
  it "does not go on with the line when what a command prints cannot be written" $
    -- ls prints to standard output by itself, which is closed: a line that
    -- waited in a buffer was only found unwritten at the end, and the
    -- command after it had run, and ge succeeded
    withEmptyTempGraph \dir -> withSystemTempDirectory "ge-data" \dataHome -> do
      writeChainGraph False dir
      environment <- getEnvironment
      let ge arguments =
            proc "ge" (dir : arguments)
              & setEnv (geEnvironment dataHome environment)
              & setStdin nullStream
              & setStderr byteStringOutput
      withProcessTerm (ge ["-e", "nid"] & setStdout byteStringOutput) waitExitCode
        `shouldReturn` ExitSuccess
      Just (exitCode, err) <-
        withProcessTerm (ge ["-e", "ls b ; mk zz"] & setStdout closed) \running ->
          timeout 60000000 $
            (,) <$> waitExitCode running <*> atomically (getStderr running)
      exitCode `shouldBe` ExitFailure 1
      text err `shouldContain` "could not write the output (output-failed): "
      files <- listDirectory dir
      contents :: ByteString <- concat <$> traverse (readFile . (dir </>)) files
      ("\"zz\"" `isInfixOf` contents) `shouldBe` False
  it "has written what the line printed when SIGTERM ends it" do
    -- into a pipe: what ls printed is out, though the signal ends ge
    -- without its writing anything more
    made <- aFileMore
    Just (exitCode, out, _) <-
      signalledWhen Signals.sigTERM False made ("ls b ; mk zz ; ls " <> endless)
    exitCode `shouldBe` by Signals.sigTERM
    text out `shouldContain` "@000000000001"
  it "says that vi had changed the data file when the line is interrupted" $
    -- the editor writes the file itself, and it is counted as a change of
    -- the graph like the others
    withEditorRunning (const "echo edited >> \"$1\"") \_ withEditor -> do
      let edited dir = doesFileExist (dir </> "000000000000.data")
      Just (exitCode, _, err) <-
        signalledThus withEditor Nothing (Signals.signalProcess Signals.sigINT) False edited ("vi ; ls " <> endless)
      -- the signal is sent when the file is there, which is when the editor
      -- has written it and may or may not have ended
      (exitCode, text err)
        `shouldSatisfy` \(code, said) -> code == bySigINT && said `elem` hadWritten
  it "says nothing of vi when the editor has not changed the data file" $
    -- the editor runs and ends, and nothing of the graph was written
    withEditorRunning (\own -> "touch '" <> encodeUtf8 (pack (own </> "ran")) <> "'") \own withEditor -> do
      let ran _ = doesFileExist (own </> "ran")
      Just (exitCode, _, err) <-
        signalledThus withEditor Nothing (Signals.signalProcess Signals.sigINT) False ran ("vi ; ls " <> endless)
      (exitCode, text err) `shouldBe` (bySigINT, "")
  it "is interrupted in vi, and does not fail, when the wait for the editor fails" do
    -- The editor writes the file and runs on, and the system reaps what ge
    -- starts (a test-only option), so the wait for the editor when the
    -- interrupt stops it fails each time, as it does by chance when the
    -- interrupt arrives as the editor ends ('runToEnd'). That failure is
    -- not to replace the interrupt.
    let script own =
          "echo edited >> \"$1\"\ntouch '"
            <> encodeUtf8 (pack (own </> "edited"))
            <> "'\nexec sleep 60"
    withEditorRunning script \own withEditor -> do
      let edited _ = doesFileExist (own </> "edited")
          reaped (arguments, environment) =
            withEditor
              (arguments <> ["--test-only-children-reaped-by-the-system"], environment)
      Just (exitCode, _, err) <-
        signalledThus reaped Nothing (Signals.signalProcess Signals.sigINT) False edited ("vi ; ls " <> endless)
      (exitCode, text err)
        `shouldBe` ( bySigINT,
                     "Note: the command that was interrupted had already changed the"
                       <> " graph; what it did stays.\n"
                   )
  it "stays when it gets a signal that it was started to ignore" do
    -- SIGHUP, as under nohup, from the shell that starts ge; SIGTERM ends
    -- it some seconds later
    made <- aFileMore
    let ignoringHup (arguments, environment) =
          ("sh", ["-c", "trap '' HUP; exec \"$0\" \"$@\"", "ge"] <> arguments, environment)
        -- the process may be gone already, if the signal ended it
        sending signal pid = void . tryAny $ Signals.signalProcess signal pid
        hupThenTerm pid = do
          sending Signals.sigHUP pid
          threadDelay 3000000
          sending Signals.sigTERM pid
    Just (exitCode, _, _) <-
      signalledThus ignoringHup Nothing hupThenTerm False made ("mk zz ; ls " <> endless)
    exitCode `shouldBe` by Signals.sigTERM
  it "ends at a second SIGTERM without waiting for what it has to say" do
    -- Its standard error is a full pipe that nobody reads and the line has
    -- a warning and a note to write there, which waits for good, and it is
    -- given an hour for that: the second signal ends it all the same.
    (readFd, writeFd) <- createPipe
    setFdOption writeFd NonBlockingRead True
    let fill = fdWrite writeFd (replicate 4096 'x') >> fill
    fill `catch` \(_ :: IOError) -> pure ()
    setFdOption writeFd NonBlockingRead False
    full <- fdToHandle writeFd
    made <- aFileMore
    let anHour (arguments, environment) =
          ("ge", arguments <> ["--test-only-ending-allowance", "3600000000"], environment)
        twice pid = do
          Signals.signalProcess Signals.sigTERM pid
          threadDelay 200000
          Signals.signalProcess Signals.sigTERM pid
    ended <-
      signalledThus anHour (Just full) twice True made ("ls b/d ; mk zz ; ls " <> endless)
        `finally` closeFd readFd
    fmap (\(exitCode, _, _) -> exitCode) ended
      `shouldBe` Just (by Signals.sigTERM)
  -- @ge@ as a terminal emulator starts it: the leader of a session of its
  -- own, with a new terminal as its controlling terminal and its standard
  -- input, output and error. Nothing reads the terminal, and the terminal
  -- stays open here as well (as it does when a program of the session has
  -- opened /dev/tty, which the line editor does): when such a process ends,
  -- the system waits for the terminal to take what is queued for it, for
  -- good, and no signal ends the process then. Closing the terminal ends
  -- that wait, which is done when a minute is over.
  let leadingItsSession :: [String] -> IO (Maybe ProcessStatus)
      leadingItsSession arguments =
        withEmptyTempGraph \dir -> withSystemTempDirectory "ge-data" \dataHome -> do
          writeChainGraph False dir
          environment <- getEnvironment
          -- the system nodes, which opening the graph would warn of creating
          let ge =
                proc "ge" [dir, "-e", "nid"]
                  & setEnv (geEnvironment dataHome environment)
                  & setStdin nullStream
                  & setStdout byteStringOutput
                  & setStderr byteStringOutput
          withProcessTerm ge waitExitCode `shouldReturn` ExitSuccess
          (terminal, ownEnd) <- openPseudoTerminal
          for_ [terminal, ownEnd] \fd -> setFdOption fd CloseOnExec True
          pid <- forkProcess do
            _ <- createSession
            throwErrnoIfMinus1_ "ioctl" (c_ioctl ownEnd c_TIOCSCTTY 0)
            for_ [0, 1, 2] \fd -> dupTo ownEnd (Fd fd)
            executeFile "ge" True (dir : arguments) (Just (geEnvironment dataHome environment))
          ended <- async (getProcessStatus True False pid)
          -- once: a number that is closed is soon another file's
          open <- newMVar True
          let closing = modifyMVar_ open \isOpen -> do
                when isOpen (closeFd terminal >> closeFd ownEnd)
                pure False
          ( do
              -- once it has written to the terminal, and a moment more for
              -- it to get to where it waits
              _ <- timeout 60000000 (threadWaitRead terminal)
              threadDelay 500000
              Signals.signalProcess Signals.sigTERM pid
              outcome <- timeout 60000000 (wait ended)
              when (isNothing outcome) do
                closing
                void . tryAny $ Signals.signalProcess Signals.sigKILL pid
                void (waitCatch ended)
              pure (join outcome)
            )
            `onException` (closing >> void (tryAny (Signals.signalProcess Signals.sigKILL pid)))
            `finally` closing
      byTERM = Just (Terminated Signals.sigTERM False)
  it "ends by SIGTERM in time at its prompt when it leads its session and nobody reads" $
    -- it waits for the terminal to take what it wrote, and when that is
    -- not done in time, it has the terminal discard it
    leadingItsSession [] `shouldReturn` byTERM
  it "ends by SIGTERM in time while it writes when it leads its session and nobody reads" do
    -- Each listing is a line longer than the terminal's queue, which is
    -- written at once: the thread that writes it waits in the system. A
    -- thread that still writes when the queue is discarded fills it again,
    -- unless the terminal discards what is written to it too.
    let long = replicate 6000 'a'
    leadingItsSession
      ["-e", intercalate " ; " (("mk " <> long) : replicate 10 ("ls " <> long))]
      `shouldReturn` byTERM

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
