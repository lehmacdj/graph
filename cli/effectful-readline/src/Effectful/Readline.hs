{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}

-- | A 'Readline' effect backed by haskeline. Interactive interpretation goes
-- through a haskeline session that runs in a thread of its own
-- ('InputState'), so that the rest of the program can run in 'Eff' over IO
-- instead of needing @InputT IO@ as the base monad.
module Effectful.Readline
  ( Readline (..),
    getInputLine,
    outputStr,
    withInterrupt,
    handleInterrupt,
    outputStrLn,
    runReadlineHaskeline,
    runReadlineAsking,
    installHandlerUnheld,
    runReadlineNonInteractive,

    -- * haskeline sessions
    H.Settings (..),
    InputState,
    initializeInput,
    askSession,
    sessionThread,
    cancelInput,
    closeInput,
    SessionEnded (..),
    OutputFailed (..),
  )
where

import Control.Concurrent
  ( ThreadId,
    forkIO,
    killThread,
    myThreadId,
  )
import Control.Concurrent.STM
  ( TMVar,
    TVar,
    atomically,
    check,
    newEmptyTMVarIO,
    newTVarIO,
    orElse,
    putTMVar,
    readTVar,
    takeTMVar,
    writeTVar,
  )
import Control.Exception qualified as E
import Control.Monad (unless, when)
import Control.Monad.Catch qualified as C
import Data.Functor (($>))
import Data.Maybe (isJust)
import Effectful
import Effectful.Dispatch.Dynamic
import Effectful.TH (makeEffect)
import Foreign.C.Error (throwErrnoIfMinus1_)
import Foreign.C.Types (CInt (..))
import Foreign.Marshal.Alloc (allocaBytes)
import Foreign.Ptr (Ptr, nullPtr)
import System.Console.Haskeline qualified as H
import System.Posix.Signals qualified as Signals

data Readline :: Effect where
  GetInputLine :: String -> Readline m (Maybe String)
  OutputStr :: String -> Readline m ()
  -- | Throw 'H.Interrupt' on ctrl-c within the enclosed action
  WithInterrupt :: m a -> Readline m a
  -- | @HandleInterrupt handler action@ runs @handler@ if 'H.Interrupt' is
  -- thrown while running @action@
  HandleInterrupt :: m a -> m a -> Readline m a

makeEffect ''Readline

outputStrLn :: (Readline :> effs) => String -> Eff effs ()
outputStrLn s = outputStr (s ++ "\n")

-- | A haskeline session in a thread of its own, which runs what it is asked
-- ('askSession'), one request after the other, so that the rest of the
-- program need not run in @InputT@. It is that of
-- "System.Console.Haskeline.IO" but for what an exception does to it and to
-- whoever asked:
--
-- * Whatever a request throws is thrown where it was asked, and the session
--   goes on. (A request that failed, as writing to a closed output does,
--   once ended the thread of the session, and whoever had asked waited for
--   an answer for good.)
-- * Whoever asks a session that has ended, or is waiting when it ends, gets
--   'SessionEnded' instead of waiting.
-- * An 'H.Interrupt' that reaches the thread of the session where it waits
--   for the next request is dropped: there is nothing to stop. Such a one
--   ended the session once, and no prompt was ever shown again.
--
-- No interrupt is meant for the session by itself: Ctrl-C interrupts the
-- thread that asks ('withInterruptIO'), which then has the session stop
-- what it runs for that thread ('askSession'). So a Ctrl-C belongs to
-- whatever the asking thread is at when it arrives: the line that is being
-- read, or the command of a line that was handed over a moment ago.
data InputState = InputState
  { requests :: TMVar (Maybe Request),
    ended :: TVar Bool,
    -- | the thread that the session runs in
    sessionThread :: ThreadId
  }

-- | What the session is to run, where its result goes (or what it threw),
-- and how far it has got.
data Request
  = forall a.
    Request (H.InputT IO a) (TMVar (Either E.SomeException a)) (TVar Progress)

-- | A request waits for the session to take it, is run by it, or was
-- withdrawn by whoever asked before the session took it (it is then not
-- run).
data Progress = Waiting | Taken | Withdrawn

-- | The session has ended: it was closed or cancelled, or what it runs in
-- could not go on.
data SessionEnded = SessionEnded
  deriving (Show)

instance E.Exception SessionEnded where
  displayException SessionEnded =
    "the session with the terminal has ended (session-ended)"

-- | Start a session. The terminal is the session's until 'cancelInput' or
-- 'closeInput'.
initializeInput :: H.Settings IO -> IO InputState
initializeInput settings = do
  requests <- newEmptyTMVarIO
  ended <- newTVarIO False
  -- The session runs as masked as whoever starts it is. That it has ended
  -- is recorded whatever ends it, also what arrives as the thread starts:
  -- nothing can before the thread is ready for it.
  sessionThread <- E.mask \restore ->
    forkIO $
      restore (H.runInputT settings (serve requests))
        `E.finally` atomically (writeTVar ended True)
  pure InputState {requests, ended, sessionThread}
  where
    serve requests =
      next requests >>= \case
        Nothing -> pure ()
        Just (Request asked answer progress) -> do
          wanted <-
            liftIO . atomically $
              readTVar progress >>= \case
                Withdrawn -> pure False
                _ -> writeTVar progress Taken $> True
          when wanted do
            result <- C.try asked
            case result of
              -- what stops the thread from outside ends the session, and
              -- whoever asked learns that it ended
              Left e
                | isJust (E.fromException @E.SomeAsyncException e) ->
                    liftIO (E.throwIO e)
              _ -> liftIO (atomically (putTMVar answer result))
          serve requests
    -- an interrupt for a request that is over: there is nothing to stop
    next requests =
      H.handleInterrupt (next requests) (liftIO (atomically (takeTMVar requests)))

-- | Have the session run something, and wait for the result. What the
-- request throws is thrown here, and 'SessionEnded' if the session has
-- ended or ends before it has answered.
--
-- When the one that waits is stopped (ctrl-c interrupts the thread of the
-- REPL), the request is withdrawn if the session has not taken it, and
-- else the session is told to stop running it and has stopped when this
-- returns. A session that went on with it would read the next line for
-- nobody: that line was echoed and then lost.
askSession :: InputState -> H.InputT IO a -> IO a
askSession session asked = E.mask \restore -> do
  answer <- newEmptyTMVarIO
  progress <- newTVarIO Waiting
  let -- waits while the session has not taken the request before, unless
      -- it has ended; can be stopped while it waits, and has then handed
      -- nothing over
      handOver = do
        over <- readTVar (ended session)
        unless over (putTMVar (requests session) (Just (Request asked answer progress)))
        pure (not over)
      -- an answer that is there counts, also when the session has ended
      -- since
      await =
        (Just <$> takeTMVar answer)
          `orElse` (readTVar (ended session) >>= check >> pure Nothing)
      -- The throw returns when the session's thread has got the interrupt:
      -- in the request, which that ends, or waiting for the next one, where
      -- it is dropped. Not interruptible, so that no request of this thread
      -- can follow before then and get the interrupt instead.
      withdraw = do
        taken <-
          atomically $
            readTVar progress >>= \case
              Waiting -> writeTVar progress Withdrawn $> False
              _ -> pure True
        when taken . E.uninterruptibleMask_ $
          E.throwTo (sessionThread session) H.Interrupt
  handedOver <- atomically handOver
  unless handedOver (E.throwIO SessionEnded)
  restore (atomically await) `E.onException` withdraw >>= \case
    Nothing -> E.throwIO SessionEnded
    Just (Left e) -> E.throwIO e
    Just (Right result) -> pure result

-- | End the session at once, whatever it is doing, and give the terminal
-- back.
cancelInput :: InputState -> IO ()
cancelInput session = killThread (sessionThread session) >> awaitEnd session

-- | End the session when it has run what it was asked before.
closeInput :: InputState -> IO ()
closeInput session = do
  atomically do
    over <- readTVar (ended session)
    unless over (putTMVar (requests session) Nothing)
  awaitEnd session

awaitEnd :: InputState -> IO ()
awaitEnd session = atomically (readTVar (ended session) >>= check)

-- | Interpret via a live haskeline session created with 'initializeInput'.
runReadlineHaskeline ::
  (IOE :> effs) =>
  InputState ->
  Eff (Readline : effs) a ->
  Eff effs a
runReadlineHaskeline session = runReadlineAsking (askSession session)

-- | Interpret by asking a haskeline session, given how it is asked.
--
-- The session is asked in the calling thread, which sleeps until the answer
-- is there: a line that was typed is handed over once, by whoever is asked,
-- and nothing stands between the two that could drop it.
runReadlineAsking ::
  (IOE :> effs) =>
  (forall x. H.InputT IO x -> IO x) ->
  Eff (Readline : effs) a ->
  Eff effs a
runReadlineAsking ask = interpret \env -> \case
  -- Not under haskeline's own @withInterrupt@, which has the interrupt
  -- signal throw 'H.Interrupt' at the thread of the session for as long as
  -- the line is read, and a moment longer: a ctrl-c right after the line
  -- was entered went to a session that had nothing to stop any more, and
  -- the command of the line ran on. The signal interrupts this thread
  -- instead, all the time ('WithInterrupt'), and asking is what passes that
  -- on to the session ('askSession').
  GetInputLine prompt -> liftIO $ ask (H.getInputLine prompt)
  OutputStr s ->
    liftIO $
      ask (H.outputStr s) `E.catch` \(e :: E.IOException) ->
        E.throwIO (OutputFailed e)
  WithInterrupt m ->
    localSeqUnliftIO env \unlift -> withInterruptIO (unlift m)
  HandleInterrupt handler m ->
    localSeqUnliftIO env \unlift ->
      -- The handler runs once the catch is left, not as the handler of an
      -- @E.catch@: that one runs with asynchronous exceptions masked, so
      -- whatever a handler went on to do would not see the next ctrl-c.
      E.try (unlift m) >>= either (\H.Interrupt -> unlift handler) pure

-- | What the program has to say could not be written where its output
-- goes: that is closed, or whoever read it has gone, say. With the error of
-- the system as it is.
newtype OutputFailed = OutputFailed E.IOException
  deriving (Show)

instance E.Exception OutputFailed where
  displayException (OutputFailed e) =
    "could not write the output (output-failed): " <> E.displayException e

-- | Deliver ctrl-c as an 'H.Interrupt' exception to the current thread while
-- the action runs, like haskeline's @withInterrupt@ (which only works in
-- @InputT@). The signal is handled when it arrives ('installHandlerUnheld').
withInterruptIO :: IO a -> IO a
withInterruptIO act = do
  tid <- myThreadId
  let interruptHandler = Signals.Catch (E.throwTo tid H.Interrupt)
      install = installHandlerUnheld Signals.sigINT interruptHandler
      restore old = Signals.installHandler Signals.sigINT old Nothing
  E.bracket install restore (const act)

-- | 'Signals.installHandler' (with no mask), and the handler runs when the
-- signal arrives, also while the runtime collects garbage. Returns the
-- handler before.
--
-- The GHC runtime blocks every signal that has a Haskell handler for the
-- length of each garbage collection (with @sigprocmask@, which on macOS
-- blocks it in every thread of the process). A signal that arrives then is
-- held by the system for the first thread of the process, the main one, and
-- the end of the collection does not tell that thread: it gets the signal
-- when it next wakes for some other reason. The main thread is the thread
-- of the REPL, which sleeps while a line is read. So a ctrl-c that fell
-- into a collection (the idle one, a moment after the last key, or one of
-- the work of a TAB completion) was handled when the next line was entered,
-- and stopped that line's command instead; a SIGTERM there was not handled
-- at all until something else woke the main thread.
--
-- So the signal is taken out of what the runtime blocks: the runtime is
-- told to forget its handler, as for a signal that is ignored, and the
-- signal then gets back the runtime's own handler, which passes it on to the
-- Haskell handler as before. Nothing else changes: a collection only delays
-- the Haskell handler until it is over. A signal that arrives in the moment
-- between the two is ignored. Installing a handler for the signal later
-- (as restoring the one before does) undoes this, and then finds 'Ignore' as
-- the handler before.
installHandlerUnheld :: Signals.Signal -> Signals.Handler -> IO Signals.Handler
installHandlerUnheld signal handler = do
  old <- Signals.installHandler signal handler Nothing
  -- room for a struct sigaction anywhere
  allocaBytes 1024 \action -> do
    throwErrnoIfMinus1_ "sigaction" (c_sigaction signal nullPtr action)
    -- STG_SIG_IGN: the runtime takes the signal out of what it blocks
    _ <- c_stg_sig_install signal (-2) nullPtr
    throwErrnoIfMinus1_ "sigaction" (c_sigaction signal action nullPtr)
  pure old

foreign import ccall unsafe "sigaction"
  c_sigaction :: CInt -> Ptr () -> Ptr () -> IO CInt

foreign import ccall unsafe "stg_sig_install"
  c_stg_sig_install :: CInt -> CInt -> Ptr () -> IO CInt

-- | Interpretation for non-interactive contexts (e.g. running an app stack
-- for tab completion): no input is available and output goes to stdout.
runReadlineNonInteractive ::
  (IOE :> effs) =>
  Eff (Readline : effs) a ->
  Eff effs a
runReadlineNonInteractive = interpret \env -> \case
  GetInputLine _ -> pure Nothing
  OutputStr s -> liftIO $ putStr s
  WithInterrupt m -> localSeqUnlift env \unlift -> unlift m
  HandleInterrupt _ m -> localSeqUnlift env \unlift -> unlift m
