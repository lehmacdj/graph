{-# LANGUAGE TemplateHaskell #-}

module Effect.IOWrapper.Editor where

import Control.Exception qualified as E
import MyPrelude
import System.Directory
import System.Environment
import System.IO (hGetLine)
import System.Posix.IO qualified as Posix
import System.Process.Typed
import Utils.Testing.External

data Editor :: Effect where
  InvokeEditor :: [FilePath] -> Editor m ()

makeEffect ''Editor

-- | Makes the assumption that the graph is implemented using the filesystem
-- structure with nid.json/nid.data for each node
interpretEditorAsIOVim ::
  (IOE :> effs) =>
  -- | interprets effect
  Eff (Editor : effs) ~> Eff effs
interpretEditorAsIOVim = interpret \_ -> \case
  InvokeEditor paths -> do
    editor <- liftIO $ getEnv "EDITOR"
    editorToUse <- fromMaybe "vim" <$> liftIO (findExecutable editor)
    liftIO $ runToEnd stopProcess (proc editorToUse paths)

-- | Run a process until it ends, like 'runProcess_': an exit status other
-- than success is an 'ExitCodeException'. When whoever runs this is stopped
-- from outside (an interrupt, the end of the program at a signal), the
-- process is stopped with the given action ('stopProcess': it is ended and
-- waited for), and what stopped this comes out of it as itself, also when
-- stopping the process fails.
--
-- With 'runProcess_' a failure of the stop came out instead. And the stop
-- of typed-process fails when the interrupt arrives as the process ends:
-- its own thread that waits for the process has reaped it, but is stopped
-- before it has recorded that, and the stop then waits for the process
-- again, which fails with "waitForProcess: does not exist (No child
-- processes)". An interrupted @vi@ then failed with that message, instead
-- of being interrupted (in a program that threw an exception at a thread in
-- 'runProcess_' of @true@ 0 to 3 ms after it started, 3 and 20 times in
-- 3,000).
runToEnd ::
  (Process stdin stdout stderr -> IO ()) ->
  ProcessConfig stdin stdout stderr ->
  IO ()
runToEnd stop config = E.mask \restore -> do
  running <- startProcess config
  restore (checkExitCode running)
    `E.onException` (stop running `E.catch` \(_ :: IOException) -> pure ())
  stop running

spec_runToEnd :: Spec
spec_runToEnd = do
  it "succeeds when the process does" $
    runToEnd stopProcess (proc "true" [])
  it "fails when the process does" $
    runToEnd stopProcess (proc "false" [])
      `shouldThrow` \(_ :: ExitCodeException) -> True
  it "is stopped by an interrupt as by itself, also when stopping the process fails" do
    -- The process says that it runs and then waits for a minute. The stop
    -- ends it and then fails, as the stop of typed-process does when the
    -- interrupt arrives as the process ends.
    (readFd, writeFd) <- Posix.createPipe
    fromProcess <- Posix.fdToHandle readFd
    toThis <- Posix.fdToHandle writeFd
    let stopFailing running = do
          stopProcess running
          throwIO (userError "waitForProcess: no child processes")
        config =
          proc "sh" ["-c", "echo running; exec sleep 60"]
            & setStdin nullStream
            & setStdout (useHandleClose toThis)
    runner <- async (runToEnd stopFailing config)
    said <- timeout 60000000 (hGetLine fromProcess) `finally` hClose fromProcess
    said `shouldBe` Just "running"
    E.throwTo (asyncThreadId runner) E.UserInterrupt
    outcome <- timeout 60000000 (waitCatch runner)
    fmap (either (Left . E.fromException @E.AsyncException) Right) outcome
      `shouldBe` Just (Left (Just E.UserInterrupt))
