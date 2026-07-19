{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}

-- | A 'Readline' effect backed by haskeline. Interactive interpretation goes
-- through haskeline's "System.Console.Haskeline.IO" query interface so that
-- the rest of the program can run in 'Eff' over IO instead of needing
-- @InputT IO@ as the base monad.
module Effectful.Readline
  ( Readline (..),
    getInputLine,
    outputStr,
    withInterrupt,
    handleInterrupt,
    outputStrLn,
    runReadlineHaskeline,
    runReadlineNonInteractive,

    -- * re-exports for constructing haskeline sessions
    H.Settings (..),
    HIO.InputState,
    HIO.initializeInput,
    HIO.cancelInput,
    HIO.closeInput,
  )
where

import Control.Concurrent (myThreadId)
import Control.Exception qualified as E
import Effectful
import Effectful.Dispatch.Dynamic
import Effectful.TH (makeEffect)
import System.Console.Haskeline qualified as H
import System.Console.Haskeline.IO qualified as HIO
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

-- | Interpret via a live haskeline session created with 'HIO.initializeInput'.
runReadlineHaskeline ::
  (IOE :> effs) =>
  HIO.InputState ->
  Eff (Readline : effs) a ->
  Eff effs a
runReadlineHaskeline hd = interpret \env -> \case
  GetInputLine prompt -> do
    -- While haskeline reads a line the terminal is in raw mode and ctrl-c
    -- never raises SIGINT; it is only seen as an 'H.Interrupt' by
    -- haskeline's own (InputT-level) @withInterrupt@. Run under it inside
    -- the session, then rethrow in this thread so the effect-level
    -- 'HandleInterrupt' sees a uniform 'H.Interrupt'.
    result <-
      liftIO . HIO.queryInput hd $
        H.handleInterrupt (pure Nothing) $
          H.withInterrupt (Just <$> H.getInputLine prompt)
    case result of
      Nothing -> liftIO $ E.throwIO H.Interrupt
      Just line -> pure line
  OutputStr s -> liftIO $ HIO.queryInput hd (H.outputStr s)
  WithInterrupt m ->
    localSeqUnliftIO env \unlift -> withInterruptIO (unlift m)
  HandleInterrupt handler m ->
    localSeqUnliftIO env \unlift ->
      unlift m `E.catch` \H.Interrupt -> unlift handler

-- | Deliver ctrl-c as an 'H.Interrupt' exception to the current thread while
-- the action runs, like haskeline's @withInterrupt@ (which only works in
-- @InputT@).
withInterruptIO :: IO a -> IO a
withInterruptIO act = do
  tid <- myThreadId
  let interruptHandler = Signals.Catch (E.throwTo tid H.Interrupt)
      install = Signals.installHandler Signals.sigINT interruptHandler Nothing
      restore old = Signals.installHandler Signals.sigINT old Nothing
  E.bracket install restore (const act)

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
