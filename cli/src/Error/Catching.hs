-- | Catching the exceptions of IO actions as user errors. This is apart from
-- "Error.UserError" because it knows the exceptions of the native
-- libraries (HTTP, file coordination) and leaves alone the interrupts of the
-- REPL ("Error.Interrupt", which imports haskeline): so that the type of user
-- errors, and what is built on it, imports none of them.
module Error.Catching
  ( embedCatchingErrors,
    mapExceptionToUserError,
    exceptionAsUserError,
    spec_embedCatchingErrors,
  )
where

import Control.Concurrent (myThreadId, threadDelay)
import Control.Exception qualified as E
import Error.FileCoordination (NSErrorException (..))
import Error.Interrupt
import Error.UserError
import MyPrelude
import Network.HTTP.Conduit (HttpException)
import System.Console.Haskeline qualified as Haskeline
import Utils.Testing.External

-- | capture errors and convert them to an error
--
-- Only what the action itself threw. An interrupt, the end of a 'timeout'
-- or the killing of the thread that arrives while the action runs passes
-- through as it is ('fromExceptionVia'): a command that is stopped while it
-- reads a file is stopped like any other, and is not reported as a command
-- that failed.
embedCatchingErrors ::
  (Members [Error UserError, IOE] r) => IO a -> Eff r a
embedCatchingErrors = fromExceptionVia mapExceptionToUserError

-- | The user error that an exception stands for
mapExceptionToUserError :: SomeException -> UserError
mapExceptionToUserError = \case
  (E.fromException @IOError -> Just e) -> IOFail e
  e@(E.fromException @HttpException -> Just _) -> WebError e
  (E.fromException @NSErrorException -> Just e) -> FileCoordinationError e
  e -> OtherException e

-- | Like 'mapExceptionToUserError', for an exception that may be a user
-- error thrown as one (as an unlifted action throws it), which stays as it is
exceptionAsUserError :: SomeException -> UserError
exceptionAsUserError = \case
  (E.fromException @UserError -> Just e) -> e
  e -> mapExceptionToUserError e

spec_embedCatchingErrors :: Spec
spec_embedCatchingErrors = do
  let embedded :: IO () -> IO (Either UserError ())
      embedded = runEff . runErrorNoCallStack @UserError . embedCatchingErrors
      -- as Ctrl-C, a signal or a time-out arrive: thrown at the thread
      -- while it runs the action
      arriving :: (Exception e) => e -> IO ()
      arriving e = myThreadId >>= (`E.throwTo` e)
  it "makes a user error of an exception of the action" do
    Left (IOFail e) <- embedded (throwIO (userError "disk on fire"))
    e `shouldBe` userError "disk on fire"
  it "makes a user error of an error call" do
    Left (OtherException e) <- embedded (error "broken invariant")
    displayException e `shouldContain` "broken invariant"
  it "passes on the Ctrl-C of the REPL" $
    embedded (arriving Haskeline.Interrupt)
      `shouldThrow` \Haskeline.Interrupt -> True
  it "passes on an interrupt signal" $
    embedded (arriving E.UserInterrupt) `shouldThrow` (== E.UserInterrupt)
  it "passes on the killing of the thread" $
    embedded (arriving E.ThreadKilled) `shouldThrow` (== E.ThreadKilled)
  it "is ended by a time-out" do
    -- the action would take a minute
    result <- timeout 10000 (embedded (threadDelay 60000000))
    isNothing result `shouldBe` True
