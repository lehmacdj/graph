-- | What stops a command from outside, as opposed to failing in it: an
-- asynchronous exception, the user's interrupt, or a signal that asks the
-- program to end. And catching the exceptions of an action into the 'Error'
-- effect, which leaves those alone.
--
-- This is apart from "MyPrelude.Effect" because Ctrl-C in the REPL is
-- haskeline's 'Haskeline.Interrupt', which only this module knows of: so
-- that "MyPrelude" does not import haskeline.
module Error.Interrupt
  ( fromException,
    fromExceptionVia,
    isAsyncOrInterrupt,
    EndedBySignal (..),
    isInterrupt,
  )
where

import Control.Exception qualified as E
import Foreign.C.Types (CInt)
import MyPrelude
import System.Console.Haskeline qualified as Haskeline

-- | Run an IO action, catching exceptions of type @e@ and rethrowing them
-- via the 'Error' effect. Matches polysemy's @fromException@.
fromException ::
  forall e effs a.
  (Exception e, Error e :> effs, IOE :> effs) =>
  IO a ->
  Eff effs a
fromException = fromExceptionVia @e id

-- | Like 'fromException' but map the exception before rethrowing it.
--
-- An exception that stops the action from outside ('isAsyncOrInterrupt') is
-- not caught, whatever type is asked for: it is not an error of the action.
fromExceptionVia ::
  forall exc err effs a.
  (Exception exc, Error err :> effs, IOE :> effs) =>
  (exc -> err) ->
  IO a ->
  Eff effs a
fromExceptionVia mapper action =
  liftIO (E.tryJust ofTheAction action) >>= \case
    Left e -> throwError (mapper e)
    Right x -> pure x
  where
    ofTheAction :: exc -> Maybe exc
    ofTheAction e
      | isAsyncOrInterrupt (E.toException e) = Nothing
      | otherwise = Just e

-- | Whether an exception stops a computation from outside instead of being
-- a failure of it: an asynchronous exception (an interrupt signal, the end
-- of a 'timeout', a killed thread), or the 'Haskeline.Interrupt' that Ctrl-C
-- is in the REPL. That one is thrown at the thread that runs a command like
-- the others, but haskeline does not declare it as asynchronous, so what
-- tells the two kinds apart by that (@catchAny@, @tryAny@) takes it for a
-- failure.
--
-- Such an exception must go on to whoever stops for it. What catches
-- exceptions to report them must leave these alone.
isAsyncOrInterrupt :: SomeException -> Bool
isAsyncOrInterrupt e =
  isJust (E.fromException @E.SomeAsyncException e)
    || isJust (E.fromException @Haskeline.Interrupt e)

-- | A signal that asks the program to end (SIGTERM, SIGHUP), as it arrives
-- in the main thread: with the number of the signal.
newtype EndedBySignal = EndedBySignal CInt
  deriving stock (Show)

-- | Asynchronous, like the interrupt that the runtime makes of SIGINT: what
-- reports the failures of a command leaves it alone ('isAsyncOrInterrupt').
instance Exception EndedBySignal where
  toException = E.asyncExceptionToException
  fromException = E.asyncExceptionFromException

-- | Whether an exception is what stops a command line on purpose, from
-- outside: the user's interrupt (Ctrl-C), which is the
-- 'Haskeline.Interrupt' in the REPL and the 'E.UserInterrupt' that the
-- runtime makes of an interrupt signal in @ge -e@, or a signal that asks
-- the program to end ('EndedBySignal').
isInterrupt :: SomeException -> Bool
isInterrupt e =
  isJust (E.fromException @Haskeline.Interrupt e)
    || E.fromException e == Just E.UserInterrupt
    || isJust (E.fromException @EndedBySignal e)
