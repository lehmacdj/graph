-- | Core effect vocabulary for the app, built on effectful. Re-exports the
-- pieces of effectful that every module needs plus compatibility helpers
-- retained from the polysemy era.
module MyPrelude.Effect
  ( module X,
    module MyPrelude.Effect,
  )
where

import ClassyPrelude
import Control.Exception qualified as E
import Data.Kind (Constraint)
import Effectful as X
import Effectful.Dispatch.Dynamic as X
import Effectful.Error.Static as X
  ( Error,
    catchError,
    runErrorNoCallStack,
    throwError,
    tryError,
  )
import Effectful.State.Dynamic qualified as State
import Effectful.TH as X (makeEffect)
import Foreign.C.Types (CInt)
import System.Console.Haskeline qualified as Haskeline

-- | Constraint that all of the effects in the first list are present in the
-- second. Mirrors polysemy's @Members@; effectful only provides '(:>)'.
type Members :: [Effect] -> [Effect] -> Constraint
type family Members xs es where
  Members '[] es = ()
  Members (x ': xs) es = (x :> es, Members xs es)

-- | The identity function, used to pin down the effects of a computation
withEffects :: forall effs a. Eff effs a -> Eff effs a
withEffects = id

-- | Strict version of @modify@, which the dynamically dispatched State
-- effect doesn't provide.
modify' :: (State.State s :> effs) => (s -> s) -> Eff effs ()
modify' f = State.get >>= \s -> State.put $! f s

-- | Run an error, discarding the 'E.CallStack' effectful attaches to it.
-- Matches the semantics of polysemy's @runError@.
runError :: Eff (Error e : effs) a -> Eff effs (Either e a)
runError = runErrorNoCallStack

-- | Handle error without allowing error to be present in resulting
-- computation. Added in migration from freer-simple to polysemy, soft
-- deprecated, consider finding a sufficient replacement in the near future
handleError :: Eff (Error e : effs) a -> (e -> Eff effs a) -> Eff effs a
handleError action handler = do
  result <- runError action
  case result of
    Right x -> pure x
    Left e -> handler e

-- | Transform the error thrown by a computation.
mapError ::
  forall e1 e2 effs a.
  (Error e2 :> effs) =>
  (e1 -> e2) ->
  Eff (Error e1 : effs) a ->
  Eff effs a
mapError f action = action `handleError` (throwError . f)

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

errorToLeft ::
  (Show e) => Eff (Error e : effs) a -> Eff effs (Either e a)
errorToLeft = runError

errorToNothing ::
  Eff (Error e : effs) a -> Eff effs (Maybe a)
errorToNothing = (`handleError` const (pure Nothing)) . fmap Just

untry :: Eff r (Either e a) -> Eff (Error e : r) a
untry x =
  raise x >>= \case
    Left e -> throwError e
    Right v -> pure v
