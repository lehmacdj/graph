-- | Core effect vocabulary for the app, built on effectful. Re-exports the
-- pieces of effectful that every module needs plus compatibility helpers
-- retained from the polysemy era.
module MyPrelude.Effect
  ( module X,
    module MyPrelude.Effect,
  )
where

import ClassyPrelude
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
