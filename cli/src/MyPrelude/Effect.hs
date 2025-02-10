module MyPrelude.Effect
  ( module X,
    module MyPrelude.Effect,
  )
where

import ClassyPrelude
import Polysemy as X
import Polysemy.Error
import Polysemy.State

-- | The identity funciton
withEffects :: forall effs a. Sem effs a -> Sem effs a
withEffects = id

-- | Handle error without allowing error to be present in resulting computation.
-- Added in migration from freer-simple to polysemy, soft deprecated, consider
-- finding a sufficient replacement in the near future
handleError :: Sem (Error e : effs) a -> (e -> Sem effs a) -> Sem effs a
handleError action handler = do
  result <- runError action
  case result of
    Right x -> pure x
    Left e -> handler e

embedStateful :: forall s r a. Member (State s) r => (s -> (a, s)) -> Sem r a
embedStateful f = do
  state <- get
  let (result, newState) = f state
  put newState
  pure result

errorToLeft ::
  Show e => Sem (Error e : effs) a -> Sem effs (Either e a)
errorToLeft = (`handleError` pure . Left) . fmap Right

errorToNothing ::
  Sem (Error e : effs) a -> Sem effs (Maybe a)
errorToNothing = (`handleError` const (pure Nothing)) . fmap Just

untry :: Sem r (Either e a) -> Sem (Error e : r) a
untry x =
  raise x >>= \case
    Left e -> throw e
    Right v -> pure v
