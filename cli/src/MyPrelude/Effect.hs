module MyPrelude.Effect
  ( module X,
    module MyPrelude.Effect,
  )
where

import ClassyPrelude
import Effectful as X
import Effectful.Error.Static
import Effectful.State.Static.Local

-- | The identity function
withEffects :: forall es a. Eff es a -> Eff es a
withEffects = id

-- | Handle error without allowing error to be present in resulting computation.
-- Added in migration from freer-simple to polysemy, soft deprecated, consider
-- finding a sufficient replacement in the near future
handleError :: Eff (Error e : es) a -> (e -> Eff es a) -> Eff es a
handleError action handler = do
  result <- runError action
  case result of
    Right x -> pure x
    Left e -> handler e

embedStateful :: forall s es a. (State s :> es) => (s -> (a, s)) -> Eff es a
embedStateful f = do
  s <- get
  let (result, newState) = f s
  put newState
  pure result

errorToLeft ::
  (Show e) => Eff (Error e : es) a -> Eff es (Either e a)
errorToLeft = (`handleError` pure . Left) . fmap Right

errorToNothing ::
  Eff (Error e : es) a -> Eff es (Maybe a)
errorToNothing = (`handleError` const (pure Nothing)) . fmap Just

untry :: Eff es (Either e a) -> Eff (Error e : es) a
untry x =
  raise x >>= \case
    Left e -> throwError e
    Right v -> pure v
