-- |
--   Interpreters for polysemy effects into MonadIO, MonadState, or MonadReader
module Polysemy.MTL where

import qualified Control.Monad.Reader as MTL
import qualified Control.Monad.State as MTL
import MyPrelude
import Polysemy.Input
import Polysemy.State

runStateMonadState ::
  forall m s r a.
  (MTL.MonadState s m, Member (Embed m) r) =>
  Sem (State s : r) a ->
  Sem r a
runStateMonadState = interpret $ \case
  Get -> embed MTL.get
  Put x -> embed (MTL.put x)

runInputMonadReader ::
  forall m i r a.
  (MTL.MonadReader i m, Member (Embed m) r) =>
  Sem (Input i : r) a ->
  Sem r a
runInputMonadReader = interpret $ \case
  Input -> embed MTL.ask
