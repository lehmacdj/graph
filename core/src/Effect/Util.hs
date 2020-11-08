{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Effect.Util where

import Control.Lens
import MyPrelude hiding (Reader, ask)
import Polysemy.Error
import Polysemy.Input
import Polysemy.Reader
import Polysemy.State

-- | Crazy function for weird utility cases
paramToInput ::
  forall r a effs. (r -> Sem effs a) -> Sem (Input r : effs) a
paramToInput f = do
  r <- input @r
  raise $ f r

data NoInputProvided = NoInputProvided
  deriving (Show, Eq, Ord)

applyInput ::
  forall i r a. Member (Input i) r => (i -> Sem r a) -> Sem r a
applyInput f = input @i >>= f

applyMaybeInput ::
  forall i r a.
  Members [Input (Maybe i), Error NoInputProvided] r =>
  (i -> Sem r a) ->
  Sem r a
applyMaybeInput f =
  mapError (\None -> NoInputProvided)
    . subsume @(Input (Maybe i))
    . inputFromJust
    . raiseUnder @(Error None)
    . applyInput
    $ raise @(Input i) . f

applyInputOf ::
  forall i env r a.
  Member (Input env) r =>
  Lens' env i ->
  (i -> Sem r a) ->
  Sem r a
applyInputOf l f = input @env >>= f . view l

applyMaybeInputOf ::
  forall i env r a.
  Members [Input (Maybe env), Error NoInputProvided] r =>
  Lens' env i ->
  (i -> Sem r a) ->
  Sem r a
applyMaybeInputOf _ _ = undefined

-- | Utility function to interpret a reader effect as a State effect, via a
-- the inclusion Reader < State.
runReaderAsState ::
  forall r effs a.
  Member (State r) effs =>
  Sem (Input r : effs) a ->
  Sem effs a
runReaderAsState = interpret $ \Input -> get

-- | unit for readThrowMaybe
data None = None

-- | Relaxes an input such that it can work for a input that only has maybe
-- TODO: switch to using a more descriptive name if I can think of a good one
inputFromJust ::
  forall r effs a.
  Member (Error None) effs =>
  Sem (Input r : effs) a ->
  Sem (Input (Maybe r) : effs) a
inputFromJust = reinterpret $
  \Input -> do
    r <- input @(Maybe r)
    case r of
      Nothing -> throw None
      Just x -> pure x

readThrowMaybe ::
  forall r effs a.
  Member (Error None) effs =>
  Sem (Input r : effs) a ->
  Sem (Input (Maybe r) : effs) a
readThrowMaybe = inputFromJust
{-# DEPRECATED readThrowMaybe "use inputFromJust" #-}

readerToInput ::
  forall x r a.
  Member (Input x) r =>
  Sem (Reader x : r) a ->
  Sem r a
readerToInput = go id
  where
    go :: forall a'. (x -> x) -> Sem (Reader x : r) a' -> Sem r a'
    go modifier = interpretH $ \case
      Ask -> do
        value <- modifier <$> input @x
        pureT value
      Local f m -> do
        m' <- runT m
        raise $ go (f . modifier) m'
