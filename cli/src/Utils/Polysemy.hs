{-# LANGUAGE AllowAmbiguousTypes #-}

-- | Specialized utilities for Polysemy that aren't general enough to include
-- in my prelude directly
module Utils.Polysemy where

import Control.Lens
import Control.Monad.Reader qualified as MTL
import Control.Monad.State qualified as MTL
import MyPrelude hiding (Reader, ask, fromEither)
import Polysemy.Error
import Polysemy.Input
import Polysemy.Reader
import Polysemy.State
import Utils.Testing

data NoInputProvided = NoInputProvided
  deriving (Show, Eq, Ord)

applyInput ::
  forall i r a. (Member (Input i) r) => (i -> Sem r a) -> Sem r a
applyInput f = input @i >>= f

applyInput2 ::
  forall i r a b. (Member (Input i) r) => (i -> b -> Sem r a) -> b -> Sem r a
applyInput2 f b = input @i >>= flip f b

applyMaybeInput ::
  forall i r a.
  (Members [Input (Maybe i), Error NoInputProvided] r) =>
  (i -> Sem r a) ->
  Sem r a
applyMaybeInput f =
  mapError (\None -> NoInputProvided)
    . subsume @(Input (Maybe i))
    . inputFromJust
    . raiseUnder @(Error None)
    . applyInput
    $ raise @(Input i)
      . f

applyMaybeInput2 ::
  forall i r a b.
  (Members [Input (Maybe i), Error NoInputProvided] r) =>
  (i -> b -> Sem r a) ->
  b ->
  Sem r a
applyMaybeInput2 f b =
  mapError (\None -> NoInputProvided)
    . subsume @(Input (Maybe i))
    . inputFromJust
    . raiseUnder @(Error None)
    . applyInput
    $ raise @(Input i)
      . flip f b

applyInputOf ::
  forall i env r a.
  (Member (Input env) r) =>
  Lens' env i ->
  (i -> Sem r a) ->
  Sem r a
applyInputOf l f = input @env >>= f . view l

applyInput2Of ::
  forall i env r a b.
  (Member (Input env) r) =>
  Lens' env i ->
  (i -> b -> Sem r a) ->
  b ->
  Sem r a
applyInput2Of l f b = input @env >>= flip f b . view l

applyMaybeInputOf ::
  forall i env r a.
  (Members [Input (Maybe env), Error NoInputProvided] r) =>
  Lens' env i ->
  (i -> Sem r a) ->
  Sem r a
applyMaybeInputOf l f =
  mapError (\None -> NoInputProvided)
    . subsume @(Input (Maybe env))
    . inputFromJust
    . raiseUnder @(Error None)
    . applyInputOf l
    $ raise @(Input env)
      . f

applyMaybeInput2Of ::
  forall i env r a b.
  (Members [Input (Maybe env), Error NoInputProvided] r) =>
  Lens' env i ->
  (i -> b -> Sem r a) ->
  b ->
  Sem r a
applyMaybeInput2Of l f b =
  mapError (\None -> NoInputProvided)
    . subsume @(Input (Maybe env))
    . inputFromJust
    . raiseUnder @(Error None)
    . applyInputOf l
    $ raise @(Input env)
      . flip f b

-- | Utility function to interpret a reader effect as a State effect, via a
-- the inclusion Reader < State.
runReaderAsState ::
  forall r effs a.
  (Member (State r) effs) =>
  Sem (Input r : effs) a ->
  Sem effs a
runReaderAsState = interpret $ \Input -> get

-- | unit for readThrowMaybe
data None = None

-- | Relaxes an input such that it can work for a input that only has maybe
-- TODO: switch to using a more descriptive name if I can think of a good one
inputFromJust ::
  forall r effs a.
  (Member (Error None) effs) =>
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
  (Member (Error None) effs) =>
  Sem (Input r : effs) a ->
  Sem (Input (Maybe r) : effs) a
readThrowMaybe = inputFromJust
{-# DEPRECATED readThrowMaybe "use inputFromJust" #-}

readerToInput ::
  forall x r a.
  (Member (Input x) r) =>
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

-- | Map an `Input` contravariantly.
-- taken from: polysemy-extra-0.1.1.0
contramapInput ::
  forall i i' r a.
  (Members '[Input i'] r) =>
  (i' -> i) ->
  Sem (Input i ': r) a ->
  Sem r a
contramapInput f = interpret $ \case
  Input -> f <$> input @i'

-- | Map an `Input` contravariantly through a monadic function.
-- taken from: polysemy-extra-0.1.1.0
contramapInputSem ::
  forall i i' r a.
  (Members '[Input i'] r) =>
  (i' -> Sem r i) ->
  Sem (Input i ': r) a ->
  Sem r a
contramapInputSem f = interpret $ \case
  Input -> f =<< input @i'

supplyInputVia ::
  forall i r.
  Sem r i ->
  Sem (Input i : r) ~> Sem r
supplyInputVia supplier = interpret $ \Input -> supplier

-- | Modify state returning the original value
modifying :: forall s r. (Member (State s) r) => (s -> s) -> Sem r s
modifying f = do
  v <- get
  modify f
  pure v

subsumeReaderState ::
  forall x i r. (Member (State x) r) => (x -> i) -> Sem (Reader i : r) ~> Sem r
subsumeReaderState getter =
  interpret (\Input -> getter <$> get @x)
    . readerToInput
    . raiseUnder @(Input i)

runStateInputIORef ::
  (Members [Input (IORef s), Embed IO] r) =>
  Sem (State s : r) a ->
  Sem r a
runStateInputIORef = applyInput2 runStateIORef

runStateInputIORefOf ::
  forall s env r a.
  (Members [Input env, Embed IO] r) =>
  Lens' env (IORef s) ->
  Sem (State s : r) a ->
  Sem r a
runStateInputIORefOf l = applyInput2Of l runStateIORef

-- | Run an 'Input' effect by always returning the value returned by a monadic
-- action once. This is useful for initializing an input in terms of a State
-- effect for example:
-- prop> runState 0 . runInputConstSem (get @Int) = runState 0 . runInputConst 0
runInputConstSem :: Sem r x -> Sem (Input x : r) a -> Sem r a
runInputConstSem initializationAction action =
  initializationAction >>= \val -> runInputConst val action

-- | even after modifying the state variable used to initialize the Input
-- the input should be the original value; i.e. the initialization action
-- should not be called multiple times
unit_runInputConstSem_alwaysSame :: Assertion
unit_runInputConstSem_alwaysSame =
  (run . runState 0 . runInputConstSem initAction) action @=? (0, 0)
  where
    initAction :: (Member (State Int) r) => Sem r Int
    initAction = get @Int
    action :: (Members [State Int, Input Int] r) => Sem r Int
    action = do
      orig <- input @Int
      modify @Int (+ 1)
      newInput <- input @Int
      put @Int newInput
      pure orig

-- | Run an error ignoring any result that fails and returning Nothing,
-- returning Just only if the result is successful.
runErrorMaybe :: Sem (Error e : r) a -> Sem r (Maybe a)
runErrorMaybe = fmap forgetLeft . runError

fromEitherSem ::
  (Member (Error e) r) => Sem r (Either e a) -> Sem r a
fromEitherSem = (>>= fromEither)

fromEitherSemVia ::
  (Member (Error err) r) => (e -> err) -> Sem r (Either e a) -> Sem r a
fromEitherSemVia mapper = fromEitherSem . fmap (first mapper)

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
