{-# LANGUAGE AllowAmbiguousTypes #-}

-- | Specialized utilities for Effectful that aren't general enough to include
-- in my prelude directly
module Utils.Polysemy where

import Control.Lens
import Control.Monad.Reader qualified as MTL
import Control.Monad.State qualified as MTL
import MyPrelude hiding (Reader, ask, fromEither)
import Effectful.Error.Static
import Effectful.Reader.Static
import Effectful.State.Static.Local
import Effectful (IOE)
import Utils.Testing

-- Note: In effectful, Input is typically replaced with Reader
-- We use Reader where polysemy used Input

data NoInputProvided = NoInputProvided
  deriving (Show, Eq, Ord)

applyInput ::
  forall i es a. (Reader i :> es) => (i -> Eff es a) -> Eff es a
applyInput f = ask @i >>= f

applyInput2 ::
  forall i es a b. (Reader i :> es) => (i -> b -> Eff es a) -> b -> Eff es a
applyInput2 f b = ask @i >>= flip f b

applyMaybeInput ::
  forall i es a.
  (Reader (Maybe i) :> es, Error NoInputProvided :> es) =>
  (i -> Eff es a) ->
  Eff es a
applyMaybeInput f = do
  maybeVal <- ask @(Maybe i)
  case maybeVal of
    Nothing -> throwError NoInputProvided
    Just val -> f val

applyMaybeInput2 ::
  forall i es a b.
  (Reader (Maybe i) :> es, Error NoInputProvided :> es) =>
  (i -> b -> Eff es a) ->
  b ->
  Eff es a
applyMaybeInput2 f b = do
  maybeVal <- ask @(Maybe i)
  case maybeVal of
    Nothing -> throwError NoInputProvided
    Just val -> f val b

applyInputOf ::
  forall i env es a.
  (Reader env :> es) =>
  Lens' env i ->
  (i -> Eff es a) ->
  Eff es a
applyInputOf l f = ask @env >>= f . view l

applyInput2Of ::
  forall i env es a b.
  (Reader env :> es) =>
  Lens' env i ->
  (i -> b -> Eff es a) ->
  b ->
  Eff es a
applyInput2Of l f b = ask @env >>= flip f b . view l

applyMaybeInputOf ::
  forall i env es a.
  (Reader (Maybe env) :> es, Error NoInputProvided :> es) =>
  Lens' env i ->
  (i -> Eff es a) ->
  Eff es a
applyMaybeInputOf l f = do
  maybeEnv <- ask @(Maybe env)
  case maybeEnv of
    Nothing -> throwError NoInputProvided
    Just env -> f (view l env)

applyMaybeInput2Of ::
  forall i env es a b.
  (Reader (Maybe env) :> es, Error NoInputProvided :> es) =>
  Lens' env i ->
  (i -> b -> Eff es a) ->
  b ->
  Eff es a
applyMaybeInput2Of l f b = do
  maybeEnv <- ask @(Maybe env)
  case maybeEnv of
    Nothing -> throwError NoInputProvided
    Just env -> f (view l env) b

-- | Utility function to interpret a reader effect as a State effect, via a
-- the inclusion Reader < State.
runReaderAsState ::
  forall r es a.
  (State r :> es) =>
  Eff (Reader r : es) a ->
  Eff es a
runReaderAsState = runReader =<< get

-- | unit for readThrowMaybe
data None = None

-- | Relaxes an input such that it can work for a input that only has maybe
-- TODO: switch to using a more descriptive name if I can think of a good one
inputFromJust ::
  forall r es a.
  (Error None :> es) =>
  Eff (Reader r : es) a ->
  Eff (Reader (Maybe r) : es) a
inputFromJust action = do
  maybeR <- ask @(Maybe r)
  case maybeR of
    Nothing -> throwError None
    Just r -> runReader r action

readThrowMaybe ::
  forall r es a.
  (Error None :> es) =>
  Eff (Reader r : es) a ->
  Eff (Reader (Maybe r) : es) a
readThrowMaybe = inputFromJust
{-# DEPRECATED readThrowMaybe "use inputFromJust" #-}

readerToInput ::
  forall x es a.
  (Reader x :> es) =>
  Eff (Reader x : es) a ->
  Eff es a
readerToInput action = do
  value <- ask @x
  runReader value action

-- | Map a `Reader` contravariantly.
contramapInput ::
  forall i i' es a.
  (Reader i' :> es) =>
  (i' -> i) ->
  Eff (Reader i : es) a ->
  Eff es a
contramapInput f action = do
  i' <- ask @i'
  runReader (f i') action

-- | Map a `Reader` contravariantly through a monadic function.
contramapInputSem ::
  forall i i' es a.
  (Reader i' :> es) =>
  (i' -> Eff es i) ->
  Eff (Reader i : es) a ->
  Eff es a
contramapInputSem f action = do
  i' <- ask @i'
  i <- f i'
  runReader i action

supplyInputVia ::
  forall i es.
  Eff es i ->
  Eff (Reader i : es) ~> Eff es
supplyInputVia supplier action = do
  val <- supplier
  runReader val action

-- | Modify state returning the original value
modifying :: forall s es. (State s :> es) => (s -> s) -> Eff es s
modifying f = do
  v <- get
  modify f
  pure v

subsumeReaderState ::
  forall x i es. (State x :> es) => (x -> i) -> Eff (Reader i : es) ~> Eff es
subsumeReaderState getter action = do
  x <- get @x
  runReader (getter x) action

runStateInputIORef ::
  (Reader (IORef s) :> es, IOE :> es) =>
  Eff (State s : es) a ->
  Eff es a
runStateInputIORef action = do
  ref <- ask @(IORef s)
  runStateIORef ref action

runStateInputIORefOf ::
  forall s env es a.
  (Reader env :> es, IOE :> es) =>
  Lens' env (IORef s) ->
  Eff (State s : es) a ->
  Eff es a
runStateInputIORefOf l action = do
  env <- ask @env
  runStateIORef (view l env) action

-- | Run a 'Reader' effect by always returning the value returned by a monadic
-- action once. This is useful for initializing a reader in terms of a State
-- effect for example:
runInputConstSem :: Eff es x -> Eff (Reader x : es) a -> Eff es a
runInputConstSem initializationAction action = do
  val <- initializationAction
  runReader val action

-- | even after modifying the state variable used to initialize the Reader
-- the reader should be the original value; i.e. the initialization action
-- should not be called multiple times
unit_runInputConstSem_alwaysSame :: Assertion
unit_runInputConstSem_alwaysSame =
  (runPureEff . evalState 0 . runInputConstSem initAction) action @=? 0
  where
    initAction :: (State Int :> es) => Eff es Int
    initAction = get @Int
    action :: (State Int :> es, Reader Int :> es) => Eff es Int
    action = do
      orig <- ask @Int
      modify @Int (+ 1)
      newInput <- ask @Int
      put @Int newInput
      pure orig

-- | Run an error ignoring any result that fails and returning Nothing,
-- returning Just only if the result is successful.
runErrorMaybe :: Eff (Error e : es) a -> Eff es (Maybe a)
runErrorMaybe = fmap forgetLeft . runError

fromEitherSem ::
  (Error e :> es) => Eff es (Either e a) -> Eff es a
fromEitherSem = (>>= fromEither)

fromEitherSemVia ::
  (Error err :> es) => (e -> err) -> Eff es (Either e a) -> Eff es a
fromEitherSemVia mapper = fromEitherSem . fmap (first mapper)

runStateMonadState ::
  forall m s es a.
  (MTL.MonadState s m, IOE :> es) =>
  Eff (State s : es) a ->
  Eff es a
runStateMonadState action = do
  initialState <- unsafeEff_ MTL.get
  (result, finalState) <- runState initialState action
  unsafeEff_ (MTL.put finalState)
  pure result

runInputMonadReader ::
  forall m i es a.
  (MTL.MonadReader i m, IOE :> es) =>
  Eff (Reader i : es) a ->
  Eff es a
runInputMonadReader action = do
  i <- unsafeEff_ MTL.ask
  runReader i action
