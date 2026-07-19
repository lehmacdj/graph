{-# LANGUAGE AllowAmbiguousTypes #-}

-- | Specialized utilities for effectful that aren't general enough to include
-- in my prelude directly
module Utils.Effectful where

import Control.Lens
import Effect.Input
import Effectful.Reader.Dynamic (Reader (..))
import Effectful.Reader.Static qualified as SR
import Effectful.State.Dynamic
import MyPrelude hiding (Reader, ask)
import Utils.Testing

applyInput2Of ::
  forall i env effs a b.
  (Input env :> effs) =>
  Lens' env i ->
  (i -> b -> Eff effs a) ->
  b ->
  Eff effs a
applyInput2Of l f b = input @env >>= flip f b . view l

-- | Utility function to interpret an Input effect as a State effect, via the
-- inclusion Reader < State.
runReaderAsState ::
  forall r effs a.
  (State r :> effs) =>
  Eff (Input r : effs) a ->
  Eff effs a
runReaderAsState = interpret \_ -> \case Input -> get

-- | Interpret a 'Reader' in terms of an ambient 'State' effect, reading the
-- value through the provided getter. 'Local' is supported by tracking the
-- stack of modifier functions in a private static reader; it modifies the
-- value produced by 'Ask' without touching the underlying state.
subsumeReaderState ::
  forall x i r. (State x :> r) => (x -> i) -> Eff (Reader i : r) ~> Eff r
subsumeReaderState getter =
  reinterpret (SR.runReader (id :: i -> i)) \env -> \case
    Ask -> do
      modifier <- SR.ask @(i -> i)
      modifier . getter <$> get @x
    Local f m ->
      localSeqUnlift env \unlift -> SR.local (f .) (unlift m)

-- | Run a 'State' effect backed by an 'IORef'.
runStateIORef ::
  (IOE :> r) =>
  IORef s ->
  Eff (State s : r) a ->
  Eff r a
runStateIORef ref = interpret \env -> \case
  Get -> readIORef ref
  Put s -> writeIORef ref s
  State f -> do
    s <- readIORef ref
    let (a, s') = f s
    writeIORef ref s'
    pure a
  StateM f -> localSeqUnlift env \unlift -> do
    s <- readIORef ref
    (a, s') <- unlift (f s)
    writeIORef ref s'
    pure a

runStateInputIORefOf ::
  forall s env r a.
  (Members [Input env, IOE] r) =>
  Lens' env (IORef s) ->
  Eff (State s : r) a ->
  Eff r a
runStateInputIORefOf l = applyInput2Of l runStateIORef

-- | even after modifying the state variable used to initialize the Input
-- the input should be the original value; i.e. the initialization action
-- should not be called multiple times
unit_runInputConstSem_alwaysSame :: Assertion
unit_runInputConstSem_alwaysSame =
  (runPureEff . runStateLocal 0 . runInputConstSem initAction) action
    @=? (0, 0)
  where
    initAction :: (State Int :> r) => Eff r Int
    initAction = get @Int
    action :: (Members [State Int, Input Int] r) => Eff r Int
    action = do
      orig <- input @Int
      modify @Int (+ 1)
      newInput <- input @Int
      put @Int newInput
      pure orig
