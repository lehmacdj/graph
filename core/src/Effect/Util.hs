{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Effect.Util where

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

-- | Relaxes a reader such that it can work for a reader that only has maybe
-- TODO: add a more descriptive error than None
readThrowMaybe ::
  forall r effs a.
  Member (Error None) effs =>
  Sem (Input r : effs) a ->
  Sem (Input (Maybe r) : effs) a
readThrowMaybe = reinterpret $
  \Input -> do
    r <- input @(Maybe r)
    case r of
      Nothing -> throw None
      Just x -> pure x

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
