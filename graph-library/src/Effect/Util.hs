{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE AllowAmbiguousTypes #-}

module Effect.Util where

import ClassyPrelude hiding (ask, Reader)

import Control.Monad.Freer
import Control.Monad.Freer.Reader
import Control.Monad.Freer.State
import Control.Monad.Freer.Error

-- | Crazy function for weird utility cases
paramToReader
  :: forall r a effs. (r -> Eff effs a) -> Eff (Reader r : effs) a
paramToReader f = do
  r <- ask
  raise $ f r

-- | Utility function to interpret a reader effect as a State effect, via a
-- the inclusion Reader < State.
runReaderAsState
  :: forall r effs a. Member (State r) effs
  => Eff (Reader r : effs) a -> Eff effs a
runReaderAsState = interpret $ \Ask -> get

-- unit for readThrowMaybe
data None = None

readThrowMaybe
  :: forall r effs. Member (Error None) effs
  => Eff (Reader r : effs) ~> Eff (Reader (Maybe r) : effs)
readThrowMaybe = reinterpret $
  \Ask -> do
    r <- ask @(Maybe r)
    case r of
      Nothing -> throwError None
      Just x -> pure x
