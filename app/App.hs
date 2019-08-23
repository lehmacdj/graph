{-# LANGUAGE NoImplicitPrelude #-}

-- | Instantiates things with a lot of effects, evaluating them in the AppBase
-- monad.
module App where

import ClassyPrelude

import Control.Lens
import Control.Repl
import Env

import Control.Monad.Freer.State

import Control.Monad.Freer
import Control.Monad.Freer.Fresh
import Effect.Graph
import Effect.Throw
import Effect.Web
import Effect.Console
import Effect.Load
import Effect.NodeLocated
import Effect.Filesystem

import Control.Arrow ((>>>))

type App = Repl Env
type AppBase = ReplBase Env

runLocableAppBase
  :: LastMember AppBase effs
  => Eff (GetLocation : SetLocation : effs) ~> Eff effs
runLocableAppBase = interpret setLocHandler . interpret getLocHandler where
  setLocHandler :: LastMember AppBase effs => SetLocation x -> Eff effs x
  setLocHandler (Tell x) = sendM $ modifyOf currentNID (const x) >> pure ()
  getLocHandler :: LastMember AppBase effs => GetLocation x -> Eff effs x
  getLocHandler Ask = sendM $ view currentNID >>= readIORef

evalFreshAppBase
  :: LastMember AppBase effs
  => Eff (Fresh : effs) ~> Eff effs
evalFreshAppBase = interpret $ \case
  Fresh -> sendM freshNID

runStateAppBaseIORef
  :: LastMember AppBase effs
  => Lens' Env (IORef s) -> Eff (State s : effs) ~> Eff effs
runStateAppBaseIORef l = interpret $ \case
  Get -> sendM $ view l >>= readIORef
  Put x -> sendM $ modifyOf l (const x) >> pure ()

runDualizeableAppBase
  :: LastMember AppBase effs
  => Eff (Dualizeable : effs) ~> Eff effs
runDualizeableAppBase = runStateAppBaseIORef isDualized

runLoadAppBase
  :: LastMember AppBase effs
  => Eff (Load : effs) ~> Eff effs
runLoadAppBase = interpret $ \case
  SetLoaded s -> sendM $ modifyOf filePath (const (Just s)) >> pure ()

interpretAsAppBase
  ::
  (forall effs. -- ^ this is an extistential type
    ( Members [Console, Throw, SetLocation, GetLocation, Fresh, Dualizeable] effs
    , Members [FileSystemTree, Web, Load] effs
    , HasGraph String effs
    ) => Eff effs ())
  -> AppBase ()
interpretAsAppBase v = do
  fp <- view filePath >>= readIORef
  case fp of
    Nothing -> liftIO $ putStrErr "no filepath means we can't access the graph!"
    Just fp' -> do
      let
        handler =
          runReadGraphDualizeableIO @String fp'
          >>> runWriteGraphDualizeableIO @String fp'
          >>> runWebIO
          >>> runFileSystemTreeIO
          >>> interpretConsoleIO
          >>> runDualizeableAppBase
          >>> printErrors
          >>> runLocableAppBase
          >>> runLoadAppBase
          >>> evalFreshAppBase
          >>> runM
      handler v
