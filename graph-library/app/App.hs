{-# LANGUAGE NoImplicitPrelude #-}

-- | Instantiates things with a lot of effects, evaluating them in the AppBase
-- monad.
module App where

import MyPrelude hiding (ask, Reader)

import Control.Lens
import Control.Repl
import Env

import Control.Monad.Freer.State

import Control.Monad.Freer.Fresh
import Control.Monad.Freer.Writer
import Effect.Graph
import Effect.Throw
import Effect.Warn
import Effect.Web
import Effect.Console
import Effect.Load
import Effect.NodeLocated
import Effect.Filesystem
import Effect.Util
import UserError
import Effect.Time

import Control.Arrow ((>>>))

import System.Directory
import System.FilePath
import Text.Read (readMaybe)

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
  :: (LastMember AppBase effs, HasGraph String effs, Member (Writer NID) effs)
  => Eff (Load : effs) ~> Eff effs
runLoadAppBase = interpret $ \case
  SetLoaded dir -> do
    sendM $ modifyOf filePath (const (Just dir)) >> pure ()
    linkFileNames <- liftIO $ filter (".json" `isSuffixOf`) <$> listDirectory dir
    let nids = mapMaybe (readMaybe . dropExtension) linkFileNames
    sendM $ modifyOf nextId (const (maximum (1 `ncons` nids) + 1)) >> pure ()

runReaderAppBaseIORef
  :: LastMember AppBase effs
  => Lens' Env (IORef r) -> Eff (Reader r : effs) ~> Eff effs
runReaderAppBaseIORef l = runStateAppBaseIORef l . translate (\Ask -> Get)

runWriterAppBaseIORef
  :: LastMember AppBase effs
  => Lens' Env (IORef r) -> Eff (Writer r : effs) ~> Eff effs
runWriterAppBaseIORef l = runStateAppBaseIORef l . translate (\(Tell x) -> Put x)

interpretAsAppBase
  ::
  (forall effs. -- ^ this is an extistential type
    ( Members [Console, ThrowUserError, SetLocation, GetLocation, Fresh, Dualizeable] effs
    , Members [FileSystemTree, Web, Load, Error None, Writer NID, Warn UserErrors] effs
    , Member GetTime effs
    , HasGraph String effs
    ) => Eff effs ())
  -> AppBase ()
interpretAsAppBase v = do
  let
    handler =
      runLoadAppBase
      >>> paramToReader . (flip (runReadGraphDualizeableIO @String))
      >>> readThrowMaybe
      >>> subsume
      >>> paramToReader . (flip (runWriteGraphDualizeableIO @String))
      >>> readThrowMaybe
      >>> subsume
      >>> (`handleError` (\None -> echo "there is no set filepath so we can't access the graph"))
      >>> runReaderAppBaseIORef filePath
      >>> runWebIO
      >>> runFileSystemTreeIO
      >>> runDualizeableAppBase
      >>> interpretConsoleIO
      >>> printWarnings @UserErrors
      >>> printErrors
      >>> interpretTimeAsIO
      >>> runLocableAppBase
      >>> evalFreshAppBase
      >>> runM
  handler v
