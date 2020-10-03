{-# LANGUAGE NoImplicitPrelude #-}

-- | Instantiates things with a lot of effects, evaluating them in the AppBase
-- monad.
module App where

import Control.Arrow ((>>>))
import Control.Lens
import Control.Monad.Freer.State
import Control.Monad.Freer.Writer
import Control.Repl
import Effect.Console
import Effect.Editor
import Effect.Filesystem
import Effect.FreshNID
import Effect.Graph
import Effect.Load
import Effect.NodeLocated
import Effect.Throw
import Effect.Time
import Effect.Util
import Effect.Warn
import Effect.Web
import Env
import History
import MyPrelude hiding (Reader, ask)
import System.Directory
import System.FilePath
import System.Random
import UserError

type App = Repl Env

type AppBase = ReplBase Env

runLocableHistoryState ::
  Member (State History) effs =>
  Eff (GetLocation : SetLocation : effs) ~> Eff effs
runLocableHistoryState = subsumeReaderState _now >>> runSetLocationHistoryState

-- | we have the extra State History parameter here to sidestep issues of
-- having to introduce another effect, want to move away from using this method
-- because it is messier than using the separate methods in most cases probably
runLocableAppBase ::
  LastMember AppBase effs =>
  Eff (GetLocation : SetLocation : State History : effs) ~> Eff effs
runLocableAppBase = runLocableHistoryState >>> runStateAppBaseIORef history
{-# DEPRECATED runLocableAppBase "use runLocableHistoryState" #-}

evalFreshAppBase ::
  LastMember AppBase effs =>
  Eff (FreshNID : effs) ~> Eff effs
evalFreshAppBase = interpret $ \case
  FreshNID -> sendM (liftIO randomIO)

runStateAppBaseIORef ::
  LastMember AppBase effs =>
  Lens' Env (IORef s) ->
  Eff (State s : effs) ~> Eff effs
runStateAppBaseIORef l = interpret $ \case
  Get -> sendM $ view l >>= readIORef
  Put x -> sendM $ modifyOf l (const x) >> pure ()

runDualizeableAppBase ::
  LastMember AppBase effs =>
  Eff (Dualizeable : effs) ~> Eff effs
runDualizeableAppBase = runStateAppBaseIORef isDualized

runLoadAppBase ::
  (LastMember AppBase effs, HasGraph String effs, Member (Writer NID) effs) =>
  Eff (Load : effs) ~> Eff effs
runLoadAppBase = interpret $ \case
  SetLoaded dir -> do
    sendM $ modifyOf filePath (const (Just dir)) >> pure ()
    linkFileNames <- liftIO $ filter (".json" `isSuffixOf`) <$> listDirectory dir
    let nids = mapMaybe (readMay . dropExtension) linkFileNames
    sendM $ modifyOf nextId (const (maximum (1 `ncons` nids) + 1)) >> pure ()

runReaderAppBaseIORef ::
  LastMember AppBase effs =>
  Lens' Env (IORef r) ->
  Eff (Reader r : effs) ~> Eff effs
runReaderAppBaseIORef l = runStateAppBaseIORef l . translate (\Ask -> Get)

runWriterAppBaseIORef ::
  LastMember AppBase effs =>
  Lens' Env (IORef r) ->
  Eff (Writer r : effs) ~> Eff effs
runWriterAppBaseIORef l = runStateAppBaseIORef l . translate (\(Tell x) -> Put x)

runEditorAppBase ::
  (LastMember AppBase effs, Member ThrowUserError effs) =>
  Eff (Editor : effs) ~> Eff effs
runEditorAppBase eff = do
  fp <- sendM $ view filePath >>= readIORef
  case fp of
    Nothing -> throw $ OtherError "doesn't have filepath so can't start editor"
    Just fp' -> interpretEditorAsIOVimFSGraph fp' eff

subsumeReaderState ::
  forall x i r. Member (State x) r => (x -> i) -> Eff (Reader i : r) ~> Eff r
subsumeReaderState getter = interpret $ \case
  Ask -> getter <$> get @x

runSetLocationHistoryState ::
  Member (State History) r => Eff (SetLocation : r) ~> Eff r
runSetLocationHistoryState = interpret $ \case
  Tell nid -> modify @History (addToHistory nid)

-- | The existential in the type here is necessary to allow an arbitrary order
-- to be picked here
interpretAsAppBase ::
  ( forall effs.
    ( Members [Console, ThrowUserError, SetLocation, GetLocation, FreshNID, Dualizeable] effs,
      Members [FileSystemTree, Web, Load, Error None, Warn UserErrors, State History] effs,
      Member Editor effs,
      Member GetTime effs,
      HasGraph String effs
    ) =>
    Eff effs ()
  ) ->
  AppBase ()
interpretAsAppBase v = do
  let handler =
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
          >>> runEditorAppBase
          >>> printErrors
          >>> interpretTimeAsIO
          >>> runLocableHistoryState
          >>> runStateAppBaseIORef history
          >>> evalFreshAppBase
          >>> runM
  handler v
