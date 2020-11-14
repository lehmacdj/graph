{-# LANGUAGE NoImplicitPrelude #-}

-- | Instantiates things with a lot of effects, evaluating them in the AppBase
-- monad.
module App where

import Control.Arrow ((>>>))
import Control.Lens
import Control.Repl
import Effect.Console
import Effect.Editor
import Effect.Filesystem
import Effect.FreshNID
import Effect.Graph
import Effect.Load
import Effect.NodeLocated
import Effect.Time
import Effect.Util
import Effect.Warn
import Effect.Web
import Env
import History
import MyPrelude hiding (Reader, ask)
import Polysemy.Embed
import Polysemy.Error hiding (throw)
import Polysemy.Input
import Polysemy.MTL
import Polysemy.Output
import Polysemy.Reader
import Polysemy.State
import System.Directory
import System.FilePath
import UserError

type App = Repl Env

type AppBase = ReplBase Env

runLocableHistoryState ::
  Member (State History) effs =>
  Sem (GetLocation : SetLocation : effs) ~> Sem effs
runLocableHistoryState = subsumeReaderState _now >>> runSetLocationHistoryState

-- | we have the extra State History parameter here to sidestep issues of
-- having to introduce another effect, want to move away from using this method
-- because it is messier than using the separate methods in most cases probably
runLocableAppBase ::
  Member (Embed AppBase) effs =>
  Sem (GetLocation : SetLocation : State History : effs) ~> Sem effs
runLocableAppBase = runLocableHistoryState >>> runStateAppBaseIORef history
{-# DEPRECATED runLocableAppBase "use runLocableHistoryState" #-}

runStateAppBaseIORef ::
  Member (Embed AppBase) effs =>
  Lens' Env (IORef s) ->
  Sem (State s : effs) ~> Sem effs
runStateAppBaseIORef l = interpret $ \case
  Get -> embed $ view l >>= readIORef
  Put x -> embed $ modifyOf l (const x) >> pure ()
{-# DEPRECATED runStateAppBaseIORef "use runStateIORef with Input Env" #-}

runLoadAppBase ::
  (Member (Embed AppBase) effs, HasGraph String effs, Member (Output NID) effs) =>
  Sem (Load : effs) ~> Sem effs
runLoadAppBase = interpret $ \case
  SetLoaded dir -> do
    embed $ modifyOf filePath (const (Just dir)) >> pure ()
    linkFileNames <- embed . liftIO $ filter (".json" `isSuffixOf`) <$> listDirectory dir
    let nids = mapMaybe (readMay . dropExtension) linkFileNames
    embed $ modifyOf nextId (const (maximum (1 `ncons` nids) + 1)) >> pure ()

subsumeReaderState ::
  forall x i r. Member (State x) r => (x -> i) -> Sem (Reader i : r) ~> Sem r
subsumeReaderState getter =
  interpret (\Input -> getter <$> get @x)
    . readerToInput
    . raiseUnder @(Input i)

runSetLocationHistoryState ::
  Member (State History) r => Sem (SetLocation : r) ~> Sem r
runSetLocationHistoryState = interpret $ \case
  Output nid -> modify @History (addToHistory nid)

warnOnNoInput ::
  Member (Warn UserErrors) r =>
  String ->
  Sem (Error NoInputProvided : r) () ->
  Sem r ()
warnOnNoInput msg v = v `handleError` \NoInputProvided -> warnString msg

-- | The existential in the type here is necessary to allow an arbitrary order
-- to be picked here + to allow other effects (such as Error NoInputProvided)
-- to automatically be raised into the the list of effects but not others
interpretAsAppBase ::
  ( forall effs.
    ( Members [Console, ThrowUserError, SetLocation, GetLocation, FreshNID, Dualizeable] effs,
      Members [FileSystemTree, Web, Load, Warn UserErrors, State History] effs,
      Members [Editor, GetTime, Embed AppBase, Embed IO] effs,
      HasGraph String effs
    ) =>
    Sem effs ()
  ) ->
  AppBase ()
interpretAsAppBase v = do
  let handler =
        runLoadAppBase
          >>> applyMaybeInput2 (runWriteGraphDualizeableIO @String)
          >>> applyMaybeInput2 (runReadGraphDualizeableIO @String)
          >>> warnOnNoInput "there is no set filepath so we can't access the graph"
          >>> applyMaybeInput2 interpretEditorAsIOVimFSGraph
          >>> warnOnNoInput "doesn't have a filepath so can't start editor"
          >>> contramapInputSem @(Maybe FilePath) (embed . readIORef . view filePath)
          >>> runWebIO
          >>> runFileSystemTreeIO
          >>> applyInput2Of isDualized runStateIORef
          >>> interpretConsoleIO
          >>> printWarnings @UserErrors
          >>> printErrors
          >>> interpretTimeAsIO
          >>> runLocableHistoryState
          >>> applyInput2Of history runStateIORef
          >>> runFreshNIDState
          >>> applyInput2Of nextId runStateIORef
          >>> withEffects @[Input Env, Embed IO, Embed AppBase]
          >>> runInputMonadReader @AppBase
          >>> runEmbedded liftIO
          >>> runM
  handler v
