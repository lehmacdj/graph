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

evalFreshAppBase ::
  Member (Embed AppBase) effs =>
  Sem (FreshNID : effs) ~> Sem effs
evalFreshAppBase = interpret $ \case
  FreshNID -> embed Env.freshNID

runStateAppBaseIORef ::
  Member (Embed AppBase) effs =>
  Lens' Env (IORef s) ->
  Sem (State s : effs) ~> Sem effs
runStateAppBaseIORef l = interpret $ \case
  Get -> embed $ view l >>= readIORef
  Put x -> embed $ modifyOf l (const x) >> pure ()
{-# DEPRECATED runStateAppBaseIORef "use runStateIORef with Input Env" #-}

runDualizeableAppBase ::
  Member (Embed AppBase) effs =>
  Sem (Dualizeable : effs) ~> Sem effs
runDualizeableAppBase = runStateAppBaseIORef isDualized

runLoadAppBase ::
  (Member (Embed AppBase) effs, HasGraph String effs, Member (Output NID) effs) =>
  Sem (Load : effs) ~> Sem effs
runLoadAppBase = interpret $ \case
  SetLoaded dir -> do
    embed $ modifyOf filePath (const (Just dir)) >> pure ()
    linkFileNames <- embed . liftIO $ filter (".json" `isSuffixOf`) <$> listDirectory dir
    let nids = mapMaybe (readMay . dropExtension) linkFileNames
    embed $ modifyOf nextId (const (maximum (1 `ncons` nids) + 1)) >> pure ()

runReaderAppBaseIORef ::
  forall r effs.
  Member (Embed AppBase) effs =>
  Lens' Env (IORef r) ->
  Sem (Reader r : effs) ~> Sem effs
runReaderAppBaseIORef l =
  runStateAppBaseIORef l
    . reinterpret (\Input -> get)
    . readerToInput
    . raiseUnder @(Input r)

runWriterAppBaseIORef ::
  Member (Embed AppBase) effs =>
  Lens' Env (IORef r) ->
  Sem (Output r : effs) ~> Sem effs
runWriterAppBaseIORef l = runStateAppBaseIORef l . reinterpret (\(Output x) -> put x)

runEditorAppBase ::
  (Members [Embed AppBase, ThrowUserError, Embed IO] effs) =>
  Sem (Editor : effs) ~> Sem effs
runEditorAppBase eff = do
  fp <- embed @AppBase $ view filePath >>= readIORef
  case fp of
    Nothing -> throw $ OtherError "doesn't have filepath so can't start editor"
    Just fp' -> interpretEditorAsIOVimFSGraph fp' eff

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
          >>> (`handleError` (\NoInputProvided -> echo "there is no set filepath so we can't access the graph"))
          >>> contramapInputSem @(Maybe FilePath) (embed . readIORef . view filePath)
          >>> runWebIO
          >>> runFileSystemTreeIO
          >>> runDualizeableAppBase
          >>> interpretConsoleIO
          >>> printWarnings @UserErrors
          >>> runEditorAppBase
          >>> printErrors
          >>> interpretTimeAsIO
          >>> runLocableHistoryState
          >>> applyInput2Of history runStateIORef
          >>> evalFreshAppBase
          >>> withEffects @[Input Env, Embed IO, Embed AppBase]
          >>> runInputMonadReader @AppBase
          >>> runEmbedded liftIO
          >>> runM
  handler v
