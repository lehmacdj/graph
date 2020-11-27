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
import Polysemy.State
import UserError

type App = Repl Env

type AppBase = ReplBase Env

runLocableHistoryState ::
  Member (State History) effs =>
  Sem (GetLocation : SetLocation : effs) ~> Sem effs
runLocableHistoryState = subsumeReaderState _now >>> runSetLocationHistoryState

runSetLocationHistoryState ::
  Member (State History) r => Sem (SetLocation : r) ~> Sem r
runSetLocationHistoryState = interpret $ \case
  Output nid -> modify @History (addToHistory nid)

errorOnNoInput ::
  Member (Error UserErrors) r =>
  String ->
  Sem (Error NoInputProvided : r) a ->
  Sem r a
errorOnNoInput msg v =
  v `handleError` \NoInputProvided -> UserError.throwString msg

printingErrorsAndWarnings ::
  Member (Embed IO) effs =>
  Sem (Warn UserErrors : Error UserErrors : effs) () ->
  Sem effs ()
printingErrorsAndWarnings = printWarnings >>> printErrors

-- | general function for interpreting the entire stack of effects in terms
-- of real world things
-- it takes a function that handles the errors, because that is necessary for
-- this to have an arbitrary return type
runMainEffects ::
  forall a.
  ( forall effs.
    Members [Input Env, Embed IO] effs =>
    Sem (Warn UserErrors : Error UserErrors : effs) a ->
    Sem effs a
  ) ->
  ( forall effs.
    ( Members [Console, ThrowUserError, SetLocation, GetLocation, FreshNID, Dualizeable] effs,
      Members [FileSystemTree, Web, Warn UserErrors, State History] effs,
      Members [Editor, GetTime, Embed AppBase, Embed IO] effs,
      HasGraph String effs
    ) =>
    Sem effs a
  ) ->
  AppBase a
runMainEffects errorHandlingBehavior v = do
  let handler =
        applyMaybeInput2 (runWriteGraphDualizeableIO @String)
          >>> applyMaybeInput2 (runReadGraphDualizeableIO @String)
          >>> errorOnNoInput "there is no set filepath so we can't access the graph"
          >>> applyMaybeInput2 interpretEditorAsIOVimFSGraph
          >>> errorOnNoInput "doesn't have a filepath so can't start editor"
          >>> contramapInputSem @(Maybe FilePath) (embed . readIORef . view filePath)
          >>> runWebIO
          >>> runFileSystemTreeIO
          >>> runStateInputIORefOf @IsDual isDualized
          >>> interpretConsoleIO
          >>> interpretTimeAsIO
          >>> runLocableHistoryState
          >>> runStateInputIORefOf @History history
          >>> runFreshNIDState
          >>> runStateInputIORefOf nextId
          >>> errorHandlingBehavior
          >>> withEffects @[Input Env, Embed IO, Embed AppBase]
          >>> runInputMonadReader @AppBase
          >>> runEmbedded liftIO
          >>> runM
  handler v

-- | The existential in the type here is necessary to allow an arbitrary order
-- to be picked here + to allow other effects (such as Error NoInputProvided)
-- to automatically be raised into the the list of effects but not others
interpretAsAppBase ::
  ( forall effs.
    ( Members [Console, ThrowUserError, SetLocation, GetLocation, FreshNID, Dualizeable] effs,
      Members [FileSystemTree, Web, Warn UserErrors, State History] effs,
      Members [Editor, GetTime, Embed AppBase, Embed IO] effs,
      HasGraph String effs
    ) =>
    Sem effs ()
  ) ->
  AppBase ()
interpretAsAppBase = runMainEffects printingErrorsAndWarnings
