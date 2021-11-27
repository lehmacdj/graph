{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TemplateHaskell #-}

-- | Interpreters for running various large stacks of effects that it might
-- make sense to use while using this library.
module Interpreters where

import Control.Arrow ((>>>))
import Control.Lens
import Effect.Console
import Effect.Editor
import Effect.FileTypeOracle
import Effect.Filesystem
import Effect.FreshNID
import Effect.Graph
import Effect.NodeLocated
import Effect.Time
import Effect.Util
import Effect.Warn
import Effect.Web
import Graph
import History
import MyPrelude hiding (Reader, ask)
import Polysemy.Embed
import Polysemy.Error hiding (throw)
import Polysemy.Input
import Polysemy.Output
import Polysemy.Readline
import Polysemy.State
import System.Console.Haskeline (InputT)
import qualified System.Console.Haskeline as H
import UserError

data Env = Env
  { _filePath :: IORef FilePath,
    -- | next id that is unique within the current graph
    _nextId :: IORef NID,
    _history :: IORef History,
    _isDualized :: IORef IsDual,
    _replSettings :: H.Settings IO
  }

makeLenses ''Env

initEnv ::
  FilePath ->
  -- | the next node id to use for generating fresh nodes
  NID ->
  H.Settings IO ->
  IO Env
initEnv graphDir nid _replSettings =
  Env
    <$> newIORef graphDir
    <*> newIORef nid
    <*> newIORef (History [] nilNID [])
    <*> newIORef (IsDual False)
    <*> pure _replSettings

runLocableHistoryState ::
  Member (State History) effs =>
  Sem (GetLocation : SetLocation : effs) ~> Sem effs
runLocableHistoryState = subsumeReaderState _now >>> runSetLocationHistoryState

runSetLocationHistoryState ::
  Member (State History) r => Sem (SetLocation : r) ~> Sem r
runSetLocationHistoryState = interpret $ \case
  Output nid -> modify @History (addToHistory nid)

errorOnNoInput ::
  Member (Error UserError) r =>
  String ->
  Sem (Error NoInputProvided : r) a ->
  Sem r a
errorOnNoInput msg v =
  v `handleError` \NoInputProvided -> UserError.throwString msg

printingErrorsAndWarnings ::
  Member (Embed IO) effs =>
  Sem (Warn UserError : Error UserError : effs) () ->
  Sem effs ()
printingErrorsAndWarnings = printWarnings >>> printErrors

type HasMainEffects effs =
  ( Members
      [ Console,
        Error UserError,
        SetLocation,
        GetLocation,
        FreshNID,
        Dualizeable,
        FileSystemTree,
        Web,
        Warn UserError,
        State History,
        Editor,
        GetTime,
        Embed IO,
        Readline,
        FileTypeOracle,
        RawGraph
      ]
      effs,
    HasGraph String effs
  )

-- | general function for interpreting the entire stack of effects in terms
-- of real world things
-- it takes a function that handles the errors, because that is necessary for
-- this to have an arbitrary return type
runMainEffectsIOWithErrorHandling ::
  forall a.
  ( forall effs.
    Members [Input Env, Embed IO] effs =>
    Sem (Warn UserError : Error UserError : effs) a ->
    Sem effs a
  ) ->
  Env ->
  (forall effs. HasMainEffects effs => Sem effs a) ->
  IO a
runMainEffectsIOWithErrorHandling errorHandlingBehavior env v = do
  let handler =
        applyInput2 (runWriteGraphDualizeableIO @String)
          >>> applyInput2 (runReadGraphDualizeableIO @String)
          >>> applyInput2 interpretEditorAsIOVimFSGraph
          >>> runRawGraphAsInput
          >>> contramapInputSem @FilePath (embed . readIORef . view filePath)
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
          >>> runReadlineFinal
          >>> runFileTypeOracle
          >>> withEffects @[Input Env, Embed IO, Embed (InputT IO), Final (InputT IO)]
          >>> runInputConst env
          >>> runEmbedded liftIO
          >>> withEffects @'[Embed (InputT IO), Final (InputT IO)]
          >>> embedToFinal @(InputT IO)
          >>> runFinal
          >>> H.runInputT (_replSettings env)
  handler v

-- | The existential in the type here is necessary to allow an arbitrary order
-- to be picked here + to allow other effects (such as Error NoInputProvided)
-- to automatically be raised into the the list of effects but not others
runMainEffectsIO ::
  Env ->
  (forall effs. HasMainEffects effs => Sem effs ()) ->
  IO ()
runMainEffectsIO = runMainEffectsIOWithErrorHandling printingErrorsAndWarnings
