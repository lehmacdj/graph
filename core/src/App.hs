{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TemplateHaskell #-}

-- | Instantiates things with a lot of effects, evaluating them in the AppBase
-- monad.
module App where

import Control.Arrow ((>>>))
import Control.Lens
import Control.Monad.Catch
import Control.Monad.Reader.Class
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
import Graph
import History
import MyPrelude hiding (Reader, ask)
import Polysemy.Embed
import Polysemy.Error hiding (throw)
import Polysemy.Input
import Polysemy.MTL
import Polysemy.Output
import Polysemy.Readline
import Polysemy.State
import UserError

newtype AppM env a = AppM {unAppM :: ReaderT env IO a}
  deriving
    ( Functor,
      Monad,
      MonadReader env,
      MonadIO,
      Applicative,
      MonadThrow,
      MonadCatch,
      MonadMask
    )

runAppM :: env -> AppM env a -> IO a
runAppM env = (`runReaderT` env) . unAppM

type App = AppM Env

data Env = Env
  { _filePath :: IORef FilePath,
    -- | next id that is unique within the current graph
    _nextId :: IORef NID,
    _history :: IORef History,
    _isDualized :: IORef IsDual,
    _replSettings :: Settings App
  }

makeLenses ''Env

initEnv ::
  FilePath ->
  -- | the next node id to use for generating fresh nodes
  NID ->
  Settings App ->
  IO Env
initEnv graphDir nid settings =
  Env
    <$> newIORef graphDir
    <*> newIORef nid
    <*> newIORef (History [] nilNID [])
    <*> newIORef (IsDual False)
    <*> pure settings

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

-- | general function for interpreting the entire stack of effects in terms
-- of real world things
-- it takes a function that handles the errors, because that is necessary for
-- this to have an arbitrary return type
runMainEffects ::
  forall a.
  ( forall effs.
    Members [Input Env, Embed IO] effs =>
    Sem (Warn UserError : Error UserError : effs) a ->
    Sem effs a
  ) ->
  ( forall effs.
    ( Members [Console, Error UserError, SetLocation, GetLocation, FreshNID, Dualizeable] effs,
      Members [FileSystemTree, Web, Warn UserError, State History] effs,
      Members [Editor, GetTime, Embed App, Embed IO, Readline] effs,
      HasGraph String effs
    ) =>
    Sem effs a
  ) ->
  App a
runMainEffects errorHandlingBehavior v = do
  let handler =
        applyInput2 (runWriteGraphDualizeableIO @String)
          >>> applyInput2 (runReadGraphDualizeableIO @String)
          >>> applyInput2 interpretEditorAsIOVimFSGraph
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
          >>> applyInput2Of replSettings (runReadline @App)
          >>> withEffects @[Input Env, Embed IO, Embed App]
          >>> runInputMonadReader @App
          >>> runEmbedded liftIO
          >>> runM
  handler v

-- | The existential in the type here is necessary to allow an arbitrary order
-- to be picked here + to allow other effects (such as Error NoInputProvided)
-- to automatically be raised into the the list of effects but not others
interpretAsApp ::
  ( forall effs.
    ( Members [Console, Error UserError, SetLocation, GetLocation, FreshNID, Dualizeable] effs,
      Members [FileSystemTree, Web, Warn UserError, State History] effs,
      Members [Editor, GetTime, Embed App, Embed IO, Readline] effs,
      HasGraph String effs
    ) =>
    Sem effs ()
  ) ->
  App ()
interpretAsApp = runMainEffects printingErrorsAndWarnings
