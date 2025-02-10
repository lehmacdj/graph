-- | Interpreters for running various large stacks of effects that it might
-- make sense to use while using this library.
module Effect.Interpreters where

import Control.Arrow ((>>>))
import Control.Lens
import Effect.Console
import Effect.Editor
import Effect.FileSystem
import Effect.FileTypeOracle
import Effect.FreshNID
import Effect.NodeLocated
import Effect.Time
import Error.UserError
import Effect.Util
import Effect.Warn
import Effect.Web
import Graph.Effect
import Models.History
import Models.NID
import MyPrelude hiding (Reader, ask)
import Polysemy.Embed
import Polysemy.Input
import Polysemy.Output
import Polysemy.Reader
import Polysemy.Readline
import Polysemy.State
import System.Console.Haskeline (InputT)
import qualified System.Console.Haskeline as H
import System.Random
import Effect.RawGraph

data Env = Env
  { filePath :: IORef FilePath,
    -- | random generator used for creating new node ids
    randomGen :: IORef StdGen,
    history :: IORef History,
    isDualized :: IORef IsDual,
    replSettings :: H.Settings IO
  }
  deriving (Generic)

initEnv ::
  FilePath ->
  StdGen ->
  H.Settings IO ->
  IO Env
initEnv graphDir nidGenerator _replSettings =
  Env
    <$> newIORef graphDir
    <*> newIORef nidGenerator
    <*> newIORef (History [] nilNID [])
    <*> newIORef (IsDual False)
    <*> pure _replSettings

runLocableHistoryState ::
  Member (State History) effs =>
  Sem (GetLocation : SetLocation : effs) ~> Sem effs
runLocableHistoryState = subsumeReaderState (view #now) >>> runSetLocationHistoryState

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
  v `handleError` \NoInputProvided -> throwString msg

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
        FileSystem,
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

-- | general function for interpreting the entire stack of effects in terms of real world things
-- it takes a function that handles the errors, because that is necessary for
-- this to have an arbitrary return type
runMainEffectsIO ::
  forall a.
  ( forall effs.
    Members [Input Env, Embed IO] effs =>
    Sem (Warn UserError : Error UserError : effs) a ->
    Sem effs a
  ) ->
  (forall effs. (Member (Embed IO) effs) => Sem (GetTime : effs) ~> Sem effs) ->
  Env ->
  (forall effs. HasMainEffects effs => Sem effs a) ->
  IO a
runMainEffectsIO errorHandlingBehavior timeBehavior env v = do
  let handler =
        runFreshNIDRandom
          >>> applyInput2 (runWriteGraphDualizeableIO @String)
          >>> applyInput2 (runReadGraphDatalessDualizeableIO @String)
          >>> applyInput2 (runReadGraphDualizeableIO @String)
          >>> applyInput2 interpretEditorAsIOVimFSGraph
          >>> runRawGraphAsInput
          >>> contramapInputSem @FilePath (embed . readIORef . view #filePath)
          >>> runWebIO
          >>> runFileSystemIO
          >>> runStateInputIORefOf @IsDual #isDualized
          >>> interpretConsoleIO
          >>> timeBehavior
          >>> runLocableHistoryState
          >>> runStateInputIORefOf @History #history
          >>> runStateInputIORefOf #randomGen
          >>> errorHandlingBehavior
          >>> runReadlineFinal
          >>> runFileTypeOracle
          >>> withEffects @[Input Env, Embed IO, Embed (InputT IO), Final (InputT IO)]
          >>> runInputConst env
          >>> runEmbedded liftIO
          >>> withEffects @'[Embed (InputT IO), Final (InputT IO)]
          >>> embedToFinal @(InputT IO)
          >>> runFinal
          >>> H.runInputT (view #replSettings env)
  handler v

-- | Less capable, but less demanding interpreter.
runReadWriteGraphIO ::
  FilePath ->
  Sem
    [ WriteGraph String,
      ReadGraph String (Maybe ByteString),
      Warn UserError,
      Error UserError,
      FreshNID,
      Embed IO
    ]
    () ->
  IO ()
runReadWriteGraphIO dir =
  runWriteGraphIO dir
    >>> runReadGraphIO dir
    >>> printWarnings @UserError
    >>> printErrors
    >>> raiseUnder
    >>> runFreshNIDRandom
    >>> (\m -> initStdGen >>= \stdGen -> evalState stdGen m)
    >>> runM

runLocatedReadWriteGraphIO ::
  FilePath ->
  NID ->
  Sem
    [ GetLocation,
      WriteGraph String,
      ReadGraph String (Maybe ByteString),
      Warn UserError,
      Error UserError,
      FreshNID,
      Embed IO
    ]
    () ->
  IO ()
runLocatedReadWriteGraphIO base nid =
  runReader nid >>> runReadWriteGraphIO base
