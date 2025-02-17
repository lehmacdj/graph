-- | Interpreters for running various large stacks of effects that it might
-- make sense to use while using this library.
module Effect.Interpreters where

import Control.Arrow ((>>>))
import Control.Lens
import Effect.IOWrapper.DisplayImage
import Effect.IOWrapper.Editor
import Effect.IOWrapper.Echo
import Effect.IOWrapper.FileSystem
import Effect.IOWrapper.FileTypeOracle
import Effect.FreshNID
import Effect.NodeLocated
import Effect.IOWrapper.GetTime
import Error.UserError
import Effect.Util
import Effect.Warn
import Effect.IOWrapper.Web
import Graph.Effect
import Models.History
import Models.NID
import MyPrelude
import Polysemy.Embed
import Polysemy.Input
import Polysemy.Output
import Polysemy.Readline
import Polysemy.State
import System.Console.Haskeline (InputT)
import qualified System.Console.Haskeline as H
import System.Random
import DAL.RawGraph
import DAL.FileSystemOperations.Metadata
import DAL.FileSystemOperations.MetadataWriteDiff
import DAL.FileSystemOperations.Data
import Graph.GraphMetadataEditing
import Polysemy.Scoped

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

type family Concat (a :: [k]) (b :: [k]) :: [k] where
  Concat '[] b = b
  Concat (a ': as) b = a ': Concat as b

type GraphEditorEffects :: [Effect]
type GraphEditorEffects =
  [ FreshNID,
    ReadGraph String (Maybe ByteString),
    ReadGraphDataless String,
    WriteGraph String,
    Scoped_ (GraphMetadataEditing Text),
    -- GraphDataEditing,
    Dualizeable,
    GetLocation,
    SetLocation,
    State History
  ]

runGraphEditorEffects ::
  ( Members PermissiveDependencyEffects r,
    Members IOWrapperEffects r,
    Members ErrorEffects r
  ) => Sem (Concat GraphEditorEffects r) ~> Sem r
runGraphEditorEffects =
  raiseUnder @(State StdGen)
    >>> runFreshNIDRandom
    >>> subsume
    >>> raise3Under @(Input FilePath)
    >>> applyInput2 (runReadGraphDualizeableIO @String)
    >>> applyInput2 (runReadGraphDatalessDualizeableIO @String)
    >>> applyInput2 (runWriteGraphDualizeableIO @String)
    >>> supplyInputVia getGraphFilePath
    >>> runScopedGraphMetadataEditingTransactionally
    >>> subsume
    >>> runLocableHistoryState
    >>> subsume

type IOWrapperEffects :: [Effect]
type IOWrapperEffects =
  [ Web,
    FileSystem,
    FileTypeOracle,
    GetTime,
    GraphMetadataFilesystemOperations,
    GraphMetadataFilesystemOperationsWriteDiff,
    GraphDataFilesystemOperations,
    Echo,
    Editor,
    DisplayImage
  ]

type TimeBehavior =
  forall r. Member (Embed IO) r =>
  Sem (GetTime : r) ~> Sem r

runIOWrapperEffects ::
  ( Members PermissiveDependencyEffects r,
    Members ErrorEffects r
  ) =>
  TimeBehavior ->
  Sem (Concat IOWrapperEffects r) ~> Sem r
runIOWrapperEffects timeBehavior =
  runWebIO
    >>> runFileSystemIO
    >>> runFileTypeOracle
    >>> timeBehavior
    >>> runGraphMetadataFilesystemOperationsIO
    >>> runGraphMetadataFilesystemOperationsWriteDiffIO
    >>> runGraphDataFilesystemOperationsIO
    >>> subsume
    >>> interpretEditorAsIOVim
    >>> interpretDisplayImageIO

type ErrorEffects :: [Effect]
type ErrorEffects =
  [ Warn UserError,
    Error UserError
  ]

type ErrorHandlingBehavior a =
  forall r. Member (Embed IO) r =>
  Sem (Warn UserError : Error UserError : r) a -> Sem r a

runErrorEffects ::
  Members PermissiveDependencyEffects r =>
  ErrorHandlingBehavior a ->
  Sem (Concat ErrorEffects r) a -> Sem r a
runErrorEffects errorHandlingBehavior = errorHandlingBehavior

type PermissiveDependencyEffects :: [Effect]
type PermissiveDependencyEffects =
  [ Echo,
    Dualizeable,
    State StdGen,
    Embed IO,
    State History,
    RawGraph
  ]

runPermisiveDependencyEffects ::
  ( Member (Embed IO) r
  ) =>
  FilePath ->
  Sem (Concat PermissiveDependencyEffects r) ~> Sem r
runPermisiveDependencyEffects path =
  runEchoIO
    >>> evalState (IsDual False)
    >>> (\m -> initStdGen >>= \stdGen -> evalState stdGen m)
    >>> subsume
    >>> evalState (History [] nilNID [])
    >>> runRawGraphWithPath path

runPermissiveDependencyEffectsEnv ::
  ( Members FinalEffects r
  ) =>
  Sem (Concat PermissiveDependencyEffects r) ~> Sem r
runPermissiveDependencyEffectsEnv =
    runEchoReadline
      >>> runStateInputIORefOf #isDualized
      >>> runStateInputIORefOf #randomGen
      >>> subsume
      >>> runStateInputIORefOf @History #history
      >>> raiseUnder
      >>> runRawGraphAsInput
      >>> contramapInputSem @FilePath (embed @IO . readIORef . view #filePath)

type FinalEffects =
  [ Readline,
    Input Env,
    Embed IO,
    Embed (InputT IO),
    Final (InputT IO)
  ]

runFinalEffects ::
  Env ->
  Sem FinalEffects ~> IO
runFinalEffects env =
  runReadlineFinal
    >>> runInputConst env
    >>> runEmbedded liftIO
    >>> withEffects @'[Embed (InputT IO), Final (InputT IO)]
    >>> embedToFinal @(InputT IO)
    >>> runFinal
    >>> H.runInputT env.replSettings

type AppEffects :: [Effect]
type AppEffects =
  GraphEditorEffects
    `Concat` IOWrapperEffects
    `Concat` ErrorEffects
    `Concat` PermissiveDependencyEffects
    `Concat` FinalEffects

runAppEffects ::
  ErrorHandlingBehavior a ->
  TimeBehavior ->
  Env ->
  Sem AppEffects a ->
  IO a
runAppEffects errorHandlingBehavior timeBehavior env =
  runGraphEditorEffects
    >>> runIOWrapperEffects timeBehavior
    >>> runErrorEffects errorHandlingBehavior
    >>> runPermissiveDependencyEffectsEnv
    >>> runFinalEffects env
