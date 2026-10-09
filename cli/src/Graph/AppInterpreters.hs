-- too big signatures/inference in this module
{-# LANGUAGE NoImpredicativeTypes #-}

-- | Interpreters for running various large stacks of effects that it might
-- make sense to use while using this library.
module Graph.AppInterpreters where

import Control.Lens
import DAL.FileSystemOperations.Data
import DAL.FileSystemOperations.Metadata
import DAL.FileSystemOperations.MetadataWriteDiff
import DAL.RawGraph
import Effect.IOWrapper.DisplayImage
import Effect.IOWrapper.Echo
import Effect.IOWrapper.Editor
import Effect.IOWrapper.FileSystem
import Effect.IOWrapper.FileTypeOracle
import Effect.IOWrapper.GetTime
import Effect.IOWrapper.Web
import Effect.Input
import Effectful.Provider.List
import Effectful.Readline
import Effectful.State.Dynamic
import Error.UserError
import Error.Warn
import Graph.Dualizeable
import Graph.FreshNID
import Graph.GraphEditing
import Graph.NodeLocated
import Models.History
import Models.NID
import MyPrelude
import System.Console.Haskeline qualified as H
import System.Random
import Utils.Effectful

data Env = Env
  { filePath :: IORef FilePath,
    -- | random generator used for creating new node ids
    randomGen :: IORef StdGen,
    history :: IORef History,
    isDualized :: IORef IsDual,
    replSettings :: H.Settings IO,
    -- | what is kept of the changes made to the graph's files
    graphWrites :: GraphWrites
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
    <*> newGraphWrites

printingErrorsAndWarnings :: ErrorHandlingBehavior ()
printingErrorsAndWarnings = printWarnings >>> printErrors

type GraphEditorEffects :: [Effect]
type GraphEditorEffects =
  [ FreshNID,
    ProviderList_ GraphEditing (),
    Dualizeable,
    GetLocation,
    SetLocation,
    State History
  ]

runGraphEditorEffects ::
  ( Members PermissiveDependencyEffects r,
    Members IOWrapperEffects r,
    Members ErrorEffects r
  ) =>
  Eff (Concat GraphEditorEffects r) ~> Eff r
runGraphEditorEffects =
  runFreshNIDRandom
    >>> runScopedGraphEditingTransactionally
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
  forall r.
  (IOE :> r) =>
  Eff (GetTime : r) ~> Eff r

type FilesystemOperationsBehavior =
  forall r a.
  (Members [RawGraph, IOE, Error UserError, Warn UserError] r) =>
  Eff (GraphMetadataFilesystemOperations : GraphMetadataFilesystemOperationsWriteDiff : GraphDataFilesystemOperations : r) a ->
  Eff r a

filesystemBehaviorDryRun :: FilesystemOperationsBehavior
filesystemBehaviorDryRun =
  runGraphMetadataFilesystemOperationsDryRun True
    >>> runGraphMetadataFilesystemOperationsWriteDiffDryRun
    >>> runGraphDataFilesystemOperationsDryRun

filesystemBehaviorIO :: FilesystemOperationsBehavior
filesystemBehaviorIO =
  runGraphMetadataFilesystemOperationsIO True
    >>> runGraphMetadataFilesystemOperationsWriteDiffIO
    >>> runGraphDataFilesystemOperationsIO

runIOWrapperEffects ::
  ( Members PermissiveDependencyEffects r,
    Members ErrorEffects r
  ) =>
  TimeBehavior ->
  FilesystemOperationsBehavior ->
  Eff (Concat IOWrapperEffects r) ~> Eff r
runIOWrapperEffects timeBehavior filesystemBehavior =
  runWebIO
    >>> runFileSystemIO
    >>> runFileTypeOracle
    >>> timeBehavior
    >>> filesystemBehavior
    >>> subsume
    >>> interpretEditorAsIOVim
    >>> interpretDisplayImageIO

type ErrorEffects :: [Effect]
type ErrorEffects =
  [ Warn UserError,
    Error UserError
  ]

type ErrorHandlingBehavior a =
  forall r.
  (IOE :> r) =>
  Eff (Warn UserError : Error UserError : r) a ->
  Eff r a

runErrorEffects ::
  (Members PermissiveDependencyEffects r) =>
  ErrorHandlingBehavior a ->
  Eff (Concat ErrorEffects r) a ->
  Eff r a
runErrorEffects errorHandlingBehavior = errorHandlingBehavior

type PermissiveDependencyEffects :: [Effect]
type PermissiveDependencyEffects =
  [ Echo,
    Dualizeable,
    State StdGen,
    IOE,
    State History,
    RawGraph,
    State DamageReadPast
  ]

runPermisiveDependencyEffects ::
  (IOE :> r) =>
  FilePath ->
  Eff (Concat PermissiveDependencyEffects r) ~> Eff r
runPermisiveDependencyEffects path =
  runEchoIO
    >>> evalStateLocal (IsDual False)
    >>> (\m -> initStdGen >>= \stdGen -> evalStateLocal stdGen m)
    >>> subsume
    >>> evalStateLocal (History [] nilNID [])
    >>> runRawGraphWithPath path
    >>> evalStateLocal noDamageReadPast

runPermissiveDependencyEffectsEnv ::
  (Members FinalEffects r) =>
  Eff (Concat PermissiveDependencyEffects r) ~> Eff r
runPermissiveDependencyEffectsEnv =
  runEchoReadline
    >>> runStateInputIORefOf #isDualized
    >>> runStateInputIORefOf #randomGen
    >>> subsume
    >>> runStateInputIORefOf @History #history
    >>> runRawGraphFromEnv
    >>> evalStateLocal noDamageReadPast

-- | Interpret 'RawGraph' by reading the (mutable) graph path out of the
-- ambient 'Env'.
runRawGraphFromEnv ::
  (Members [Input Env, IOE] r) =>
  Eff (RawGraph : r) ~> Eff r
runRawGraphFromEnv = interpret \_ -> \case
  GetGraphFilePath -> liftIO . readIORef . view #filePath =<< input @Env
  GetGraphWrites -> view #graphWrites <$> input @Env

type FinalEffects =
  [ Readline,
    Input Env,
    IOE
  ]

-- | How to interpret the 'Readline' effect; interactive sessions use
-- 'runReadlineHaskeline', non-interactive contexts (e.g. running the app
-- stack to compute completions) use 'runReadlineNonInteractive'.
type ReadlineBehavior = forall r. (IOE :> r) => Eff (Readline : r) ~> Eff r

runFinalEffects ::
  ReadlineBehavior ->
  Env ->
  Eff FinalEffects ~> IO
runFinalEffects readlineBehavior env =
  readlineBehavior
    >>> runInputConst env
    >>> runEff

type AppEffects :: [Effect]
type AppEffects =
  GraphEditorEffects
    `Concat` IOWrapperEffects
    `Concat` ErrorEffects
    `Concat` PermissiveDependencyEffects
    `Concat` FinalEffects

runAppEffects ::
  ReadlineBehavior ->
  ErrorHandlingBehavior a ->
  TimeBehavior ->
  FilesystemOperationsBehavior ->
  Env ->
  Eff AppEffects a ->
  IO a
runAppEffects readlineBehavior errorHandlingBehavior timeBehavior filesystemBehavior env =
  runGraphEditorEffects
    >>> runIOWrapperEffects timeBehavior filesystemBehavior
    >>> runErrorEffects errorHandlingBehavior
    >>> runPermissiveDependencyEffectsEnv
    >>> runFinalEffects readlineBehavior env
