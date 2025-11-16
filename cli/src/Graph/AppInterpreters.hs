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
import Effect.Readline
import Effectful
import Effectful.Error.Static
import Effectful.Provider
import Effectful.Reader.Static
import Effectful.State.Static.Local
import Error.UserError
import Error.Warn
import Graph.Effect
import Graph.FreshNID
import Graph.GraphMetadataEditing
import Graph.NodeLocated
import Models.History
import Models.NID
import MyPrelude
import System.Console.Haskeline (InputT)
import System.Console.Haskeline qualified as H
import System.Random
import Utils.Polysemy

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

printingErrorsAndWarnings :: ErrorHandlingBehavior ()
printingErrorsAndWarnings = printWarnings . printErrors

-- Core effect stacks
type GraphEditorEffects =
  [ FreshNID,
    ReadGraph Text (Maybe ByteString),
    ReadGraphDataless Text,
    WriteGraph Text,
    Provider_ GraphMetadataEditing,
    Dualizeable,
    GetLocation,
    SetLocation,
    State History
  ]

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

type ErrorEffects =
  [ Warn UserError,
    Error UserError
  ]

type PermissiveDependencyEffects =
  [ Echo,
    Dualizeable,
    State StdGen,
    IOE,
    State History,
    RawGraph
  ]

type FinalEffects =
  [ Readline,
    Reader Env,
    IOE
  ]

type AppEffects =
  FreshNID
    : ReadGraph Text (Maybe ByteString)
    : ReadGraphDataless Text
    : WriteGraph Text
    : Provider_ GraphMetadataEditing
    : Dualizeable
    : GetLocation
    : SetLocation
    : State History
    : Web
    : FileSystem
    : FileTypeOracle
    : GetTime
    : GraphMetadataFilesystemOperations
    : GraphMetadataFilesystemOperationsWriteDiff
    : GraphDataFilesystemOperations
    : Echo
    : Editor
    : DisplayImage
    : Warn UserError
    : Error UserError
    : Echo
    : Dualizeable
    : State StdGen
    : IOE
    : State History
    : RawGraph
    : Readline
    : Reader Env
    : IOE
    : '[]

-- Effect runners
runGraphEditorEffects ::
  forall es a.
  ( Reader FilePath :> es,
    Echo :> es,
    Dualizeable :> es,
    State StdGen :> es,
    IOE :> es,
    State History :> es,
    RawGraph :> es,
    GraphMetadataFilesystemOperations :> es,
    GraphMetadataFilesystemOperationsWriteDiff :> es,
    GraphDataFilesystemOperations :> es,
    Warn UserError :> es,
    Error UserError :> es
  ) =>
  Eff
    ( FreshNID
        : ReadGraph Text (Maybe ByteString)
        : ReadGraphDataless Text
        : WriteGraph Text
        : Provider_ GraphMetadataEditing
        : GetLocation
        : SetLocation
        : es
    )
    a ->
  Eff es a
runGraphEditorEffects action = do
  path <- ask @FilePath
  action
    & runFreshNIDRandom
    & runReadGraphDualizeableIO @Text path
    & runReadGraphDatalessDualizeableIO @Text path
    & runWriteGraphDualizeableIO @Text path
    & runScopedGraphMetadataEditingTransactionally
    & runLocableHistoryState

type TimeBehavior =
  forall es a.
  (IOE :> es) =>
  Eff (GetTime : es) a ->
  Eff es a

type FilesystemOperationsBehavior =
  forall es a.
  (RawGraph :> es, IOE :> es, Error UserError :> es) =>
  Eff
    ( GraphMetadataFilesystemOperations
        : GraphMetadataFilesystemOperationsWriteDiff
        : GraphDataFilesystemOperations
        : es
    )
    a ->
  Eff es a

filesystemBehaviorDryRun :: FilesystemOperationsBehavior
filesystemBehaviorDryRun =
  runGraphDataFilesystemOperationsDryRun
    . runGraphMetadataFilesystemOperationsWriteDiffDryRun
    . runGraphMetadataFilesystemOperationsDryRun True

filesystemBehaviorIO :: FilesystemOperationsBehavior
filesystemBehaviorIO =
  runGraphDataFilesystemOperationsIO
    . runGraphMetadataFilesystemOperationsWriteDiffIO
    . runGraphMetadataFilesystemOperationsIO True

runIOWrapperEffects ::
  forall es a.
  ( Echo :> es,
    Dualizeable :> es,
    State StdGen :> es,
    IOE :> es,
    State History :> es,
    RawGraph :> es,
    Warn UserError :> es,
    Error UserError :> es
  ) =>
  TimeBehavior ->
  FilesystemOperationsBehavior ->
  Eff
    ( Web
        : FileSystem
        : FileTypeOracle
        : GetTime
        : GraphMetadataFilesystemOperations
        : GraphMetadataFilesystemOperationsWriteDiff
        : GraphDataFilesystemOperations
        : Echo
        : Editor
        : DisplayImage
        : es
    )
    a ->
  Eff es a
runIOWrapperEffects timeBehavior filesystemBehavior =
  interpretDisplayImageIO
    . interpretEditorAsIOVim
    . runFileTypeOracle
    . runFileSystemIO
    . runWebIO
    . timeBehavior
    . filesystemBehavior

type ErrorHandlingBehavior a =
  forall es.
  (IOE :> es) =>
  Eff (Warn UserError : Error UserError : es) a ->
  Eff es a

runErrorEffects ::
  forall es a.
  (Echo :> es, Dualizeable :> es, State StdGen :> es, IOE :> es, State History :> es, RawGraph :> es) =>
  ErrorHandlingBehavior a ->
  Eff (Warn UserError : Error UserError : es) a ->
  Eff es a
runErrorEffects errorHandlingBehavior = errorHandlingBehavior

runPermissiveDependencyEffects ::
  (IOE :> es) =>
  FilePath ->
  Eff (Echo : Dualizeable : State StdGen : State History : RawGraph : es) a ->
  Eff es a
runPermissiveDependencyEffects path action = do
  stdGen <- liftIO initStdGen
  action
    & runRawGraphWithPath path
    & evalState (History [] nilNID [])
    & evalState stdGen
    & evalState (IsDual False)
    & runEchoIO

runPermissiveDependencyEffectsEnv ::
  forall es a.
  (Readline :> es, Reader Env :> es, IOE :> es) =>
  Eff (Echo : Dualizeable : State StdGen : State History : RawGraph : es) a ->
  Eff es a
runPermissiveDependencyEffectsEnv action = do
  env <- ask @Env
  path <- liftIO $ readIORef env.filePath
  action
    & runReader path . runRawGraphAsReader . raiseUnder
    & runStateInputIORefOf @History #history
    & runStateInputIORefOf #randomGen
    & runStateInputIORefOf #isDualized
    & runEchoReadline

runFinalEffects ::
  Env ->
  Eff (Readline : Reader Env : IOE : '[]) a ->
  IO a
runFinalEffects env =
  runEff
    . runReader env
    . runReadlineIO

runAppEffects ::
  ErrorHandlingBehavior a ->
  TimeBehavior ->
  FilesystemOperationsBehavior ->
  Env ->
  Eff AppEffects a ->
  IO a
runAppEffects errorHandlingBehavior timeBehavior filesystemBehavior env action =
  action
    & runGraphEditorEffects
    & runIOWrapperEffects timeBehavior filesystemBehavior
    & runErrorEffects errorHandlingBehavior
    & runPermissiveDependencyEffectsEnv
    & runFinalEffects env
