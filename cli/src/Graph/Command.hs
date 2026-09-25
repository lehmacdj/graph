module Graph.Command where

import Control.Monad (zipWithM_)
import DAL.DirectoryFormat (legacyNodeDataFile)
import DAL.RawGraph
import Effect.IOWrapper.DisplayImage
import Effect.IOWrapper.Echo
import Effect.IOWrapper.Editor
import Effect.IOWrapper.FileSystem
import Effect.IOWrapper.FileTypeOracle
import Effect.IOWrapper.GetTime
import Effect.IOWrapper.Web
import Effectful.Provider
import Effectful.Readline
import Effectful.State.Dynamic
import Error.Missing
import Error.UserError
import Error.Warn
import Graph.Augmentation
import Graph.Check
import Graph.Effect
import Graph.FreshNID
import Graph.GraphMetadataEditing (GraphMetadataEditing, GraphMetadataReading)
import Graph.Import.ByteString
import Graph.Import.FileSystem
import Graph.NodeLocated
import Graph.PathPattern.Execution qualified as Paths
import Graph.PathPattern.Resolve (ResolvedPath (..), resolvePath)
import Graph.Time (tagNodesWithTime, taggingFreshNodesWithTime)
import Graph.Utils
import Models.Augmentation.Bundled
import Models.Augmentation.Preview
import Models.Augmentation.Tags
import Models.Augmentation.Timestamps
import Models.Command
import Models.Connect
import Models.Edge
import Models.History
import Models.NID
import Models.Path.ParsedPath
import Models.Path.Simple (Path)
import Models.Path.Simple qualified as Simple
import Models.PathPattern.Match (ConcreteHop (..), ConcreteRoute (..), selectedRoutes)
import MyPrelude
import System.Console.Terminal.Size qualified as Terminal
import Utils.Prettyprinter qualified as PP
import Utils.Singleton

singleErr :: Text -> Set NID -> UserError
singleErr cmd xs =
  OtherError $
    cmd
      ++ " needs a path that resolves to a single node\n"
      ++ "but it resolved to: "
      ++ tshow (setToList xs)

printTransitions ::
  (Echo :> effs) =>
  Set (Connect Text) ->
  Eff effs ()
printTransitions = mapM_ (echo . dtransition)
  where
    dtransition (Connect t nid) = show t ++ " at " ++ show nid

promptYesNo ::
  (Readline :> r) =>
  String ->
  Eff r Bool
promptYesNo prompt = do
  line <- getInputLine prompt
  let result
        | line `elem` map Just ["yes", "y", "Y"] = pure True
        | line `elem` map Just ["n", "no", "N"] = pure False
        | otherwise = promptYesNo "please enter (y/n): "
  result

-- | Check to make sure that the current state of the graph is not dualized
guardDangerousDualizedOperation ::
  (Members [Readline, Error UserError, Dualizeable] r) => Eff r ()
guardDangerousDualizedOperation = do
  isDual' <- view #isDual <$> get @IsDual
  when isDual' do
    outputStrLn "the graph is currently dualized"
    outputStrLn "the operation you are attempting may be dangerous in that state"
    promptYesNo "proceed (y/n): " >>= bool (throwError OperationCancelled) (pure ())

interpretDirective ::
  ( Members
      [ GraphMetadataReading,
        State History,
        GetLocation,
        Error UserError,
        GetTime,
        Web,
        FreshNID,
        Dualizeable,
        Readline
      ]
      effs,
    HasGraph Text effs
  ) =>
  SourceRange ->
  Directive 'WithDirectives ->
  Eff effs Path
interpretDirective sourceRange = \case
  LocationFromHistory i -> gets @History (Absolute . fst . backInTime i)
  Targets p -> do
    currentNid <- currentLocation
    p' <- handleDirectivesWith interpretDirective p
    targets <- Paths.queryTargets currentNid p'
    -- this is a little bit inefficient of an embedding, but not too bad
    pure $ foldl' (Simple.:+) Zero (mapSet Absolute targets)
  HttpResource uri -> do
    guardDangerousDualizedOperation
    Absolute <$> subsumeUserError @Missing (importUrl uri)
  Splice expr ->
    throwError $
      OtherError $
        "the splice directive %{"
          ++ pack expr
          ++ "} is not supported in the CLI"
          ++ " it is only supported in the QuasiQuoter"
          ++ "\n"
          ++ "at "
          ++ tshow sourceRange

interpretCommand ::
  ( Members [DisplayImage, Echo, Error UserError, SetLocation, GetLocation, Dualizeable] effs,
    Members [FileSystem, Web, FreshNID, GetTime, Editor, State History] effs,
    Members [FileTypeOracle, Readline, Warn UserError, Provider_ GraphMetadataEditing ()] effs,
    -- TODO: remove this inclusion of RawGraph + IOE here; probably the
    -- best way to do this is to allow commands to be defined as @stack@
    -- scripts, and then rewrite resolve and any other commands that
    -- require this outside of
    Members [RawGraph, IOE] effs,
    HasGraph Text effs
  ) =>
  Command ->
  Eff effs ()
interpretCommand = \case
  ChangeNode p -> do
    nid <- currentLocation
    let err = singleErr "cd"
    nid' <- the' err =<< Paths.queryTargets nid p
    changeLocation nid'
  NodeId -> currentLocation >>= echo . show
  Dualize -> dualize
  Make p -> do
    nid <- currentLocation
    newNodes <- Paths.constructPath False nid p
    subsumeUserError @Missing (tagNodesWithTime newNodes)
  MakeAllowingOrphans p -> do
    nid <- currentLocation
    newNodes <- Paths.constructPath True nid p
    subsumeUserError @Missing (tagNodesWithTime newNodes)
  Merge p -> do
    nid <- currentLocation
    nids <- Paths.orderedQueryTargets nid p
    whenNonNull nids $ \xs -> do
      nid' <- subsumeUserError (Graph.Utils.mergeNodes @Text xs)
      changeLocation nid'
  Remove p -> do
    nid <- currentLocation
    Paths.removeSelectedEdges nid p
  RemoveNode p -> do
    nid <- currentLocation
    nids <- Paths.queryTargets nid p
    forM_ nids $ deleteNode @Text
  Tag p q -> do
    nid <- currentLocation
    let err = singleErr "the last argument of tag"
    target <- the' err =<< Paths.queryTargets nid q
    newNodes <- Paths.constructTag nid p target
    -- the target and the prefix's created nodes share one timestamp
    subsumeUserError @Missing $ tagNodesWithTime (insertSet target newNodes)
  Text t s -> do
    nid <- currentLocation
    -- text always adds a fresh data node, even if the label already exists
    vNid <-
      (subsumeUserError . taggingFreshNodesWithTime)
        (transitionsFreshVia nid t)
    setData vNid (Just (encodeUtf8 s))
  Describe d -> interpretCommand (Text "description" d)
  At p c -> do
    nid <- currentLocation
    locations <- Paths.queryTargets nid p
    forM_ locations $ \nid' -> local @NID (const nid') $ interpretCommand c
  Dedup t -> do
    nid <- currentLocation
    ambiguities <- Paths.queryTargets nid (Literal t)
    let noSuffix = repeat ""
        suffixes
          | length ambiguities < 2 = noSuffix
          | otherwise = show <$> ([1 ..] :: [Int])
    forM_ ambiguities $ \amb -> deleteEdge (Edge nid t amb)
    zipWithM_
      (\a s -> insertEdge (Edge nid (t <> pack s) a))
      (toList ambiguities)
      suffixes
  ShowImage -> do
    n <- subsumeUserError (currentNode @Text)
    forM_ n.rawData $ subsumeUserError @Missing . displayImage . fromStrict
  -- it probably would make sense to factor these commands out into separate
  -- layers of commands that can be handled at different levels
  Import fp -> do
    guardDangerousDualizedOperation
    fp' <- subsumeUserError @IOError . untry $ canonicalizePath fp
    subsumeUserError $ importDirectory fp' nilNID
    changeLocation nilNID
  ImportUrl uri -> do
    guardDangerousDualizedOperation
    nid <- subsumeUserError (importUrl uri)
    changeLocation nid
  Debug -> do
    echo "current node:"
    currentLocation
      >>= subsumeUserError . getNodeSem
      >>= echo . unpack . nshowWith (\x -> x {showAugmentation = Nothing})
    echo "history:"
    get @History >>= echo . show
  Check -> reportToConsole @Text (fsck @Text)
  Fix -> fixErrors @Text (fsck @Text)
  Move p q -> do
    nid <- currentLocation
    let err = singleErr "the last argument of mv"
    target <- the' err =<< Paths.queryTargets nid q
    Paths.moveSelectedEdges nid target p
  Rename p q -> do
    nid <- currentLocation
    Paths.relabelPath True nid p q
  Copy p q -> do
    nid <- currentLocation
    target <-
      the' (singleErr "the last argument of cp") =<< Paths.queryTargets nid q
    Paths.copySelectedEdges nid target p
  Alias p q -> do
    nid <- currentLocation
    Paths.relabelPath False nid p q
  Edit -> do
    n <- subsumeUserError @Missing currentLocation
    path <- getGraphFilePath
    invokeEditor [legacyNodeDataFile path n]
  Back n -> do
    history <- get @History
    let (_, history') = backInTime n history
    -- technically this could lead to being on an invalid node that is already
    -- deleted. we don't make an effort to change the past when things are
    -- deleted in the future right now
    -- we don't need to use changeLocation here because setting the history
    -- already modifies the location and if we set the location we would end up
    -- adding duplicates to the history
    put @History history'
  Seq ps -> do
    toList ps & traverse_ interpretCommand
  Preview p -> do
    nid <- currentLocation
    provide_ do
      p' <- handleDirectivesWith interpretDirective p
      -- preview joins its outputs like every other query
      mp <- resolvePath nid (p' Simple.:/ One)
      unless (null mp.nonexistentNodes) $
        say ("nonexistent nodes: " ++ tshow mp.nonexistentNodes)
      targets <-
        for (ordNub (concatMap selectedRoutes mp.resolutions)) \route -> do
          let listedNid = maybe route.root (.target) (lastMay route.hops)
              transitions = map (.label) route.hops
          node <- getNodeWith (fetchTags #| fetchTimestamps) listedNid
          pure (transitions, listedNid, node)
      when (null targets) do
        say "no targets"
      tz <- currentTimeZone
      now <- currentTime
      w <-
        liftIO Terminal.size >>= \case
          Just (Terminal.Window _ width) -> pure width
          Nothing -> pure 80
      for_ targets $ \case
        ([], lnid, Nothing) ->
          say ("current node " ++ tshow lnid ++ " did not exist")
        (ts, lnid, Nothing) ->
          say $
            renderNodeListing w $
              docTransitions ts
                <> PP.softline
                <> "led to nonexistent node "
                <> PP.pretty (tshow lnid)
        (ts, _, Just n) -> say $ renderNodeListing w $ docNodeListing tz now ts n
