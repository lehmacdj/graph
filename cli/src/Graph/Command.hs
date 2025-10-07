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
import Error.Missing
import Error.UserError
import Error.Warn
import Graph.Augmentation
import Graph.Check
import Graph.Effect
import Graph.Export.FileSystem (exportToDirectory)
import Graph.FreshNID
import Graph.GraphMetadataEditing (GraphMetadataEditing, GraphMetadataReading)
import Graph.Import.ByteString
import Graph.Import.FileSystem
import Graph.LegacyPathMaterialization
import Graph.MaterializePath (materializeNPath)
import Graph.NodeLocated
import Graph.Time (taggingFreshNodesWithTime)
import Graph.Utils
import Models.Augmentation.Tags
import Models.Command
import Models.Connect
import Models.Edge
import Models.History
import Models.MaterializedPath
import Models.NID
import Models.Node
import Models.NormalizedPath (leastConstrainedNormalizedPath, normalizePath)
import Models.Path.ParsedPath
import Models.Path.Simple (Path)
import Models.Path.Simple qualified as Simple
import MyPrelude
import Polysemy.Internal.Scoped (Scoped, scoped_)
import Polysemy.Readline
import Polysemy.State
import Utils.Singleton

singleErr :: Text -> Set NID -> UserError
singleErr cmd xs =
  OtherError $
    cmd
      ++ " needs a path that resolves to a single node\n"
      ++ "but it resolved to: "
      ++ tshow (setToList xs)

printTransitions ::
  (Member Echo effs) =>
  Set (Connect String) ->
  Sem effs ()
printTransitions = mapM_ (echo . dtransition)
  where
    dtransition (Connect t nid) = show t ++ " at " ++ show nid

promptYesNo ::
  (Member Readline r) =>
  String ->
  Sem r Bool
promptYesNo prompt = do
  line <- getInputLine prompt
  let result
        | line `elem` map Just ["yes", "y", "Y"] = pure True
        | line `elem` map Just ["n", "no", "N"] = pure False
        | otherwise = promptYesNo "please enter (y/n): "
  result

-- | Check to make sure that the current state of the graph is not dualized
guardDangerousDualizedOperation ::
  (Members [Readline, Error UserError, Dualizeable] r) => Sem r ()
guardDangerousDualizedOperation = do
  isDual' <- view #isDual <$> get @IsDual
  when isDual' do
    outputStrLn "the graph is currently dualized"
    outputStrLn "the operation you are attempting may be dangerous in that state"
    promptYesNo "proceed (y/n): " >>= bool (throw OperationCancelled) (pure ())

interpretDirective ::
  ( Members
      [ GraphMetadataReading,
        State History,
        GetLocation,
        Error UserError
      ]
      effs
  ) =>
  SourceRange ->
  Directive Identity Text ->
  Sem effs (Path Text)
interpretDirective sourceRange = \case
  LocationFromHistory i -> gets @History (Absolute . fst . backInTime i)
  Targets p -> do
    currentNid <- currentLocation
    p' <- handleDirectivesWith interpretDirective p
    let np = normalizePath p'
    mp <- materializeNPath currentNid (leastConstrainedNormalizedPath np)
    -- this is a little bit inefficient of an embedding, but not too bad
    pure $ foldl' (Simple.:+) Zero (mapSet Absolute $ getTargets mp.path)
  Splice expr ->
    throw $
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
    Members [FileTypeOracle, Readline, Warn UserError, Scoped () GraphMetadataEditing] effs,
    -- TODO: remove this inclusion of RawGraph + Embed IO here; probably the
    -- best way to do this is to allow commands to be defined as @stack@
    -- scripts, and then rewrite materialize and any other commands that
    -- require this outside of
    Members [RawGraph, Embed IO] effs,
    HasGraph String effs
  ) =>
  Command ->
  Sem effs ()
interpretCommand = \case
  ChangeNode p -> do
    nid <- currentLocation
    let err = singleErr "cd"
    nid' <- the' err =<< subsumeUserError @Missing (resolvePathSuccesses nid p)
    changeLocation nid'
  NodeId -> currentLocation >>= echo . show
  Dualize -> dualize
  Make p -> do
    nid <- currentLocation
    subsumeUserError . taggingFreshNodesWithTime . void $ mkPath nid p
  Merge p -> do
    nid <- currentLocation
    nids <- subsumeUserError (resolvePathSuccessesDetail' nid p)
    whenNonNull (mapMaybe successfulDPathEndpoint $ toList nids) $ \xs -> do
      nid' <- subsumeUserError (Graph.Utils.mergeNodes @String xs)
      changeLocation nid'
  Remove p -> do
    nid <- currentLocation
    subsumeUserError $ delPath nid p
  RemoveNode p -> do
    nid <- currentLocation
    nids <- subsumeUserError (resolvePathSuccesses nid p)
    forM_ nids $ deleteNode @String
  Clone p t -> do
    nid <- currentLocation
    let err = singleErr "clone"
    nid' <- the' err =<< subsumeUserError (resolvePathSuccesses nid p)
    nid'' <- (subsumeUserError . taggingFreshNodesWithTime) (cloneNode @String nid')
    cnid <- currentLocation
    insertEdge $ Edge cnid t nid''
  Query p t -> do
    nid <- currentLocation
    nids <- subsumeUserError (resolvePathSuccesses nid p)
    nnid <- (subsumeUserError . taggingFreshNodesWithTime) (nid `transitionsFreshVia` t)
    _ <- subsumeUserError (Graph.Utils.mergeNodes @String (nnid `ncons` toList nids))
    pure ()
  Tag p q -> do
    nid <- currentLocation
    let err = singleErr "the last argument of tag"
    target <- the' err =<< subsumeUserError (resolvePathSuccesses nid q)
    nnids <- (subsumeUserError . taggingFreshNodesWithTime) (mkPath nid (p Simple.:/ Literal ""))
    _ <- subsumeUserError (Graph.Utils.mergeNodes @String (target `ncons` toList nnids))
    pure ()
  Text t s -> do
    nid <- currentLocation
    vNid <- the' (error "only creating one path") =<< (subsumeUserError . taggingFreshNodesWithTime) (mkPath nid (Literal t))
    setData vNid (Just (encodeUtf8 (fromString s)))
  Describe d -> interpretCommand (Text "description" d)
  At p c -> do
    nid <- currentLocation
    locations <- subsumeUserError (resolvePathSuccesses nid p)
    forM_ locations $ \nid' -> local @NID (const nid') $ interpretCommand c
  Dedup t -> do
    nid <- currentLocation
    ambiguities <- subsumeUserError (resolvePathSuccesses nid (Literal t))
    let noSuffix = repeat ""
        suffixes
          | length ambiguities < 2 = noSuffix
          | otherwise = show <$> ([1 ..] :: [Int])
    forM_ ambiguities $ \amb -> deleteEdge (Edge nid t amb)
    zipWithM_
      (\a s -> insertEdge (Edge nid (t ++ s) a))
      (toList ambiguities)
      suffixes
  Flatten t -> do
    nid <- currentLocation
    let err =
          const
            . OtherError
            $ "flatten only works if there is only a single node that the literal resolves to"
    nodeToFlattenFrom <- the' err =<< subsumeUserError (resolvePathSuccesses nid (Literal t))
    nodesToFlatten <- subsumeUserError $ resolvePathSuccesses nodeToFlattenFrom Wild
    deleteEdge (Edge nid t nodeToFlattenFrom)
    for_ [Edge nid t nid' | nid' <- toList nodesToFlatten] insertEdge
  ListOut -> do
    n <- subsumeUserError currentNode
    printTransitions n.outgoing
  ShowImage -> do
    n <- subsumeUserError (currentNode @String)
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
  AddText text -> do
    guardDangerousDualizedOperation
    nid <- subsumeUserError (importData (encodeUtf8 text))
    changeLocation nid
  Debug -> do
    echo "current node:"
    currentLocation
      >>= subsumeUserError . getNodeSem
      >>= echo . unpack . nshowWith (\x -> x {showAugmentation = Nothing})
    echo "history:"
    get @History >>= echo . show
  -- echo "node-ids in the graph:"
  -- nodeManifest @String >>= echo . show
  Check -> reportToConsole @String (fsck @String)
  Fix -> fixErrors @String (fsck @String)
  Move p q -> do
    nid <- currentLocation
    let err = singleErr "the last argument of mv"
    target <- the' err =<< subsumeUserError (resolvePathSuccesses nid q)
    subsumeUserError (mvPath nid p target)
  Rename p q -> do
    nid <- currentLocation
    let err xs =
          OtherError $
            "the first argument to rn require the path to only resolve to "
              ++ "one node but they resolved to \n"
              ++ (tshow . map endPoint . setToList $ xs)
    c <- the' err =<< subsumeUserError (resolvePathSuccessesDetail nid p)
    subsumeUserError (renameDPath c nid q)
  Alias p q -> do
    nid <- currentLocation
    let err xs =
          OtherError $
            "the first argument to rn require the path to only resolve to "
              ++ "one node but they resolved to \n"
              ++ (tshow . map endPoint . setToList $ xs)
    c <- the' err =<< subsumeUserError (resolvePathSuccessesDetail nid p)
    subsumeUserError (aliasDPath c nid q)
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
  Materialize fp -> do
    nid <- subsumeUserError @Missing currentLocation
    fp' <- subsumeUserError @IOError . untry $ canonicalizePath fp
    exportToDirectory nid fp'
  Collect t -> do
    currentNid <- currentLocation
    nids <- subsumeUserError (resolvePathSuccesses currentNid (Literal t))
    newNid <-
      the' (error "only creating one path")
        =<< (subsumeUserError . taggingFreshNodesWithTime) (mkPath currentNid (Literal t))
    for_ nids $ \nid -> do
      deleteEdge (Edge currentNid t nid)
      insertEdge (Edge newNid "" nid)
  Seq ps -> do
    toList ps & traverse_ interpretCommand
  V2Path p -> do
    nid <- currentLocation
    -- say $ "current location: " ++ tshow nid
    -- say $ "parsed path: " ++ tshow p
    scoped_ do
      p' <- handleDirectivesWith interpretDirective p
      -- say $ "prenormal path: " ++ tshow p'
      let np = normalizePath p'
      -- say $ "normalized path: " ++ tshow np
      mp <- materializeNPath nid (leastConstrainedNormalizedPath np)
      -- say $ "materialized path: " <> tshow mp.path
      -- if null mp.graph
      --   then say "no nodes materialized"
      --   else do
      --     say "fetched graph:"
      --     sayShow $ subtractiveFilterGraph (\n -> n.augmentation == Fetched) mp.graph
      unless (null mp.nonexistentNodes) $
        say ("nonexistent nodes: " ++ tshow mp.nonexistentNodes)
      targets <- traverse (getNodeWith fetchTags) (toList (getTargets mp.path))
      when (null targets) do
        say "no targets"
      for_ targets $ \n -> maybe (say $ tshow n ++ "did not exist") sayShow n
