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
import Utils.Parsing.Common (showSpan)
import Utils.Prettyprinter qualified as PP
import Utils.Singleton
import Utils.Testing hiding (node)

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

-- | URLs already imported by the current command line, so that a URL
-- (e.g. inside @at@ or repeated in a @;@ sequence) is fetched at most once
-- per command line.
newtype ImportedUrls = ImportedUrls (Map URI NID)

-- | The @%targets(…)@ directives the current command expanded to nothing,
-- rendered with their position (newest first); mentioned if it fails.
newtype EmptyTargets = EmptyTargets [Text]

interpretDirective ::
  ( Members
      [ GraphMetadataReading,
        State History,
        State ImportedUrls,
        State EmptyTargets,
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
    when (null targets) $
      let shown =
            "%targets("
              ++ tshow p
              ++ ") at "
              ++ pack (showSpan sourceRange.startPos sourceRange.endPos)
       in modify \(EmptyTargets xs) -> EmptyTargets (shown : xs)
    -- this is a little bit inefficient of an embedding, but not too bad
    pure $ foldl' (Simple.:+) Zero (mapSet Absolute targets)
  HttpResource uri -> Absolute <$> importUrlOnce uri
  Splice expr ->
    throwError $
      OtherError $
        "The splice directive %{"
          ++ pack expr
          ++ "} only works in the path quasiquoter, not the CLI; remove the"
          ++ " one at "
          ++ pack (showSpan sourceRange.startPos sourceRange.endPos)
          ++ "."

-- | Import a URL unless this command line already imported it (and that
-- node still exists: @mk x/URL ; rmnf x ; mk y/URL@ imports it again).
importUrlOnce ::
  ( Members
      [ State ImportedUrls,
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
  URI ->
  Eff effs NID
importUrlOnce uri = do
  ImportedUrls imported <- get
  (nid, imported') <-
    reuseOrImport
      (fmap isJust . getNodeDataless @Text)
      do
        guardDangerousDualizedOperation
        subsumeUserError @Missing (importUrl uri)
      uri
      imported
  put (ImportedUrls imported')
  pure nid

-- | The cache logic of 'importUrlOnce': reuse a cached node if it still
-- exists, otherwise import (again) and cache the result.
reuseOrImport ::
  (Monad m) =>
  -- | does the node still exist?
  (NID -> m Bool) ->
  -- | import the URL
  m NID ->
  URI ->
  Map URI NID ->
  m (NID, Map URI NID)
reuseOrImport exists importIt uri cache = do
  reusable <- case lookup uri cache of
    Just nid -> bool Nothing (Just nid) <$> exists nid
    Nothing -> pure Nothing
  case reusable of
    Just nid -> pure (nid, cache)
    Nothing -> do
      nid <- importIt
      pure (nid, insertMap uri nid cache)

test_reuseOrImport :: TestTree
test_reuseOrImport =
  testGroup
    "reuseOrImport"
    [ testCase "imports an uncached URL" $
        run (const True) mempty @?= ((fresh, singletonMap uri fresh), 1),
      testCase "reuses a cached node that exists" $
        run (const True) (singletonMap uri cached)
          @?= ((cached, singletonMap uri cached), 0),
      testCase "imports again when the cached node was deleted" $
        run (const False) (singletonMap uri cached)
          @?= ((fresh, singletonMap uri fresh), 1)
    ]
  where
    uri = [quri|https://example.com/x|]
    cached = smallNID 1
    fresh = smallNID 2
    -- counts the imports
    run exists cache =
      runPureEff . runStateLocal (0 :: Int) $
        reuseOrImport
          (pure . exists)
          (modify (+ 1) $> fresh)
          uri
          cache

-- | Expand a command path's directives when the command runs: relative to
-- the current location (so inside @at p cmd@ they are relative to each of
-- @p@'s targets) and the history at that moment. A URL is imported here,
-- before the command does anything else, so the import stays even if the
-- command then fails (e.g. an ambiguous @mk@). Each URL is imported at
-- most once per command line. Parsing finishes before any command runs, so
-- a parse error never imports anything.
expandDirectives ::
  ( Members
      [ Provider_ GraphMetadataEditing (),
        State History,
        State ImportedUrls,
        State EmptyTargets,
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
  ParsedPath ->
  Eff effs Path
expandDirectives p = provide_ (handleDirectivesWith interpretDirective p)

type CommandEffects effs =
  ( Members [DisplayImage, Echo, Error UserError, SetLocation, GetLocation, Dualizeable] effs,
    Members [FileSystem, Web, FreshNID, GetTime, Editor, State History] effs,
    Members [FileTypeOracle, Readline, Warn UserError, Provider_ GraphMetadataEditing ()] effs,
    -- TODO: remove this inclusion of RawGraph + IOE here; probably the
    -- best way to do this is to allow commands to be defined as @stack@
    -- scripts, and then rewrite resolve and any other commands that
    -- require this outside of
    Members [RawGraph, IOE] effs,
    HasGraph Text effs
  )

-- | Run one command line.
interpretCommand :: (CommandEffects effs) => Command -> Eff effs ()
interpretCommand = evalStateLocal (ImportedUrls mempty) . runCommand

-- | Run a command, noting in its error any @%targets(…)@ that it expanded
-- to nothing (it then means @%never@, which is easy to miss).
runCommand ::
  (CommandEffects effs, State ImportedUrls :> effs) =>
  Command ->
  Eff effs ()
runCommand c = evalStateLocal (EmptyTargets []) do
  result <- tryError @UserError (runCommand' c)
  EmptyTargets empties <- get
  case result of
    Right () -> pure ()
    Left (_, e)
      | null empties -> throwError e
      | otherwise ->
          throwError . (e <>) . OtherError $
            "Note: "
              ++ intercalate ", " (reverse empties)
              ++ " matched no nodes, so "
              ++ (if length empties == 1 then "it" else "they")
              ++ " expanded to %never."

runCommand' ::
  ( CommandEffects effs,
    Members [State ImportedUrls, State EmptyTargets] effs
  ) =>
  Command ->
  Eff effs ()
runCommand' = \case
  ChangeNode p0 -> do
    p <- expandDirectives p0
    nid <- currentLocation
    let err = singleErr "cd"
    nid' <- the' err =<< Paths.queryTargets nid p
    changeLocation nid'
  NodeId -> currentLocation >>= echo . show
  Dualize -> dualize
  Make p0 -> do
    p <- expandDirectives p0
    nid <- currentLocation
    newNodes <- Paths.constructPath False nid p
    subsumeUserError @Missing (tagNodesWithTime newNodes)
  MakeAllowingOrphans p0 -> do
    p <- expandDirectives p0
    nid <- currentLocation
    newNodes <- Paths.constructPath True nid p
    subsumeUserError @Missing (tagNodesWithTime newNodes)
  Merge p0 -> do
    p <- expandDirectives p0
    nid <- currentLocation
    nids <- Paths.orderedQueryTargets nid p
    whenNonNull nids $ \xs -> do
      nid' <- subsumeUserError (Graph.Utils.mergeNodes @Text xs)
      changeLocation nid'
  Remove p0 -> do
    p <- expandDirectives p0
    nid <- currentLocation
    Paths.removeSelectedEdges nid p
  RemoveNode p0 -> do
    p <- expandDirectives p0
    nid <- currentLocation
    nids <- Paths.queryTargets nid p
    forM_ nids $ deleteNode @Text
  Tag p0 q0 -> do
    p <- expandDirectives p0
    q <- expandDirectives q0
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
  Describe d -> runCommand' (Text "description" d)
  At p0 c -> do
    p <- expandDirectives p0
    nid <- currentLocation
    locations <- Paths.queryTargets nid p
    -- c's directives are expanded per target, relative to that target,
    -- and each target starts from the same history; at leaves the history
    -- (and so the location) as it found it
    history <- get @History
    forM_ locations $ \nid' -> do
      result <- tryError @UserError $ local @NID (const nid') (runCommand c)
      put history
      either (throwError . snd) pure result
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
  Move p0 q0 -> do
    p <- expandDirectives p0
    q <- expandDirectives q0
    nid <- currentLocation
    let err = singleErr "the last argument of mv"
    target <- the' err =<< Paths.queryTargets nid q
    Paths.moveSelectedEdges nid target p
  Rename parents p0 q0 -> do
    p <- expandDirectives p0
    q <- expandDirectives q0
    nid <- currentLocation
    Paths.relabelPath True parents nid p q
  Copy p0 q0 -> do
    p <- expandDirectives p0
    q <- expandDirectives q0
    nid <- currentLocation
    target <-
      the' (singleErr "the last argument of cp") =<< Paths.queryTargets nid q
    Paths.copySelectedEdges nid target p
  Alias parents p0 q0 -> do
    p <- expandDirectives p0
    q <- expandDirectives q0
    nid <- currentLocation
    Paths.relabelPath False parents nid p q
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
    toList ps & traverse_ runCommand
  Preview p -> do
    nid <- currentLocation
    p' <- expandDirectives p
    provide_ do
      -- preview joins its outputs like every other query
      mp <- resolvePath nid (p' Simple.:/ One)
      unless (null mp.nonexistentNodes) $
        say
          ( "nonexistent nodes: "
              ++ intercalate ", " (map tshow (toList mp.nonexistentNodes))
          )
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
