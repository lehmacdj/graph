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
import Effectful.Provider.List
import Effectful.Readline
import Effectful.State.Dynamic
import Error.Missing
import Error.UserError
import Error.Warn
import Graph.Augmentation
import Graph.Check
import Graph.Dualizeable
import Graph.FreshNID
import Graph.GraphDataEditing
import Graph.GraphEditing
import Graph.GraphMetadataEditing
import Graph.Import.ByteString
import Graph.Import.FileSystem
import Graph.NodeLocated
import Graph.PathPattern.Execution qualified as Paths
import Graph.PathPattern.Resolve (ResolvedPath (..), resolvePath)
import Graph.SystemNodes (systemNodeNIDs)
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
import Models.PathPattern.Compile (TooManyAlternatives (..), checkAlternatives)
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
  Eff effs ParsedPath
interpretDirective sourceRange = \case
  LocationFromHistory i -> gets @History (Absolute . fst . backInTime i)
  Targets p -> do
    currentNid <- currentLocation
    p' <- handleDirectivesWith (pure Relational) interpretDirective p
    checkAlternativesLimit p'
    targets <- Paths.queryTargets currentNid p'
    when (null targets) $
      let shown =
            "%targets("
              ++ tshow p
              ++ ") at "
              ++ pack (showSpan sourceRange.startPos sourceRange.endPos)
       in modify \(EmptyTargets xs) -> EmptyTargets (shown : xs)
    -- this is a little bit inefficient of an embedding, but not too bad;
    -- it is a + union, lowered like a + written where %targets is
    pure $ foldl' (:+) Zero (mapSet Absolute targets)
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
  -- 'handleDirectivesWith' lowers these itself
  Override settings p -> pure (Directive sourceRange (Override settings p))

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
      (fmap isJust . getNodeMetadata)
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
      [ ProviderList_ GraphEditing (),
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
      effs
  ) =>
  ParsedPath ->
  Eff effs Path
expandDirectives = expandDirectivesAs Relational

-- | 'expandDirectives' for a path that is constructed: @+@ is @++@.
expandConstruction ::
  ( Members
      [ ProviderList_ GraphEditing (),
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
      effs
  ) =>
  ParsedPath ->
  Eff effs Path
expandConstruction = expandDirectivesAs Shared

expandDirectivesAs ::
  ( Members
      [ ProviderList_ GraphEditing (),
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
      effs
  ) =>
  UnionKind ->
  ParsedPath ->
  Eff effs Path
expandDirectivesAs plus p0 = do
  p <- editing (handleDirectivesWith (pure plus) interpretDirective p0)
  checkAlternativesLimit p
  pure p

-- | Edit the graph as one transaction: the edits are written to the graph
-- when the action finishes, and not at all if it fails.
editing ::
  (ProviderList_ GraphEditing () :> effs) =>
  Eff (GraphMetadataEditing : GraphDataEditing : effs) a ->
  Eff effs a
editing = provideList_ @GraphEditing

-- | Fail readably, before compiling it for real, if a path would make more
-- alternatives than its limit allows.
checkAlternativesLimit :: (Error UserError :> effs) => Path -> Eff effs ()
checkAlternativesLimit p = case checkAlternatives p of
  Right () -> pure ()
  Left (TooManyAlternatives n) ->
    throwError . OtherError $
      "This path would make more than "
        ++ tshow n
        ++ " alternatives, since each copy distribution makes of a | union"
        ++ " chooses its side. Write ++ for a union whose side every copy"
        ++ " shares, wrap the path in %override({plus = shared}, …), narrow"
        ++ " it, or raise the limit with %override({max-alternatives = …},"
        ++ " …). (TooManyAlternatives)"
  Left (TooLargeToCount n) ->
    throwError . OtherError $
      "This path's unions make too much to count its alternatives within"
        ++ " the limit of "
        ++ tshow n
        ++ ": each copy distribution makes of a | union chooses its side,"
        ++ " and what is made on the way is limited too, however few"
        ++ " alternatives it comes to. Write ++ for a union whose side every"
        ++ " copy shares, wrap the path in %override({plus = shared}, …),"
        ++ " narrow it, or raise the limit with"
        ++ " %override({max-alternatives = …}, …). (TooLargeToCount)"

type CommandEffects effs =
  ( Members [DisplayImage, Echo, Error UserError, SetLocation, GetLocation, Dualizeable] effs,
    Members [FileSystem, Web, FreshNID, GetTime, Editor, State History] effs,
    Members [FileTypeOracle, Readline, Warn UserError, ProviderList_ GraphEditing ()] effs,
    -- TODO: remove this inclusion of RawGraph + IOE here; probably the
    -- best way to do this is to allow commands to be defined as @stack@
    -- scripts, and then rewrite resolve and any other commands that
    -- require this outside of
    Members [RawGraph, IOE] effs
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

-- | Merge the nodes of @mg@'s arguments (paired with the node each
-- resolved to) and return the survivor. Arguments resolving to the same node
-- count once; fewer than two distinct nodes is an error. The @--keep@ node
-- survives with its NID and data, and the others' data is discarded with a
-- warning. Without @--keep@ the first argument's node survives, and at most
-- one node may have data, which the survivor then takes; otherwise nothing
-- changes and the error asks for @--keep@. Empty data counts as no data. A
-- system node ('systemNodeNIDs', e.g. the origin) is never merged away: it
-- survives without @--keep@, and @--keep@ on another node, or merging two
-- system nodes, is an error.
mergeTargets ::
  (CommandEffects effs, HasGraph Text effs) =>
  [(MergeArgument, NID)] ->
  Eff effs NID
mergeTargets targets = do
  let nids = ordNub (map snd targets)
      describeNode n =
        tshow n
          ++ " ("
          ++ intercalate
            ", "
            [ tshow (mergeArgumentPath a)
            | (a, n') <- targets,
              n' == n
            ]
          ++ ")"
  when (length nids < 2) . throwError . OtherError $
    "mg needs at least two distinct nodes to merge\n"
      ++ "but its arguments all resolved to "
      ++ intercalate ", " (map tshow nids)
  datas <-
    subsumeUserError @Missing $
      mapM (fmap (view #augmentation) . getNodeWithDataSem @Text) nids
  -- empty data counts as no data
  let withData = [n | (n, Just d) <- zip nids datas, not (null d)]
      dataOf n = join (lookup n (zip nids datas))
      -- the arguments again, keeping the first node with data
      suggestion =
        unwords . ("mg" :) $
          zipWith
            ( \i (a, _) ->
                (if Just i == firstWithData then "--keep " else "")
                  ++ tshow (mergeArgumentPath a)
            )
            [0 :: Int ..]
            targets
      firstWithData =
        fst <$> find ((`elem` withData) . snd . snd) (zip [0 ..] targets)
      kept = snd <$> find (isKept . fst) targets
      systemNodes = filter (`member` systemNodeNIDs) nids
      systemError why =
        throwError . OtherError $
          "mg cannot merge a system node away (the origin and the special "
            ++ "nodes such as tags must keep their NIDs)\n"
            ++ why
  -- a system node (the origin, tags, …) must survive the merge
  systemSurvivor <- case systemNodes of
    [] -> pure Nothing
    [n]
      | maybe True (== n) kept -> pure (Just n)
      | otherwise ->
          systemError $
            "--keep would merge away the system node "
              ++ describeNode n
              ++ "; keep it instead"
    _ ->
      systemError $
        "more than one system node: "
          ++ intercalate ", " (map describeNode systemNodes)
  (survivor, dropped) <- case kept of
    Just k -> pure (k, filter (/= k) withData)
    Nothing -> do
      let s = fromMaybe (headEx nids) systemSurvivor
      case withData of
        [] -> pure ()
        [d] -> setRawData s (dataOf d)
        _ ->
          throwError . OtherError $
            "mg would discard data: more than one of the nodes has data\n"
              ++ "nodes with data: "
              ++ intercalate ", " (map describeNode withData)
              ++ "\nmark the node to keep (with its data) with --keep, e.g. "
              ++ suggestion
              ++ "\n(the others' data is discarded)"
      pure (s, [])
  subsumeUserError @Missing $
    mergeNodes @Text survivor (filter (/= survivor) nids)
  unless (null dropped) . warnText $
    "mg --keep "
      ++ tshow survivor
      ++ " discarded the data of "
      ++ intercalate ", " (map describeNode dropped)
  pure survivor

-- | Tell the user that the path @rm@, @mv@ or @cp@ takes its edges from
-- matched nothing (as it was written, before its directives were expanded),
-- so the command did nothing. This is a warning, not an error: the rest of
-- a @;@ sequence still runs, and so does @at p cmd@ at its other targets,
-- where the path may well match. Inside @at@ the notice names the target.
noticeNoMatch ::
  (Members [Warn UserError, State History] effs) =>
  Text ->
  ParsedPath ->
  NID ->
  Eff effs ()
noticeNoMatch what p cur = do
  prompt <- gets @History (view #now)
  warnText $
    what
      ++ " ("
      ++ tshow p
      ++ ") matched nothing"
      ++ (if cur == prompt then "" else " at " ++ tshow cur)
      ++ "; nothing was changed."

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
    nid' <- the' err =<< editing (Paths.queryTargets nid p)
    changeLocation nid'
  NodeId -> currentLocation >>= echo . show
  Dualize -> dualize
  Make p0 -> do
    p <- expandConstruction p0
    nid <- currentLocation
    prompt <- gets @History (view #now)
    editing do
      newNodes <- Paths.constructPath prompt False nid p
      subsumeUserError @Missing (tagNodesWithTime newNodes)
  MakeAllowingOrphans p0 -> do
    p <- expandConstruction p0
    nid <- currentLocation
    prompt <- gets @History (view #now)
    editing do
      newNodes <- Paths.constructPath prompt True nid p
      subsumeUserError @Missing (tagNodesWithTime newNodes)
  Merge args -> do
    nid <- currentLocation
    targets <- forM (zip [1 :: Int ..] (toList args)) $ \(i, arg) -> do
      let p0 = mergeArgumentPath arg
          err =
            singleErr $
              "argument " ++ tshow i ++ " of mg (" ++ tshow p0 ++ ")"
      p <- expandDirectives p0
      target <- the' err =<< editing (Paths.queryTargets nid p)
      pure (arg, target)
    survivor <- editing (mergeTargets targets)
    changeLocation survivor
  Remove p0 -> do
    p <- expandDirectives p0
    nid <- currentLocation
    editing do
      selected <- Paths.removeSelectedEdges nid p
      when (null selected) $ noticeNoMatch "the path of rm" p0 nid
  RemoveNode p0 -> do
    p <- expandDirectives p0
    nid <- currentLocation
    editing do
      nids <- Paths.queryTargets nid p
      forM_ nids $ deleteNode @Text
  Tag p0 q0 -> do
    p <- expandConstruction p0
    q <- expandDirectives q0
    nid <- currentLocation
    let err = singleErr "the last argument of tag"
    prompt <- gets @History (view #now)
    editing do
      target <- the' err =<< Paths.queryTargets nid q
      newNodes <- Paths.constructTag prompt nid p target
      -- the target and the prefix's created nodes share one timestamp
      subsumeUserError @Missing $ tagNodesWithTime (insertSet target newNodes)
  Text t s -> do
    nid <- currentLocation
    -- text always adds a fresh data node, even if the label already exists
    editing do
      vNid <-
        (subsumeUserError . taggingFreshNodesWithTime)
          (transitionsFreshVia nid t)
      writeRawData vNid (encodeUtf8 s)
  Describe d -> runCommand' (Text "description" d)
  At p0 c -> do
    p <- expandDirectives p0
    nid <- currentLocation
    locations <- editing (Paths.queryTargets nid p)
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
    editing do
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
    n <- editing $ subsumeUserError (currentNode @Text)
    forM_ n.rawData $ subsumeUserError @Missing . displayImage . fromStrict
  -- it probably would make sense to factor these commands out into separate
  -- layers of commands that can be handled at different levels
  Import fp -> do
    guardDangerousDualizedOperation
    fp' <- subsumeUserError @IOError . untry $ canonicalizePath fp
    editing . subsumeUserError $ importDirectory fp' nilNID
    changeLocation nilNID
  ImportUrl uri -> editing (importUrlOnce uri) >>= changeLocation
  Debug -> do
    echo "current node:"
    currentLocation
      >>= editing . subsumeUserError . getNodeSem @Text
      >>= echo . unpack . nshowWith (\x -> x {showAugmentation = Nothing})
    echo "history:"
    get @History >>= echo . show
  Check -> editing $ reportToConsole @Text (fsck @Text)
  Fix -> editing $ fixErrors @Text (fsck @Text)
  Move p0 q0 -> do
    p <- expandDirectives p0
    q <- expandDirectives q0
    nid <- currentLocation
    let err = singleErr "the last argument of mv"
    editing do
      target <- the' err =<< Paths.queryTargets nid q
      selected <- Paths.moveSelectedEdges nid target p
      when (null selected) $ noticeNoMatch "the first argument of mv" p0 nid
  Rename parents p0 q0 -> do
    p <- expandDirectives p0
    q <- expandDirectives q0
    nid <- currentLocation
    editing (Paths.relabelPath True parents nid p q)
  Copy p0 q0 -> do
    p <- expandDirectives p0
    q <- expandDirectives q0
    nid <- currentLocation
    editing do
      target <-
        the' (singleErr "the last argument of cp") =<< Paths.queryTargets nid q
      selected <- Paths.copySelectedEdges nid target p
      when (null selected) $ noticeNoMatch "the first argument of cp" p0 nid
  Alias parents p0 q0 -> do
    p <- expandDirectives p0
    q <- expandDirectives q0
    nid <- currentLocation
    editing (Paths.relabelPath False parents nid p q)
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
    editing do
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
