module Graph.Command where

import Control.Monad (zipWithM_)
import DAL.DirectoryFormat (legacyNodeDataFile)
import DAL.FileSystemOperations.Metadata
  ( GraphMetadataFilesystemOperations,
    onlyDescribesWrites,
  )
import DAL.RawGraph
import DAL.Serialization (doesNodeExist)
import Data.List.NonEmpty qualified as NonEmpty
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
import Graph.Dualizeable
import Graph.Fix (fix)
import Graph.FreshNID
import Graph.Fsck (fsck)
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
import Models.Damage
import Models.Edge
import Models.History
import Models.NID
import Models.Path.Parse (pPath)
import Models.Path.ParsedPath
import Models.Path.Simple (Path)
import Models.Path.Simple qualified as Simple
import Models.PathPattern.Compile
  ( CopiedMeetingPoint (..),
    IdleJoin (..),
    UnionReading (..),
    WalksAcross (..),
    checkCopies,
  )
import Models.PathPattern.Match
  ( ConcreteHop (..),
    ConcreteRoute (..),
    routeLastEdges,
    selectedRoutes,
  )
import Models.PathPattern.Suggest (renderSurfacePath)
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

-- | The warnings about paths that the current command line has shown, so
-- that a path that is expanded more than once (inside @at@, or written
-- twice) is warned about once.
newtype ShownWarnings = ShownWarnings (Set Text)

interpretDirective ::
  ( Members
      [ GraphMetadataReading,
        State History,
        State ImportedUrls,
        State EmptyTargets,
        State ShownWarnings,
        Warn UserError,
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
    checkPath (plusWritten p) p'
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
        State ShownWarnings,
        Warn UserError,
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
        State ShownWarnings,
        Warn UserError,
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
        State ShownWarnings,
        Warn UserError,
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
  checkPath (plusWritten p0) p
  pure p

-- | Edit the graph as one transaction: the edits are written to the graph
-- when the action finishes, and not at all if it fails.
editing ::
  (ProviderList_ GraphEditing () :> effs) =>
  Eff (GraphMetadataEditing : GraphDataEditing : effs) a ->
  Eff effs a
editing = provideList_ @GraphEditing

-- | Where the path as expanded has a union written @+@, and where it has
-- what a @%targets(…)@ became, a @+@ of nodes; each with whether an
-- @%override({plus = …}, …)@ around it, and not the command, says what
-- kind of union that @+@ is. A union is told by where it is, not by what
-- it looks like: a @|@ of nodes can be typed beside a @%targets(…)@.
data PlusWritten = PlusWritten
  { unions :: [(Place, Bool)],
    targets :: [(Place, Bool)]
  }

plusWritten :: ParsedPath -> PlusWritten
plusWritten p =
  PlusWritten
    { unions = placesIn (\overridden -> \case DefaultUnion {} -> Just overridden; _ -> Nothing) p,
      targets = placesIn (\overridden -> \case Directive _ (Targets _) -> Just overridden; _ -> Nothing) p
    }

-- | For a union shown in an error, at a place of the path as expanded:
-- whether it was written with @+@ (typed so, or inside what a
-- @%targets(…)@ became), and if so, whether an @%override({plus = …}, …)@
-- around it says what kind of union it is.
plusAt :: PlusWritten -> Place -> Maybe Bool
plusAt written place =
  lookup place written.unions
    <|> (snd <$> find ((`isPrefixOf` place) . fst) written.targets)

-- | Whether a union shown in an error was written with @+@ ('plusAt').
writtenPlus :: PlusWritten -> Place -> Bool
writtenPlus written = isJust . plusAt written

-- | Whether a union written with @+@ ('writtenPlus') has its kind from
-- an @%override({plus = …}, …)@ around it, and not from the command.
plusOverridden :: PlusWritten -> Place -> Bool
plusOverridden written place = plusAt written place == Just True

-- | What a path had as typed that it does not show once its directives
-- are expanded, for what a refusal of its shape suggests. The path is one
-- that mk or tag builds: a + is a ++ unless an override says otherwise.
asTyped :: ParsedPath -> Paths.AsTyped
asTyped p0 =
  Paths.AsTyped
    { override = anywhere overrides p0,
      targets = directivePlaces targets' p0,
      relationalPlus = relational Shared p0
    }
  where
    -- whether a directive of the path (outside a %targets(…), whose path
    -- is only looked up) is one of these
    anywhere :: (Directive 'WithDirectives -> Bool) -> ParsedPath -> Bool
    anywhere ok = \case
      Directive _ d@(Override _ p) -> ok d || anywhere ok p
      Directive _ d -> ok d
      Backwards' p -> anywhere ok p
      l ::/ r -> anywhere ok l || anywhere ok r
      l ::& r -> anywhere ok l || anywhere ok r
      Union _ l r -> anywhere ok l || anywhere ok r
      DefaultUnion () l r -> anywhere ok l || anywhere ok r
      _ -> False
    overrides = \case
      Override _ _ -> True
      _ -> False
    targets' = \case
      Targets _ -> True
      _ -> False
    -- whether a + of the path is read as |, given how a + is read here
    relational plus = \case
      DefaultUnion () l r -> plus == Relational || relational plus l || relational plus r
      Directive _ (Override settings p) -> relational (fromMaybe plus settings.plus) p
      Directive _ _ -> False
      Backwards' p -> relational plus p
      l ::/ r -> relational plus l || relational plus r
      l ::& r -> relational plus l || relational plus r
      Union _ l r -> relational plus l || relational plus r
      _ -> False

spec_asTyped :: Spec
spec_asTyped =
  it "has the places where the path as expanded has what each %targets(…) became" do
    let -- with each %targets(…) expanded to this mark
        marked p = handleDirectivesWith (Just Shared) (\_ -> mark) p
        mark = \case
          Targets _ -> Just (Literal "%targets")
          _ -> Nothing
        partAt :: Place -> Path -> Maybe Path
        partAt place p = case (place, p) of
          ([], _) -> Just p
          (0 : rest, Simple.Backwards q) -> partAt rest q
          (i : rest, l Simple.:/ r) -> partAt rest =<< [l, r] `index` i
          (i : rest, l Simple.:& r) -> partAt rest =<< [l, r] `index` i
          (i : rest, Simple.Union _ l r) -> partAt rest =<< [l, r] `index` i
          _ -> Nothing
        marksAt t = do
          p <- parseForTest "path" pPath t
          let places = (asTyped p).targets
          pure
            ( length places,
              all (\place -> (marked p >>= partAt place) == Just (Literal "%targets")) places
            )
    -- how many %targets each has, outside a %targets
    for_
      [ ("(%targets(a/b) | @000000000001)/(c & d)", 1),
        ("(%never ++ @000000000001 ++ @000000000002)/c & %targets(a)", 1),
        ("%override({plus = relational}, %targets(a)/b)", 1),
        ("%override({plus = shared}, x/%targets(a) & ~(%targets(b)/c))", 2),
        ("%override({}, %override({plus = relational}, ~(%targets(a)) + b) & %targets(c))", 2),
        ("%targets(%targets(a))/b", 1),
        ("(a ++ b)/c", 0)
      ]
      \(t, n) -> marksAt t `shouldReturn` (n, True)

-- | Fail readably, before a path is compiled for a command, if no
-- command runs it: a product in it would copy a meeting point
-- ('checkCopies'). Warn, once for each path of a command line, about an
-- @ that does nothing because a product copies it. The first argument
-- says which unions of the path were written @+@. Nothing here looks at
-- how large the path is: a path that is run is compiled however long
-- that takes (docs/path-cost-limits.md).
checkPath ::
  (Members [Error UserError, Warn UserError, State ShownWarnings] effs) =>
  PlusWritten ->
  Path ->
  Eff effs ()
checkPath written p = case checkCopies p of
  Left refusal ->
    throwError . OtherError $ renderCopiedMeetingPoint written refusal
  Right idle ->
    for_ (renderIdleJoins idle) \warning -> do
      ShownWarnings shown <- get
      unless (warning `member` shown) do
        put (ShownWarnings (insertSet warning shown))
        warnText warning

-- | A path as part of a sentence: as it would be typed, with parentheses
-- around a conjunction or a union.
quoted :: Path -> Text
quoted p = "`" ++ shown ++ "`"
  where
    shown = case p of
      _ Simple.:& _ -> "(" ++ renderSurfacePath p ++ ")"
      Union {} -> "(" ++ renderSurfacePath p ++ ")"
      _ -> renderSurfacePath p

-- | A path as part of a sentence, as it would be typed.
plain :: Path -> Text
plain p = "`" ++ renderSurfacePath p ++ "`"

-- | Why no command runs a path in which a product would copy a meeting
-- point, and what to write instead. Commands before it on the line have
-- run, and the directives of the path are expanded already (a URL in it
-- is imported), so the error says only that this command was not run.
renderCopiedMeetingPoint :: PlusWritten -> CopiedMeetingPoint -> Text
renderCopiedMeetingPoint written r =
  "This path would copy a meeting point, which has no clear meaning, so"
    ++ " the command was not run. "
    ++ ( case r.across of
           Nothing ->
             "In "
               ++ quoted r.meeting
               ++ " several walks meet at the "
               ++ quoted r.join
               ++ " or leave it"
               ++ ( if r.copied == r.meeting
                      then ""
                      else ", inside " ++ quoted r.copied
                  )
               ++ ", and "
               ++ quoted r.copier
               ++ " would copy it, once for each of its conjuncts: it"
               ++ " stands next to the group "
               ++ quoted r.group
               ++ " with no join of their chain between the two."
           Just walks ->
             quoted r.copier
               ++ " would copy "
               ++ quoted r.meeting
               ++ ", once for each of its conjuncts: it stands next to"
               ++ " the group "
               ++ quoted r.group
               ++ " with no join of their chain between the two. In the"
               ++ " copy for "
               ++ quoted walks.from
               ++ ", which holds several walks together, those walks "
               ++ (if walks.arriving then "meet at" else "all leave from")
               ++ " the "
               ++ quoted r.join
               ++ " of "
               ++ quoted r.meeting
               ++ ", and in the other copies they do not."
       )
    ++ concatMap reading r.readings
    ++ " If the walks should come together between the two groups, put a"
    ++ " join between them"
    ++ maybe " (/@)" ((": " ++) . plain) r.joinedFirst
    ++ ". Otherwise write the copies out, one conjunct for each pair"
    ++ maybe "" ((": " ++) . plain) r.writtenApart
    ++ "."
    ++ plusNote
    ++ " (CopiedMeetingPoint)"
  where
    reading = \case
      SideInPlace u side here
        | isShared u ->
            " That is the path with "
              ++ quoted side
              ++ " chosen in "
              ++ quoted u
              ++ ( if writtenPlus written here
                     then " (written with +, which is ++ " ++ plusFrom here ++ ")"
                     else ""
                 )
              ++ ": a path is refused when one choice of its ++ unions is."
        | otherwise ->
            " That is how the path reads for a route that takes the side "
              ++ quoted side
              ++ " of "
              ++ quoted u
              ++ plusAs here
              ++ ": a path is refused when one way to take the sides of"
              ++ " its | unions is."
      AsOneStep u here ->
        " Routes that take different sides of "
          ++ quoted u
          ++ plusAs here
          ++ " pass it as one step, so it keeps the two groups apart for"
          ++ " none of them; write ++ there to choose one side for the"
          ++ " whole path."
    plusAs here
      | writtenPlus written here =
          " (written with +, which is | " ++ plusFrom here ++ ")"
      | otherwise = ""
    -- what makes a + the kind of union it is
    plusFrom here
      | plusOverridden written here = "by the %override({plus = …}, …) around it"
      | otherwise = "in this command"
    isShared = \case
      Union Shared _ _ -> True
      _ -> False
    stepped = [here | AsOneStep _ here <- r.readings, writtenPlus written here]
    sided =
      [ here
      | SideInPlace u _ here <- r.readings,
        not (isShared u),
        writtenPlus written here
      ]
    -- what else makes a + a ++: an override around the path, or, where
    -- the + is in one that makes it a | already, that one saying shared
    sharedInstead us
      | any (plusOverridden written) us =
          "say plus = shared in the %override around it"
      | otherwise = "wrap the path in %override({plus = shared}, …)"
    plusNote
      | not (null stepped) =
          " For that +, write ++ instead, or "
            ++ sharedInstead stepped
            ++ "."
      | not (null sided) =
          " Writing ++ for that +, or "
            ++ ( if any (plusOverridden written) sided
                   then "plus = shared in the %override around it"
                   else "%override({plus = shared}, …)"
               )
            ++ ", does not help here, since the path is refused with that"
            ++ " side alone."
      | otherwise = ""

spec_renderCopiedMeetingPoint :: Spec
spec_renderCopiedMeetingPoint =
  it "says that a union was written with + only where a + or a %targets(…) was" do
    let -- the message for a path that ls refuses, with each %targets(…)
        -- finding @000000000001 and @000000000002
        message t = do
          p0 <- parseForTest "path" pPath t
          let found = \case
                Targets _ ->
                  Just (foldl' (\u n -> DefaultUnion () u (Absolute (smallNID n))) Zero [1, 2])
                _ -> Nothing
          p <- maybe (fail "a directive other than %targets") pure (handleDirectivesWith (Just Relational) (const found) p0)
          pure (either (Just . renderCopiedMeetingPoint (plusWritten p0)) (const Nothing) (checkCopies p))
        noted t = fmap (isInfixOf "(written with +") <$> message t
        twoGroups u = "(a & b)/" <> u <> "/((c & y)/@/e & d)"
    for_
      [ -- what a %targets(…) became, and a + typed
        (twoGroups "%targets(t)", True),
        (twoGroups "(@000000000001 + @000000000002)", True),
        (twoGroups "~(x/%targets(t))", True),
        (twoGroups "(@000000000001 + @000000000002)" <> " & (@000000000001 | @000000000002)", True),
        -- a | typed, though a %targets(…) or a + elsewhere in the path
        -- becomes a union written the same
        (twoGroups "(@000000000001 | @000000000002)" <> " & %targets(t)", False),
        ("%override({plus = shared}, " <> twoGroups "(@000000000001 | @000000000002)" <> " & %targets(t))", False),
        (twoGroups "(@000000000001 | @000000000002)" <> " & (@000000000001 + @000000000002)", False),
        (twoGroups "~(x/(@000000000001 | @000000000002))" <> " & ~(x/%targets(t))", False)
      ]
      \(t, plus) -> do
        said <- noted t
        (t, said) `shouldBe` (t, Just plus)
    -- and an %override that says what a + is, around what a %targets(…)
    -- became but not around a | typed
    let overridden = "by the %override({plus = …}, …) around it"
    fmap (isInfixOf overridden)
      <$> message ("%override({plus = relational}, " <> twoGroups "%targets(t)" <> ")")
        `shouldReturn` Just True
    fmap (isInfixOf overridden)
      <$> message (twoGroups "(@000000000001 | @000000000002)" <> " & %override({plus = relational}, %targets(t))")
        `shouldReturn` Just False

-- | The warning about the joins of a path that do nothing, if it has any.
renderIdleJoins :: [IdleJoin] -> Maybe Text
renderIdleJoins = \case
  [] -> Nothing
  j : more ->
    Just $
      "Warning: the @ that "
        ++ ( case (j.begins, j.ends) of
               (True, False) -> "begins "
               (False, True) -> "ends "
               _ -> "begins and ends "
           )
        ++ quoted j.conjunct
        ++ " does nothing here: "
        ++ quoted j.copier
        ++ " copies "
        ++ quoted j.conjunct
        ++ " whole, once for each of its conjuncts, so the @ joins nothing"
        ++ " and pins nothing."
        ++ concatMap reading j.readings
        ++ ( case j.pinned of
               Just p ->
                 " If one node was meant, write "
                   ++ plain p
                   ++ "; otherwise leave the @ out."
               Nothing ->
                 " An @ pins only in the chain it is written in, as in"
                   ++ " x/@/(c & d): leave this one out, or write out"
                   ++ " what it should join."
           )
        ++ ( case length more of
               0 -> ""
               1 -> " The same holds for one more @ in this path."
               n -> " The same holds for " ++ tshow n ++ " more @ in this path."
           )
        ++ " (JoinDoesNothing)"
  where
    reading = \case
      SideInPlace u side _ ->
        " (That is with the side " ++ quoted side ++ " of " ++ quoted u ++ ".)"
      AsOneStep u _ ->
        " (Routes that take different sides of "
          ++ quoted u
          ++ " pass it as one step.)"

type CommandEffects effs =
  ( Members [DisplayImage, Echo, Error UserError, SetLocation, GetLocation, Dualizeable] effs,
    Members [FileSystem, Web, FreshNID, GetTime, Editor, State History] effs,
    Members [FileTypeOracle, Readline, Warn UserError, ProviderList_ GraphEditing ()] effs,
    -- TODO: remove this inclusion of RawGraph + IOE here; probably the
    -- best way to do this is to allow commands to be defined as @stack@
    -- scripts, and then rewrite resolve and any other commands that
    -- require this outside of
    Members [RawGraph, IOE, State DamageReadPast] effs,
    -- for fix, which repairs node files that no transaction accepts
    GraphMetadataFilesystemOperations :> effs
  )

-- | Run one command line. Damage that the commands of the line read past is
-- warned of once, when the line is done (see 'warningOfDamageReadPast').
--
-- The first argument is asked to make the graph ready before each command
-- that needs it open ('needsOpenGraph'), and fails if it can't. That is not
-- done for the line as a whole: @fsck@ and @fix@ run on a graph that can't be
-- opened, and a command after @fix@ finds the graph repaired.
interpretCommand ::
  (CommandEffects effs) => (Opening -> Eff effs ()) -> Command -> Eff effs ()
interpretCommand open command =
  warningOfDamageReadPast
    . evalStateLocal (ImportedUrls mempty)
    . evalStateLocal (ShownWarnings mempty)
    $ do
      -- An interrupt is no error and says nothing by itself: when it stops
      -- a line that has changed files of the graph, the user is told so,
      -- unless a part of the line has said it more exactly (at, a ; line,
      -- fix).
      start <- graphFilesChanged
      notingWhatInterruptLeaves
        ( untoldGraphChanges start <&> \untold ->
            [interruptedAfterChanging | untold > 0]
        )
        (runCommand (raise . raise . open) command)

-- | What an interrupted command line says when files of the graph were
-- changed before the interrupt stopped it. Whether they were is known from
-- where they are changed ('untoldGraphChanges'), not from what the command
-- is: a command that only reads says nothing, and one that reads a URL has
-- imported it.
interruptedAfterChanging :: Text
interruptedAfterChanging =
  "Note: the command that was interrupted had already changed the graph;"
    <> " what it did stays."

-- | What the commands ask of whoever opens the graph for them
data Opening
  = -- | Make the graph ready for commands, if that hasn't been done
    EnsureOpen
  | -- | Do so again for the next command that needs it: the node files were
    -- repaired, and what opening adds to the graph may be missing again
    OpenAgain
  deriving stock (Eq, Show)

-- | Run a command, noting in its error any @%targets(…)@ that it expanded
-- to nothing (it then means @%never@, which is easy to miss).
--
-- A command is one decision ('asOneDecision'): once it has read past damage,
-- in resolving its arguments or the targets of @at@, say, it writes nothing.
runCommand ::
  (CommandEffects effs, Members [State ImportedUrls, State ShownWarnings] effs) =>
  -- | open the graph (see 'interpretCommand')
  (Opening -> Eff effs ()) ->
  Command ->
  Eff effs ()
runCommand open c = evalStateLocal (EmptyTargets []) do
  -- no part of the command's decision: the REPL has opened the graph before
  -- it runs any command
  when (needsOpenGraph c) (raise (open EnsureOpen))
  result <- tryError @UserError (deciding (runCommand' (raise . open) c))
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
  where
    -- Each command of a @;@ line is a decision of its own, and so is a
    -- command that is no part of another one. What a command runs (@at@ its
    -- command at each target) is part of its decision.
    deciding = case c of
      Seq _ -> id
      _ -> asOneDecision

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
      systemNodes = systemNodesAmong nids
      systemError =
        throwError
          . systemNodeError "mg cannot merge a system node away" "keep their NIDs"
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

-- | The system nodes among some nodes ('systemNodeNIDs': the origin and the
-- special nodes such as tags). @ge@ needs them to open the graph, so no
-- command takes one out of it: @mg@ never merges one away and @rmnf@ doesn't
-- delete one.
systemNodesAmong :: [NID] -> [NID]
systemNodesAmong = filter (`member` systemNodeNIDs)

-- | The error of a command that would take a system node out of the graph:
-- what it can't do, what the system nodes must do instead, and which nodes
-- of the command this is about.
systemNodeError :: Text -> Text -> Text -> UserError
systemNodeError cannot must which =
  OtherError $
    cannot
      ++ " (the origin and the special nodes such as tags must "
      ++ must
      ++ ")\n"
      ++ which

spec_systemNodeError :: Spec
spec_systemNodeError =
  it "says what can't be done to a system node, why, and to which" $
    lines
      ( show $
          systemNodeError
            "mg cannot merge a system node away"
            "keep their NIDs"
            "more than one system node: @000000000000 (a), @0daCJjMrQel8 (b)"
      )
      `shouldBe` [ "mg cannot merge a system node away (the origin and the"
                     <> " special nodes such as tags must keep their NIDs)",
                   "more than one system node: @000000000000 (a),"
                     <> " @0daCJjMrQel8 (b)"
                 ]

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

-- | Say how many nodes and edges @mk@ created; in a dry run, how many it
-- would create.
reportConstructed ::
  (Members [Echo, GraphMetadataFilesystemOperations] effs) =>
  Paths.Constructed ->
  Eff effs ()
reportConstructed made = do
  dry <- onlyDescribesWrites
  echo . unpack $
    (if dry then "would create " else "created ")
      ++ counted (length made.nodes) "node"
      ++ " and "
      ++ counted made.edges "edge"
  where
    counted :: Int -> Text -> Text
    counted k what = tshow k ++ " new " ++ what ++ (if k == 1 then "" else "s")

-- | What @at@ fails with when targets of its path have no node file, given
-- the edges that the path can end with: for each such target the edges that
-- lead to it, as the damage that they are.
targetsWithoutFile :: NonEmpty NID -> Set (Edge Text) -> UserError
targetsWithoutFile missing lastEdges =
  TargetsWithoutFile $
    missing <&> \target -> (target, mapMaybe (leadingTo target) (toList lastEdges))
  where
    -- the other node of the edge records it: that is how the path got here
    leadingTo target e
      | e.sink == target = Just (EdgeToMissingNode OutgoingRecord e)
      | e.source == target = Just (EdgeToMissingNode IncomingRecord e)
      | otherwise = Nothing

spec_targetsWithoutFile :: Spec
spec_targetsWithoutFile = do
  let n = smallNID
      b = Edge (n 0) "b" (n 2)
  it "names the edge that leads to a target without a file, and its file" $
    lines
      ( show $
          targetsWithoutFile
            (n 2 :| [])
            (setFromList [b, Edge (n 0) "a" (n 1)])
      )
      `shouldBe` [ "at did nothing: a target has no node file (damaged-graph)",
                   "- the edge @000000000000 -\"b\"-> @000000000002 is among"
                     <> " the outgoing edges in 000000000000.json but there is"
                     <> " no 000000000002.json",
                   "fsck lists all such damage; fix repairs it, or says why it"
                     <> " can't"
                 ]
  it "names an edge that was followed backwards by the file that records it" $
    lines
      (show (targetsWithoutFile (n 2 :| []) (singletonSet (Edge (n 2) "d" (n 1)))))
      `shouldBe` [ "at did nothing: a target has no node file (damaged-graph)",
                   "- the edge @000000000002 -\"d\"-> @000000000001 is among"
                     <> " the incoming edges in 000000000001.json but there is"
                     <> " no 000000000002.json",
                   "fsck lists all such damage; fix repairs it, or says why it"
                     <> " can't"
                 ]
  it "says which targets no edge led to" $
    lines (show (targetsWithoutFile (n 2 :| [n 3]) (singletonSet b)))
      `shouldBe` [ "at did nothing: targets have no node file (damaged-graph)",
                   "- the edge @000000000000 -\"b\"-> @000000000002 is among"
                     <> " the outgoing edges in 000000000000.json but there is"
                     <> " no 000000000002.json",
                   "- there is no 000000000003.json, the file of the target"
                     <> " @000000000003",
                   "fsck lists all such damage; fix repairs it, or says why it"
                     <> " can't"
                 ]
  it "is a refusal for the damage that it names, like that of a change" do
    -- so that a line that read past that damage doesn't warn of it as well
    let refusal = targetsWithoutFile (n 2 :| [n 3]) (singletonSet b)
        damage = singletonSet (EdgeToMissingNode OutgoingRecord b)
    damageRefusedBy refusal `shouldBe` damage
    -- as the error of a later command of a line, which says so in a note
    damageRefusedBy (refusal <> OtherError "Note: the command before…")
      `shouldBe` damage
    damageRefusedBy
      (DamagedGraph (EdgeToMissingNode OutgoingRecord b :| []) <> OtherError "Note")
      `shouldBe` damage
    damageRefusedBy (OtherError "another failure") `shouldBe` mempty

-- | Whether an error is how @fsck@ or @fix@ says that it found or left
-- damage. They fail for the exit code, having done all they do.
isDamageReport :: UserError -> Bool
isDamageReport = \case
  DamageFound _ -> True
  DamageNotRepaired _ -> True
  _ -> False

runCommand' ::
  ( CommandEffects effs,
    Members [State ImportedUrls, State EmptyTargets, State ShownWarnings] effs
  ) =>
  -- | open the graph (see 'interpretCommand')
  (Opening -> Eff effs ()) ->
  Command ->
  Eff effs ()
runCommand' open = \case
  ChangeNode p0 -> do
    p <- expandDirectives p0
    nid <- currentLocation
    let err = singleErr "cd"
    nid' <- editing do
      target <- the' err =<< Paths.queryTargets nid p
      -- every later command would start from a node that isn't there; an
      -- edge can only lead to one in a damaged graph
      _ <- subsumeUserError @Missing (getNodeSem @Text target)
      pure target
    changeLocation nid'
  NodeId -> currentLocation >>= echo . show
  Dualize -> dualize
  Make maxRepairs p0 -> do
    p <- expandConstruction p0
    nid <- currentLocation
    prompt <- gets @History (view #now)
    made <- editing do
      made <- Paths.constructPath maxRepairs prompt nid (asTyped p0) p
      made <$ subsumeUserError @Missing (tagNodesWithTime made.nodes)
    reportConstructed made
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
      -- like mg, which never merges one away; nothing is deleted then
      unless (null (systemNodesAmong (toList nids))) . throwError $
        systemNodeError
          "rmnf cannot delete a system node"
          "stay in the graph"
          ( "the path of rmnf ("
              ++ tshow p0
              ++ ") resolves to "
              ++ intercalate ", " (map tshow (systemNodesAmong (toList nids)))
          )
      forM_ nids $ deleteNode @Text
  Tag maxRepairs p0 q0 -> do
    p <- expandConstruction p0
    q <- expandDirectives q0
    nid <- currentLocation
    let err = singleErr "the last argument of tag"
    prompt <- gets @History (view #now)
    editing do
      target <- the' err =<< Paths.queryTargets nid q
      newNodes <-
        Paths.constructTag maxRepairs prompt nid (asTyped p0) p target
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
  Describe d -> runCommand' open (Text "description" d)
  At p0 c -> do
    p <- expandDirectives p0
    nid <- currentLocation
    locations <- editing (Paths.queryTargets nid p)
    -- A command run at a node that has no file would mostly do nothing and
    -- say nothing. Only whether there is anything under the file's name is
    -- looked at (what is there may not be usable as a node, which the command
    -- finds out when it reads it): reading the targets here as well would
    -- read each of them twice.
    graphDir <- getGraphFilePath
    missing <- filterM (fmap not . doesNodeExist graphDir) (toList locations)
    for_ (NonEmpty.nonEmpty missing) \missing' -> do
      -- the path is resolved once more for the edges that led to them; this
      -- only happens on a damaged graph
      lastEdges <-
        editing $
          foldMap (fold . routeLastEdges) <$> Paths.resolveCommandQuery nid p
      throwError (targetsWithoutFile missing' lastEdges)
    -- c's directives are expanded per target, relative to that target,
    -- and each target starts from the same history; at leaves the history
    -- (and so the location) as it found it
    history <- get @History
    let alreadyRan done =
          "Note: at had already run the command at "
            ++ intercalate ", " (map tshow done)
            ++ "; what it did there stays."
    -- The targets that the command was started at, the last one first,
    -- each with the number of files of the graph that had been changed when
    -- it was.
    started <- newIORef []
    start <- graphFilesChanged
    let -- An interrupt is no error, and it can stop the command at a target
        -- where it has written already: the note names every target at
        -- which something was written, that one too. (What a target
        -- changed is what was changed until the next one was started.)
        changedWhenInterrupted = do
          now <- graphFilesChanged
          _ <- untoldGraphChanges start
          -- a target that an at in the command has named already is not
          -- named again: the changes up to this count are not named
          unnamedUpTo <- (start +) <$> unnamedAtTargetChanges start
          starts <- readIORef started
          let changedAt =
                [ nid'
                | ((nid', since), upTo) <- zip starts (now : map snd starts),
                  min upTo unnamedUpTo > since
                ]
          pure
            [ "Note: at had already changed the graph at "
                ++ intercalate ", " (map tshow (reverse changedAt))
                ++ "; what it did there stays."
            | not (null changedAt)
            ]
    notingWhatInterruptLeaves changedWhenInterrupted $
      forM_ (zip (inits (toList locations)) (toList locations)) $ \(done, nid') -> do
        since <- graphFilesChanged
        modifyIORef started ((nid', since) :)
        -- also when the command is interrupted: the REPL goes on where it
        -- was
        result <-
          tryError @UserError (local @NID (const nid') (runCommand open c))
            `finally` put history
        case result of
          Right () -> pure ()
          -- each target is a change of its own: what was done at the targets
          -- before it stays, and what failed here was not written, unless
          -- its error says what of it was (a change that could be written
          -- only in part, data that could not be changed)
          Left (_, e)
            | null done -> throwError e
            | otherwise -> throwError (e <> OtherError (alreadyRan done))
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
  Check -> fsck
  Fix mode -> do
    result <- tryError @UserError (fix mode)
    -- Neither the node files that a command which fix is part of read
    -- before it nor the damage that it read past are as it read them: what
    -- fix didn't repair is found again by a later step that reads its files.
    startDecisionAfresh
    open OpenAgain
    either (throwError . snd) pure result
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
    -- The editor writes the data file itself, but this is a change of the
    -- node's data like any other: it is refused, before the editor starts,
    -- if the graph is damaged where the node is read, and if the node has no
    -- file (it was deleted while it was the current node): the data would
    -- belong to no node.
    editing do
      _ <- subsumeUserError @Missing (getNodeSem @Text n)
      editingDataOutside n
    path <- getGraphFilePath
    writes <- getGraphWrites
    -- counted as a change when the editor has changed the file, which an
    -- interrupted line then says, as for any other change
    let file = legacyNodeDataFile path n
    changingGraphFileOutside writes file (invokeEditor [file])
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
    -- fsck and fix fail when they find or leave damage, which is for the
    -- exit code: it doesn't stop the line (fsck ; fix must reach fix), and
    -- is kept for the end of it
    reports <- newIORef []
    let alreadyRan before =
          "Note: the "
            ++ (if before == 1 then "command" else tshow before ++ " commands")
            ++ " before this one in the line had already run; what "
            ++ (if before == 1 then "it" else "they")
            ++ " did stays."
    -- how many commands of the line have run, and how many files of the
    -- graph had been changed when the one that is running began
    start <- graphFilesChanged
    running <- newIORef (0 :: Int, start)
    let -- An interrupt is no error. It gets a note when files were changed:
        -- by the command that it stopped, which can have written already
        -- (unless that command has said so itself), and by the commands
        -- before it.
        changedWhenInterrupted = do
          (before, since) <- readIORef running
          byThisOne <- untoldGraphChanges since
          byEarlier <- untoldGraphChanges start
          pure $
            [interruptedAfterChanging | byThisOne > 0]
              <> [alreadyRan before | byEarlier > 0]
    notingWhatInterruptLeaves changedWhenInterrupted $
      forM_ (zip [0 :: Int ..] (toList ps)) $ \(before, command) -> do
        since <- graphFilesChanged
        writeIORef running (before, since)
        -- each command is a change of its own: what the commands before it
        -- did stays, and what failed was not written, unless its error says
        -- what of it was (a change that could be written only in part, data
        -- that could not be changed)
        runCommand open command `catchError` \_ e ->
          if isDamageReport e
            then modifyIORef reports (e :)
            else do
              earlier <- reverse <$> readIORef reports
              throwError . sconcat . NonEmpty.prependList earlier . singleton $
                if before == 0
                  then e
                  else e <> OtherError (alreadyRan before)
    readIORef reports
      >>= traverse_ (throwError . sconcat) . NonEmpty.nonEmpty . reverse
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
