module Graph.Command where

import Control.Monad (zipWithM_)
import DAL.DirectoryFormat (legacyNodeDataFile)
import DAL.FileSystemOperations.Metadata (GraphMetadataFilesystemOperations)
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
import Models.Path.ParsedPath
import Models.Path.Simple (Path)
import Models.Path.Simple qualified as Simple
import Models.PathPattern.Compile
  ( CopiedMeetingPoint (..),
    IdleJoin (..),
    TooManyAlternatives (..),
    UnionReading (..),
    WalksAcross (..),
    asRead,
    checkAlternatives,
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
    checkPath (plusWritten Relational p) p'
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
  checkPath (plusWritten plus p0) p
  pure p

-- | Edit the graph as one transaction: the edits are written to the graph
-- when the action finishes, and not at all if it fails.
editing ::
  (ProviderList_ GraphEditing () :> effs) =>
  Eff (GraphMetadataEditing : GraphDataEditing : effs) a ->
  Eff effs a
editing = provideList_ @GraphEditing

-- | The unions of a path that are written @+@, as an error about the
-- path shows them ('asRead') once @+@ is lowered to the given kind (or
-- to what an @%override@ around it says); those of them whose kind an
-- @%override({plus = …}, …)@ around them says, and not the command; and
-- whether the path has a @%targets(…)@, which is a @+@ of nodes, and
-- whether one is inside such an override.
data PlusWritten = PlusWritten
  { unions :: [Path],
    overridden :: [Path],
    targets :: Bool,
    targetsOverridden :: Bool
  }

plusWritten :: UnionKind -> ParsedPath -> PlusWritten
plusWritten = go False
  where
    go inOverride plus = \case
      p@(DefaultUnion () l r) ->
        let shown = toList (lowered plus p >>= asRead)
         in PlusWritten shown (if inOverride then shown else []) False False
              <> go inOverride plus l
              <> go inOverride plus r
      Directive _ (Targets _) -> PlusWritten [] [] True inOverride
      Directive _ (Override settings p) ->
        go (inOverride || isJust settings.plus) (fromMaybe plus settings.plus) p
      Directive _ _ -> mempty
      Backwards' p -> go inOverride plus p
      l ::/ r -> go inOverride plus l <> go inOverride plus r
      l ::& r -> go inOverride plus l <> go inOverride plus r
      Union _ l r -> go inOverride plus l <> go inOverride plus r
      _ -> mempty
    -- without the directives that need the graph
    lowered kind = handleDirectivesWith (Just kind) (\_ _ -> Nothing)

instance Semigroup PlusWritten where
  PlusWritten a b c d <> PlusWritten a' b' c' d' =
    PlusWritten (a <> a') (b <> b') (c || c') (d || d')

instance Monoid PlusWritten where
  mempty = PlusWritten [] [] False False

-- | Whether a union shown in an error was written with @+@: one of
-- those of the path, or a union of nodes where the path has a
-- @%targets(…)@.
writtenPlus :: PlusWritten -> Path -> Bool
writtenPlus written u =
  u `elem` written.unions || (written.targets && unionOfNodes u)

-- | Whether a union written with @+@ ('writtenPlus') has its kind from
-- an @%override({plus = …}, …)@ around it, and not from the command.
plusOverridden :: PlusWritten -> Path -> Bool
plusOverridden written u =
  u `elem` written.overridden
    || (u `notElem` written.unions && written.targetsOverridden && unionOfNodes u)

unionOfNodes :: Path -> Bool
unionOfNodes = all isNode . sidesOf
  where
    sidesOf = \case
      Union _ l r -> sidesOf l <> sidesOf r
      p -> [p]
    isNode = \case
      Simple.Absolute _ -> True
      _ -> False

-- | Fail readably, before a path is compiled for a command, if no
-- command runs it: a product in it would copy a meeting point
-- ('checkCopies'), or it would make more alternatives than its limit
-- allows. A path that is both is refused for the first, which no limit
-- cures. Warn, once for each path of a command line, about an @ that
-- does nothing because a product copies it. The first argument says
-- which unions of the path were written @+@.
checkPath ::
  (Members [Error UserError, Warn UserError, State ShownWarnings] effs) =>
  PlusWritten ->
  Path ->
  Eff effs ()
checkPath written p = case checkCopies p of
  Left refusal ->
    throwError . OtherError $ renderCopiedMeetingPoint written refusal
  Right idle -> do
    for_ (renderIdleJoins idle) \warning -> do
      ShownWarnings shown <- get
      unless (warning `member` shown) do
        put (ShownWarnings (insertSet warning shown))
        warnText warning
    checkAlternativesLimit p

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
      SideInPlace u side
        | isShared u ->
            " That is the path with "
              ++ quoted side
              ++ " chosen in "
              ++ quoted u
              ++ ( if writtenPlus written u
                     then " (written with +, which is ++ " ++ plusFrom u ++ ")"
                     else ""
                 )
              ++ ": a path is refused when one choice of its ++ unions is."
        | otherwise ->
            " That is how the path reads for a route that takes the side "
              ++ quoted side
              ++ " of "
              ++ quoted u
              ++ plusAs u
              ++ ": a path is refused when one way to take the sides of"
              ++ " its | unions is."
      AsOneStep u ->
        " Routes that take different sides of "
          ++ quoted u
          ++ plusAs u
          ++ " pass it as one step, so it keeps the two groups apart for"
          ++ " none of them; write ++ there to choose one side for the"
          ++ " whole path."
    plusAs u
      | writtenPlus written u =
          " (written with +, which is | " ++ plusFrom u ++ ")"
      | otherwise = ""
    -- what makes a + the kind of union it is
    plusFrom u
      | plusOverridden written u = "by the %override({plus = …}, …) around it"
      | otherwise = "in this command"
    isShared = \case
      Union Shared _ _ -> True
      _ -> False
    stepped = [u | AsOneStep u <- r.readings, writtenPlus written u]
    sided =
      [ u
      | SideInPlace u _ <- r.readings,
        not (isShared u),
        writtenPlus written u
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
      SideInPlace u side ->
        " (That is with the side " ++ quoted side ++ " of " ++ quoted u ++ ".)"
      AsOneStep u ->
        " (Routes that take different sides of "
          ++ quoted u
          ++ " pass it as one step.)"

-- | Fail readably if a path would make more alternatives than its limit
-- allows ('checkPath').
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
  Left (TooManyRoutes n) ->
    throwError . OtherError $
      "This path has too many routes to count the alternatives its | unions"
        ++ " make (the limit is "
        ++ tshow n
        ++ ", but its conjunctions alone multiply to more routes than are"
        ++ " listed to count, so no higher number helps). Write ++ for a"
        ++ " union whose side every copy shares, wrap the path in"
        ++ " %override({plus = shared}, …), narrow it, or lift the limit"
        ++ " with %override({max-alternatives = unlimited}, …), which"
        ++ " compiles it however long that takes. (TooManyRoutes)"

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
    $ runCommand (raise . raise . open) command

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
    forM_ (zip (inits (toList locations)) (toList locations)) $ \(done, nid') -> do
      result <- tryError @UserError $ local @NID (const nid') (runCommand open c)
      put history
      case result of
        Right () -> pure ()
        -- each target is a change of its own: what failed here was not
        -- written, what was done at the targets before it stays
        Left (_, e)
          | null done -> throwError e
          | otherwise ->
              throwError . (e <>) . OtherError $
                "Note: at had already run the command at "
                  ++ intercalate ", " (map tshow done)
                  ++ "; what it did there stays."
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
    -- if the graph is damaged where the node is read.
    editing do
      _ <- getNodeMetadata n
      editingDataOutside n
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
    -- fsck and fix fail when they find or leave damage, which is for the
    -- exit code: it doesn't stop the line (fsck ; fix must reach fix), and
    -- is kept for the end of it
    reports <- newIORef []
    forM_ (zip [0 :: Int ..] (toList ps)) $ \(before, command) ->
      -- each command is a change of its own: what failed was not written,
      -- what the commands before it did stays
      runCommand open command `catchError` \_ e ->
        if isDamageReport e
          then modifyIORef reports (e :)
          else do
            earlier <- reverse <$> readIORef reports
            throwError . sconcat . NonEmpty.prependList earlier . singleton $
              if before == 0
                then e
                else
                  e
                    <> OtherError
                      ( "Note: the "
                          ++ (if before == 1 then "command" else tshow before ++ " commands")
                          ++ " before this one in the line had already run; what "
                          ++ (if before == 1 then "it" else "they")
                          ++ " did stays."
                      )
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
