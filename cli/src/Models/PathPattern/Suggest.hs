-- | Paths that choose one repair of an ambiguous construction
-- (docs/path-route-ir.md, Residuation, @mk@).
--
-- For a surviving repair, keep only the @+@ alternative it came from, then
-- pin with @/\@N@ every step whose output it binds to an existing node, and
-- exclude with @/!{…}@ the nodes other survivors bind a step's output to
-- where it creates one. Rewrites that are not needed are dropped again. A
-- candidate is only suggested if residuating it gives exactly that repair,
-- so a suggested path is never wrong; when none is confirmed there is none.
module Models.PathPattern.Suggest
  ( suggestPaths,
    suggestPathsReference,
    renderSurfacePath,
    parseSurfacePath,
    spec_suggestPath,
    test_renderSurfacePath,
  )
where

import Control.Monad.State.Strict qualified as S
import Data.Functor.Const (Const (..))
import Data.Map.Strict qualified as Map
import Graph.SystemNodes (tagsNID)
import Models.Graph (Graph, maybeNodeLookup)
import Models.NID
import Models.Path.Parse (pPath)
import Models.Path.Simple
import Models.Path.TH (path)
import Models.PathPattern
import Models.PathPattern.Compile (compileAlternatives, compilePath)
import Models.PathPattern.Residuate
import Models.PathPattern.TestCorpus
import MyPrelude
import Utils.Parsing (isIdentChar, whitespace)
import Utils.Parsing.Common (eof, runParser)
import Utils.Testing

-- | What to add after a step.
data Rewrite = Pin NID | Exclude (Set NID)

-- | For each of the given survivors, a path that residuates to exactly it
-- (modulo renaming of created nodes), if one is found, given every survivor
-- of the same path. The lookup and chooser must be the ones the survivors
-- were residuated with.
--
-- The path's alternatives are compiled once, each with a choice of @+@
-- sides that gives it ('compileAlternatives'), until those of the given
-- survivors are found: at most about the compilation residuation already
-- did. Each suggestion then
-- residuates paths without @+@, at most 'maxResiduations' times.
--
-- The suggestions are those of 'suggestPathsReference'. Two things spare
-- residuations, which cost as much as the one that gave the survivors
-- when a candidate path pins little:
--
-- * A candidate path is residuated once, whichever survivors it is tried
--   for: its repair, if it has exactly one, is kept by what it compiles to.
-- * A candidate that compiles to just an alternative that two or more of
--   the survivors are of is not residuated: it has those repairs still
--   ('stillAmbiguous').
suggestPaths ::
  forall m.
  (Monad m) =>
  NodeLookup m ->
  LabelChooser m ->
  NID ->
  Path ->
  NonEmpty Residual ->
  [Residual] ->
  m [Maybe Path]
suggestPaths lookupNode chooser cur surface survivors =
  \wanted -> flip S.evalStateT mempty do
    let bases = choicesFor (setFromList (map (.patternPath) wanted))
    for wanted \survivor ->
      case lookup survivor.patternPath bases of
        Nothing -> pure Nothing
        Just base -> suggestFrom survivor base
  where
    -- a choice for each wanted alternative, stopping once all are found
    choicesFor :: Set PatternPath -> Map PatternPath Path
    choicesFor = go mempty (compileAlternatives surface)
      where
        go found _ wanted | null wanted = found
        go found [] _ = found
        go found ((alternative, choice) : rest) wanted
          | alternative `member` wanted =
              go
                (insertMap alternative choice found)
                rest
                (deleteSet alternative wanted)
          | otherwise = go found rest wanted
    suggestFrom survivor base = do
      let rewrites = stepRewrites survivors survivor base
          candidate keep =
            reassociate (applyRewrites (Map.restrictKeys rewrites keep) base)
          dropIfUnneeded keep i = do
            let keep' = deleteSet i keep
            ok <- confirms survivor (candidate keep')
            pure (if ok then keep' else keep)
          -- within the budget; rewrites beyond it are kept
          droppable = take (maxResiduations - 1) (keys rewrites)
      ok <- confirms survivor (candidate (keysSet rewrites))
      if ok
        then
          Just . candidate
            <$> foldM dropIfUnneeded (keysSet rewrites) droppable
        else pure Nothing
    -- the text shown parses back to the path, and the path has exactly this
    -- repair
    confirms :: Residual -> Path -> Confirming m Bool
    confirms survivor p
      | parseSurfacePath (renderSurfacePath p) /= Just p = pure False
      | otherwise =
          onlyRepair (compilePath p) <&> \case
            Just patch ->
              patchSubsumes patch survivor.patch
                && patchSubsumes survivor.patch patch
            Nothing -> False
    -- the patch of a query's repair, if it has exactly one
    onlyRepair :: PathQuery -> Confirming m (Maybe Patch)
    onlyRepair q
      | stillAmbiguous survivors q = pure Nothing
      | otherwise =
          S.gets (lookup q) >>= \case
            Just known -> pure known
            Nothing -> do
              found <-
                lift (residuate lookupNode chooser cur q) <&> \case
                  Right (Repairs (r :| [])) -> Just r.patch
                  _ -> Nothing
              S.modify' (insertMap q found)
              pure found

-- | Confirming candidate paths, with the repair of each query residuated
-- so far, if it has exactly one.
type Confirming m = S.StateT (Map PathQuery (Maybe Patch)) m

-- | Whether a query is just one alternative that two or more of the
-- survivors are of. Residuating it alone then gives two repairs or more,
-- or an error, and never just one:
--
-- * its candidates are those the alternative had among the path's, since
--   an alternative's candidates do not depend on the others, so none of
--   them has an empty patch and there are no more of them;
-- * a candidate is dominated only by one of its alternative, so the same
--   ones are;
-- * a survivor was no strict superset of any candidate of the path with
--   pins among its own, so it is none of a candidate of the alternative;
-- * two survivors are not equal modulo renaming, so deduplication keeps
--   a patch for each.
stillAmbiguous :: NonEmpty Residual -> PathQuery -> Bool
stillAmbiguous survivors q = case toList q.alternatives of
  [alternative] ->
    not . null . drop 1 $
      filter ((== alternative) . (.patternPath)) (toList survivors)
  _ -> False

-- | 'suggestPaths' as it is defined, residuating every candidate path:
-- the reference it is tested against.
--
-- For each of the given survivors, a path that residuates to exactly it
-- (modulo renaming of created nodes), if one is found, given every survivor
-- of the same path. The lookup and chooser must be the ones the survivors
-- were residuated with.
--
-- The path's alternatives are compiled once, each with a choice of @+@
-- sides that gives it ('compileAlternatives'), until those of the given
-- survivors are found: at most about the compilation residuation already
-- did. Each suggestion then
-- residuates paths without @+@, at most 'maxResiduations' times.
suggestPathsReference ::
  forall m.
  (Monad m) =>
  NodeLookup m ->
  LabelChooser m ->
  NID ->
  Path ->
  NonEmpty Residual ->
  [Residual] ->
  m [Maybe Path]
suggestPathsReference lookupNode chooser cur surface survivors =
  \wanted -> do
    let bases = choicesFor (setFromList (map (.patternPath) wanted))
    for wanted \survivor ->
      case lookup survivor.patternPath bases of
        Nothing -> pure Nothing
        Just base -> suggestFrom survivor base
  where
    -- a choice for each wanted alternative, stopping once all are found
    choicesFor :: Set PatternPath -> Map PatternPath Path
    choicesFor = go mempty (compileAlternatives surface)
      where
        go found _ wanted | null wanted = found
        go found [] _ = found
        go found ((alternative, choice) : rest) wanted
          | alternative `member` wanted =
              go
                (insertMap alternative choice found)
                rest
                (deleteSet alternative wanted)
          | otherwise = go found rest wanted
    suggestFrom survivor base = do
      let rewrites = stepRewrites survivors survivor base
          candidate keep =
            reassociate (applyRewrites (Map.restrictKeys rewrites keep) base)
          dropIfUnneeded keep i = do
            let keep' = deleteSet i keep
            ok <- confirms survivor (candidate keep')
            pure (if ok then keep' else keep)
          -- within the budget; rewrites beyond it are kept
          droppable = take (maxResiduations - 1) (keys rewrites)
      ok <- confirms survivor (candidate (keysSet rewrites))
      if ok
        then
          Just . candidate
            <$> foldM dropIfUnneeded (keysSet rewrites) droppable
        else pure Nothing
    -- the text shown parses back to the path, and the path has exactly this
    -- repair
    confirms :: Residual -> Path -> m Bool
    confirms survivor p
      | parseSurfacePath (renderSurfacePath p) /= Just p = pure False
      | otherwise =
          residuate lookupNode chooser cur (compilePath p) <&> \case
            Right (Repairs (r :| [])) ->
              patchSubsumes r.patch survivor.patch
                && patchSubsumes survivor.patch r.patch
            _ -> False

-- | Residuations spent on one suggestion at most.
maxResiduations :: Int
maxResiduations = 16

-- | Apply to each step (with @~@ pushed down to steps), in written order.
traverseSteps :: (Applicative f) => (Path -> f Path) -> Path -> f Path
traverseSteps f = \case
  p@(Literal _) -> f p
  p@Wild -> f p
  p@(RegexMatch _) -> f p
  p@(Backwards _) -> f p
  l :/ r -> (:/) <$> traverseSteps f l <*> traverseSteps f r
  l :& r -> (:&) <$> traverseSteps f l <*> traverseSteps f r
  Union k l r -> Union k <$> traverseSteps f l <*> traverseSteps f r
  p -> pure p

-- | Replace the i-th step by @f i step@.
mapStepsIndexed :: (Int -> Path -> Path) -> Path -> Path
mapStepsIndexed f p =
  S.evalState (traverseSteps (\s -> S.state \i -> (f i s, i + 1)) p) 0

stepMatcher :: Path -> Matcher
stepMatcher = \case
  Literal t -> MLiteral t
  Wild -> MWild
  RegexMatch r -> MRegex r
  Backwards s -> stepMatcher s
  _ -> error "stepMatcher: not a step"

-- | The rewrite for each step of a union-free path, from the survivor's
-- binding of the step's outputs. A step that distribution copied into
-- routes with different bindings gets none.
stepRewrites :: NonEmpty Residual -> Residual -> Path -> Map Int Rewrite
stepRewrites survivors survivor p =
  case toList (compilePath tagged).alternatives of
    [t]
      | untag t == survivor.patternPath ->
          mapFromList
            [ (i, rw)
            | (i, vs) <- mapToList (outputs t),
              Just rw <- [rewriteFor (toList vs)]
            ]
    _ -> mempty
  where
    -- Compile with each step's matcher replaced by a unique literal, so the
    -- hops say which step they came from. Compilation does not look at
    -- matchers, so the pattern is the same otherwise.
    tag i = "\0step " <> tshow (i :: Int)
    tagged = flip mapStepsIndexed p \i -> \case
      Backwards _ -> Backwards (Literal (tag i))
      _ -> Literal (tag i)
    steps = zip [0 ..] (getConst (traverseSteps (\s -> Const [s]) p))
    matchers =
      mapFromList [(tag i, stepMatcher s) | (i, s) <- steps] :: Map Text Matcher
    stepIndex = mapFromList [(tag i, i) | (i, _) <- steps] :: Map Text Int
    untag t = PatternPath t.tests (untagRoute <$> t.routes)
    untagRoute rt = Route rt.root (untagHop <$> rt.hops)
    untagHop h = Hop h.edge h.direction (untagMatcher h.matcher) h.target
    untagMatcher = \case
      MLiteral l | Just m <- lookup l matchers -> m
      m -> m
    outputs :: PatternPath -> Map Int (Set PVertex)
    outputs t =
      Map.fromListWith
        (<>)
        [ (i, singletonSet h.target)
        | rt <- toList t.routes,
          h <- rt.hops,
          MLiteral l <- [h.matcher],
          Just i <- [lookup l stepIndex]
        ]
    isVar = \case
      Var _ -> True
      _ -> False
    rewriteFor vs
      | not (all isVar vs) = Nothing
      | otherwise = case ordNub (mapMaybe (`lookup` survivor.sigma) vs) of
          [Existing n] -> Just (Pin n)
          bs
            | all isFresh bs,
              not (null excluded) ->
                Just (Exclude excluded)
          _ -> Nothing
      where
        isFresh = \case
          Fresh _ -> True
          Existing _ -> False
        excluded =
          setFromList
            [ n
            | other <- toList survivors,
              other.patternPath == survivor.patternPath,
              v <- vs,
              Just (Existing n) <- [lookup v other.sigma]
            ]

applyRewrites :: Map Int Rewrite -> Path -> Path
applyRewrites rewrites = mapStepsIndexed \i s -> case lookup i rewrites of
  Just (Pin n) -> s :/ Absolute n
  Just (Exclude ns) -> s :/ ExcludingNIDs ns
  Nothing -> s

-- | Associate sequences to the left, as written without parentheses.
reassociate :: Path -> Path
reassociate = \case
  p@(_ :/ _) -> case map reassociate (sequenceParts p) of
    q : qs -> foldl' (:/) q qs
    [] -> error "reassociate: a sequence without parts"
  l :& r -> reassociate l :& reassociate r
  Union k l r -> Union k (reassociate l) (reassociate r)
  Backwards p -> Backwards (reassociate p)
  p -> p
  where
    sequenceParts = \case
      l :/ r -> sequenceParts l <> sequenceParts r
      p -> [p]

-- | A path as it would be typed: labels unquoted where possible, NIDs in
-- full, and only the parentheses that grouping needs.
renderSurfacePath :: Path -> Text
renderSurfacePath = go 0
  where
    go :: Int -> Path -> Text
    go d = \case
      One -> "@"
      Zero -> "%never"
      Wild -> "*"
      Literal t -> label t
      RegexMatch r -> renderRegex r
      Absolute n -> tshow n
      ExcludingNIDs ns -> "!{" <> intercalate ", " (map tshow (toList ns)) <> "}"
      Backwards p -> parens (d > 8) ("~" <> go 9 p)
      Absolute n :/ Literal t | n == tagsNID -> "#" <> label t
      l :/ r -> parens (d > 7) (go 7 l <> "/" <> go 8 r)
      l :& r -> parens (d > 6) (go 6 l <> " & " <> go 7 r)
      Union k l r ->
        parens (d > 5) (go (leftPrec k l) l <> symbol k <> go 6 r)
      WithLimits _ l p ->
        "%override({"
          <> intercalate ", " (map pack (showLimits l))
          <> "}, "
          <> go 0 p
          <> ")"
    -- a chain of one union associates to the left; a different one inside
    -- needs parentheses
    leftPrec k = \case
      Union k' _ _ | k' /= k -> 6
      _ -> 5
    symbol Relational = " | "
    symbol Shared = " ++ "
    parens b t = if b then "(" <> t <> ")" else t
    label t
      | not (null t) && all isIdentChar t = t
      | otherwise = tshow t
    -- a pattern is written between quotes it does not contain; one with
    -- both kinds of quote cannot be written (and does not parse back)
    renderRegex r =
      let shown = tshow r
          pattern' =
            fromMaybe shown (stripPrefix "regex:\"" shown >>= stripSuffix "\"")
       in if '"' `elem` pattern' && '\'' `notElem` pattern'
            then "regex:'" <> pattern' <> "'"
            else shown

-- | Parse a path without directives or @+@ (whose meaning depends on the
-- command).
parseSurfacePath :: Text -> Maybe Path
parseSurfacePath t = case runParser (whitespace *> pPath <* eof) "" t of
  Left _ -> Nothing
  Right p -> handleDirectivesWith Nothing (\_ _ -> Nothing) p

-- * Tests

-- | The suggestion for each survivor of an ambiguous @mk p@.
suggestionsIn :: Graph Text () -> Path -> [Maybe Text]
suggestionsIn g p =
  case residuatePure lookupNode (const Nothing) corpusCurrent (compilePath p) of
    Right (Repairs rs) ->
      map (fmap renderSurfacePath) . runIdentity $
        suggestPaths (pure . lookupNode) noLabels corpusCurrent p rs (toList rs)
    other -> error ("expected repairs: " <> show other)
  where
    lookupNode = (`maybeNodeLookup` g)

spec_suggestPath :: Spec
spec_suggestPath = describe "suggestPaths" do
  let n = tshow . smallNID
  it "pins the node a step binds: mk a/b with two a children" $
    suggestionsIn (graphWithEdges [edge' 1 "a" 2, edge' 1 "a" 3]) [path| a/b |]
      `shouldBe` [Just ("a/" <> n 2 <> "/b"), Just ("a/" <> n 3 <> "/b")]
  it "keeps the alternative: mk a + b" $
    suggestionsIn (graphWithEdges []) [path| a ++ b |]
      `shouldBe` [Just "a", Just "b"]
  it "keeps the alternative of an expanded %targets" $
    suggestionsIn
      (graphWithEdges [edge' 2 "x" 2, edge' 3 "x" 3])
      [path| x/(%never ++ @000000000002 ++ @000000000003) |]
      `shouldBe` [Just ("x/" <> n 2), Just ("x/" <> n 3)]
  it "pins or excludes right evidence: mk a/b/@5" $
    suggestionsIn (graphWithEdges [edge' 6 "b" 5]) [path| a/b/@000000000005 |]
      `shouldBe` [ Just ("a/!{" <> n 6 <> "}/b/" <> n 5),
                   Just ("a/" <> n 6 <> "/b/" <> n 5)
                 ]
  it "writes or excludes a pinned node: mk a & a/@2" $
    suggestionsIn (graphWithEdges [edge' 2 "x" 2]) [path| a & a/@000000000002 |]
      `shouldBe` [ Just ("a/!{" <> n 2 <> "} & a/" <> n 2),
                   Just ("a/" <> n 2 <> " & a/" <> n 2)
                 ]
  it "pins to the current node: mk a/~a" $
    suggestionsIn (graphWithEdges []) [path| a/~a |]
      `shouldBe` [Just ("a/~a/!{" <> n 1 <> "}"), Just ("a/~a/" <> n 1)]
  it "keeps an @ alternative: mk (a & b)/(@ + c)" $
    suggestionsIn (graphWithEdges []) [path| (a & b)/(@ ++ c) |]
      `shouldBe` [Just "(a & b)/@", Just "(a & b)/c"]
  it "omits a repair that no path selects: mk a/b/@5 & c/b/@5" $
    -- a created node shared by two routes cannot be named, and nothing
    -- forbids sharing it
    suggestionsIn
      (graphWithEdges [edge' 5 "x" 5])
      [path| a/b/@000000000005 & c/b/@000000000005 |]
      `shouldBe` [Nothing, Nothing]
  it "chooses among many written + without enumerating them" do
    -- 2^30 written choices, each %never side compiling to nothing
    let p =
          foldl'
            (\q _ -> q :/ (Zero :++ Literal "c"))
            [path| a/b |]
            [1 .. 30 :: Int]
        g = graphWithEdges [edge' 1 "a" 2, edge' 1 "a" 3]
    result <- timeout 10000000 (evaluate (force (suggestionsIn g p)))
    fmap (map isJust) result `shouldBe` Just [True, True]
  it "costs about one compilation more than residuation" do
    -- 3^11 written choices, most contradicting each other only once glued
    let p =
          foldl'
            ( \q _ ->
                q
                  :/ (Absolute (smallNID 2) :++ Absolute (smallNID 3) :++ One)
            )
            [path| a/b |]
            [1 .. 11 :: Int]
        g = graphWithEdges [edge' 1 "a" 2, edge' 1 "a" 3]
    -- about 2.5s; recompiling per + choice and repair took about 9 times as long
    result <- timeout 15000000 (evaluate (force (suggestionsIn g p)))
    fmap (all isJust) result `shouldBe` Just True
  it "every suggestion over the corpus selects exactly its repair" do
    let survivors =
          [ (surface, g, r, suggestion)
          | surface <- corpusQueries,
            g <- corpusGraphs,
            Right (Repairs rs@(_ :| _ : _)) <-
              [ residuatePure
                  (`maybeNodeLookup` g)
                  (const Nothing)
                  corpusCurrent
                  (compilePath surface)
              ],
            (r, suggestion) <-
              zip (toList rs) . runIdentity $
                suggestPaths
                  (pure . (`maybeNodeLookup` g))
                  noLabels
                  corpusCurrent
                  surface
                  rs
                  (toList rs)
          ]
        suggested = [(surface, g, r, s) | (surface, g, r, Just s) <- survivors]
    -- only steps that distribution copies with different bindings, as in
    -- a/(a & b), get none (4049 of 4109 before the corpus had relational
    -- unions copied by distribution, 94% after)
    (length suggested * 100 `div` length survivors) `shouldSatisfy` (>= 90)
    forM_ suggested \(surface, g, r, s) -> do
      let context = show (surface, g, renderSurfacePath s)
      (context, parseSurfacePath (renderSurfacePath s))
        `shouldBe` (context, Just s)
      let lookupNode = (`maybeNodeLookup` g)
      case residuatePure lookupNode (const Nothing) corpusCurrent (compilePath s) of
        Right (Repairs (r' :| [])) ->
          ( context,
            patchSubsumes r'.patch r.patch && patchSubsumes r.patch r'.patch
          )
            `shouldBe` (context, True)
        other -> expectationFailure (context <> ": " <> show other)

test_renderSurfacePath :: TestTree
test_renderSurfacePath =
  testGroup
    "renderSurfacePath"
    [ roundTrips "a/b/c" [path| a/b/c |],
      roundTrips "a/(b/c)" [path| a/(b/c) |],
      roundTrips "(a ++ b)/c & d" [path| (a ++ b)/c & d |],
      roundTrips "a & (b | c)" [path| a & (b | c) |],
      roundTrips "a | b | c" [path| a | b | c |],
      roundTrips "a | (b | c)" [path| a | (b | c) |],
      roundTrips "(a | b) ++ c" [path| (a | b) ++ c |],
      roundTrips "a ++ (b | c)" [path| a ++ (b | c) |],
      roundTrips "~a/~(b/c)" [path| ~a/~(b/c) |],
      roundTrips "\"a b\"/\"\"/*" [path| "a b"/""/* |],
      roundTrips "regex:\"^a.*\" & a" [path| regex:"^a.*" & a |],
      roundTrips "regex:'a\"b'" [path| regex:'a"b' |],
      roundTrips "#t/x & ~#\"a b\"" [path| #t/x & ~#"a b" |],
      roundTrips "@/%never" [path| @/%never |],
      roundTrips
        "!{@000000000001, @000000000002}/@000000000003"
        [path| !{@000000000001,@000000000002}/@000000000003 |],
      testProperty "parses back" $
        forAll (sized genPath) \p ->
          let t = renderSurfacePath p
           in counterexample (unpack t) (parseSurfacePath t == Just p)
    ]
  where
    roundTrips expected p =
      testCase (unpack expected) do
        renderSurfacePath p @?= expected
        parseSurfacePath expected @?= Just p
    genLabel =
      oneof
        [ elements ["a", "b", "foo", "x-y", "a.b", "_", "'", "123", "regex"],
          pack <$> listOf (elements "a b\"\\&\8594\233\0\SOH1-'.:/()%@#!{}~+*\t")
        ]
    -- a pattern with both quotes cannot be written
    genRegex =
      elements ["^a.*", "x", "a\"b", "\\.", "a'b", "\233", "[0-9]{2}"]
        <&> \t -> either (const Wild) RegexMatch (compileRegex (encodeUtf8 t))
    genPath :: Int -> Gen Path
    genPath 0 =
      oneof
        [ Literal <$> genLabel,
          pure Wild,
          pure One,
          pure Zero,
          genRegex,
          Absolute . smallNID <$> choose (0, 1000),
          pure (Absolute tagsNID),
          ExcludingNIDs . setFromList . map smallNID <$> listOf1 (choose (0, 50))
        ]
    genPath k =
      frequency
        [ (3, genPath 0),
          (1, Backwards <$> genPath (k - 1)),
          (2, (:/) <$> genPath (k `div` 2) <*> genPath (k `div` 2)),
          (2, (:&) <$> genPath (k `div` 2) <*> genPath (k `div` 2)),
          (1, (:||) <$> genPath (k `div` 2) <*> genPath (k `div` 2)),
          (1, (:++) <$> genPath (k `div` 2) <*> genPath (k `div` 2))
        ]
