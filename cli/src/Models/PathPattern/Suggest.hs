-- | Paths that choose one possibility of an ambiguous construction
-- (docs/path-route-ir.md section 5).
--
-- The possibilities of a path differ in the alternative they are of and
-- in the node taken wherever steps had edges to several nodes to follow
-- ('Branch'). The line for a possibility is the path with only the @+@
-- alternative it came from, @/\@N@ after each step that took an existing
-- node there, and @/!{…}@ naming the other nodes after each step that took
-- the created one. A possibility that cannot be written that way has no
-- line: one whose step could have gone to another created node, which has
-- no name, or one in which the copies of a written step went different
-- ways. And a line is only shown if walking it creates exactly the
-- possibility, so a shown line is never wrong.
module Models.PathPattern.Suggest
  ( suggestPaths,
    alternativeChoices,
    Written (..),
    writtenSteps,
    stepAsTyped,
    typedTimes,
    joinAfter,
    mapStepsIndexed,
    reassociate,
    renderSurfacePath,
    parseSurfacePath,
    spec_suggestPath,
    test_renderSurfacePath,
  )
where

import Control.Monad (zipWithM)
import Control.Monad.State.Strict qualified as S
import Data.Functor.Const (Const (..))
import Data.Map.Strict qualified as Map
import Graph.SystemNodes (tagsNID)
import Models.Graph (Graph)
import Models.NID
import Models.Path.Parse (pPath)
import Models.Path.Simple
import Models.Path.TH (path)
import Models.PathPattern
import Models.PathPattern.Compile (checkCopies, compileAlternatives, compilePath)
import Models.PathPattern.Residuate
import Models.PathPattern.TestCorpus
import MyPrelude
import Utils.Parsing (isIdentChar, whitespace)
import Utils.Parsing.Common (eof, runParser)
import Utils.Testing

-- | What to add after a step: @/\@N@ or @/!{…}@.
data Rewrite = Pin NID | Exclude (Set NID)
  deriving stock (Eq, Ord, Show)

-- | For each of the given possibilities of the path, a path that creates
-- exactly it (modulo the names of created nodes), where one can be
-- written. The lookup and chooser must be the ones the possibilities were
-- found with.
--
-- The path's alternatives are compiled once more, each with a choice of
-- @+@ sides that gives it ('compileAlternatives'), until those of the
-- given possibilities are found: at most about the compilation the walk
-- already had. Each line then costs two compilations of a path without
-- @+@ and one walk of it, within the given limit: the one of the command
-- the line is shown for, which is the limit of the line when it is typed
-- as the command with its path replaced.
--
-- The walk is what makes a shown line right. What is written after a
-- step does more than choose: a step with @/\@N@ creates its edge to @N@
-- if there is none, where the step it is written for only followed an
-- edge that another step creates, and that edge then counts for steps it
-- did not count for; it changes the order of the operations; and the
-- path with it may be one whose shape @mk@ refuses, which has no line.
suggestPaths ::
  (Monad m) =>
  RepairsLimit ->
  NodeLookup m ->
  LabelChooser m ->
  NID ->
  Path ->
  [Residual] ->
  m [Maybe Path]
suggestPaths limit lookupNode chooser cur surface wanted =
  for wanted \r -> case lookup r.patternPath bases >>= lineFor r of
    Nothing -> pure Nothing
    Just line ->
      residuateWithin limit lookupNode chooser cur (compilePath line) <&> \case
        Right (Repairs (r' :| []))
          | patchSubsumes r'.patch r.patch && patchSubsumes r.patch r'.patch ->
              Just line
        _ -> Nothing
  where
    bases = alternativeChoices surface (setFromList (map (.patternPath) wanted))

-- | A choice of @+@ sides for each wanted alternative of the path,
-- stopping once all are found.
alternativeChoices :: Path -> Set PatternPath -> Map PatternPath Path
alternativeChoices surface = go mempty (compileAlternatives surface)
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

-- | The line for a possibility, from the path of its alternative (a path
-- without @+@ that compiles to the possibility's pattern): after each
-- written step with a copy into a vertex where the walk had several nodes
-- to go to, @/\@N@ if it took the existing node @N@, and @/!{…}@ with the
-- other nodes if it took a created one. There is no line if another of
-- the nodes is a created one too, if the copies of a step need different
-- things written, or if the result cannot be typed or is a path that no
-- command runs ('checkCopies').
lineFor :: Residual -> Path -> Maybe Path
lineFor r base = do
  positions <- lineUp r.patternPath base
  let -- the vertices that the copies of each written step lead to
      outputs :: Map Int [PVertex]
      outputs = Map.fromListWith (<>) [(i, [h.target]) | (h, i) <- positions]
      branchAt :: Map PVertex Branch
      branchAt = mapFromList [(b.vertex, b) | b <- r.branches]
      rewriteOf b = case b.taken of
        Existing n -> Just (Pin n)
        Fresh _ ->
          Exclude . setFromList
            <$> for b.others \case
              Existing n -> Just n
              Fresh _ -> Nothing
  rewrites <-
    for outputs \vs -> case ordNub (map rewriteOf (mapMaybe (`lookup` branchAt) vs)) of
      [] -> Just Nothing
      [rewrite] -> Just <$> rewrite
      _ -> Nothing
  let line = reassociate (applyRewrites (Map.mapMaybe id rewrites) base)
  guard (parseSurfacePath (renderSurfacePath line) == Just line)
  guard (isRight (checkCopies line))
  pure line

-- | The path with each step's matcher replaced by a unique literal, and
-- the step and matcher each literal stands for. The hops of what it
-- compiles to say which step they are copies of. Compilation does not
-- look at matchers, so the pattern is the same otherwise.
tagSteps :: Path -> (Path, Map Text (Int, Matcher))
tagSteps p =
  ( flip mapStepsIndexed p \i -> \case
      Backwards _ -> Backwards (Literal (tag i))
      _ -> Literal (tag i),
    mapFromList
      [ (tag i, (i, stepMatcher s))
      | (i, s) <- zip [0 ..] (getConst (traverseSteps (\s -> Const [s]) p))
      ]
  )
  where
    tag i = "\0step " <> tshow (i :: Int)

-- | Compile a path without @+@ and line its routes up with a pattern's,
-- hop by hop: for every hop of the pattern, the written step the hop is a
-- copy of. 'Nothing' when the path is not one alternative with the same
-- routes, directions and matchers.
lineUp :: PatternPath -> Path -> Maybe [(Hop, Int)]
lineUp p written = case toList (compilePath tagged).alternatives of
  [t]
    | length t.routes == length p.routes ->
        concat <$> zipWithM lineRoute (toList p.routes) (toList t.routes)
  _ -> Nothing
  where
    (tagged, steps) = tagSteps written
    lineRoute rp rt
      | length rp.hops /= length rt.hops = Nothing
      | otherwise = zipWithM hop rp.hops rt.hops
    hop hp ht = case ht.matcher of
      MLiteral l
        | Just (i, m) <- lookup l steps,
          m == hp.matcher,
          hp.direction == ht.direction ->
            Just (hp, i)
      _ -> Nothing

-- | The steps of one alternative of a path as they were written: the path
-- with only the sides of its unions that give the alternative (as
-- 'alternativeChoices' finds it, with @~@ pushed down to the steps), its
-- steps in that path's order ('traverseSteps'),
-- and for each step of the alternative the written step it is a copy of,
-- by its place in that order. Several steps of the alternative are copies
-- of one written step where the product rule copies it. For each of those
-- written steps, its place among the steps of the whole path as typed
-- (all union sides, in the order typed, also inside a @~(…)@), and those
-- steps as typed ('stepAsTyped').
data Written = Written
  { path :: Path,
    steps :: [Path],
    copyOf :: Map EdgeId Int,
    typedAt :: [Int],
    typed :: [Text]
  }
  deriving stock (Eq, Show)

-- | 'Written' for an alternative of the path: 'Nothing' when no choice of
-- union sides gives it, or its steps cannot be lined up with the path as
-- typed. It compiles the path, with each step made a literal of its own
-- ('tagTyped'), until it finds the alternative, as 'alternativeChoices'
-- does.
writtenSteps :: Path -> PatternPath -> Maybe Written
writtenSteps surface alternative = do
  let (tagged, originals) = tagTyped surface
      origin :: Map Text (Int, Path)
      origin = mapFromList [(stepTag i, (i, s)) | (i, (s, _)) <- zip [0 ..] originals]
      untagHop h = case h.matcher of
        MLiteral l | Just (_, s) <- lookup l origin -> Just (Hop h.edge h.direction (stepMatcher s) h.target)
        _ -> Nothing
      untagged a =
        PatternPath a.tests
          <$> for a.routes \r -> Route r.root <$> for r.hops untagHop
  (found, choice) <-
    headMay [(a, c) | (a, c) <- compileAlternatives tagged, untagged a == Just alternative]
  let choiceSteps = getConst (traverseSteps (\s -> Const [s]) choice)
      tagOf = \case
        Literal l -> lookup l origin
        Backwards (Literal l) -> lookup l origin
        _ -> Nothing
  places <- for choiceSteps (fmap fst . tagOf)
  let base = flip mapStepsIndexed choice \_ -> \case
        Literal l | Just (_, s) <- lookup l origin -> s
        Backwards (Literal l) | Just (_, s) <- lookup l origin -> Backwards s
        s -> s
      baseIndex = Map.fromListWith (<>) [(i, [j]) | (j, i) <- zip [0 :: Int ..] places]
  copies <- for (concatMap (.hops) (toList found.routes)) \h -> case h.matcher of
    MLiteral l
      | Just (i, _) <- lookup l origin,
        Just [j] <- lookup i baseIndex ->
          Just (h.edge, j)
    _ -> Nothing
  pure
    Written
      { path = base,
        steps = getConst (traverseSteps (\s -> Const [s]) base),
        copyOf = mapFromList copies,
        typedAt = places,
        typed = map snd originals
      }

-- | A written step as it was typed: @#t@ for the step after the node of
-- tags, and ('Written') where else it is written as that.
stepAsTyped :: Written -> Int -> Text
stepAsTyped w i = fromMaybe "" (headMay (drop i w.typedAt) >>= \k -> headMay (drop k w.typed))

-- | How often the step is typed in the whole path, and which of those it
-- is, from 1.
typedTimes :: Written -> Int -> (Int, Int)
typedTimes w i = case headMay (drop i w.typedAt) of
  Nothing -> (1, 1)
  Just k ->
    let t = fromMaybe "" (headMay (drop k w.typed))
     in (length (filter (== t) w.typed), 1 + length (filter (== t) (take k w.typed)))

-- | The path with each step made the literal 'stepTag' of its place among
-- the steps as typed: in the order typed, a @~@ around several steps
-- taken apart (a step written @~a@, or @~(~a)@, is one step). With each
-- step, the step without its direction and how it
-- is typed.
tagTyped :: Path -> (Path, [(Path, Text)])
tagTyped p0 = second reverse (S.runState (go False p0) [])
  where
    go :: Bool -> Path -> S.State [(Path, Text)] Path
    go afterTags = \case
      p@(Literal t) -> found True p ((if afterTags then "#" else "") <> renderSurfacePath (Literal t))
      p@Wild -> found True p (renderSurfacePath p)
      p@(RegexMatch _) -> found True p (renderSurfacePath p)
      p@(Backwards q) | Just (forward, s) <- directed q -> found (not forward) s (renderSurfacePath p)
      Backwards p -> Backwards <$> go False p
      l :/ r -> (:/) <$> go afterTags l <*> go (endsWithTags l) r
      l :& r -> (:&) <$> go False l <*> go False r
      Union k l r -> Union k <$> go False l <*> go False r
      p -> pure p
    found forward s shown = S.state \steps' ->
      let tagged = Literal (stepTag (length steps'))
       in (if forward then tagged else Backwards tagged, (s, shown) : steps')
    -- a step under any number of ~, and whether it goes forward
    directed = \case
      Backwards q -> first not <$> directed q
      s | isStep s -> Just (True, s)
      _ -> Nothing
    isStep = \case
      Literal _ -> True
      Wild -> True
      RegexMatch _ -> True
      _ -> False
    endsWithTags = \case
      Absolute n -> n == tagsNID
      _ :/ r -> endsWithTags r
      _ -> False

stepTag :: Int -> Text
stepTag i = "\0typed step " <> tshow i

-- | The path with a join (@/\@@) after its i-th step, so that what
-- follows the step continues from one edge of it: @*/(a & b)@ becomes
-- @*/\@/(a & b)@.
joinAfter :: Int -> Path -> Path
joinAfter i = reassociate . mapStepsIndexed \j s -> if j == i then s :/ One else s

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

-- | The possibilities of an ambiguous @mk p@.
possibilitiesIn :: Graph Text () -> Path -> NonEmpty Residual
possibilitiesIn g p =
  case residuatePure (corpusNode g) (const Nothing) corpusCurrent (compilePath p) of
    Right (Repairs rs) -> rs
    other -> error ("expected possibilities: " <> show other)

-- | The line for each possibility of an ambiguous @mk p@.
suggestionsIn :: Graph Text () -> Path -> [Maybe Text]
suggestionsIn = suggestionsWithin (AtMostRepairs defaultMaxRepairs)

-- | 'suggestionsIn', walking each line within this limit.
suggestionsWithin :: RepairsLimit -> Graph Text () -> Path -> [Maybe Text]
suggestionsWithin limit g p =
  map (fmap renderSurfacePath) . runIdentity $
    suggestPaths
      limit
      (pure . (corpusNode g))
      noLabels
      corpusCurrent
      p
      (toList (possibilitiesIn g p))

spec_suggestPath :: Spec
spec_suggestPath = describe "suggestPaths" do
  let n = tshow . smallNID
  it "writes the node a step took: mk a/b with two a children" $
    suggestionsIn (graphWithEdges [edge' 1 "a" 2, edge' 1 "a" 3]) [path| a/b |]
      `shouldBe` [Just ("a/" <> n 2 <> "/b"), Just ("a/" <> n 3 <> "/b")]
  it "walks each line within the command's limit" do
    -- each line (a/@2/b, a/@3/b) has one walk; with none allowed it is
    -- not walked and not shown. (No command has a limit of 0: this only
    -- shows that a line is walked within the limit given.)
    let g = graphWithEdges [edge' 1 "a" 2, edge' 1 "a" 3]
    suggestionsWithin (AtMostRepairs 1) g [path| a/b |]
      `shouldBe` [Just ("a/" <> n 2 <> "/b"), Just ("a/" <> n 3 <> "/b")]
    suggestionsWithin (AtMostRepairs 0) g [path| a/b |]
      `shouldBe` [Nothing, Nothing]
    suggestionsWithin UnlimitedRepairs g [path| a/b |]
      `shouldBe` [Just ("a/" <> n 2 <> "/b"), Just ("a/" <> n 3 <> "/b")]
  it "keeps the alternative: mk a + b" $
    suggestionsIn (graphWithEdges []) [path| a ++ b |]
      `shouldBe` [Just "a", Just "b"]
  it "keeps the alternative of an expanded %targets" $
    suggestionsIn
      (graphWithEdges [edge' 2 "x" 2, edge' 3 "x" 3])
      [path| x/(%never ++ @000000000002 ++ @000000000003) |]
      `shouldBe` [Just ("x/" <> n 2), Just ("x/" <> n 3)]
  it "keeps an @ alternative: mk (a & b)/(@ + c)" $
    suggestionsIn (graphWithEdges []) [path| (a & b)/(@ ++ c) |]
      `shouldBe` [Just "(a & b)/@", Just "(a & b)/c"]
  it "writes a node this command creates: mk a/@2/x & a/@3/y & a/z" $
    suggestionsIn
      (graphWithEdges [edge' 2 "q" 2, edge' 3 "q" 3])
      [path| a/@000000000002/x & a/@000000000003/y & a/z |]
      `shouldBe` [ Just ("a/" <> n 2 <> "/x & a/" <> n 3 <> "/y & a/" <> n 2 <> "/z"),
                   Just ("a/" <> n 2 <> "/x & a/" <> n 3 <> "/y & a/" <> n 3 <> "/z")
                 ]
  it "excludes the other nodes for the created one: mk c & */b" $
    -- b under the existing a node, or under the c node this command creates
    suggestionsIn (graphWithEdges [edge' 1 "a" 2]) [path| c & */b |]
      `shouldMatchList` [ Just ("c & */" <> n 2 <> "/b"),
                          Just ("c & */!{" <> n 2 <> "}/b")
                        ]
  it "writes a join once for each step into it: mk (a & c)/@" $
    suggestionsIn (graphWithEdges [edge' 1 "a" 2, edge' 1 "c" 5]) [path| (a & c)/@ |]
      `shouldBe` [ Just ("(a/" <> n 2 <> " & c/" <> n 2 <> ")/@"),
                   Just ("(a/" <> n 5 <> " & c/" <> n 5 <> ")/@")
                 ]
  it "has no line where the other node is a created one: mk a & b & */c" $
    -- c under the new a node or the new b node: neither has a name
    suggestionsIn (graphWithEdges []) [path| a & b & */c |]
      `shouldBe` [Nothing, Nothing]
  it "has no line where copies of a step went different ways: mk a/(b & c)" $
    map isJust (suggestionsIn (graphWithEdges [edge' 1 "a" 2, edge' 1 "a" 3]) [path| a/(b & c) |])
      `shouldMatchList` [True, True, False, False]
  it "shows no line that would create something else: mk (a & b)/(@ + a)" $
    -- For the possibility in which the first a goes round the loop at the
    -- current node and the last a goes round it again, the line would be
    -- (a/@1 & b)/a/@1; but written so, the b has an edge to follow that it
    -- did not have, and less is created. Each line is confirmed by a walk.
    suggestionsIn (graphWithEdges [edge' 1 "a" 1, edge' 1 "a" 2]) [path| (a & b)/(@ ++ a) |]
      `shouldSatisfy` any isNothing
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
  it "every line over the corpus creates exactly its possibility" do
    let listed =
          [ (surface, g, r, line)
          | surface <- corpusQueries,
            g <- corpusGraphs,
            Right (Repairs rs@(_ :| _ : _)) <-
              [ residuatePure
                  (corpusNode g)
                  (const Nothing)
                  corpusCurrent
                  (compilePath surface)
              ],
            (r, line) <-
              zip (toList rs) . runIdentity $
                suggestPaths
                  (AtMostRepairs defaultMaxRepairs)
                  (pure . (corpusNode g))
                  noLabels
                  corpusCurrent
                  surface
                  (toList rs)
          ]
        lines' = [(surface, g, r, s) | (surface, g, r, Just s) <- listed]
    -- only steps that distribution copies and that go different ways, as
    -- in a/(a & b), get none
    (length lines' * 100 `div` length listed) `shouldSatisfy` (>= 90)
    forM_ lines' \(surface, g, r, s) -> do
      let context = show (surface, g, renderSurfacePath s)
      (context, parseSurfacePath (renderSurfacePath s))
        `shouldBe` (context, Just s)
      (context, isRight (checkCopies s)) `shouldBe` (context, True)
      case residuatePure (corpusNode g) (const Nothing) corpusCurrent (compilePath s) of
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
