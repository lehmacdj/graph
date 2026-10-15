-- | The laws of what @mk@ creates (docs/path-route-ir.md section 5), on
-- random paths and graphs that @mk@ builds (the paths whose shape it
-- refuses are filtered out, and counted) and on paths built so that steps
-- follow edges that other routes create; and what building costs.
module Models.PathPattern.WalkSpec where

import Control.Exception qualified as E
import Models.Edge
import Models.Graph (Graph, emptyGraph, insertEdges, insertNode)
import Models.NID
import Models.Node (emptyNode)
import Models.Path.Simple qualified as P
import Models.Path.TH (path)
import Models.PathPattern
import Models.PathPattern.Admit
import Models.PathPattern.Compile (checkCopies, compilePath)
import Models.PathPattern.Residuate
import Models.PathPattern.ResiduateSpec
import Models.PathPattern.TestCorpus
import MyPrelude
import System.Mem
  ( disableAllocationLimit,
    enableAllocationLimit,
    getAllocationCounter,
    setAllocationCounter,
  )
import Test.QuickCheck.Gen (unGen)
import Test.QuickCheck.Random (mkQCGen)
import Utils.Testing

-- * What mk does, up to the names of new nodes

-- | What @mk p@ does at the corpus's current node, with at most 300 walks.
run :: Graph Text () -> P.Path -> Either ResidualError Residuation
run = residuateUpTo (P.AtMostRepairs 300)

-- | The kind of result, and what each possibility creates.
outcome :: Either ResidualError Residuation -> (Text, [Patch' PatchLabel])
outcome = \case
  Right AlreadyResolves -> ("nothing to do", [])
  Right Unsatisfiable -> ("cannot be constructed", [])
  Right (Repairs (r :| [])) -> ("creates", [known r.patch])
  Right (Repairs rs) -> ("asks", map (known . (.patch)) (toList rs))
  Left (TooManyRepairs _) -> ("too many walks", [])
  Left (MissingNodes _) -> ("needs nodes that do not exist", [])
  Left (Refused refusal) -> ("refused: " <> refusalCode refusal, [])
  Left (UnsupportedLabel _ _ _ rs) -> ("needs a label", map (.patch) (toList rs))
  where
    known p =
      Patch p.nodes (setFromList [Edge e.source (Known e.transition) e.sink | e <- toList p.edges])

-- | The same kind of result with the same possibilities, each as often, up
-- to the names of new nodes.
sameOutcome ::
  Either ResidualError Residuation -> Either ResidualError Residuation -> Bool
sameOutcome a b = kindA == kindB && sameList patchesA patchesB
  where
    (kindA, patchesA) = outcome a
    (kindB, patchesB) = outcome b
    sameList [] ys = null ys
    sameList (x : xs) ys = case break (samePatch x) ys of
      (before, _ : rest) -> sameList xs (before <> rest)
      _ -> False

tooMany :: Either ResidualError Residuation -> Bool
tooMany = \case
  Left (TooManyRepairs _) -> True
  _ -> False

-- | The patch that is created, if @mk@ decides: none if the path resolves.
decided :: Either ResidualError Residuation -> Maybe Patch
decided = \case
  Right AlreadyResolves -> Just (Patch mempty mempty)
  Right (Repairs (r :| [])) -> Just r.patch
  _ -> Nothing

-- | The written nodes that mk refuses a path for.
refusedFor :: Either ResidualError Residuation -> [NID]
refusedFor = \case
  Left (MissingNodes ns) -> toList ns
  _ -> []

-- | A path that commands run (a refused path never reaches @mk@).
runs :: P.Path -> Bool
runs = isRight . checkCopies

-- | The bans that refuse a path at the corpus's current node, by their
-- codes: none for a path that @mk@ builds. X is looked at first, as in
-- @mk@: a path it refuses is not compiled.
bans :: P.Path -> [Text]
bans p = case checkExpansion p of
  Just refusal -> [refusalCode refusal]
  Nothing -> ordNub (map refusalCode (refusals corpusCurrent (compilePath p)))

-- | A path that commands run and whose shape @mk@ does not refuse.
admitted :: P.Path -> Bool
admitted p = runs p && null (bans p)

-- | Whether a path has no @*@ and no regex.
literalOnly :: P.Path -> Bool
literalOnly = \case
  P.Wild -> False
  P.RegexMatch _ -> False
  l P.:/ r -> literalOnly l && literalOnly r
  l P.:& r -> literalOnly l && literalOnly r
  P.Union _ l r -> literalOnly l && literalOnly r
  P.Backwards p -> literalOnly p
  _ -> True

-- | Whether some alternative of a path can be built in a graph as far as
-- its written nodes go: they all exist, and each passes what the path
-- tests of it.
viable :: Graph Text () -> P.Path -> Bool
viable g p =
  or
    [ and
        [ isJust (nodeIn g n) && testAccepts (vertexTest a v) n
        | v <- patternVertices a,
          n <- case v of
            Current -> [corpusCurrent]
            Fixed m -> [m]
            Var _ -> []
        ]
    | a <- toList (compilePath p).alternatives
    ]

-- | Two patches as one: the new nodes of the second are numbered after
-- those of the first.
together :: (Ord l) => Patch' l -> Patch' l -> Patch' l
together a b =
  Patch
    (a.nodes <> setFromList (map shift (toList b.nodes)))
    (a.edges <> setFromList [Edge (shift e.source) e.transition (shift e.sink) | e <- toList b.edges])
  where
    count = length [() | Fresh _ <- toList a.nodes]
    shift = \case
      Fresh k -> Fresh (count + k)
      existing -> existing

-- * The laws

-- | Swap the sides of every @&@ (first) or of every union (second).
mirror :: Bool -> Bool -> P.Path -> P.Path
mirror ands unions' = go
  where
    go = \case
      l P.:& r -> if ands then go r P.:& go l else go l P.:& go r
      P.Union k l r -> if unions' then P.Union k (go r) (go l) else P.Union k (go l) (go r)
      l P.:/ r -> go l P.:/ go r
      P.Backwards p -> P.Backwards (go p)
      p -> p

-- | The laws of a path that @mk@ builds. After @mk@ the path resolves,
-- and running it again does nothing (for what is created, and for each of
-- the first five possibilities listed); a path that resolves, and only
-- such a path, does nothing; the result does not depend on the order of
-- conjuncts or of union sides, and with the sides swapped the path is
-- still one that @mk@ builds; and a path with literal labels only, of
-- which some alternative has its written nodes, all passing what the path
-- tests of them, is never answered "cannot be constructed", nor refused
-- for missing nodes or a label.
lawsHold :: Graph Text () -> P.Path -> Property
lawsHold g p =
  conjoin $
    [ counterexample "does nothing iff the path resolves" $
        (whole == Right AlreadyResolves) === resolvesIn g p,
      counterexample "can be constructed" $
        not (literalOnly p && viable g p)
          || fst (outcome whole) `elem` ["nothing to do", "creates", "asks"],
      counterexample "is refused only for written nodes that do not exist" $
        all (\n -> n `elem` writtenNodes p && isNothing (nodeIn g n)) (refusedFor whole),
      counterexample "and is not refused for them when they all exist" $
        any (isNothing . nodeIn g) (writtenNodes p) || null (refusedFor whole),
      counterexample "creates only new nodes" $
        null [() | patch <- created, Existing _ <- toList patch.nodes]
    ]
      <> [ counterexample ("after creating " <> show patch) . conjoin $
             [ counterexample "the path resolves" (resolvesIn g' p),
               counterexample "mk again does nothing" $
                 run g' p === Right AlreadyResolves
             ]
         | patch <- take 5 created,
           let g' = applyPatch patch g
         ]
      <> [ counterexample ("mirrored: " <> show p') $
             counterexample (show (whole, other)) (sameOutcome whole other)
         | (ands, unions') <- [(True, False), (False, True), (True, True)],
           let p' = mirror ands unions' p,
           p' /= p,
           runs p',
           let other = run g p'
         ]
  where
    whole = run g p
    created = case whole of
      Right (Repairs rs) -> map (.patch) (toList rs)
      _ -> []

-- | A conjunct that shares no label and no node with a path, but the
-- node both start from: a label, a chain, a join, or out and back.
genUnrelated :: Gen P.Path
genUnrelated =
  elements
    [ u,
      u P.:/ v,
      (u P.:& v) P.:/ P.One,
      u P.:/ P.Backwards u P.:/ w,
      (u P.:/ v) P.:& w
    ]
  where
    u = P.Literal "unrelated-u"
    v = P.Literal "unrelated-v"
    w = P.Literal "unrelated-w"

-- | A conjunct that shares no label and no node with the rest changes
-- nothing of what the rest creates: what @mk p & q@ does is what @mk p@
-- does, with what @mk q@ creates beside it in every possibility. (The
-- path has literal labels only: a @*@ shares every label.)
unrelatedChangesNothing :: Graph Text () -> P.Path -> P.Path -> Property
unrelatedChangesNothing g p q =
  counterexample (show (alone, with)) $ case (outcome alone, outcome with) of
    ((kind, patches), (kind', patches'))
      | kind == "nothing to do" ->
          kind' === "creates" .&&. property (sameLists [aside] patches')
      | kind `elem` ["creates", "asks"] ->
          kind' === kind .&&. property (sameLists (map (`together` aside) patches) patches')
      | otherwise -> kind' === kind
  where
    alone = run g p
    with = run g (p P.:& q)
    aside = case outcome (run g q) of
      ("creates", [patch]) -> patch
      other -> error ("an unrelated conjunct that is not created: " <> show other)
    sameLists [] ys = null ys
    sameLists (x : xs) ys = case break (samePatch x) ys of
      (before, _ : rest) -> sameLists xs (before <> rest)
      _ -> False

spec_walkLaws :: Spec
spec_walkLaws = describe "the laws of what mk creates" do
  it "hold on random paths and graphs that mk builds" . withMaxSuccess 3000 $
    forAll genRepairCase \(p, g) ->
      (fewAlternatives 300 p && admitted p && not (tooMany (run g p)))
        ==> lawsHold g p
  it "hold on paths built so that steps follow edges of other routes" . withMaxSuccess 3000 $
    forAll (oneof [genKnotCase, genCircleCase]) \(p, g) ->
      (admitted p && not (tooMany (run g p))) ==> lawsHold g p
  it "hold on paths that write no label twice" . withMaxSuccess 2000 $
    forAll genDistinctCase \(p, g) ->
      (admitted p && not (tooMany (run g p))) ==> lawsHold g p
  it "a conjunct that shares no label and no node with the rest changes nothing" . withMaxSuccess 3000 $
    forAll (oneof [genRepairCase, genKnotCase, genCircleCase, genDistinctCase]) \(p, g) ->
      forAll genUnrelated \q ->
        ( fewAlternatives 300 p
            && literalOnly p
            && admitted p
            && admitted (p P.:& q)
            && not (tooMany (run g p))
            && not (tooMany (run g (p P.:& q)))
        )
          ==> unrelatedChangesNothing g p q
  it "a path that mk builds is still one with the sides of & and of unions swapped" . withMaxSuccess 3000 $
    forAll (oneof [genRepairCase, genKnotCase, genCircleCase, genDistinctCase]) \(p, _) ->
      (fewAlternatives 300 p && runs p)
        ==> conjoin
          [ counterexample (show p') (sort (bans p') === sort (bans p))
          | (ands, unions') <- [(True, False), (False, True), (True, True)],
            let p' = mirror ands unions' p,
            runs p'
          ]
  it "the generators give paths that mk builds often enough to mean something" do
    -- the same paths every time; the shares are of the paths that commands run
    let share gen =
          let cases = filter runs [fst (unGen gen (mkQCGen seed) 30) | seed <- [1 .. 2000]]
           in (length (filter (null . bans) cases) * 100) `div` length cases
    -- measured: 60, 64, 20 and 94 in a hundred
    zipWith
      (>=)
      (map share [genRepairCase, genKnotCase, genCircleCase, genDistinctCase])
      [50, 50, 15, 80]
      `shouldBe` [True, True, True, True]

-- * Staging

-- | The parts of a top-level sequence.
sequenceParts :: P.Path -> [P.Path]
sequenceParts = \case
  l P.:/ r -> sequenceParts l <> sequenceParts r
  p -> [p]

-- | Whether a path begins by saying where it already is: with @\@@, @\@N@
-- or @!{…}@, or with a group or union one of whose members does. Cut
-- before such a part, the part before it does not know where it ends.
startsWithJoin :: P.Path -> Bool
startsWithJoin = \case
  P.One -> True
  P.Absolute _ -> True
  P.ExcludingNIDs _ -> True
  l P.:/ _ -> startsWithJoin l
  l P.:& r -> startsWithJoin l || startsWithJoin r
  P.Union _ l r -> startsWithJoin l || startsWithJoin r
  -- reversed, the path begins with what it ends with
  P.Backwards p -> endsWithJoin p
  _ -> False
  where
    endsWithJoin = \case
      P.One -> True
      P.Absolute _ -> True
      P.ExcludingNIDs _ -> True
      _ P.:/ r -> endsWithJoin r
      l P.:& r -> endsWithJoin l || endsWithJoin r
      P.Union _ l r -> endsWithJoin l || endsWithJoin r
      P.Backwards p -> startsWithJoin p
      _ -> False

-- | A path that writes no label twice and has no @*@ or regex, over the
-- labels a to l, with @&@, @~@, @\@@, @\@N@, @!{…}@, @|@ and @++@; and a
-- graph over nodes 1 to 5 with edges of those labels.
genDistinctCase :: Gen (P.Path, Graph Text ())
genDistinctCase = do
  size <- choose (2, 9)
  labels <- shuffle (map singleton "abcdefghijkl")
  surface <- fst <$> genPath size labels
  nodes <- sublistOf [1 .. 5 :: Int]
  edges <- flip vectorOf genEdge =<< choose (0, 7)
  pure
    ( surface,
      withWritten surface $
        insertEdges
          edges
          (foldl' (\g k -> insertNode (emptyNode (smallNID k)) g) emptyGraph nodes)
    )
  where
    genEdge =
      edge' <$> choose (1, 5) <*> elements (map singleton "abcdefghijkl") <*> choose (1, 5)
    -- a path of about this many parts from these labels, and the labels left
    genPath :: Int -> [Text] -> Gen (P.Path, [Text])
    genPath size labels
      | size <= 1 = case labels of
          l : rest ->
            frequency
              [ (12, pure (P.Literal l, rest)),
                (2, pure (P.Backwards (P.Literal l), rest)),
                (1, pure (P.One, labels)),
                (2, (,labels) . P.Absolute . smallNID <$> choose (1, 6)),
                ( 1,
                  (,labels) . P.ExcludingNIDs . setFromList . map smallNID
                    <$> (sublistOf [1 .. 4] `suchThat` (not . null))
                )
              ]
          [] -> pure (P.One, [])
      | otherwise = do
          k <- choose (1, size - 1)
          (l, rest) <- genPath k labels
          (r, rest') <- genPath (size - k) rest
          op <-
            frequency
              [ (6, pure (P.:/)),
                (4, pure (P.:&)),
                (1, pure (P.:||)),
                (1, pure (P.:++))
              ]
          wrap <- frequency [(9, pure id), (1, pure P.Backwards)]
          pure (wrap (op l r), rest')

-- | A cut of a path @p/q@ for the staging law: the prefix @p@, what
-- @mk p/q@ alone creates, what @mk p@ creates, and what @mk p/q@ does
-- after it.
data StagingCut = StagingCut
  { prefix :: P.Path,
    whole :: Patch,
    first :: Patch,
    after :: Either ResidualError Residuation
  }
  deriving stock (Eq, Show)

-- | The cuts of a path that the staging law speaks of, on a graph: @q@
-- begins with a step (not with @\@@, @\@N@ or @!{…}@), and @mk p@ and
-- @mk p/q@ alone both decide without asking.
stagingCuts :: P.Path -> Graph Text () -> [StagingCut]
stagingCuts p g =
  [ StagingCut {prefix, whole = whole', first = first', after = run (applyPatch first' g) p}
  | let parts = sequenceParts p,
    k <- [1 .. length parts - 1],
    not (startsWithJoin (parts `indexEx` k)),
    let prefix = foldl1Ex' (P.:/) (take k parts),
    admitted prefix,
    Just first' <- [decided (run g prefix)],
    Just whole' <- [decided (run g p)]
  ]

-- | Whether a way of @mk p/q@ after @mk p@, with what @mk p@ created,
-- creates what @mk p/q@ alone creates (the nodes @mk p@ created are new
-- nodes of the whole, as in 'both').
completes :: StagingCut -> Patch -> Bool
completes c way = samePatch (both c.first way) c.whole

-- | The staging law without its exception: after @mk p@, @mk p/q@
-- decides, and with what @mk p@ created it creates what @mk p/q@ alone
-- creates.
stagingLawAlone :: StagingCut -> Bool
stagingLawAlone c = case decided c.after of
  Just rest -> completes c rest
  Nothing -> False

-- | The documented exception to the staging law (docs/commands.md 4.7,
-- docs/path-route-ir.md section 5): after @mk p@, @mk p/q@ asks; exactly
-- one of its ways is the one the law asks for; and each of the others,
-- with what @mk p@ created and that command's nodes counted as new again,
-- creates everything @mk p/q@ alone creates and more, up to the naming of
-- new nodes. Alone such a way is dropped (selection drops a way that
-- contains another), and it is listed after @mk p@ only because an
-- existing node is never renamed.
stagingException :: StagingCut -> Bool
stagingException c = case c.after of
  Right (Repairs rs@(_ :| _ : _)) ->
    case partition (completes c) (map (.patch) (toList rs)) of
      ([_], others) -> all larger others
      _ -> False
  _ -> False
  where
    larger way =
      let combined = both c.first way
       in patchSubsumes c.whole combined && not (patchSubsumes combined c.whole)

-- | The staging law as the documents state it, with its exception.
stagingHolds :: StagingCut -> Bool
stagingHolds c = stagingLawAlone c || stagingException c

spec_walkStaging :: Spec
spec_walkStaging = describe "staging: mk p and then mk p/q" do
  -- Not a law of every path (the examples of what breaks it are in
  -- "Models.PathPattern.ResiduateSpec"). It holds where q begins with a
  -- step, mk p and mk p/q alone both decide without asking, and the path
  -- writes no label twice and has no * or regex; with one exception,
  -- 'stagingException'.
  let guarded (p, g) = admitted p && not (tooMany (run g p))
  it "leaves what mk p/q alone leaves, where no label is written twice, but for the exception" . withMaxSuccess 4000 $
    forAll genDistinctCase \c@(p, g) ->
      guarded c
        ==> conjoin
          [counterexample (show cut) (stagingHolds cut) | cut <- stagingCuts p g]
  it "is tried on cuts of every kind" do
    -- the same paths every time
    let cases = [unGen genDistinctCase (mkQCGen seed) 30 | seed <- [1 .. 3000]]
        tried = concatMap (uncurry stagingCuts) (filter guarded cases)
        creating =
          [ ()
          | c <- tried,
            not (null c.first.edges),
            c.after /= Right AlreadyResolves
          ]
    length tried `shouldSatisfy` (> 1000)
    length creating `shouldSatisfy` (> 300)
    -- none of them is the exception
    filter (not . stagingLawAlone) tried `shouldBe` []
  describe "the exception: the cases it was found on" do
    -- docs/commands.md 4.7 and cli/tmp/law-staging's report, with node 1
    -- for the current node (node 0 there); q is the last part
    let exceptionAt nodes edges t = do
          let p = typed t
              g = corpusGraph nodes edges
              cuts = stagingCuts p g
              last' = [c | c <- cuts, c.prefix == foldl1Ex' (P.:/) (initEx (sequenceParts p))]
          guarded (p, g) `shouldBe` True
          -- at the last cut the law fails, and the exception holds
          map stagingLawAlone last' `shouldBe` [False]
          map stagingException last' `shouldBe` [True]
          filter (not . stagingHolds) cuts `shouldBe` []
    it "with no union: 1 -x-> 1, 1 -x-> 5 and (x & @)/i/(l & @)/@/b" $
      exceptionAt [5] [(1, "x", 1), (1, "x", 5)] "(x & @)/i/(l & @)/@/b"
    it "on a graph with no edges: ((a | !{@2}) & @)/i/(l & @)/@/b" $
      exceptionAt [] [] "((a | !{@2}) & @)/i/(l & @)/@/b"
    it "the same with ((a | @) & @)" $
      exceptionAt [] [] "((a | @) & @)/i/(l & @)/@/b"
    it "the same with ++" $
      exceptionAt [] [] "((a ++ !{@2}) & @)/i/(l & @)/@/b"
    it "from the generator, seed 6170797, shrunk" $
      exceptionAt [3] [] "((@ & (j ++ @1))/f/(l & !{@2,@3,@4}))/!{@1,@2}/e"
    it "the case the property first failed on" $
      exceptionAt
        [3, 4, 5]
        [(1, "e", 3), (3, "i", 5), (4, "b", 5), (5, "c", 5)]
        "(((a | @5) | !{@2}) & @)/i/(l & @)/@/b"
  where
    -- node 1 (the corpus's current node), these nodes and these edges
    corpusGraph :: [Int] -> [(Int, Text, Int)] -> Graph Text ()
    corpusGraph nodes edges =
      insertEdges
        [edge' s l t | (s, l, t) <- edges]
        (foldl' (\g k -> insertNode (emptyNode (smallNID k)) g) emptyGraph (1 : nodes))

-- * Paths built so that steps could follow each other's edges

-- | Routes of which the last step creates an edge that the first step of
-- another could follow (labels from a small set): rings, chains and what
-- comes between, with a few plain routes beside them. Where steps could
-- follow each other's edges in a circle the path is refused: about a
-- third of these are. Node 1 is the current node; nodes 2 to 5 may exist.
genKnotCase :: Gen (P.Path, Graph Text ())
genKnotCase = do
  k <- choose (2, 5)
  routes <- vectorOf k genRoute
  nodes <- sublistOf [2 .. 5 :: Int]
  edges <-
    flip vectorOf (edge' <$> choose (1, 5) <*> genLabel <*> choose (1, 5))
      =<< frequency [(3, pure 0), (2, choose (1, 3))]
  let p = foldl1Ex' (P.:&) routes
  pure
    ( p,
      withWritten p $
        insertEdges
          edges
          (foldl' (\g n -> insertNode (emptyNode (smallNID n)) g) emptyGraph (1 : nodes))
    )
  where
    genLabel = elements ["a", "b", "c"]
    step = P.Literal <$> genLabel
    written = P.Absolute . smallNID <$> choose (2, 5)
    -- out and back along the same label, to stand on the current node again
    there = step <&> \s -> s P.:/ P.Backwards s
    seq' = foldl1Ex' (P.:/)
    genRoute =
      frequency
        [ -- a/~a/b/@N: the last step creates an edge at the current node
          (6, (\t s n -> seq' [t, s, n]) <$> there <*> step <*> written),
          -- the same, twice over
          ( 2,
            (\t s n t' s' n' -> seq' [t, s, n, P.Backwards s, t', s', n'])
              <$> there
              <*> step
              <*> written
              <*> there
              <*> step
              <*> written
          ),
          -- a step that can follow the edge and goes on from there
          (2, (\s s' -> s P.:/ s') <$> step <*> step),
          (1, step),
          (1, frequency [(3, pure P.Wild), (1, pure (P.RegexMatch [re|^[ab]$|]))]),
          -- a step into a written node, and one into a join
          (1, (P.:/) <$> step <*> written),
          (1, (\s s' -> (s P.:& s') P.:/ P.One) <$> step <*> step)
        ]

-- | Families of this many routes (node 1 is the current node), to count
-- what walking them takes:
--
-- * 'plain': nothing follows, every route creates its own three edges;
-- * 'follow': the third step of each route follows the edge that another
--   route creates with its first step;
-- * 'waiting': a regex step and a literal step at the same distance; the
--   regex step comes after the literal one. With more than one such pair
--   the path has several regex steps, and is refused (V);
-- * 'ring': two joins at the same distance, each of which could follow
--   the edge the other creates: a circle, which is refused (C);
-- * 'relay': the family of the review that was walked again for each
--   route when a created edge counted whenever it was created: route i can
--   follow a loop that route i + 1 creates, but two steps further along
--   than route i is, so at the same pace it does not;
-- * 'relayAtPace': the same relay with every edge created no further
--   along than the step that follows it, and every start known at once.
plain, follow, waiting, ring, relay, relayAtPace :: Int -> (P.Path, Graph Text ())
plain k =
  ( conj [seq3 ("a" <> tshow i) ("b" <> tshow i) ("c" <> tshow i) | i <- [1 .. k]],
    graphWithNodes [1] []
  )
  where
    seq3 a b c = P.Literal a P.:/ P.Literal b P.:/ P.Literal c
follow k =
  ( conj
      [ r
      | i <- [1 .. k],
        let x = P.Literal ("x" <> tshow i)
            a = P.Literal ("a" <> tshow i),
        r <- [x P.:/ P.Backwards x P.:/ a, a P.:/ familyNode i]
      ],
    graphWithNodes (1 : [2000 + i | i <- [1 .. k]]) []
  )
waiting k =
  ( conj
      [ r
      | i <- [1 .. k],
        let a = "a" <> tshow i,
        r <- [P.RegexMatch (either (error . unpack) id (compileRegex (encodeUtf8 ("^" <> a <> "$")))), P.Literal a]
      ],
    graphWithNodes [1] []
  )
ring k =
  ( conj
      [ r
      | i <- [1 .. k],
        let l c = P.Literal (c <> tshow i),
        r <- [(l "b" P.:& l "c") P.:/ P.One, (l "b" P.:& l "a") P.:/ P.One]
      ],
    graphWithNodes [1] $
      concat [[edge' 1 ("c" <> tshow i) (2000 + i), edge' 1 ("a" <> tshow i) 1] | i <- [1 .. k]]
  )
relay k =
  ( conj $
      [P.Literal (p i) P.:/ P.Literal (p (i - 1)) P.:/ home | i <- [1 .. k - 1]]
        <> [P.Literal "d" P.:/ P.Backwards (P.Literal "d") P.:/ P.Literal (p (k - 1)) P.:/ home],
    graphWithNodes [1] []
  )
  where
    p i = "p" <> tshow i
    home = P.Absolute (smallNID 1)
relayAtPace k =
  ( conj $
      [ foldl1Ex' (P.:/) (replicate (k - i + 1) (P.Literal "z") :: [P.Path])
          P.:/ home
          P.:/ P.Literal (p i)
          P.:/ P.Literal (p (i - 1))
          P.:/ home
      | i <- [1 .. k - 1]
      ]
        <> [P.Literal "d" P.:/ P.Backwards (P.Literal "d") P.:/ P.Literal (p (k - 1)) P.:/ home],
    graphWithNodes [1] [edge' 1 "z" 1]
  )
  where
    p i = "p" <> tshow i
    home = P.Absolute (smallNID 1)

-- | A family beside this many steps that have two edges to follow each.
beside :: Int -> (P.Path, Graph Text ()) -> (P.Path, Graph Text ())
beside m (p, g) =
  ( conj (p : [P.Literal ("e" <> tshow j) P.:/ P.Literal ("f" <> tshow j) | j <- [1 .. m]]),
    insertEdges [edge' 1 ("e" <> tshow j) (3000 + 2 * j + d) | j <- [1 .. m], d <- [0, 1]] g
  )

conj :: [P.Path] -> P.Path
conj = foldl1Ex' (P.:&)

familyNode :: Int -> P.Path
familyNode i = P.Absolute (smallNID (2000 + i))

graphWithNodes :: [Int] -> [Edge Text] -> Graph Text ()
graphWithNodes nodes edges =
  insertEdges edges (foldl' (\g n -> insertNode (emptyNode (smallNID n)) g) emptyGraph nodes)

-- | What a family creates, in edges and new nodes, with no limit on walks.
familyCreates :: (P.Path, Graph Text ()) -> Maybe (Int, Int)
familyCreates (p, g) = case residuateUpTo P.UnlimitedRepairs g p of
  Right (Repairs (r :| [])) -> Just (length r.patch.edges, length [() | Fresh _ <- toList r.patch.nodes])
  _ -> Nothing

-- | Paths in which steps at the same distance could follow each other's
-- edges: joins, steps into written nodes and steps that exclude nodes, all
-- from the current node (node 1), with a step or two after some. Most of
-- these have a circle and are refused (four in five). Nodes 2 to 4 exist.
genCircleCase :: Gen (P.Path, Graph Text ())
genCircleCase = do
  k <- choose (2, 5)
  routes <- frequency [(1, vectorOf k genRoute), (1, (<>) <$> circle <*> vectorOf (k - 2) genRoute)]
  edges <-
    flip vectorOf (edge' <$> choose (1, 4) <*> genLabel <*> choose (1, 4))
      =<< frequency [(3, pure 0), (2, choose (1, 3))]
  pure (conj routes, graphWithNodes [1 .. 4] edges)
  where
    genLabel = elements ["a", "b", "c"]
    step = P.Literal <$> genLabel
    written = P.Absolute . smallNID <$> choose (2, 4)
    excluding = P.ExcludingNIDs . setFromList . map smallNID <$> (sublistOf [2 .. 4] `suchThat` (not . null))
    -- x/!{@N} could follow the join's x edge, if the join goes along y to
    -- node M; and the join could follow the x edge that x/!{@N} creates
    circle = do
      (x, y) <- ((,) <$> genLabel <*> genLabel) `suchThat` uncurry (/=)
      (n, m) <- ((,) <$> choose (2, 4) <*> choose (2, 4)) `suchThat` uncurry (/=)
      let at = P.Absolute . smallNID
      pure
        [ P.Literal x P.:/ P.ExcludingNIDs (setFromList [smallNID n]),
          (P.Literal y P.:& P.Literal x) P.:/ P.One,
          P.Literal y P.:/ at m,
          P.Literal x P.:/ at n
        ]
    genRoute = do
      first' <-
        frequency
          [ (4, (\s s' -> (s P.:& s') P.:/ P.One) <$> step <*> step),
            (3, (P.:/) <$> step <*> written),
            (3, (P.:/) <$> step <*> excluding),
            (1, step),
            (1, pure P.Wild)
          ]
      frequency [(3, pure first'), (1, (first' P.:/) <$> step)]

spec_walkFamilies :: Spec
spec_walkFamilies = describe "steps that follow edges other routes create" do
  describe "the families" do
    it "nothing follows: every route creates its own three edges" $
      familyCreates (plain 6) `shouldBe` Just (18, 18)
    it "a later step follows the edge another route creates nearer its start" $
      -- 1 -x-> new and 1 -a-> N for each pair of routes
      familyCreates (follow 6) `shouldBe` Just (12, 6)
    it "a regex step comes after a literal step at its own distance" $
      -- only the a edge
      familyCreates (waiting 1) `shouldBe` Just (1, 1)
    it "several such pairs are several regex steps: refused" $
      bans (fst (waiting 6)) `shouldBe` ["SeveralWildcards"]
    it "a ring, two joins that could follow each other's edge, is refused" $
      bans (fst (ring 6)) `shouldBe` ["CircularSuppliers"]
    it "the relay: no first step sees the loop created two steps further along" $
      -- each route creates its two edges: the last, 1 -d-> new and 1 -p5-> 1
      familyCreates (relay 6) `shouldBe` Just (12, 6)
    it "the relay at the same pace: each first step follows the loop the next route creates" $
      -- 1 -p5-> 1 by the last route, then 1 -p4-> 1 … 1 -p0-> 1, and 1 -d-> new
      familyCreates (relayAtPace 6) `shouldBe` Just (7, 1)
  it "the result does not depend on the order of the routes" do
    -- every order of the conjuncts of each family
    let orders (p, g) = [(conj routes, g) | routes <- permutations (routesOf p)]
    forM_ [f k | f <- [plain, follow, relay, relayAtPace], k <- [2, 3]] \c@(p, g) ->
      forM_ (orders c) \(p', _) ->
        (show p', sameOutcome (run g p) (run g p')) `shouldBe` (show p', True)
  it "on random routes: any order of the routes gives the same" . withMaxSuccess 3000 $
    forAll (oneof [genKnotCase, genCircleCase]) \(p, g) ->
      forAll (shuffle (routesOf p)) \routes ->
        (admitted p && not (tooMany (run g p)))
          ==> let p' = conj routes
               in counterexample (show (run g p, run g p')) $
                    sameOutcome (run g p) (run g p')
  it "and a path that is refused is refused in any order, by the same bans" . withMaxSuccess 3000 $
    forAll (oneof [genKnotCase, genCircleCase]) \(p, _) ->
      forAll (shuffle (routesOf p)) \routes ->
        runs p ==> sort (bans (conj routes)) === sort (bans p)
  where
    routesOf = \case
      l P.:& r -> routesOf l <> routesOf r
      p -> [p]

-- * Cost

-- | Bytes this thread allocates while running an action. Unlike time, this
-- does not depend on the load of the machine.
allocatedBy :: IO a -> IO Int64
allocatedBy action = do
  before <- getAllocationCounter
  _ <- action
  afterwards <- getAllocationCounter
  pure (before - afterwards)

-- | What walking a family of this many routes allocates: the path is
-- compiled and the graph forced before the count starts, and everything
-- that is created is counted before it stops.
walkAllocates :: (Int -> (P.Path, Graph Text ())) -> Int -> IO Int64
walkAllocates family k = do
  let (p, g) = family k
      q = compilePath p
  _ <- evaluate (force q)
  _ <- evaluate (length (show g))
  allocatedBy do
    let walked = candidatesUpTo Nothing (const Nothing) g q
    created <-
      evaluate . force $
        fmap (map (\c -> (length c.residual.patch.edges, length c.residual.patch.nodes))) walked
    fmap (not . null) created `shouldBe` Just True

-- | The plans of a path's alternatives.
plansOf :: P.Path -> [Plan]
plansOf p = map (planOf corpusCurrent) (toList (compilePath p).alternatives)

-- | What building a random case took, for the property below: if @mk@
-- builds its path, with few alternatives and at most 300 walks. The walks
-- are counted, not read off what 'run' answers: a path that resolves is
-- answered by matching once its walks pass the limit, and such a path can
-- have millions of walks ('resolvesWithManyWalks').
costedCase :: (P.Path, Graph Text ()) -> Maybe WalkCost
costedCase (p, g)
  | fewAlternatives 300 p && admitted p = walkCostUpTo 300 g p
  | otherwise = Nothing

-- | A case of the generators that resolves, with 1,953,125 walks (5 ^ 9).
resolvesWithManyWalks :: (P.Path, Graph Text ())
resolvesWithManyWalks =
  ( [path| (a & a & a)/~c/(a & a & a)/~b |],
    graphWithEdges
      [ edge' 1 "a" 2,
        edge' 1 "c" 2,
        edge' 2 "a" 2,
        edge' 2 "a" 3,
        edge' 2 "b" 2,
        edge' 2 "b" 3,
        edge' 2 "c" 2,
        edge' 3 "b" 1,
        edge' 3 "c" 3,
        edge' 4 "b" 2
      ]
  )

-- | The value of an action, or 'Nothing' if it allocates more than this
-- many bytes: a count of work, so this does not depend on the load. The
-- limit is off again however the action ends.
withinAllocation :: Int64 -> IO a -> IO (Maybe a)
withinAllocation bytes action = do
  result <-
    E.bracket_
      (setAllocationCounter bytes >> enableAllocationLimit)
      disableAllocationLimit
      (E.try action)
  pure case result of
    Left E.AllocationLimitExceeded -> Nothing
    Right a -> Just a

-- | What building takes, in counts of work ('WalkCost': walks, and
-- batches of operations over all walks), not in time.
spec_walkCostCounted :: Spec
spec_walkCostCounted = describe "what building takes, counted" do
  let cost = uncurry (flip walkCost)
  it "a construction has no more batches of operations than operations" . withMaxSuccess 3000 $
    forAll (oneof [genRepairCase, genKnotCase, genCircleCase, genDistinctCase]) \c@(p, _) ->
      case costedCase c of
        Nothing -> discard
        Just spent ->
          let plans = plansOf p
           in conjoin
                [ -- every operation is in one batch, and no batch is empty
                  conjoin
                    [ length plan.batches <= length plan.operations
                        && sort (concat plan.batches) == sort plan.operations
                        && not (any null plan.batches)
                    | plan <- plans
                    ],
                  -- a walk goes through the batches of its alternative
                  -- once: nothing is done again
                  counterexample (show spent) $
                    spent.batches
                      <= max 1 spent.walks * sum (map (length . (.operations)) plans)
                ]
  it "counts no case past 300 walks, also one that resolves" do
    let (p, g) = resolvesWithManyWalks
    -- mk answers it by matching, past the limit
    run g p `shouldBe` Right AlreadyResolves
    -- walked to the end, it allocates gigabytes
    counted <-
      withinAllocation (512 * 1024 * 1024) . evaluate . force $
        fmap (\spent -> (spent.walks, spent.batches)) (costedCase (p, g))
    counted `shouldBe` Just Nothing
  it "limits allocation only while the action runs, however it ends" do
    -- an action that fails with another exception than the limit's
    failed <- E.try (withinAllocation (1024 * 1024) (E.throwIO (E.ErrorCall "failed") :: IO ()))
    failed `shouldBe` Left (E.ErrorCall "failed")
    -- far more than the limit, afterwards
    allocated <- E.try @E.AllocationLimitExceeded (evaluate (length (show [1 .. 1000000 :: Int])))
    either (const Nothing) Just allocated `shouldBe` Just 6888897
  it "one walk where steps follow edges created nearer the start" $
    forM_ [plain, follow, relay, relayAtPace] \family -> forM_ [5, 20] \k ->
      (cost (family k)).walks `shouldBe` 1
  it "the relay takes three batches at every length" $
    -- the first steps; the steps home, with the out and back of the last
    -- route; and the last route's step home
    forM_ [2, 4, 8, 16, 32, 64] \k ->
      (k, cost (relay k)) `shouldBe` (k, WalkCost 1 3)
  it "the batches do not grow with the number of routes" $
    forM_ [plain, follow, relay] \family ->
      (cost (family 40)).batches `shouldBe` (cost (family 10)).batches
  it "the relay at the same pace has a batch for each distance, and a few" $
    (cost (relayAtPace 40)).batches `shouldSatisfy` (< 100)
  it "steps with two edges to follow multiply the walks once" do
    (cost (beside 5 (relayAtPace 10))).walks `shouldBe` 32
    (cost (beside 5 (relay 10))).walks `shouldBe` 32

-- | Building a path costs no more than polynomially in its steps: with
-- four times the routes, a family in which nothing follows allocates about
-- four times as much, and no family more than about sixteen times as much
-- (quadratic). Cubic work would allocate 64 times as much. In the relay at
-- the same pace the routes grow with their number, so twice the routes
-- are four times the steps. (The tests are built without optimization,
-- like the development build.)
spec_walkCost :: Spec
spec_walkCost = describe "what walking allocates, with four times the steps" do
  it "nothing follows: about linearly" $ grows 8 plain 30 120
  it "steps follow edges created nearer the start: at most quadratically" $ grows 24 follow 30 120
  it "the relay: at most quadratically" $ grows 24 relay 30 120
  it "the relay at the same pace: at most quadratically" $ grows 24 relayAtPace 12 24
  where
    grows :: Double -> (Int -> (P.Path, Graph Text ())) -> Int -> Int -> IO ()
    grows limit family small large = do
      lesser <- walkAllocates family small
      greater <- walkAllocates family large
      let ratio = fromIntegral greater / fromIntegral lesser :: Double
      unless (ratio < limit) do
        expectationFailure $
          "4 times the steps allocated "
            <> show ratio
            <> " times as much ("
            <> show lesser
            <> " then "
            <> show greater
            <> " bytes)"
