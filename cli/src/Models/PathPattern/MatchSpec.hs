-- | Compares the matcher, and the effectful loader in front of it, with a
-- brute-force oracle that tries every assignment of variables and labels.
module Models.PathPattern.MatchSpec where

import Effectful.State.Dynamic
import Graph.GraphMetadataEditing (runInMemoryGraphMetadataEditing)
import Graph.PathPattern.Resolve
import Models.Connect
import Models.Edge
import Models.Graph (Graph, edgeSet, nidSet)
import Models.NID
import Models.Node
import Models.Path.TH (path)
import Models.PathPattern
import Models.PathPattern.Compile (compilePath)
import Models.PathPattern.Match
import Models.PathPattern.TestCorpus
import MyPrelude
import System.Mem (getAllocationCounter)
import Utils.Testing

-- | Every witness of a pattern, by exhaustive search.
bruteForce :: Graph Text () -> NID -> PatternPath -> Set Witness
bruteForce g cur p =
  setFromList
    [ Witness (mapFromList nodes) (mapFromList labels)
    | boundOk,
      nodes <- assignments (patternVars p) allNodes,
      labels <- assignments (keys specs) allLabels,
      let w = Witness (mapFromList nodes) (mapFromList labels)
          value = \case
            Current -> cur
            Fixed n -> n
            Var v -> fromJustEx (lookup v w.nodes),
      all (\v -> testAccepts (vertexTest p v) (value v)) (patternVertices p),
      all (edgeHolds value w) (mapToList specs)
    ]
  where
    specs = patternEdges p
    allNodes = toList (nidSet g)
    edges = edgeSet g
    allLabels = ordNub (map (.transition) (toList edges))
    boundOk =
      all
        (`member` nidSet g)
        [n | v <- patternVertices p, Just n <- [boundNID v]]
    boundNID = \case
      Current -> Just cur
      Fixed n -> Just n
      Var _ -> Nothing
    edgeHolds value w (e, spec) =
      let l = fromJustEx (lookup e w.labels)
          s = value spec.source
          t = value spec.target
       in matcherAccepts spec.matcher l
            && case spec.direction of
              Out -> Edge s l t `member` edges
              In -> Edge t l s `member` edges
    assignments :: [k] -> [v] -> [[(k, v)]]
    assignments ks vs = traverse (\k -> map (k,) vs) ks

resolveIn :: Graph Text () -> PathQuery -> ResolvedPath
resolveIn g q =
  resolveQuery corpusCurrent q
    & runInMemoryGraphMetadataEditing
    & evalStateLocal g
    & runPureEff

spec_matchOracle :: Spec
spec_matchOracle = describe "matching agrees with brute force" do
  forM_ corpusQueries \surface -> it (show surface) do
    let q = compilePath surface
    forM_ corpusGraphs \g -> do
      let resolved = resolveIn g q
          byPattern =
            mapFromList
              [(r.patternPath, r) | r <- resolved.resolutions] ::
              Map PatternPath Resolution
      forM_ (toList q.alternatives) \p -> do
        let expected = bruteForce g corpusCurrent p
            pure' = matchPattern (lookupIn g) (Just corpusCurrent) p
            loaded = lookup p byPattern
            context = show (p, g)
        annotate context do
          -- the pure matcher and the loader agree with the oracle
          fmap (setFromList . witnesses) pure'
            `shouldBe` justIfTrue (not (null expected)) expected
          fmap (setFromList . witnesses) loaded
            `shouldBe` justIfTrue (not (null expected)) expected
          forM_ pure' \r -> do
            let ws = witnesses r
                keyed = map (concreteRoutes r) ws
                oracleRoutes w = concreteRoutes r w
            length ws `shouldBe` length expected
            -- the first witness is the lexicographic minimum
            headMay keyed
              `shouldBe` headMay (sort (map oracleRoutes (toList expected)))
            targets r
              `shouldBe` setFromList
                [ n
                | w <- toList expected,
                  rt <- oracleRoutes w,
                  let n = maybe rt.root (.target) (lastMay rt.hops)
                ]
            setFromList (selectedRoutes r)
              `shouldBe` mapSet
                (\w -> fromJustEx (headMay (oracleRoutes w)))
                expected
            routeLastEdges r
              `shouldBe` lastEdgesOracle r (toList expected)
  where
    annotate :: String -> IO () -> IO ()
    annotate context action =
      action `catch` \(e :: SomeException) ->
        throwIO (userError (context <> "\n" <> show e))

lastEdgesOracle :: Resolution -> [Witness] -> Map Int (Set (Edge Text))
lastEdgesOracle r ws =
  unionsWith (<>) . map (uncurry singletonMap) $
    [ (i, singletonSet (concreteEdge before h))
    | w <- ws,
      (i, rt) <- zip [0 ..] (concreteRoutes r w),
      Just h <- [lastMay rt.hops],
      let before = case reverse rt.hops of
            _ : prev : _ -> prev.target
            _ -> rt.root
    ]

-- | 'patternComponents' by merging, one edge at a time, the groups that
-- share a variable with it.
componentsReference :: PatternPath -> [Component]
componentsReference p =
  map (uncurry Component) . sortOn (headMay . map position . snd) $
    map (second (sortOn position)) (foldl' add [] order)
  where
    specs = patternEdges p
    order = edgeOrder p
    position e = length (takeWhile (/= e) order)
    varsOf e =
      setFromList
        [v | Just s <- [lookup e specs], Var v <- [s.source, s.target]]
    add groups e =
      let vs = varsOf e
          (touching, rest) =
            partition (not . null . intersection vs . fst) groups
       in (vs <> foldMap fst touching, e : concatMap snd touching) : rest

-- | Routes that share prefixes, meet at variables and pass through bound
-- vertices. Edge ids follow first appearance, as the compiler numbers them.
genRoutes :: Gen PatternPath
genRoutes = genRoutesOf 6 4

-- | 'genRoutes' with at most this many routes, each adding at most this
-- many steps of its own.
genRoutesOf :: Int -> Int -> Gen PatternPath
genRoutesOf maxRoutes maxExtra = do
  count <- chooseInt (1, maxRoutes)
  routes <- foldlM (\rs _ -> (rs <>) . pure <$> genRoute rs) [] [1 .. count]
  pure (pat routes)
  where
    genVertex =
      frequency
        [ (6, var <$> chooseInt (0, 7)),
          (1, pure current),
          (1, fixedN <$> chooseInt (1, 2))
        ]
    genRoot = elements [current, fixedN 1]
    genRoute earlier = do
      (root, prefix) <- case earlier of
        [] -> (,[]) <$> genRoot
        _ -> do
          r <- elements earlier
          shared <- chooseInt (0, length r.hops)
          if shared == 0
            then (,[]) <$> genRoot
            else pure (r.root, take shared r.hops)
      let used = length (ordNub [h.edge | r <- earlier, h <- r.hops])
      extra <- chooseInt (0, maxExtra)
      hops <- for [used .. used + extra - 1] \e ->
        Hop (EdgeId e)
          <$> elements [Out, In]
          <*> elements [MLiteral "a", MWild]
          <*> genVertex
      pure (Route root (prefix <> hops))

spec_patternComponents :: Spec
spec_patternComponents = describe "patternComponents" do
  it "agrees with a reference on the corpus" do
    forM_ corpusQueries \surface ->
      forM_ (toList (compilePath surface).alternatives) \p ->
        patternComponents p `shouldBe` componentsReference p
  it "agrees with a reference on random patterns" $
    withMaxSuccess 2000 $
      forAll genRoutes \p ->
        patternComponents p === componentsReference p

-- * Projections and steps against their definitions before indexing

-- | The witnesses of only the components the given edges belong to, by
-- scanning every component for every edge.
witnessesForReference :: [EdgeId] -> Resolution -> [Witness]
witnessesForReference es r =
  witnesses
    Resolution
      { patternPath = r.patternPath,
        current = r.current,
        components =
          [(c, ps) | (c, ps) <- r.components, any (`elem` c.edges) es]
      }

targetsReference :: Resolution -> Set NID
targetsReference r = foldMap endValues r.patternPath.routes
  where
    endValues rt = case routeEnd rt of
      Var v ->
        setFromList
          [ n
          | (c, ps) <- r.components,
            v `member` c.vars,
            p <- ps,
            Just n <- [lookup v p.nodes]
          ]
      bound -> singletonSet (vertexValue r (Witness mempty mempty) bound)

selectedRoutesReference :: Resolution -> [ConcreteRoute]
selectedRoutesReference r =
  ordNub
    [ fromJustEx (headMay (concreteRoutes r w))
    | w <- witnessesForReference (map (.edge) selected.hops) r
    ]
  where
    selected :| _ = r.patternPath.routes

routeLastEdgesReference :: Resolution -> Map Int (Set (Edge Text))
routeLastEdgesReference r =
  mapFromList
    [ (i, setFromList [lastEdge w | w <- witnessesForReference [h.edge] r])
    | (i, rt) <- zip [0 ..] (toList r.patternPath.routes),
      Just h <- [lastMay rt.hops],
      let concrete w = fromJustEx (index (concreteRoutes r w) i)
          lastEdge w = case lastMay (concrete w).hops of
            Just ch -> concreteEdge (penultimate w rt) ch
            Nothing -> error "routeLastEdgesReference: unreachable"
    ]
  where
    penultimate w rt = case reverse rt.hops of
      _ : before : _ -> vertexValue r w before.target
      _ -> vertexValue r w rt.root

-- | Everything commands read off a resolution, in the order they read it.
data Projections = Projections
  { targetNodes :: Set NID,
    selected :: [ConcreteRoute],
    lastEdges :: Map Int (Set (Edge Text)),
    firstWitness :: [ConcreteRoute]
  }
  deriving stock (Eq, Show)

projections :: Resolution -> Projections
projections r =
  Projections
    { targetNodes = targets r,
      selected = firstSelectedRoute r : selectedRoutes r,
      lastEdges = routeLastEdges r,
      firstWitness = firstWitnessKey r
    }

projectionsReference :: Resolution -> Projections
projectionsReference r =
  Projections
    { targetNodes = targetsReference r,
      selected = case selectedRoutesReference r of
        rs@(chosen : _) -> chosen : rs
        [] -> [],
      lastEdges = routeLastEdgesReference r,
      firstWitness = maybe [] (concreteRoutes r) (headMay (witnesses r))
    }

-- | A graph over nodes 1 to 3 with about a third of the possible edges
-- labelled a or b.
genGraph :: Gen (Graph Text ())
genGraph = do
  let possible = [edge' s t d | s <- [1 .. 3], d <- [1 .. 3], t <- ["a", "b"]]
  kept <- filterM (const ((== 0) <$> chooseInt (0, 2))) possible
  pure (graphWithEdges (edge' 1 "a" 1 : edge' 2 "b" 3 : kept))

spec_projections :: Spec
spec_projections = describe "projections agree with scanning every component" do
  it "on the corpus" do
    forM_ corpusQueries \surface -> do
      let q = compilePath surface
      forM_ corpusGraphs \g -> do
        let pure' =
              mapMaybe
                (matchPattern (lookupIn g) (Just corpusCurrent))
                (toList q.alternatives)
        forM_ (pure' <> (resolveIn g q).resolutions) \r ->
          projections r `shouldBe` projectionsReference r
  it "on random patterns" $
    withMaxSuccess 3000 $
      forAll ((,) <$> genRoutesOf 4 2 <*> genGraph) \(p, g) ->
        -- compared when it matches with few enough witnesses to list
        let matched = do
              r <- matchPattern (lookupIn g) (Just corpusCurrent) p
              r <$ guard (product (map (size . snd) r.components) <= 200)
            size = length . take 201
            several = any ((> 1) . size . snd) . (.components)
         in checkCoverage
              . cover 60 (isJust matched) "compared"
              . cover 30 (any several matched) "with several solutions"
              $ case matched of
                Nothing -> property True
                Just r -> projections r === projectionsReference r

-- | 'stepEdges' by trying every edge of the node.
stepEdgesReference :: Lookup -> NID -> EdgeSpec -> [(Text, NID)]
stepEdgesReference lookupNode source spec = case lookupNode source of
  Nothing -> []
  Just n ->
    [ (c.transition, c.node)
    | c <- toList case spec.direction of
        Out -> n.outgoing
        In -> n.incoming,
      matcherAccepts spec.matcher c.transition
    ]

spec_stepEdges :: Spec
spec_stepEdges = describe "stepEdges" do
  it "agrees with trying every edge, in the same order" $
    withMaxSuccess 3000 $
      forAll ((,) <$> genNode <*> genSpec) \(n, spec) ->
        stepEdges (const (Just n)) n.nid spec
          === stepEdgesReference (const (Just n)) n.nid spec
  it "finds nothing from a node that is absent" do
    stepEdges (const Nothing) (smallNID 1) (EdgeSpec current Out MWild (var 0))
      `shouldBe` []
  where
    names = ["", "a", "a ", "aa", "ab", "b", "B", "\233"] :: [Text]
    genConnect = Connect <$> elements names <*> (smallNID <$> chooseInt (0, 3))
    genConnects = setFromList <$> listOf genConnect
    genNode :: Gen (Node Text ())
    genNode = do
      incoming <- genConnects
      outgoing <- genConnects
      pure Node {nid = smallNID 0, incoming, outgoing, augmentation = ()}
    genSpec =
      EdgeSpec current
        <$> elements [Out, In]
        <*> oneof
          [ MLiteral <$> elements ("c" : "a\0" : names),
            pure MWild,
            elements [MRegex [re|a|], MRegex [re|^b$|]]
          ]
        <*> pure (var 0)

-- | Bytes this thread allocates while running an action. Unlike time, this
-- does not depend on the load of the machine.
allocatedBy :: IO a -> IO Int64
allocatedBy action = do
  before <- getAllocationCounter
  _ <- action
  after <- getAllocationCounter
  pure (before - after)

-- | One route of this many steps from the current node: one component.
chainPattern :: Int -> PatternPath
chainPattern n = pat [route current [out i "a" (var i) | i <- [1 .. n]]]

-- | The same chain with its variables numbered from the far end.
backwardChainPattern :: Int -> PatternPath
backwardChainPattern n =
  pat [route current [out i "a" (var (n - i)) | i <- [1 .. n]]]

-- | This many routes of one step each: as many components.
fanPattern :: Int -> PatternPath
fanPattern n = pat [route current [out i "a" (var i)] | i <- [1 .. n]]

-- | Loading and matching a pattern must not cost time quadratic in its
-- steps. Both sizes below differ by a factor of 16; work that is linear up
-- to a logarithm allocates about 20 times as much, quadratic work 256 times.
-- (The tests are built without optimization, like the development build,
-- so this also sees work that only an optimizer would share.)
spec_matchingIsNotQuadratic :: Spec
spec_matchingIsNotQuadratic = describe "allocation grows about linearly" do
  it "patternComponents" do
    grows \n -> do
      let cs = patternComponents (chainPattern n)
      sum (map (\c -> length c.vars + length c.edges) cs) `shouldBe` 2 * n
  it "loadPattern" do
    grows \n -> do
      let loaded =
            runIdentity (loadPattern (pure . lookupIn g) cur (chainPattern n))
      fmap (sum . map length . toList . fst) loaded `shouldBe` Just n
  it "matchPattern" do
    grows \n -> do
      let matched = matchPattern (lookupIn g) (Just cur) (chainPattern n)
      fmap (map (length . (.labels)) . witnesses) matched `shouldBe` Just [n]
  it "patternComponents, variables numbered backwards" do
    grows \n -> do
      let cs = patternComponents (backwardChainPattern n)
      sum (map (\c -> length c.vars + length c.edges) cs) `shouldBe` 2 * n
  it "routeLastEdges, one component per route" do
    grows \n -> do
      let matched = matchPattern (lookupIn g) (Just cur) (fanPattern n)
      fmap (sum . map length . toList . routeLastEdges) matched
        `shouldBe` Just n
  it "targets, one component per route" do
    grows \n -> do
      let matched = matchPattern (lookupIn g) (Just cur) (fanPattern n)
      fmap targets matched `shouldBe` Just (singletonSet cur)
  it "a literal step does not visit the other edges of its node" do
    -- 16 times the edges: visiting them all allocates 16 times as much,
    -- finding the label in the ordered set hardly more
    let wide :: Map Int (Node Text ())
        wide = mapFromList [(n, wideNode n) | n <- [small, 16 * small]]
    _ <- evaluate (force wide)
    growsLessThan 4 \n -> do
      let step i =
            stepEdges
              (const (lookup n wide))
              cur
              (EdgeSpec current Out (MLiteral (tshow i)) (var 0))
      map step [1 .. 100 :: Int]
        `shouldBe` [[(tshow i, cur)] | i <- [1 .. 100 :: Int]]
  where
    cur = smallNID 1
    g = graphWithEdges [edge' 1 "a" 1] :: Graph Text ()
    wideNode :: Int -> Node Text ()
    wideNode n =
      Node
        { nid = cur,
          incoming = mempty,
          outgoing = setFromList [Connect (tshow i) cur | i <- [1 .. n]],
          augmentation = ()
        }
    small = 250 :: Int
    grows :: (Int -> IO ()) -> IO ()
    grows = growsLessThan 64
    growsLessThan :: Double -> (Int -> IO ()) -> IO ()
    growsLessThan limit run = do
      lesser <- allocatedBy (run small)
      greater <- allocatedBy (run (16 * small))
      let ratio = fromIntegral greater / fromIntegral lesser :: Double
      unless (ratio < limit) do
        expectationFailure $
          "16 times the steps allocated "
            <> show ratio
            <> " times as much ("
            <> show lesser
            <> " then "
            <> show greater
            <> " bytes)"

spec_matchExamples :: Spec
spec_matchExamples = describe "matching examples" do
  let g = graphWithEdges
      matched edges surface =
        (resolveIn (g edges) (compilePath surface)).resolutions
  it "a & a has k * k witnesses stored as 2k partials" do
    let edges = [edge' 1 "a" n | n <- [2 .. 11]]
    case matched edges [path| a & a |] of
      [r] -> do
        length (witnesses r) `shouldBe` 100
        sum (map (length . snd) r.components) `shouldBe` 20
      rs -> expectationFailure ("expected one resolution: " <> show rs)
  it "(a & a)/@ is one component" do
    let edges = [edge' 1 "a" n | n <- [2 .. 11]]
    case matched edges [path| (a & a)/@ |] of
      [r] -> do
        length r.components `shouldBe` 1
        length (witnesses r) `shouldBe` 10
      rs -> expectationFailure ("expected one resolution: " <> show rs)
  it "(* & *)/@ chooses labels independently" do
    let edges = [edge' 1 "x" 2, edge' 1 "y" 2]
    case matched edges [path| (* & *)/@ |] of
      [r] -> length (witnesses r) `shouldBe` 4
      rs -> expectationFailure ("expected one resolution: " <> show rs)
  it "*/@/(b & c) makes one label choice for the shared step" do
    let edges =
          [edge' 1 "x" 2, edge' 1 "y" 2, edge' 2 "b" 3, edge' 2 "c" 4]
    case matched edges [path| */@/(b & c) |] of
      [r] -> length (witnesses r) `shouldBe` 2
      rs -> expectationFailure ("expected one resolution: " <> show rs)
  it "a route through a fixed node spans two components" do
    let edges = [edge' 1 "a" 5, edge' 5 "b" 6, edge' 5 "b" 7]
    case matched edges [path| a/@000000000005/b |] of
      [r] -> do
        length r.components `shouldBe` 2
        map (map (.target) . (.hops)) (selectedRoutes r)
          `shouldBe` [[smallNID 5, smallNID 6], [smallNID 5, smallNID 7]]
      rs -> expectationFailure ("expected one resolution: " <> show rs)
  it "x/(@ & a)/@ has final edges x and a, route 0 selecting x" do
    let edges = [edge' 1 "x" 2, edge' 2 "a" 2]
    case matched edges [path| x/(@ & a)/@ |] of
      [r] -> do
        routeLastEdges r
          `shouldBe` mapFromList
            [ (0, singletonSet (edge' 1 "x" 2)),
              (1, singletonSet (edge' 2 "a" 2))
            ]
        map (map (.label) . (.hops)) (selectedRoutes r) `shouldBe` [["x"]]
      rs -> expectationFailure ("expected one resolution: " <> show rs)
  it "x/(a & @)/@ selects a" do
    let edges = [edge' 1 "x" 2, edge' 2 "a" 2]
    case matched edges [path| x/(a & @)/@ |] of
      [r] ->
        map (map (.label) . (.hops)) (selectedRoutes r)
          `shouldBe` [["x", "a"]]
      rs -> expectationFailure ("expected one resolution: " <> show rs)
  it "components follow first appearance; witnesses are component-major" do
    let edges =
          [edge' 1 "a" 2, edge' 1 "a" 3, edge' 1 "b" 4, edge' 1 "b" 5]
    case matched edges [path| a & b |] of
      [r] -> do
        map ((.edges) . fst) r.components `shouldBe` [[EdgeId 0], [EdgeId 1]]
        map (map (map (.target) . (.hops)) . concreteRoutes r) (witnesses r)
          `shouldBe` [ [[smallNID x], [smallNID y]]
                     | x <- [2, 3],
                       y <- [4, 5]
                     ]
      rs -> expectationFailure ("expected one resolution: " <> show rs)
  it "a missing fixed node does not match" do
    matched [] [path| @000000000005 |] `shouldBe` []
  it "orders alternatives by their smallest witness" do
    let edges = [edge' 1 "a" 9, edge' 1 "b" 2]
        firsts =
          map
            (map (.label) . (.hops) . firstSelectedRoute)
            (matched edges [path| b ++ a |])
    firsts `shouldBe` [["a"], ["b"]]
