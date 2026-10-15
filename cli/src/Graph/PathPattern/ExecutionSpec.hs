module Graph.PathPattern.ExecutionSpec where

import Effectful.State.Dynamic
import Error.UserError
import Error.Warn (Warn, ignoreWarnings)
import Graph.FreshNID
import Graph.GraphMetadataEditing
import Graph.PathPattern.Execution
import Models.Graph (Graph)
import Models.NID
import Models.Path.Simple qualified as P
import Models.Path.TH (path)
import Models.PathPattern.Admit (checkExpansion)
import Models.PathPattern.ResiduateSpec (Does (..), designExamples, graphOf, typed)
import Models.PathPattern.WalkSpec (withinAllocation)
import MyPrelude
import System.Random (StdGen, mkStdGen)
import Utils.Testing

-- | Count every read of a node's metadata that reaches the graph.
countingReads ::
  (GraphMetadataEditing :> r, State (Map NID Int) :> r) =>
  Eff r a ->
  Eff r a
countingReads = interpose @GraphMetadataEditing \_ -> \case
  GetNodeMetadata nid -> do
    modify @(Map NID Int) (insertWith (+) nid 1)
    getNodeMetadata nid
  TouchNode nid -> touchNode @Text nid
  DeleteNode nid -> deleteNode @Text nid
  InsertEdge e -> insertEdge e
  DeleteEdge e -> deleteEdge e
  ListNodes -> listNodes @Text

-- | Run @mk@ at node 1 of a graph in memory: its outcome, the graph
-- afterwards, and how many times each node was read from the graph.
constructing ::
  Graph Text () ->
  P.Path ->
  (Either UserError (Set NID), Graph Text (), Map NID Int)
constructing g p =
  let ((result, reads), g') =
        runPureEff
          . runStateLocal g
          . runInMemoryGraphMetadataEditing
          . runStateLocal @(Map NID Int) mempty
          . evalStateLocal @StdGen (mkStdGen 0)
          . runFreshNIDRandom
          . runErrorNoCallStack @UserError
          . ignoreWarnings @UserError
          . countingReads
          $ constructPath Nothing (smallNID 1) (smallNID 1) plainlyTyped p
   in (fmap (.nodes) result, g', reads)

-- | Each node is read from the graph once while @mk@ works out what to
-- create, however often the walks of the path and of the lines it
-- suggests look at it.
spec_constructPathReads :: Spec
spec_constructPathReads = describe "mk reads each node once to plan" do
  let reads g p = let (_, _, counts) = constructing g p in counts
      expected = ["can be created in 2 ways", "use: mk a/"] :: [String]
  it "for a path that resolves: 28 routes of one node" do
    -- every step follows an edge back to the node, which the walk looks
    -- at for every step
    let g = graphWithEdges [edge' 1 l 1 | l <- ["a", "b", "c", "x"]]
        p = [path| x & (a & b & c)/(a & b & c)/(a & b & c) |]
        (result, g', counts) = constructing g p
    either (const Nothing) Just result `shouldBe` Just mempty
    g' `shouldBe` g
    counts `shouldBe` singletonMap (smallNID 1) 1
  it "for an ambiguous path, with its suggestions" do
    -- x after either a: the two possibilities are listed, each with its
    -- line, which is walked too
    let g = graphWithEdges [edge' 1 "a" 2, edge' 1 "a" 3, edge' 3 "b" 4]
        (result, g', counts) = constructing g [path| a/x |]
    either show (const "") result
      `shouldSatisfy` (\message -> all (`isInfixOf` message) expected)
    g' `shouldBe` g
    counts `shouldBe` mapFromList [(smallNID k, 1) | k <- [1, 2, 3]]
  it "for a path it then constructs" do
    -- planning reads each node once; writing the edges is not counted
    let g = graphWithEdges [edge' 1 "a" 2, edge' 2 "b" 3]
        counts = reads g [path| a/b/c & a/d |]
    counts `shouldBe` mapFromList [(smallNID k, 1) | k <- [1, 2, 3]]

-- | What a construction command says when it fails at node 0 of a graph.
failure ::
  Graph Text () ->
  ( forall r.
    ( GraphMetadataEditing :> r,
      FreshNID :> r,
      Warn UserError :> r,
      Error UserError :> r
    ) =>
    Eff r ()
  ) ->
  Text
failure g command =
  either said (const "(nothing: the command did not fail)")
    . fst
    . runPureEff
    . runStateLocal g
    . runInMemoryGraphMetadataEditing
    . evalStateLocal @StdGen (mkStdGen 0)
    . runFreshNIDRandom
    . runErrorNoCallStack @UserError
    . ignoreWarnings @UserError
    $ command
  where
    said = \case
      OtherError message -> message
      other -> tshow other

mkFailure :: Graph Text () -> Text -> Text
mkFailure g p =
  failure g (void (constructPath Nothing (smallNID 0) (smallNID 0) plainlyTyped (typed p)))

-- | What @mk@ says on a graph of nodes 0 to 3 and no edges.
refusedWith :: Text -> Text
refusedWith = mkFailure (graphOf [1, 2, 3] [])

-- | What @mk@ says on a graph of nodes 0 to 3 and no edges, for a path
-- that had this as typed.
refusedAs :: AsTyped -> Text -> Text
refusedAs asTyped p =
  failure
    (graphOf [1, 2, 3] [])
    (void (constructPath Nothing (smallNID 0) (smallNID 0) asTyped (typed p)))

-- | A path typed with @%targets(…)@ that became the unions of nodes at
-- these places of the path.
withTargetsAt :: [P.Place] -> AsTyped
withTargetsAt places = AsTyped {override = False, targets = places, relationalPlus = False}

-- | The path with every @+@ or @++@ union written outside the rest of it:
-- the union of every choice of their sides. Written here again, apart
-- from what the message is made from.
unionsOutside :: P.Path -> P.Path
unionsOutside p = case choices p of
  c : rest -> foldl' (P.:++) c rest
  [] -> p
  where
    choices = \case
      P.Union P.Shared l r -> choices l <> choices r
      P.Union k l r -> P.Union k <$> choices l <*> choices r
      l P.:/ r -> (P.:/) <$> choices l <*> choices r
      l P.:& r -> (P.:&) <$> choices l <*> choices r
      P.Backwards q -> P.Backwards <$> choices q
      q -> [q]

-- | The path with one side of each union kept, in every way.
oneSides :: P.Path -> [P.Path]
oneSides = \case
  P.Union _ l r -> oneSides l <> oneSides r
  l P.:/ r -> (P.:/) <$> oneSides l <*> oneSides r
  l P.:& r -> (P.:&) <$> oneSides l <*> oneSides r
  P.Backwards q -> P.Backwards <$> oneSides q
  q -> [q]

-- | The messages of refused paths say only what holds of the path, and
-- each rewrite they advise does what they say it does.
spec_refusalAdvice :: Spec
spec_refusalAdvice = describe "what a refusal advises applies to the path" do
  let says t p = (p, t `isInfixOf` refusedWith p)
      notTooLarge q = isNothing (checkExpansion q)
  describe "a path that is too large (ExpansionTooLarge)" do
    let tooLarge =
          [ "(x ++ y)/(c & d)/(e & f)/(g & h)/(i & j)/(k & l)",
            "(a ++ b)/(" <> intercalate " & " ["c" <> tshow i | i <- [1 .. 65 :: Int]] <> ")",
            "(a & b)/(c & d)/(e & f)/(g & h)/(i & j)/(k & l)/(m & n)",
            "(a | b)/(c & d & e & f & g)",
            -- one side of the union leaves it too large
            "(a ++ b)/(c & d)/(e & f)/(g & h)/(i & j)/(k & l)/(m & n)",
            "(a ++ b)/(c & d & e & f & g)",
            "(a ++ b)/(c ++ d)/(e & f & g & h & i)",
            "(a ++ (b | c))/(d & e & f & g & h)",
            "(p & q)/(x/@ + w/@)/((c & y)/@/e & d)",
            "(a ++ b) & (c ++ d) & (e ++ f) & (g ++ h) & (i ++ j) & (k ++ l) & (m ++ n) & (o ++ p) & (q ++ r)",
            -- a union of nodes that was typed is a union
            "(@1 ++ @2)/(c & d & e & f & g & h & i & j & k)",
            "(%never ++ @1 ++ @2)/(c & d & e & f & g & h & i & j & k)",
            -- each side alone is within the limit, though the largest
            -- part of each side together is not
            "(" <> intercalate " & " (steps "a") <> ") | " <> intercalate "/" (steps "b"),
            "(" <> intercalate " & " (steps "a") <> ") ++ " <> intercalate "/" (steps "b")
          ]
        steps l = [l <> tshow i | i <- [1 .. 17 :: Int]]
    it "is refused" $
      [p | p <- tooLarge, not ("(ExpansionTooLarge)" `isInfixOf` refusedWith p)] `shouldBe` []
    it "advises moving its unions out exactly where that brings it within the limit" $
      map (says "Write it so") tooLarge
        `shouldBe` [(p, notTooLarge (unionsOutside (typed p))) | p <- tooLarge]
    it "advises keeping one side of each union exactly where every way to keep one is within the limit" $
      map (says "eep one side of each union") tooLarge
        `shouldBe` [ ( p,
                       oneSides (typed p) /= [typed p]
                         && all notTooLarge (oneSides (typed p))
                     )
                   | p <- tooLarge
                   ]
    it "does not say that it may come to fewer route steps than the limit" $
      filter (snd . says "may come to fewer") tooLarge `shouldBe` []
    it "speaks of no union for what a %targets(…) became, but of one typed beside it" do
      let p = "(%never ++ @1 ++ @2)/(c & d & e & f & g & h & i & j & k)"
      "union" `isInfixOf` refusedAs (withTargetsAt [[0]]) p `shouldBe` False
      let q = "(@1 ++ @2)/(c & d & e & f & g & h & i & j & k) & (%never ++ @1 ++ @2)"
      ("Write it so" `isInfixOf` refusedAs (withTargetsAt [[1]]) q, "Write it so" `isInfixOf` refusedWith q)
        `shouldBe` (True, True)
    it "tells what a %targets(…) became from a union typed beside it by where it is, not by its shape" do
      -- (%targets(a/b) | @1)/(c & … & k), where %targets(a/b) found @2:
      -- the | was typed, though it extends the union of nodes
      let nine = "/(c & d & e & f & g & h & i & j & k)"
          p = "((%never ++ @2) | @1)" <> nine
      refusedAs (withTargetsAt [[0, 0]]) p
        `shouldSatisfy` isInfixOf "Keep one side of each union or create the path with several commands."
      let q = "((%never ++ @2) ++ @1)" <> nine
      refusedAs (withTargetsAt [[0, 0]]) q
        `shouldSatisfy` isInfixOf "Write it so, keep one side of each union, or create the path with several commands."
      -- each side alone is built, and so are both sides moved out
      map refusedWith ["@2" <> nine, "@1" <> nine, "@2" <> nine <> " ++ @1" <> nine]
        `shouldSatisfy` all (not . ("(ExpansionTooLarge)" `isInfixOf`))
      -- (%never ++ @1 ++ @2)/(c & … & k) & %targets(a), where %targets(a)
      -- found @1: the typed union is one, though what %targets(a) became
      -- is the start of it
      let r = "(%never ++ @1 ++ @2)" <> nine <> " & (%never ++ @1)"
      refusedAs (withTargetsAt [[1]]) r
        `shouldSatisfy` isInfixOf "Write it so, keep one side of each union, or create the path with several commands."
      -- and what %targets became, alone, is no union typed
      let createOnly = "limit of 256 route steps over all its alternatives. Create the path with several commands."
      refusedAs (withTargetsAt [[0]]) ("(%never ++ @1 ++ @2)" <> nine)
        `shouldSatisfy` isInfixOf createOnly
      -- nor are the unions inside it, where it found more than two nodes:
      -- %targets(q)/(c & d & e), where %targets(q) found @1 to @5
      refusedAs (withTargetsAt [[0]]) "(%never ++ @1 ++ @2 ++ @3 ++ @4 ++ @5)/(c & d & e)"
        `shouldSatisfy` isInfixOf createOnly
      -- under ~ too: ~((%targets(p) | @1)/(c & … & k)), where %targets(p)
      -- found @1 and @2, is too large with the side %targets(p) alone
      refusedAs (withTargetsAt [[0, 0, 0]]) ("~(((%never ++ @1 ++ @2) | @1)" <> nine <> ")")
        `shouldSatisfy` isInfixOf createOnly
      refusedWith ("~((%never ++ @1 ++ @2)" <> nine <> ")")
        `shouldSatisfy` isInfixOf "(ExpansionTooLarge)"
    it "does not say that a + made | by an %override is moved out with the same meaning" do
      let p = "%override({plus = relational}, (a ++ b)/(c & d & e & f & g) & (x + y)/(p & q))"
          relational = AsTyped {override = True, targets = [], relationalPlus = True}
          said = refusedAs relational p
      ("Written with its + and ++ unions" `isInfixOf` said, "Write it so" `isInfixOf` said)
        `shouldBe` (False, True)
      said
        `shouldSatisfy` isInfixOf "Written with its ++ unions and the + that mk reads as ++ outside"
  describe "a circle of suppliers (CircularSuppliers)" do
    it "advises no !{…}, only the node to go to or separate commands" do
      let advice =
            "Write the node a step should go to (/@N after it), or create them "
              <> "with separate commands. Nothing was changed. (CircularSuppliers)"
          circles =
            [ "a & ~a",
              "a/!{@1}/b & a/c",
              "a/!{@2} & ~a/!{@2}",
              "a/~b & ~a/b",
              "a & ~a & c & ~c",
              "#a & ~a",
              "c/~c & a/c",
              "x/(a & ~a)",
              "(@1 | @)/(@/a & ~(a))/~((a ++ a))",
              "((@ + x) | ~d/d)/(#t & c & d)"
            ]
      map (says advice) circles `shouldBe` map (,True) circles
    it "is said without trying rewrites of the path for each node written" do
      -- a circle in each of 40 alternatives, and 200 nodes written: the
      -- message was once worked out from a rewrite of the path for each
      -- node written, which allocated more than 100 GB
      let p =
            "("
              <> intercalate " ++ " ["a" <> tshow i <> "/(b & ~b)" | i <- [1 .. 40 :: Int]]
              <> ") & !{"
              <> intercalate ", " ["@" <> tshow i | i <- [1 .. 200 :: Int]]
              <> "}"
      said <- withinAllocation (512 * 1024 * 1024) (evaluate (force (refusedWith p)))
      fmap ("(CircularSuppliers)" `isSuffixOf`) said `shouldBe` Just True
  describe "a join after a copied step" do
    it "is suggested where every copy comes the same way" $
      says "write a join after it: p/b/@/(c & !{@000000000000})" "p/b/(c & !{@0})"
        `shouldBe` ("p/b/(c & !{@0})", True)
    it "is not suggested where it would make walks meet that come different ways" $
      -- (p & q)/b/@/(c & !{@0}) would make the b after p and the b after q
      -- go into one node
      says "join" "(p & q)/b/(c & !{@0})" `shouldBe` ("(p & q)/b/(c & !{@0})", False)
    it "is suggested where the walks before the step meet at a join written before it" do
      says "write a join after it: (x & y)/@/b/@/(c & !{@000000000000})" "(x & y)/@/b/(c & !{@0})"
        `shouldBe` ("(x & y)/@/b/(c & !{@0})", True)
      -- and it is built
      refusedWith "(x & y)/@/b/@/(c & !{@0})" `shouldBe` "(nothing: the command did not fail)"
    it "is not suggested for a path typed inside an %override(…)" do
      -- %override({plus = shared}, */(a & b)), whose override leaves
      -- nothing in the path
      let overridden = AsTyped {override = True, targets = [], relationalPlus = False}
      ("join" `isInfixOf` refusedAs plainlyTyped "*/(a & b)", "join" `isInfixOf` refusedAs overridden "*/(a & b)")
        `shouldBe` (True, False)
  describe "the copies of a step" do
    it "are each quoted once, with how many copies a quote stands for" do
      refusedWith "b/(c & !{@0} & d)"
        `shouldSatisfy` isInfixOf "copies it into 3 walks, and its copies, `b` twice and `b/!{@000000000000}`, could"
      refusedWith "b/(c & !{@0} & d) & b/!{@0}"
        `shouldSatisfy` isInfixOf "copies it into 3 walks: `b` twice and `b/!{@000000000000}`)"
      refusedWith "((((@/((*/(@ & !{@1}))/!{@1})) ++ @) ++ a) + @1)"
        `shouldSatisfy` isInfixOf "its copies, `*/!{@000000000001}` twice, meet"
    it "say how many of the copies they list, where not all are in the circle" do
      -- the copy b/@0 is not in it
      refusedWith "b/(c & !{@1} & d & @0)"
        `shouldSatisfy` isInfixOf "copies it into 4 walks, and 3 of its copies, `b` twice and `b/!{@000000000001}`, could"
      -- the copy ~b/@0 is not in it
      refusedWith "(@0 & b) & ~b/(b & @0 & !{@1})"
        `shouldSatisfy` isInfixOf "copies it into 3 walks, among them `~b` and `~b/!{@000000000001}`)"
    it "say which of the steps typed the same the copied one is" do
      refusedWith "*/(a & b) ++ */(c & d)"
        `shouldSatisfy` isInfixOf "The step `*` is written twice in this path; the product rule copies the first of them into 2 walks"
      refusedWith "b/(c & !{@0}) & c/(b & !{@0})"
        `shouldSatisfy` isInfixOf "The step `c` is written twice in this path; the product rule copies the second of them into 2 walks"
      refusedWith "b/(c & !{@0} & d) & b/!{@0}"
        `shouldSatisfy` isInfixOf "`b` (the first of the 2 written, and the product rule"
    it "quote a step after # as it is typed" $
      refusedWith "#x/(!{@1} & f)"
        `shouldSatisfy` isInfixOf "The step `#x` is written once"
    it "quote a step under two ~ as it is typed" do
      refusedWith "~(~b)/(c & !{@0})"
        `shouldSatisfy` isInfixOf "The step `~(~b)` is written once"
      refusedWith "~(~(~b))/(c & !{@0})"
        `shouldSatisfy` isInfixOf "The step `~(~(~b))` is written once"

-- | What @mk@ says for each path of the design's examples whose shape it
-- refuses, for paths that are too large as written (with a union that a
-- product copies, with unions that none copies, with no union), for steps
-- that the product rule copies (one written step is named once, and a
-- join after it is suggested only where it is the whole path, is built
-- and holds together only copies that came the same way), for circles
-- of steps, for steps typed more than once, and what @tag@ says for a
-- refused path.
test_refusalMessages :: TestTree
test_refusalMessages =
  goldenTest "refusal-messages" . unlines $
    [ "$ " <> name <> ": mk " <> p <> "\n" <> mkFailure (graphOf nodes edges) p <> "\n"
    | (name, nodes, edges, p, Refuses _) <- designExamples
    ]
      <> [ "$ mk " <> p <> "\n" <> mkFailure (graphOf [] []) p <> "\n"
         | p <-
             [ "(a ++ b)/(c & d & e & f & g)",
               "(p & q)/(x/@ + w/@)/((c & y)/@/e & d)",
               "a & ~a",
               "*/a & */c",
               "*/(a & b)",
               "@1/a/@/(@ & b)/@",
               "(a & b)/(c & d)/(e & f)/(g & h)/(i & j)/(k & l)/(m & n)",
               "(a | b | c)/(d & e & f & g & h & i & j)",
               "(a ++ b) & (c ++ d) & (e ++ f) & (g ++ h) & (i ++ j) & (k ++ l) & (m ++ n) & (o ++ p) & (q ++ r)",
               "x/*/(a & b)",
               "(a & b)/*",
               "*/(a & b) & regex:\"^c$\"",
               "(x & y)/*/@",
               "((x & y)/* & z)/@",
               "i/(!{@1} & f)",
               "b/(c & !{@0})",
               "b/(c & !{@0}) ++ zz",
               "b/(c & !{@0}) & b/!{@0}",
               "a/b & (a & c)/@",
               "(a & b)/@ & (b & c)/@ & (c & a)/@",
               -- moving the union out does not bring it within the limit
               "(x ++ y)/(c & d)/(e & f)/(g & h)/(i & j)/(k & l)",
               "(a | b)/(c & d & e & f & g)",
               -- nor does keeping one side of the union
               "(a ++ b)/(c & d)/(e & f)/(g & h)/(i & j)/(k & l)/(m & n)",
               -- a union of nodes that was typed is a union (and one that a
               -- %targets(…) gave is not: below)
               "(%never ++ @1 ++ @2)/(c & d & e & f & g & h & i & j & k)",
               -- circles
               "c/~c & a/c",
               "x/(a & ~a)",
               -- the join would make the b after p and the one after q meet
               "(p & q)/b/(c & !{@0})",
               "p/b/(c & !{@0})",
               -- copies quoted alike
               "b/(c & !{@0} & d)",
               "b/(c & !{@0} & d) & b/!{@0}",
               "((((@/((*/(@ & !{@1}))/!{@1})) ++ @) ++ a) + @1)",
               -- steps typed more than once, and after #
               "*/(a & b) ++ */(c & d)",
               "c/(b & (!{@1, @3}/c))",
               "#x/(!{@1} & f)",
               -- a union of nodes that was typed
               "(@1 ++ @2)/(c & d & e & f & g & h & i & j & k)",
               -- each side alone is within the limit
               "(a1 & a2 & a3 & a4 & a5 & a6 & a7 & a8 & a9 & a10 & a11 & a12 & a13 & a14 & a15 & a16 & a17) | b1/b2/b3/b4/b5/b6/b7/b8/b9/b10/b11/b12/b13/b14/b15/b16/b17",
               -- a join after a join written before the copied step
               "(x & y)/@/b/(c & !{@0})",
               -- a step under two ~
               "~(~b)/(c & !{@0})",
               -- fewer copies in the circle than the product rule makes
               "b/(c & !{@1} & d & @0)",
               "(@0 & b) & ~b/(b & @0 & !{@1})"
             ]
         ]
      <> [ "$ mk %targets(…), which became " <> p <> "\n" <> refusedAs (withTargetsAt [[0]]) p <> "\n"
         | p <- ["(%never ++ @1 ++ @2)/(c & d & e & f & g & h & i & j & k)"]
         ]
      <> [ "$ tag "
             <> p
             <> " @0\n"
             <> failure
               (graphOf [] [])
               (void (constructTag Nothing (smallNID 0) (smallNID 0) plainlyTyped (typed p) (smallNID 0)))
             <> "\n"
         | p <- ["(a & b)/@ & a", "b/@7", "(* & a)/@"]
         ]
