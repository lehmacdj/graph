-- | Tests for "Models.PathPattern.Compile": the examples in
-- docs/path-pattern-ir.md, and an independent interpreter of the distributed
-- expressions that the glued patterns must agree with.
module Models.PathPattern.CompileSpec where

import Effectful.State.Dynamic
import Graph.GraphMetadataEditing (runInMemoryGraphMetadataEditing)
import Graph.PathPattern.Resolve
import Models.Connect
import Models.Graph (Graph, maybeNodeLookup)
import Models.NID
import Models.Node
import Models.Path.Simple qualified as P
import Models.Path.TH (path)
import Models.PathPattern
import Models.PathPattern.Compile
import Models.PathPattern.Match
import Models.PathPattern.TestCorpus
import MyPrelude
import Utils.Testing

-- | Evaluate a rooted expression directly, producing each witness as its
-- ordered concrete routes. It shares no code with gluing or matching.
interpretExpr :: Graph Text () -> NID -> Expr -> [[ConcreteRoute]]
interpretExpr g cur = go Nothing
  where
    exists n = isJust (maybeNodeLookup n g)
    go input = \case
      Source Current -> [[ConcreteRoute cur []] | exists cur]
      Source (Fixed n) -> [[ConcreteRoute n []] | exists n]
      Source (Var _) -> error "interpret: variable root"
      Stay -> [[ConcreteRoute (fromJustEx input) []]]
      Step d m ->
        let i = fromJustEx input
            connects = case maybeNodeLookup i g of
              Nothing -> []
              Just n -> toList case d of
                Out -> n.outgoing
                In -> n.incoming
         in [ [ConcreteRoute i [ConcreteHop d c.transition c.node]]
            | c <- connects,
              matcherAccepts m c.transition
            ]
      Both a b rest -> map concat (traverse (go input) (a : b : rest))
      Join p test ->
        [ ws
        | ws <- go input p,
          e : es <- [map endOf ws],
          all (== e) es,
          testAccepts test e
        ]
      Sequence p q ->
        [ [ConcreteRoute w.root (w.hops ++ c.hops) | w <- ws, c <- cs]
        | ws <- go input p,
          j <- ordNub (map endOf ws),
          cs <- go (Just j) q
        ]
    endOf r = maybe r.root (.target) (lastMay r.hops)

matchedRoutes :: Graph Text () -> P.Path -> Set [ConcreteRoute]
matchedRoutes g surface =
  setFromList
    [ concreteRoutes r w
    | r <- resolved.resolutions,
      w <- witnesses r
    ]
  where
    resolved =
      resolveQuery corpusCurrent (compilePath surface)
        & runInMemoryGraphMetadataEditing
        & evalStateLocal g
        & runPureEff

spec_compileOracle :: Spec
spec_compileOracle = describe "compiled patterns agree with the interpreter" do
  forM_ corpusQueries \surface -> it (show surface) do
    let exprs = toList (surfaceExprs (pushBackwards surface))
    forM_ corpusGraphs \g ->
      (show g, matchedRoutes g surface)
        `shouldBe` ( show g,
                     setFromList (concatMap (interpretExpr g corpusCurrent) exprs)
                   )

spec_compileEquivalences :: Spec
spec_compileEquivalences = describe "equivalent paths" do
  forM_ corpusEquivalences \(a, b) ->
    it (show a <> " == " <> show b) $ compilePath a `shouldBe` compilePath b

compiles :: P.Path -> [PatternPath] -> Expectation
compiles surface expected = compilePath surface `shouldBe` query expected

spec_compileExamples :: Spec
spec_compileExamples = describe "compile examples" do
  let (v0, v1, v2, v3) = (var 0, var 1, var 2, var 3)
  it "a/(b & c) copies the unjoined prefix" $
    [path| a/(b & c) |]
      `compiles` [ pat
                     [ route current [out 0 "a" v0, out 1 "b" v1],
                       route current [out 2 "a" v2, out 3 "c" v3]
                     ]
                 ]
  it "a/@/(b & c) shares the joined prefix and its edge" $
    [path| a/@/(b & c) |]
      `compiles` [ pat
                     [ route current [out 0 "a" v0, out 1 "b" v1],
                       route current [out 0 "a" v0, out 2 "c" v2]
                     ]
                 ]
  it "(a & b)/(c & d) distributes prefix-major" $
    map (map hopLabels . toList . (.routes)) (toList (compilePath [path| (a & b)/(c & d) |]).alternatives)
      `shouldBe` [[["a", "c"], ["a", "d"], ["b", "c"], ["b", "d"]]]
  it "(a & b)/@ & c" $
    [path| (a & b)/@ & c |]
      `compiles` [ pat
                     [ route current [out 0 "a" v0],
                       route current [out 1 "b" v0],
                       route current [out 2 "c" v1]
                     ]
                 ]
  it "appending /@ to (a & b)/@ & c joins every output" $
    [path| ((a & b)/@ & c)/@ |]
      `compiles` [ pat
                     [ route current [out 0 "a" v0],
                       route current [out 1 "b" v0],
                       route current [out 2 "c" v0]
                     ]
                 ]
  it "x/(@ & a)/@ copies x and keeps the zero-hop route first" $
    [path| x/(@ & a)/@ |]
      `compiles` [ pat
                     [ route current [out 0 "x" v0],
                       route current [out 1 "x" v1, out 2 "a" v0]
                     ]
                 ]
  it "x/(a & @)/@ puts the a route first" $
    [path| x/(a & @)/@ |]
      `compiles` [ pat
                     [ route current [out 0 "x" v0, out 1 "a" v1],
                       route current [out 2 "x" v1]
                     ]
                 ]
  it "x/@/(@ & a)/@ shares x, so a is a loop" $
    [path| x/@/(@ & a)/@ |]
      `compiles` [ pat
                     [ route current [out 0 "x" v0],
                       route current [out 0 "x" v0, out 1 "a" v0]
                     ]
                 ]
  it "(@ & a)/@ is a loop on current" $
    [path| (@ & a)/@ |]
      `compiles` [pat [route current [], route current [out 0 "a" current]]]
  it "(* & *)/@ has two independent edges" $
    [path| (* & *)/@ |]
      `compiles` [pat [route current [outW 0 v0], route current [outW 1 v0]]]
  it "*/@/(b & c) shares the wildcard edge" $
    [path| */@/(b & c) |]
      `compiles` [ pat
                     [ route current [outW 0 v0, out 1 "b" v1],
                       route current [outW 0 v0, out 2 "c" v2]
                     ]
                 ]
  it "a & a has independent occurrences" $
    [path| a & a |]
      `compiles` [pat [route current [out 0 "a" v0], route current [out 1 "a" v1]]]
  it "@N at the root is a fixed vertex and every @N is one vertex" $
    [path| @000000000005/a/@000000000005 |]
      `compiles` [pat [route (fixedN 5) [out 0 "a" (fixedN 5)]]]
  it "@N & a are independent roots" $
    [path| @000000000005 & a |]
      `compiles` [pat [route (fixedN 5) [], route current [out 0 "a" v0]]]
  it "joining current with @N tests current" $
    [path| (@ & @000000000005)/@ |]
      `compiles` [ patWith
                     [(current, exactly 5)]
                     [route current [], route current []]
                 ]
  it "!{...} tests a variable" $
    [path| a/!{@000000000002} |]
      `compiles` [patWith [(v0, except [2])] [route current [out 0 "a" v0]]]
  it "exclusions meet" $
    [path| a/!{@000000000002}/!{@000000000003} |]
      `compiles` [ patWith
                     [(v0, except [2, 3])]
                     [route current [out 0 "a" v0]]
                 ]
  it "contradictory fixed nodes drop the alternative" $
    compilePath [path| a/@000000000001/@000000000002 + b |]
      `shouldBe` query [pat [route current [out 0 "b" v0]]]
  it "a fixed node that is excluded drops the alternative" $
    compilePath [path| a/!{@000000000002}/@000000000002 |]
      `shouldBe` query []
  it "~(a/b) reverses the sequence and flips each step" $
    [path| ~(a/b) |]
      `compiles` [pat [route current [inc 0 "b" v0, inc 1 "a" v1]]]
  it "~ keeps node specs in place" $
    [path| ~(a/@000000000002) |]
      `compiles` [pat [route (fixedN 2) [inc 0 "a" v0]]]
  it "%never compiles to no alternatives" $
    compilePath P.Zero `shouldBe` query []
  it "Show prints the helper syntax (a node spec joins, sharing a)" $
    show (compilePath [path| a/!{@000000000002}/(b & c) |])
      `shouldBe` "query [patWith [(var 0,except [2])] [route current \
                 \[out 0 \"a\" (var 0),out 1 \"b\" (var 1)],route current \
                 \[out 0 \"a\" (var 0),out 2 \"c\" (var 2)]]]"
  where
    hopLabels r = [l | Hop _ _ (MLiteral l) _ <- r.hops]
