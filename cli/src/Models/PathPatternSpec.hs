module Models.PathPatternSpec where

import Models.Edge (Edge' (..))
import Models.Graph (Graph' (..), edgeSet)
import Models.Node (Node' (..))
import Models.Path.TH (path)
import Models.PathPattern
import Models.PathPattern.Compile (compilePath)
import MyPrelude
import Utils.Testing

spec_pathPattern :: Spec
spec_pathPattern = describe "route patterns" do
  describe "meetTests" do
    it "unions exclusions" $
      meetTests (except [1]) (except [2]) `shouldBe` Just (except [1, 2])
    it "keeps an exact node that is not excluded" $
      meetTests (exactly 1) (except [2]) `shouldBe` Just (exactly 1)
    it "rejects an exact node that is excluded" $
      meetTests (except [1]) (exactly 1) `shouldBe` Nothing
    it "rejects two different exact nodes" $
      meetTests (exactly 1) (exactly 2) `shouldBe` Nothing

  describe "canonicalize" do
    it "numbers variables and edges by first appearance" $
      canonicalize
        ( patWith
            [(var 7, except [3])]
            [ route current [out 9 "a" (var 7), out 4 "b" (var 5)],
              route current [out 9 "a" (var 7), out 2 "c" (var 5)]
            ]
        )
        `shouldBe` patWith
          [(var 0, except [3])]
          [ route current [out 0 "a" (var 0), out 1 "b" (var 1)],
            route current [out 0 "a" (var 0), out 2 "c" (var 1)]
          ]
    it "is idempotent" do
      let p =
            pat
              [ route (fixedN 3) [inc 5 "a" (var 2)],
                route current [outW 1 (var 2)]
              ]
      canonicalize (canonicalize p) `shouldBe` canonicalize p

  describe "patternGraph" do
    it "keeps parallel pattern edges distinct" $
      length (edgeSet (patternGraph (pat [route current [outW 0 (var 0)], route current [outW 1 (var 0)]])))
        `shouldBe` 2
    it "stores a shared prefix once" $
      case toList (compilePath [path| a/@/(b & c) |]).alternatives of
        [p] -> length (edgeSet (patternGraph p)) `shouldBe` 3
        ps -> expectationFailure (show ps)
    it "orients incoming hops in stored direction" $
      toList (edgeSet (patternGraph (pat [route current [inc 0 "a" (var 0)]])))
        `shouldBe` [Edge (var 0) (PEdge (EdgeId 0) (MLiteral "a")) current]
    it "records tests as augmentations" $
      map
        (\n -> (n.nid, n.augmentation))
        (toList (patternGraph (patWith [(var 0, except [2])] [route (fixedN 1) [out 0 "a" (var 0)]])).nodeMap)
        `shouldBe` [(fixedN 1, PAug (exactly 1)), (var 0, PAug (except [2]))]
