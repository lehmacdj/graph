-- | Additional tests for resolve that require dependencies that would
-- otherwise create circular dependencies
module Graph.ResolveSpec where

import Models.Edge
import Models.NID
import Models.NormalizedPath.Parse
import Models.ResolvedPath
import MyPrelude
import Utils.Testing

spec_pathResidualsUnresolvedBy :: Spec
spec_pathResidualsUnresolvedBy = do
  let testResiduals :: [Edge Text] -> Text -> Text -> Spec
      testResiduals edges pathToResidualStr expectedStr =
        it (unpack testName) do
          let graph = graphWithEdges edges
          pathToResidual <-
            parseForTest "normalized path" pNormalizedPath pathToResidualStr
          expected <- parseForTest "normalized path" pNormalizedPath expectedStr
          pathResidualsUnresolvedBy (smallNID 1) graph pathToResidual
            `shouldBe` expected
        where
          edgesStr = tshow edges
          testName =
            "residuals of "
              <> pathToResidualStr
              <> " in graph with edges "
              <> edgesStr
              <> " = "
              <> expectedStr

  describe "rooted" do
    testResiduals [edge' 1 "a" 2] "[a & b]" "[@1<b]>@2"
    testResiduals [edge' 1 "a" 2, edge' 1 "b" 2] "[a]" "%never"
    testResiduals [edge' 1 "a" 2] "[a]" "%never"
    testResiduals [] "[a]" "[@1<a]>@1"
    testResiduals [] "[a & b]" "[@1<a & @1<b]>@1"
    testResiduals [] "[@<a & @<b]" "[@1<a & @1<b]>@1"
    describe "multiple roots" do
      testResiduals [edge' 1 "a" 3, edge' 2 "b" 3, edge' 3 "c" 4] "[a & b & c]" "%never"
      testResiduals [edge' 1 "a" 3, edge' 2 "b" 3, edge' 3 "c" 4] "[@1<a & c]" "[@2<b /@3| c]>@4"

  describe "%never" do
    describe "subtracting from %never always leaves %never" do
      testResiduals [edge' 1 "a" 2] "%never" "%never"
      testResiduals [] "%never" "%never"
    describe "subtracting nothing leaves everything" do
      testResiduals [] "[a]" "[a]"
      testResiduals [] "@" "@"
      testResiduals [] "@[a]" "@[a]"
      testResiduals [] "@[a & b /| c]" "@[a & b /| c]"

  describe "pointlike" do
    testResiduals [edge' 1 "a" 1] "@1[a & b]" "@1[b]"
    testResiduals [edge' 1 "a" 1] "@[a & b]" "@1[b]"
    testResiduals [edge' 1 "a" 1] "@1[a]" "%never"
    testResiduals [edge' 1 "a" 1] "%never" "%never"
    testResiduals [edge' 1 "a" 1, edge' 1 "b" 1] "@1[a]" "%never"
    testResiduals [] "@[a]" "@1[a]"

  describe "sequences" do
    testResiduals [edge' 1 "a" 2, edge' 2 "b" 3] "[a /| b & c]" "[@1<c]>@3"
    testResiduals [edge' 1 "a" 2] "[a /| b]" "[@2<b]"
    testResiduals [edge' 1 "a" 2] "[a /| b & c]" "[@2<b & @1<c]"

  describe "wildcard is \"fufilled\" if any specific path matches" do
    testResiduals [edge' 1 "a" 2] "[*]" "%never"

  describe "inverse" do
    describe "inverse doesn't match forward" do
      testResiduals [edge' 1 "a" 2] "[~a]" "[~a]"
    testResiduals [edge' 1 "~a" 2] "[~a]" "%never"
    testResiduals [edge' 1 "~a" 2] "[~a & ~b]" "[@1<~b]>@2"

  describe "unions" do
    testResiduals [edge' 1 "a" 2] "[a] + [b]" "[b]"
    testResiduals [edge' 1 "a" 2] "[a & b] + [c]" "[@1<b]>@2 + [c]"
    testResiduals [edge' 1 "a" 2] "[a] + [a & b]" "[@1<b]>@2"

  describe "regex is \"fulfilled\" by matching transitions" do
    testResiduals [edge' 1 "apple" 2] "[regex:'a.*']" "%never"
    testResiduals [edge' 1 "banana" 2] "[regex:'a.*']" "[@1<banana]>@2"
    testResiduals [edge' 1 "apple" 2] "[regex:'a.*' & regex:'b.*']" "[@1<regex:'b.*']>@2"
    testResiduals [edge' 1 "apple" 2] "[regex:'b.*']" "[regex:'b.*']"
