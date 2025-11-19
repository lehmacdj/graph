-- | Additional tests for resolve that require dependencies that would
-- otherwise create circular dependencies
module Graph.ResolveSpec where

import Models.Graph (Graph, compactGraphRepresentation)
import Models.NID
import Models.NormalizedPath.Parse
import Models.ResolvedPath
import MyPrelude
import Utils.Testing

spec_pathResidualsUnresolvedBy :: Spec
spec_pathResidualsUnresolvedBy = do
  let testResiduals' :: Graph Text () -> Text -> Text -> Spec
      testResiduals' graph pathToResidualStr expectedStr =
        it (unpack testName) do
          pathToResidual <-
            parseForTest "normalized path" pNormalizedPath pathToResidualStr
          expected <- parseForTest "normalized path" pNormalizedPath expectedStr
          pathResidualsUnresolvedBy (smallNID 1) graph pathToResidual
            `shouldBe` expected
        where
          testName =
            "residuals of "
              <> pathToResidualStr
              <> " in graph the graph "
              <> compactGraphRepresentation graph
              <> " = "
              <> expectedStr
      testResiduals edges = testResiduals' (graphWithEdges edges)

  describe "simple matches" do
    testResiduals [edge' 1 "a" 2, edge' 1 "b" 2] "[a]" "%never"
    testResiduals [edge' 1 "a" 2] "[a]" "%never"
    testResiduals [edge' 1 "a" 2] "[*]" "%never"
    testResiduals [edge' 1 "a" 2] [rq|[regex:"^a*$"]|] "%never"
    testResiduals [edge' 1 "apple" 2] [rq|[regex:"a"]|] "%never"
    testResiduals [edge' 1 "a" 2] "[@<a]" "%never"
    testResiduals [edge' 1 "a" 2] "[@1<a]" "%never"
    testResiduals [edge' 1 "a" 2] "[a]>@" "%never"
    testResiduals [edge' 1 "a" 2] "[a]>@2" "%never"
    testResiduals [edge' 1 "a" 2] "[@1<a]>@2" "%never"
    testResiduals [edge' 1 "~a" 2] "[~a]" "%never"

    testResiduals [edge' 1 "a" 1, edge' 1 "b" 1] "@[a]" "%never"
    testResiduals [edge' 1 "a" 1] "@[a]" "%never"
    testResiduals [edge' 1 "a" 1] "@[*]" "%never"
    testResiduals [edge' 1 "a" 1] [rq|@[regex:"^a*$"]|] "%never"
    testResiduals [edge' 1 "apple" 1] [rq|@[regex:"a"]|] "%never"
    testResiduals [edge' 1 "a" 1] "@[a]" "%never"
    testResiduals [edge' 1 "a" 1] "@1[a]" "%never"
    testResiduals [edge' 1 "~a" 1] "@[~a]" "%never"

  describe "intersection matches" do
    testResiduals [edge' 1 "a" 2, edge' 1 "b" 2] "[a & b]" "%never"
    testResiduals [edge' 1 "a" 2, edge' 2 "a" 1] "[~a & a]" "%never"
    testResiduals [edge' 1 "a" 2] "[@<a & @<b]" "[@1<b]>@2"
    testResiduals
      [edge' 1 "a" 2, edge' 3 "b" 2]
      "[@<a & @3<b & @<c]>@"
      "[@1<c]>@2"
    -- not super sure what I want the semantics of these to be
    -- because it's unanchored it seems we'd want to avoid specializing b to
    -- just work on the edges in the graph, but that seems error prone
    -- potentially, e.g. `mk a & b` should create (a & b)/@ but if we don't
    -- anchor when computing residuals it might create disconnected nodes
    -- maybe the solution is just always appending `/@` onto the end of
    -- `mk` paths and leaving the behavior of preserving unanchored nodes as
    -- join points outside of our residual computing function?
    -- testResiduals [edge' 1 "a" 2] "[a & b]" "[b]"
    -- testResiduals [edge' 1 "a" 2] "[a & b]" "[b]"
    testResiduals [edge' 1 "a" 2] "[@<a & @<b & @<c]>@" "[@1<b & @1<c]>@2"

  describe "sequence matches" do
    testResiduals [edge' 1 "a" 2, edge' 2 "b" 3] "[a /| b]" "%never"
    testResiduals [edge' 1 "a" 2] "[a /| b]" "[@2<b]"
    -- we want to keep the resulting expression minimally constrained; so we
    -- don't e.g. specialize "c /| d" to "c /@2| d"
    testResiduals [edge' 1 "a" 2] "[a /| b & c /| d]" "[@2<b & @1<(c /| d)]"
    testResiduals [edge' 1 "a" 2, edge' 2 "a" 1] "@[a /@2| ~a]" "%never"
    testResiduals [edge' 1 "a" 2, edge' 2 "a" 1] "@[a /@| ~a]" "%never"
    testResiduals [edge' 1 "a" 1, edge' 1 "a" 1] "@[a /@| ~a]" "%never"
    testResiduals [edge' 1 "a" 2, edge' 2 "a" 1] "@[a /| ~a]" "%never"
    testResiduals [edge' 1 "a" 1, edge' 1 "a" 1] "@[a /| ~a]" "%never"

  describe "unchanged when unmatched" do
    testResiduals [] "[a]" "[a]"
    testResiduals [edge' 1 "a" 2] "[~a]" "[~a]"
    testResiduals' (disconnectedGraph [1]) "[a]" "[a]"
    testResiduals [] "[@<a & @<b]" "[@<a & @<b]"
    testResiduals [] "[a & b]" "[a & b]"
    testResiduals [] "@" "@"
    testResiduals [] "@[a & b /| c]" "@[a & b /| c]"
    testResiduals [] "@[a]" "@[a]"
    testResiduals [] "@1[a]" "@1[a]"
    testResiduals [edge' 1 "a" 2] "@[a]" "@[a]"
    testResiduals [edge' 1 "a" 2] "@1[a]" "@1[a]"
    testResiduals' (disconnectedGraph [1]) "@[a]" "@[a]"
    testResiduals [edge' 1 "a" 2] "[a /@3| b]" "[a /@3| b]"
    testResiduals [edge' 1 "a" 2] [rq|[regex:"^b*$"]|] [rq|[regex:"^b*$"]|]
    testResiduals [edge' 1 "apple" 2] [rq|[regex:"b"]|] [rq|[regex:"b"]|]

  describe "%never is always %never" do
    testResiduals [edge' 1 "a" 2] "%never" "%never"
    testResiduals [] "%never" "%never"

  describe "unions" do
    testResiduals [edge' 1 "a" 2] "[a] + [b]" "[b]"
    testResiduals [edge' 1 "a" 2] "[a & b] + [c]" "[@1<b]>@2 + [c]"
    testResiduals [edge' 1 "a" 2] "[a] + [a & b]" "[@1<b]>@2"
