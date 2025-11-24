module Models.NormalizedPath.UnresolvedResiduals
  ( pathResidualsUnresolvedBy,
    spec_pathResidualsUnresolvedBy,
  )
where

import Models.Connect
import Models.Graph
import Models.NID
import Models.NormalizedPath
import Models.NormalizedPath.Parse (pNormalizedPath)
import MyPrelude
import Utils.Testing

-- | The portion of a path that can't be resolved in the given graph from the
-- given start NID. For generality this operates on `NormalizedPath Anchor`
-- rather than `FullyAnchored` or `NID`.
--
-- This can be used to get the residual path that wasn't resolved while
-- resolving a path using resolvePath if you pass the non-thin graph from the
-- ResolvedPath and the same starting NID that was used to resolve it.
--
-- You might want to use this to then create the new nodes/transitions that
-- would have been necessary for it to be resolved.
pathResidualsUnresolvedBy ::
  NID -> Graph Text () -> NormalizedPath Anchor -> NormalizedPath Anchor
pathResidualsUnresolvedBy startNid graph (NormalizedPath dps) = undefined

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
              <> " in the graph "
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
    testResiduals [edge' 2 "a" 1] "[~a]" "%never"

    testResiduals [edge' 1 "a" 1, edge' 1 "b" 1] "@[a]" "%never"
    testResiduals [edge' 1 "a" 1] "@[a]" "%never"
    testResiduals [edge' 1 "a" 1] "@[*]" "%never"
    testResiduals [edge' 1 "a" 1] [rq|@[regex:"^a*$"]|] "%never"
    testResiduals [edge' 1 "apple" 1] [rq|@[regex:"a"]|] "%never"
    testResiduals [edge' 1 "a" 1] "@[a]" "%never"
    testResiduals [edge' 1 "a" 1] "@1[a]" "%never"

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
    testResiduals [edge' 1 "b" 2] "[@<a & @<b & @<c]>@" "[@1<a & @1<c]>@2"
    testResiduals [edge' 1 "c" 2] "[@<a & @<b & @<c]>@" "[@1<a & @1<b]>@2"
    testResiduals
      [edge' 1 "c" 2, edge' 1 "a" 2]
      "[@<a & @<b & @<c]>@"
      "[@1<b]>@2"

  describe "sequence matches" do
    testResiduals [edge' 1 "a" 2, edge' 2 "b" 3] "[a /| b]" "%never"
    testResiduals [edge' 1 "a" 2] "[a /| b]" "[@2<b]"
    testResiduals [edge' 1 "a" 2] "@[a /| b]" "[@2<b]>@1"
    testResiduals [edge' 1 "a" 2] "@1[a /| b]" "[@2<b]>@1"
    -- we want to keep the resulting expression minimally constrained; so we
    -- don't e.g. specialize "c /| d" to "c /@2| d"
    testResiduals
      [edge' 1 "a" 2]
      "[@<(a /| b) & @<(c /| d)]"
      "[@2<b & @1<(c /| d)]"
    testResiduals
      [edge' 1 "c" 2]
      "[@<(a /| b) & @<(c /| d)]"
      "[@2<d & @1<(a /| b)]"
    testResiduals
      [edge' 1 "z" 2, edge' 2 "a" 3]
      "[@<z /| (a /| b & c /| d)]"
      "[@3<b & @2<(c /| d)]"
    testResiduals
      [edge' 1 "z" 2, edge' 2 "c" 3]
      "[@<z /| (a /| b & c /| d)]"
      "[@3<d & @2<(a /| b)]"
    testResiduals
      [edge' 1 "z" 2, edge' 2 "c" 3, edge' 3 "d" 4]
      "[@<z /| (a /| b & c /| d)]"
      "[@2<(a /| b)]"
    testResiduals [edge' 1 "a" 2, edge' 2 "a" 1] "@[a /@2| ~a]" "%never"
    testResiduals [edge' 1 "a" 2, edge' 2 "a" 1] "@[a /@| ~a]" "%never"
    testResiduals [edge' 1 "a" 1, edge' 1 "a" 1] "@[a /@| ~a]" "%never"
    testResiduals [edge' 1 "a" 2, edge' 2 "a" 1] "@[a /| ~a]" "%never"
    testResiduals [edge' 1 "a" 1, edge' 1 "a" 1] "@[a /| ~a]" "%never"
    testResiduals [edge' 1 "a" 2] "[a /| (b & c)]" "[@2<(b & c)]"
    testResiduals [edge' 1 "a" 2, edge' 2 "b" 3] "[a /| (b & c)]" "[@2<c]"

    describe "with loops at midpoints" do
      testResiduals [edge' 1 "a" 2, edge' 2 "c" 3] "[a /@[b]| c]" "@2[b]"
      testResiduals [edge' 1 "a" 2, edge' 2 "c" 3] "[a /@2[b]| c]" "@2[b]"
      testResiduals [edge' 1 "a" 2, edge' 2 "b" 2] "[a /@[b]| c]" "[@2<c]"
      testResiduals [edge' 1 "a" 2, edge' 2 "b" 2] "[a /@2[b]| c]" "[@2<c]"
      testResiduals
        [edge' 1 "a" 2, edge' 2 "b" 3, edge' 3 "c" 2]
        "[a /@[b/|c]| d]"
        "[@2<d]"
      testResiduals
        [edge' 1 "a" 2, edge' 2 "b" 2, edge' 2 "c" 2]
        "[a /@[b & c]| d]"
        "[@2<d]"
      testResiduals [edge' 1 "a" 2] "[a /@[b/|c]| d]" "[@2[b /| c]<d]"
      testResiduals [edge' 1 "a" 2, edge' 3 "c" 2] "[a /@[b/|c]| d]" "[@3<c /| d]"
      testResiduals [edge' 1 "a" 2] "[a /@2[b]| c]" "[@2[b]<c]"
      testResiduals [edge' 1 "a" 2] "[a /@[b & c]| d]" "[@2[b & c]<d]"

  describe "keeps deconstructed loops as deterministic as possible" do
    testResiduals
      [edge' 1 "a" 2]
      "[@[a/|b]<c]"
      -- we want to keep the resulting paths as connected/deterministic as
      -- possible, e.g. we could produce "[@2<b]>@1 + [@1<c]" which encodes the
      -- same edges, but that requires two deterministic paths instead of just
      -- one
      "[@2<b /@2| c]"
    testResiduals
      [edge' 1 "a" 2, edge' 2 "b" 3]
      "[a /@[b/|c]| d]"
      "[@3<c /@2| d]"
    testResiduals
      [edge' 1 "a" 2, edge' 3 "c" 2]
      "[a /@[b/|c]| d]"
      "[@3<~b /@2| d]"
    testResiduals
      [edge' 1 "a" 2]
      "@[a/|b/|c]"
      "[@2<b /| c]>@1"
    testResiduals
      [edge' 1 "a" 2]
      "[@[a/|b/|c]<d]"
      "[@2<b /| c /@1| d]"
    testResiduals
      [edge' 2 "c" 1]
      "[@[a/|b/|c]<d]"
      -- inverting edges is preferrable to disconnecting DPaths
      "[@2<~b /| ~a /@1| d]"
    testResiduals
      [edge' 2 "c" 1, edge' 1 "d" 3]
      "[@[a/|b/|c & d/|e/|f]<g]"
      "[[@2<~b /| ~a  & @3<e /| d]>@1<g]"
    testResiduals
      [edge' 2 "c" 1, edge' 1 "a" 3]
      "[@[a/|b/|c/|d]<e]"
      -- it's possible for it to be necessary to separate out an edge though
      "[@3<b/|c]>@2 + [@1<e]"
    testResiduals
      [edge' 2 "c" 1, edge' 1 "a" 3]
      "[@[a/|~b/|~c/|d]<e]"
      -- this should preference the original orientation of edges
      "[@3<~b/|~c]>@2 + [@1<e]"

  describe "multiple transition matches" do
    testResiduals [edge' 1 "a" 2, edge' 1 "a" 3] "[a]" "%never"
    testResiduals [edge' 1 "a" 2, edge' 1 "a" 3] "[a/|b]" "[@2<b & @3<b]"
    testResiduals [edge' 2 "b" 3, edge' 2 "c" 4] "[a/|b/|c]" "[@3<c & @4<c]"

  describe "unchanged when unmatched" do
    testResiduals [] "[a]" "[a]"
    testResiduals [edge' 1 "~a" 2] "[~a]" "[~a]"
    testResiduals [edge' 1 "~a" 1] "@[~a]" "@[~a]"
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
    testResiduals [edge' 1 "a" 2] "[@<a & @<b]>@ + [c]" "[@1<b]>@2 + [c]"
    testResiduals [edge' 1 "a" 2] "[@<a]>@ + [@<a & @<b]>@" "[@1<b]>@2"
