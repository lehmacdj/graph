module Models.NormalizedPath.UnresolvedResiduals
  ( pathResidualsUnresolvedBy,
    spec_pathResidualsUnresolvedBy,
  )
where

import Graph.GraphMetadataEditing (runInMemoryGraphMetadataEditing)
import Graph.Resolve (resolveNPath)
import Models.Graph
import Models.NID
import Models.NormalizedPath
import Models.NormalizedPath.Generators
import Models.NormalizedPath.Parse (pNormalizedPath)
import Models.NormalizedPath.TestUtils
import Models.ResolvedPath (ResolvedPath (..))
import MyPrelude
import Polysemy.State (evalState)
import System.Random
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
--
-- Note: behavior for intersections whose target is Unanchored is dubious:
-- residuals never pin Unanchored anchors, but resolution requires such
-- branches to meet at a join point, so the residual may not be sufficient
-- to make the path resolve. Callers that want join semantics must anchor
-- the target (e.g. by appending @>\@@). See
-- docs/pathResidualsUnresolvedBy-spec.md.
--
-- TODO: stub pending the rewrite specified in
-- docs/pathResidualsUnresolvedBy-spec.md; treats every path as fully
-- unresolved.
pathResidualsUnresolvedBy ::
  NID -> Graph Text () -> NormalizedPath Anchor -> NormalizedPath Anchor
pathResidualsUnresolvedBy _startNid _graph path = path

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
    testResiduals [edge' 2 "a" 2] "@2[a]" "%never"

  describe "intersection matches" do
    testResiduals [edge' 1 "a" 2, edge' 1 "b" 2] "[a & b]" "%never"
    testResiduals [edge' 1 "a" 2, edge' 2 "a" 1] "[~a & a]" "%never"
    -- the JoinPoint root @< pins to @1 because a sibling branch resolved,
    -- but the Unanchored target is never pinned
    testResiduals [edge' 1 "a" 2] "[@<a & @<b]" "[@1<b]"
    testResiduals
      [edge' 1 "a" 2, edge' 3 "b" 2]
      "[@<a & @3<b & @<c]>@"
      "[@1<c]>@2"
    -- Unanchored anchors are never pinned in residuals: neither the root nor
    -- the target gets specialized here. Paths like this one, whose
    -- intersection must meet at an Unanchored target, are exempt from the
    -- residual invariant; callers that want join semantics must anchor the
    -- target with >@ (see docs/pathResidualsUnresolvedBy-spec.md)
    testResiduals [edge' 1 "a" 2] "[a & b]" "[b]"
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
      "[@2<(a /| b) & @3<d]"
    testResiduals
      [edge' 1 "z" 2, edge' 2 "c" 3, edge' 3 "d" 4]
      "[@<z /| (a /| b & c /| d)]"
      -- the Unanchored target is not pinned to @4 where c /| d landed
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
      -- Note: loop b/|c at @2 completes via incoming c from @3, so b is inverted to ~b
      testResiduals [edge' 1 "a" 2, edge' 3 "c" 2] "[a /@[b/|c]| d]" "[@3<~b /@2| d]"
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
      "[@2<b /@1| c]"
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
      "[[@2<~b /| ~a & @3<e /| f]>@1<g]"
    testResiduals
      [edge' 2 "d" 1, edge' 1 "a" 3]
      "[@[a/|b/|c/|d]<e]"
      -- it's possible for it to be necessary to separate out an edge though
      "[@3<b/|c]>@2 + [@1<e]"
    testResiduals
      [edge' 2 "d" 1, edge' 1 "a" 3]
      "[@[a/|~b/|~c/|d]<e]"
      -- this should preference the original orientation of edges
      "[@3<~b/|~c]>@2 + [@1<e]"

  describe "multiple transition matches" do
    testResiduals [edge' 1 "a" 2, edge' 1 "a" 3] "[a]" "%never"
    testResiduals [edge' 1 "a" 2, edge' 1 "a" 3] "[a/|b]" "[@2<b & @3<b]"
    testResiduals [edge' 1 "a" 2, edge' 2 "b" 3, edge' 2 "b" 4] "[a/|b/|c]" "[@3<c & @4<c]"

  describe "unchanged when unmatched" do
    testResiduals [] "[a]" "[a]"
    testResiduals [edge' 2 "a" 3] "[a]>@3" "[a]>@3"
    testResiduals [edge' 3 "b" 4, edge' 1 "a" 2] "[a/|b]>@4" "[@2<b]>@4"
    testResiduals [edge' 3 "a" 2] "[~a]>@3" "[~a]>@3"
    testResiduals [edge' 4 "b" 3, edge' 1 "a" 2] "[a/|~b]>@4" "[@2<~b]>@4"
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

  describe "edge cases" do
    describe "multiple incoming sources" do
      -- Both @2 and @3 have 'c' edges to @1, should produce residuals from both
      testResiduals
        [edge' 2 "c" 1, edge' 3 "c" 1]
        "@[a/|b/|c]"
        "[@2<~b /| ~a]>@1 + [@3<~b /| ~a]>@1"

    describe "wildcard matching incoming" do
      -- Wildcard matches the incoming 'x' edge from @2, but 'a' still needs resolution
      testResiduals [edge' 2 "x" 1] "@[a/|*]" "[@2<~a]>@1"
      -- There still needs to be an edge that is actually a loop for the
      -- wildcard to succeed
      testResiduals [edge' 2 "x" 1] "@[*]" "@[*]"

    describe "loop paths handling from both ends" do
      describe "complete resolution" do
        testResiduals [edge' 1 "a" 2, edge' 1 "b" 2] "@[a/|~b]" "%never"
        testResiduals
          [edge' 1 "a" 2, edge' 1 "b" 2, edge' 2 "c" 2]
          "@[a/|~c/|~b]"
          "%never"
        testResiduals
          [edge' 1 "a" 2, edge' 2 "d" 2, edge' 1 "b" 2, edge' 2 "c" 2]
          "@[a/|d/|~c/|~b]"
          "%never"
      describe "partial resolution from outgoing end only" do
        testResiduals [edge' 1 "a" 2] "@[a /| b]" "[@2<b]>@1"
        testResiduals [edge' 1 "a" 2] "@1[a /| b]" "[@2<b]>@1"
        testResiduals [edge' 1 "a" 2] "[@[a /| b]<c]" "[@2<b/@1|c]"
      describe "partial resolution from incoming end only" do
        -- Multiple incoming edges forming a chain should all be resolved
        -- @2>"c">@1 handles 'c', @3>"b">@2 handles 'b', only 'a' remains
        testResiduals [edge' 2 "c" 1, edge' 3 "b" 2] "@[a/|b/|c]" "[@3<~a]>@1"
        -- Neither branch has outgoing, both complete via incoming
        testResiduals
          [edge' 2 "c" 1, edge' 3 "f" 1]
          "@[a/|b/|c & d/|e/|f]"
          "[@2<~b /| ~a & @3<~e /| ~d]>@1"
      describe "don't meet up in middle" do
        -- For @[a/|~b]: 'a' goes @1->@2, '~b' looks for incoming 'b' at @2
        -- But @1>"b">@3, not to @2, so ~b fails - residual starts from @2
        testResiduals
          [edge' 1 "a" 2, edge' 1 "b" 3]
          "@[a/|~b]"
          "[@2<~b]>@1"
        testResiduals
          [edge' 1 "a" 2, edge' 1 "b" 2, edge' 2 "c" 3]
          "@[a/|~c/|~b]"
          "[@2<~c]>@2"
        testResiduals
          [edge' 1 "a" 2, edge' 2 "d" 2, edge' 1 "b" 2, edge' 2 "c" 3]
          "@[a/|d/|~c/|~b]"
          "[@2<~c]>@2"
        testResiduals
          [edge' 1 "a" 2, edge' 2 "d" 2, edge' 1 "b" 2, edge' 2 "c" 3]
          "[@[a/|d/|~c/|~b]<e]"
          "[@2<~c]>@2 + [@1<e]"
        testResiduals
          [edge' 1 "a" 2, edge' 2 "d" 2, edge' 1 "b" 2, edge' 2 "c" 3]
          "[@[a/|(d/|~c/|~b & f)]<e]"
          "[@2<~c]>@2 + [@2<f/@1|e]"
        testResiduals
          [edge' 1 "a" 2, edge' 2 "d" 2, edge' 1 "b" 2, edge' 2 "c" 3]
          "[@[a/|(d/|~c/|~b & f & g)]<e]"
          "[@2<~c]>@2 + [@2<(f & g)/@1|e]"
      describe "flexible middle transition" do
        -- if we can resolve from both ends we can/should
        testResiduals
          [edge' 1 "a" 2, edge' 1 "b" 3]
          "@[a/|c/|~b]"
          "[@2<c]>@3"
        testResiduals
          [edge' 1 "a" 2, edge' 1 "b" 3]
          "@[a/|d/|c/|~b]"
          "[@2<d/|c]>@3"
        -- ~b only matches an edge with the orientation that creating the
        -- path would mint (@1>"b">@3); a b-edge pointing the other way
        -- (@3>"b">@1) cannot satisfy the trailing ~b, so ~b stays in the
        -- residual
        testResiduals
          [edge' 1 "a" 2, edge' 3 "b" 1]
          "@[a/|c/|~b]"
          "[@2<c /| ~b]>@1"
        testResiduals
          [edge' 1 "a" 2, edge' 3 "b" 1]
          "@[a/|d/|c/|~b]"
          "[@2<d /| c /| ~b]>@1"
        -- trailing-gap counterpart: forward consumes a and c, ~b remains
        testResiduals
          [edge' 1 "a" 2, edge' 2 "c" 3]
          "@[a/|c/|~b]"
          "[@3<~b]>@1"
    describe "JoinPoint with exclusions" do
      -- Can't reach @2 when @2 is excluded, unchanged
      testResiduals [edge' 1 "a" 2] "[a]>!{@2}" "[a]>!{@2}"
      -- @2 is not excluded, resolves fine
      testResiduals [edge' 1 "a" 2] "[a]>!{@1}" "%never"

    describe "target anchor mismatch" do
      -- Reached @2 but target requires @3, unchanged
      testResiduals [edge' 1 "a" 2] "[a]>@3" "[a]>@3"

    describe "loops at midpoint of sequence" do
      testResiduals
        [edge' 1 "a" 2, edge' 2 "x" 2]
        "[a /@[x]| b]"
        "[@2<b]"
      testResiduals
        [edge' 1 "a" 2, edge' 2 "x" 3]
        "[a /@[x]| b]"
        "[@2[x]<b]"
      testResiduals
        [edge' 1 "a" 2, edge' 2 "x" 3]
        "[a /@[x/|y]| b]"
        "[@3<y /@2| b]"
      testResiduals
        [edge' 1 "a" 2, edge' 2 "x" 3]
        "[a /@[x/|y/|z]| b]"
        "[@3<y /| z /@2| b]"

    describe "absolute root NID" do
      -- Starts from @2 (not @1), 'a' resolves to @3
      testResiduals [edge' 2 "a" 3] "[@2<a]" "%never"
      -- Same but with explicit target matching
      testResiduals [edge' 2 "a" 3] "[@2<a]>@3" "%never"

    describe "nested intersections" do
      -- Intersection in bs1 of a loop: (a & b) must BOTH succeed to reach midpoint
      -- Both a and b exist and reach @2, so loop needs c to return
      testResiduals
        [edge' 1 "a" 2, edge' 1 "b" 2, edge' 2 "c" 1]
        "@[(a & b) /| c]"
        "%never"
      -- Only 'a' exists, 'b' fails; a/c resolves, leaving only b as residual
      testResiduals
        [edge' 1 "a" 2, edge' 2 "c" 1]
        "@[(a & b) /| c]"
        "[@1<b]>@2"
      -- Both exist but reach different nodes; c gets split with residuals from both nodes
      testResiduals
        [edge' 1 "a" 2, edge' 1 "b" 3]
        "@[(a & b) /| c]"
        "[@2<c & @3<c]>@1"

      -- Intersection in bs1 of rooted path (not loop)
      testResiduals
        [edge' 1 "a" 2, edge' 1 "b" 2, edge' 2 "c" 3]
        "[(a & b) /| c]"
        "%never"
      testResiduals
        [edge' 1 "a" 2, edge' 2 "b" 3, edge' 2 "c" 3]
        "[a /| (b & c)]"
        "%never"
      testResiduals
        [edge' 1 "a" 2, edge' 2 "c" 3]
        "[a /| (b & c)]"
        -- the Unanchored target is not pinned to @3 where c landed
        "[@2<b]"
      testResiduals
        [edge' 1 "a" 2, edge' 2 "c" 3]
        "[(a & b) /| c]"
        -- the Unanchored root is not pinned to @1; the @2 target is the
        -- junction where the existing c edge continues
        "[b]>@2"

      -- Nested: intersection in bs1, then sequence in bs2
      -- a & b both reach @2, then c/|d must complete
      testResiduals
        [edge' 1 "a" 2, edge' 1 "b" 2, edge' 2 "c" 3]
        "[(a & b) /| c /| d]"
        "[@3<d]"
      -- a & b both reach @2, c fails
      testResiduals
        [edge' 1 "a" 2, edge' 1 "b" 2]
        "[(a & b) /| c /| d]"
        "[@2<c /| d]"

      -- Double nested: intersection in both bs1 and bs2
      testResiduals
        [edge' 1 "a" 2, edge' 1 "b" 2, edge' 2 "c" 3, edge' 2 "d" 3]
        "[(a & b) /| (c & d)]"
        "%never"
      -- bs1 intersection succeeds, bs2 intersection partially fails;
      -- the Unanchored target is not pinned to @3 where c landed
      testResiduals
        [edge' 1 "a" 2, edge' 1 "b" 2, edge' 2 "c" 3]
        "[(a & b) /| (c & d)]"
        "[@2<d]"

      -- Loop with intersection in bs2 and gap computation
      -- @1 -a-> @2, then (b & c) must return to @1
      -- b completes via @3>"b">@1, c fails entirely
      testResiduals
        [edge' 1 "a" 2, edge' 3 "b" 1]
        "@[a /| (b & c)]"
        "[@2<b & @2<c]>@1"

    describe "complex roots" do
      testResiduals
        [edge' 2 "a" 4, edge' 3 "b" 4]
        "[@2<a & @3<b]>@"
        "%never"
      testResiduals
        [edge' 2 "c" 2, edge' 2 "a" 3, edge' 1 "b" 3]
        "[@2[c]<a & @<b]>@"
        "%never"
      testResiduals
        [edge' 2 "c" 2, edge' 2 "a" 3]
        "[@2[c]<a & @<b]>@"
        -- the JoinPoint root @< pins to @1 once a sibling branch resolves;
        -- the JoinPoint target pins to @3 which @2<a proved
        "[@1<b]>@3"
      testResiduals
        [edge' 1 "a" 3]
        "[@2<a & @<b]>@"
        -- nothing resolves (@2 has no a-edge, @1 has no b-edge), so nothing
        -- pins and the path is returned unchanged
        "[@2<a & @<b]>@"
      testResiduals
        [edge' 2 "c" 2, edge' 1 "b" 3]
        "[@2[c]<a & @<b]>@"
        "[@2<a]>@3"
      testResiduals
        [edge' 2 "a" 3, edge' 1 "b" 3]
        "[@2[c]<a & @1<b]"
        -- Both branches resolve and agree on target @3; only the loop `c` at
        -- @2 is unresolved. Adding just `@2>"c">@2` makes the original path
        -- fully resolvable, so the residual is only the unresolved loop. The
        -- resolved `@1<b` branch is excluded from the residual.
        "[@2<c]>@2"

  describe "property tests" do
    -- paths that require an intersection to meet at an Unanchored target
    -- are exempt from the residual invariant (residuals never pin
    -- Unanchored anchors), so the generator avoids them; see
    -- docs/pathResidualsUnresolvedBy-spec.md
    let genCoveredDp =
          genDeterministicPathLiteralOnly defaultGenConfig
            `suchThat` (not . requiresUnanchoredJoin)

        resolvesIn :: Graph Text () -> NID -> NormalizedPath Anchor -> Bool
        resolvesIn graph startNid path =
          resolveNPath startNid (leastConstrainedNormalizedPath path)
            & runInMemoryGraphMetadataEditing
            & evalState graph
            & run
            & \(resolved :: ResolvedPath) -> not (null resolved.path.union)

    -- All three properties precondition on the generated graph actually
    -- resolving the path: some paths can't resolve from the chosen start
    -- NID no matter the graph (e.g. a root JoinPoint excluding it), and
    -- for those the residual is the unchanged path, not %never.

    it "resolving graph produces %never residuals for complex paths" $
      property $ do
        seed <- arbitrary
        let gen = mkStdGen seed
        dp <- genCoveredDp
        startNid <- genNID
        let path = NormalizedPath (singletonSet dp)
            graph = graphResolvingPathFrom gen startNid path
            residual = pathResidualsUnresolvedBy startNid graph path
        pure $
          resolvesIn graph startNid path
            ==> residual === NormalizedPath mempty

    it "removing one edge produces residuals that resolve in removed edge graph" $
      property $ do
        seed <- arbitrary
        let gen = mkStdGen seed
        dp <- genCoveredDp
        startNid <- genNID
        let path = NormalizedPath (singletonSet dp)
            graph = graphResolvingPathFrom gen startNid path
            edges = graphEdges graph
        pure
          $ (not (null edges) && resolvesIn graph startNid path)
            ==> forAll (elements edges)
          $ \edgeToRemove ->
            let graphMinusEdge = deleteEdge edgeToRemove graph
                residual = pathResidualsUnresolvedBy startNid graphMinusEdge path
                -- The residual (if non-empty) should resolve in just the removed edge
                singleEdgeGraph = insertEdges [edgeToRemove] emptyGraph
                residualResolved =
                  pathResidualsUnresolvedBy startNid singleEdgeGraph residual
             in -- Either path fully resolves (edge wasn't critical) or
                -- residual resolves in just the removed edge
                (residual === NormalizedPath mempty)
                  .||. (residualResolved === NormalizedPath mempty)

    -- the core invariant, stated directly: creating the residual's edges on
    -- top of the partial graph must make the original path resolvable by
    -- resolveNPath
    it "creating the residual's edges makes the path resolve" $
      property $ do
        seed <- arbitrary
        let (graphGen, residualGen) = split (mkStdGen seed)
        dp <- genCoveredDp
        startNid <- genNID
        let path = NormalizedPath (singletonSet dp)
            graph = graphResolvingPathFrom graphGen startNid path
        pure $
          resolvesIn graph startNid path
            ==> forAll (sublistOf (graphEdges graph)) \removedEdges ->
              let partialGraph = deleteEdges removedEdges graph
                  residual = pathResidualsUnresolvedBy startNid partialGraph path
                  combined =
                    partialGraph
                      <> graphResolvingPathFrom residualGen startNid residual
               in counterexample
                    ("residual: " <> show residual)
                    (resolvesIn combined startNid path)
