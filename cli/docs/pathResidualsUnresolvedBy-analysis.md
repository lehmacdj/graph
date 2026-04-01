# `pathResidualsUnresolvedBy` Analysis

## Current State

130 of 137 tests pass, 7 fail:
- 4 failures in "complex roots" (target concretization / root resolution)
- 2 property test crashes (`impureNonNull` on generated paths)
- 1 failure due to `undefined` left in code (line 89)

## 1. Ambiguous / Potentially Buggy Test Cases

### A. Complex roots: resolution when roots have specific anchors different from startNid

**Failing test (line 1819-1821):** `[@2<a & @<b]>@` in graph `[@1>"a">@3]`
- Expected: `[@2<b]>@3`
- Actual: `[@<"b"]>@ + [@000000000002<"a"]>@`

The path has two roots: `@2` (specific) and `@` (unanchored, = startNid
@1). The graph has `@1>"a">@3` but no edge from `@2`. So `@2<a` fails
(no outgoing `a` from @2), and `@<b` from @1 also fails (no `b` edge
from @1).

The expected output `[@2<b]>@3` implies that `@1>"a">@3` somehow
resolves `@2<a` AND concretizes the target to @3. This only makes sense
if `@2<a` is interpreted as "start from @2 independently" and @2 happens
to have `a` edges in other test cases — but here it doesn't.

**Design question**: When a path has `[@2<a & @<b]>@` and we start from
@1, should `@2<a` attempt resolution starting from @2 (independent of
startNid)? Or should it attempt resolution starting from startNid=@1,
failing because @1 != @2?

<!-- COMMENT: As written I'd expect this test to return exactly what the original is. Are the inputs for this test the same as a different test that has the actual output as its expected output? It's possible this is a redundant/bad test -->

### B. Complex roots: loop failure tainting resolved branches (likely test bug)

**Failing test (line 1827-1829):** `[@2[c]<a & @1<b]` in graph
`[@2>"a">@3, @1>"b">@3]`
- Expected: `[@2<c]>@2 + [@1<b]>@3`
- Actual: `[@000000000002["c"]<"a"]`

Both branches resolve and agree on target @3. But loop `c` at @2 doesn't
resolve (no `@2>"c">@2` edge). The expected residual includes
`[@1<b]>@3` — the portion that successfully resolved.

This contradicts other tests where resolved branches are excluded from
residuals. For example, line 1812-1817: `[@2[c]<a & @<b]>@` with
`@2[c]` resolved only outputs the residual for the unresolved `@<b`
branch, not the resolved `@2[c]<a`.

**Assessment**: The expected output is likely a test bug. The correct
expected output should be `[@2<c]>@2` (just the unresolved loop).

<!-- COMMENT: this seems right to me. -->

### C. Complex roots: target concretization when one root resolves

**Failing test (line 1812-1817):** `[@2[c]<a & @<b]>@` in graph
`[@2>"c">@2, @2>"a">@3]`
- Expected: `[@<b]>@3`
- Actual: `[@<"b"]>@`

Root `@2[c]<a` fully resolves (loop works, branch reaches @3). Root
`@<b` (from @1) fails. The expected output keeps only the failed branch
and concretizes the target to `>@3` because the other root proved the
target is @3.

The actual output fails to concretize the target. This is a real
implementation bug — when one root in an intersection fully resolves and
identifies the target NID, that information should propagate to the
residual for unresolved roots.

**Failing test (line 1822-1825):** `[@2[c]<a & @<b]>@` in graph
`[@2>"c">@2, @1>"b">@3]`
- Expected: `[@2<a]>@3`
- Actual: `[@000000000002<"a"]>@`

Same pattern: `@<b` from @1 resolves to @3, so the target should be
concretized to @3 in the residual for the unresolved `@2<a`.

<!-- COMMENT: -->

### D. Unanchored intersection semantics (commented-out tests, line 1474-1483)

The comments in the code explicitly note uncertainty:
> "not super sure what I want the semantics of these to be"

For `[a & b]` when only `a` resolves — should the result be `[b]`
(preserving unanchored) or `[@1<b]>@2` (anchoring)?

**Approaches:**
1. **Always anchor on partial resolution**: Concretize both root and
   target when any branch resolves. Prevents disconnected nodes when used
   with `mk`.
2. **Preserve original anchoring**: Keep Unanchored where the original
   was Unanchored. Simpler but may produce disconnected nodes.
3. **Handle at the caller level**: Keep `pathResidualsUnresolvedBy`
   minimal (preserve anchoring), and have `mk` append `/@` to enforce
   anchoring.

<!-- COMMENT: -->

### E. Property tests crash with NullError

Both property tests fail with `impureNonNull` errors before testing the
property logic. This is a crash in the implementation — likely an empty
list being passed to `impureNonNull` somewhere in
`traverseViaTransitions` or the residual logic when processing certain
generated path shapes (probably empty branch sets or degenerate
sequences).

<!-- COMMENT: -->

## 2. Repurposing `createNPath` for `graphResolvingPathFrom`

Both functions use `traverseViaTransitions` with nearly identical
structure:

| Aspect | `createNPath` | `graphResolvingPathFrom` |
|--------|--------------|------------------------|
| Input anchor type | `NID` | `Anchor` |
| Output anchor type | `NID` | `NID` |
| `fanchor` | Identity | Maps Anchor->NID (generating fresh NIDs) |
| `ftransition` | Creates literal edges | Creates edges (random labels for wildcards) |

**Proposed approach**: `graphResolvingPathFrom` does a single
`traverseViaTransitions` pass that converts `NormalizedPath Anchor` to
`NormalizedPath NID` by resolving anchors to concrete NIDs (fresh random
NIDs for Unanchored/JoinPoint, concrete NIDs for Specific) and replacing
wildcards/regex with generated labels. Then call `createNPath` on the
result.

```haskell
graphResolvingPathFrom gen startNid path =
  let (nidPath, gen') =
        run . runState gen $
          traverseViaTransitions startNid unifyAnchor resolveTransition path
      graph = graphFromGraphMetadataEditing (createNPath nidPath)
  in graph
```

Where `resolveTransition` generates fresh NIDs for unanchored targets and
concrete labels for wildcards, and `unifyAnchor` maps
Unanchored/JoinPoint to the current NID and Specific to its NID.

The only subtlety: `traverseViaTransitions`'s `fanchor` uses
`Bool -> b -> a -> Maybe b` where `isRoot=True` allows `FSpecific` to
override the current NID. For `graphResolvingPathFrom`, `Specific nid`
should always jump to `nid` regardless of current position. The existing
`unifyAnchor` in TestUtils already handles this (ignoring `_isRoot`).

<!-- COMMENT: -->

## 3. Comprehensive Spec for `pathResidualsUnresolvedBy`

**Purpose**: Given a start NID, a graph, and a normalized path, compute
the "residual" — the minimal portion of the path not already satisfied by
the graph.

**Core invariant**: `pathResidualsUnresolvedBy startNid (graph U
edgesOf(residual)) originalPath = %never`. Creating the residual's edges
and adding them to the graph should make the original path fully
resolvable.

### Behavior by path type

**Empty path (`%never`)**: Returns `%never`.

**Unions (`path1 + path2`)**: Each deterministic path processed
independently. Results collected into a union, with fully-resolved paths
filtered out.

**Pointlike paths (anchor with optional loops)**:
- Anchor doesn't unify with startNid -> return unchanged
- Node doesn't exist in graph -> return unchanged
- No loops, node exists -> return `%never`
- Loops all resolve (each returns to anchor NID) -> return `%never`
- Some loops unresolved -> return residual with only unresolved loops,
  anchored to the concrete NID if any loops resolved

**Rooted paths (`[root<branches]>target`)**:
- Each (root, branches) pair resolves independently
- Root resolution: unify root anchor with startNid (or with root's
  specific NID for `@N` anchors), check root loops
- Branch resolution: follow each branch from the resolved root NID
- Intersection semantics: ALL branches must succeed for full resolution
- Partial intersection: residual contains only failed branches, with
  root anchored to the concrete NID
- Target concretization: if any branch (from any root in the
  intersection) resolves to a destination, concretize the target to that
  NID

**Sequences (`bs1 /@ midpoint | bs2`)**:
- Follow bs1 from start -> set of destination NIDs
- Unify destinations with midpoint anchor
- Check midpoint loops resolve
- Follow bs2 from midpoint
- bs1 fails entirely -> residual is the whole sequence unchanged (but
  anchored to start if other context resolved)
- bs1 succeeds, bs2 fails -> residual is `[@midNid<bs2]>target` (or
  `[@midNid<bs2_failed]>target` for partial bs2 intersections)
- Midpoint loops fail -> residual includes the loop portion

**Loop resolution (branches in a pointlike that must return to the
anchor NID)**:
- Forward: follow direction from anchor, check if destination = anchor
- Incoming/backward: check if edges from other nodes can complete the
  loop in reverse
- Bidirectional: chain forward from anchor + chain backward from anchor
  meeting in the middle
- Partially-resolved loop -> rooted path from forward endpoint to
  backward endpoint
- Deconstructed loop residuals should stay as connected/deterministic as
  possible (prefer `[@2<b /@1| c]` over `[@2<b]>@1 + [@1<c]`)

**Anchor concretization rules**:
- Concretize root anchor to specific NID when any branch partially
  resolves
- Concretize target anchor when any branch fully resolves to a
  destination
- Preserve original anchoring (Unanchored/JoinPoint) when nothing
  resolves
- Minimize concretization: only anchor what's necessary (e.g., prefer
  `[@<b]>@3` over `[@1<b]>@3` when the root doesn't need concretizing)

<!-- COMMENT: -->

## 4. Generalizing `traverseViaTransitions` for `pathResidualsUnresolvedBy`

The current implementation is ~1800 lines with ~15 intermediate data
types because it interleaves traversal logic with residual-construction
logic. The key architectural insight is to separate these concerns.

### Current `traverseViaTransitions` limitations for residuals

- It filters out loop branches that don't return to start — but residual
  computation needs to KEEP these and transform them into rooted paths
- It produces `NormalizedPath b` with the same structure as input — but
  residuals may have fundamentally different structure (loops become
  rooted paths, sequences get truncated)
- It doesn't expose intermediate resolution state (which NIDs were
  reached at midpoints, which branches succeeded/failed)

### Proposed design: `traverseViaTransitionsAnnotated`

A new variant that walks the same structure but annotates instead of
filtering:

```haskell
data AnnotatedDP a b = AnnotatedDP
  { original :: DeterministicPath a
  , rootResults ::
      [(DeterministicPath a, OSet (DPBranch a), RootResult b)]
  }

data RootResult b
  = RootResolved b
  | RootLoopsFailed b [LoopAnnotation b]
  | RootUnresolved

data LoopAnnotation b
  = LoopResolved
  | LoopForwardOnly b   -- reached this NID going forward, didn't return
  | LoopBackwardOnly b  -- reachable going backward from this NID
  | LoopGap b b         -- forward reached b1, backward from b2

data BranchAnnotation b
  = BranchResolved (Set b)
  | BranchFailed
  | BranchPartialSeq
      (Set b)              -- midpoints reached
      [BranchAnnotation b] -- bs2 results
```

The traversal function:
```haskell
traverseViaTransitionsAnnotated ::
  b ->
  (Bool -> b -> a -> Maybe b) ->
  (b -> DPDirection -> Set b) ->  -- simplified: returns destinations
  NormalizedPath a ->
  [AnnotatedDP a b]
```

Then residual extraction is a pure function:
```haskell
extractResiduals :: [AnnotatedDP Anchor NID] -> NormalizedPath Anchor
```

### Benefits

- Traversal logic shared with `resolveNPath`/`createNPath` (DRY)
- Residual construction is pure and independently testable
- Annotation types make the intermediate state explicit and inspectable
- ~200-300 lines total vs ~1800 currently
- Loop handling and intersection handling become pattern matches on
  annotation types instead of interleaved imperative logic

### Alternative: lighter-weight approach

If the full annotation type feels heavy, a simpler variant: modify
`traverseViaTransitions` to accept an additional callback for "what to
do with non-returning loop branches" instead of silently dropping them.
This addresses the biggest pain point (loops becoming rooted paths in
residuals) while keeping the existing architecture mostly intact.
