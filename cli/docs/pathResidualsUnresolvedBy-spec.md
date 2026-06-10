# `pathResidualsUnresolvedBy` — Rewrite Spec

Spec for a from-scratch rewrite of `pathResidualsUnresolvedBy`
(`src/Models/NormalizedPath/UnresolvedResiduals.hs`). The test suite,
generator, and property tests already encode the target behavior (§7–§9);
the work is the function itself. This document is self-contained and
intended to be implemented from directly.

```haskell
pathResidualsUnresolvedBy ::
  NID -> Graph Text () -> NormalizedPath Anchor -> NormalizedPath Anchor
```

It computes the **residual**: the portion of `path` not already satisfied
by `graph` when starting traversal from `startNid`. Creating the residual's
edges must make the original path resolvable (§3). The function currently
has no callers; the intended consumer is a future `mk` command that creates
the missing parts of a path.

---

## 1. Notation and grammar

Path syntax (see `Models.NormalizedPath.Parse` for the grammar):

| syntax | meaning |
|---|---|
| `@N` | `Specific N` anchor (concrete node) |
| `@` | `JoinPoint {excluding = ∅}` |
| `!{@2}` | `JoinPoint {excluding = {@2}}` |
| *(absent)* | `Unanchored` |
| `[root< b1 & b2 ]>target` | `Rooted`: branches `b1 & b2` (intersection) from `root` to `target` |
| `[b]` | `Rooted` with unanchored root and target |
| `@N[l1 & l2]` | `Pointlike`: node `@N` with loop branches `l1`, `l2` |
| `t`, `~t`, `*`, `regex:"…"` | outgoing / incoming / wildcard / regex step |
| `a /| b` | sequence with unanchored midpoint |
| `a /@2| b`, `a /@[x]| b` | sequence with anchored midpoint / midpoint with loop |
| `p1 + p2` | union; `%never` is the empty union |

Grammar (mutually recursive; see `Models.NormalizedPath.Types`):

```
NormalizedPath    = Set DeterministicPath               -- union
DeterministicPath = Pointlike { anchor, loops :: Set Branch }
                  | Rooted { rootBranches :: Map DeterministicPath (Set Branch),
                             target :: Pointlike }
Branch            = Single Direction
                  | Sequence (Set Branch) Pointlike (Set Branch)
Direction         = Outgoing t | Incoming t             -- `t` / `~t`
Anchor            = Unanchored | JoinPoint {excluding} | Specific NID
```

Edges are written `@1>"a">@2` (edge labeled `a` from node 1 to node 2).

## 2. Matching semantics (normative)

These formulas are the single source of truth; the residual computation,
`resolveNPath` (`Graph/Resolve.hs`), and `createNPath` (`Graph/Create.hs`)
must all agree with them.

**Forward** (matches `resolveNPath`):

- step `t` from node `u` reaches `{ v | u >t> v }`
- step `~t` from node `u` reaches `{ s | s >t> u }`

**Backward** — the *exact inverse* of forward, nothing more:

- predecessors of `v` via `t` = `{ s | s >t> v }`
- predecessors of `v` via `~t` = `{ x | v >t> x }`

There is **no permissive orientation-flipping**: an edge `@3>"b">@1` does
*not* satisfy a trailing `~b` arriving at `@1` (that would require
`@1>"b">X`). Matching an edge in the wrong orientation produces residuals
that `resolveNPath` cannot actually resolve.

**Edge creation** (matches `createNPath`): instantiating step `t` from `u`
to `v` mints `u >t> v`; instantiating `~t` from `u` to `v` mints `v >t> u`.

`resolveNPath`'s forward matching already implements the formulas above;
backward matching is internal to the residual computation.

## 3. Core invariant

```
r  = pathResidualsUnresolvedBy startNid graph path
g' = graph <> edgesOf startNid r
⇒  resolveNPath startNid (leastConstrainedNormalizedPath path)
     fully resolves in g'
```

where `edgesOf startNid r` instantiates the residual exactly as
`graphResolvingPathFrom` does (§7): `Specific` anchors map
to their NID; `Unanchored`/`JoinPoint` root anchors unify with `startNid`;
other `Unanchored`/`JoinPoint` occurrences mint fresh NIDs, **one NID per
anchor occurrence** (shared across the branches that reference it), and a
loop's closing transition targets the loop's own root.

The choice of fresh NIDs is immaterial: unanchored/join-point components
unify with any NID at resolution time, so only structural consistency
(loop closure, join sharing) matters — the residual definition is *not*
coupled to the generator beyond that.

Corollaries:

- `r == %never` ⇔ `path` already fully resolves in `graph`.
- `pathResidualsUnresolvedBy startNid g' path == %never` (idempotence).

**Carve-out (per the §5 anchor rules).** The invariant is *not* required
to hold for paths containing an intersection (a branch set with more than
one member) whose target pointlike is `Unanchored`: `resolveNPath` (via
`leastConstrainedNormalizedPath`) treats a top-level unanchored target as
`FJoinPoint ∅`, i.e. the branches must meet, but the residual never pins
unanchored targets, so the instantiated residual may not make them meet.
Callers that want join semantics must write `>@` (or append it, as `mk`
will). The doc comment on `pathResidualsUnresolvedBy` must state that
behavior for unanchored intersections is dubious and may be inconsistent.
Property-test path generators must avoid such paths (or normalize them by
appending `>@`).

**Minimality is best-effort.** Correctness = the invariant. The
connectedness/orientation preferences (§6) are encoded as unit-test
expectations, not laws.

## 4. Architecture

Two layers. Layer 0 is the only code that touches the graph.

### 4.1 Layer 0: `resolveSegment`

A bidirectional matcher for one linear run of directed steps between a
forward and a backward frontier. Frontiers are **sets** — multiple matches
fan out, and multiple backward sources naturally become unions of
residuals (no special code).

```haskell
data StepInterp m = StepInterp
  { stepForward  :: NID -> Direction -> m (Set NID)
  , stepBackward :: NID -> Direction -> m (Set NID)  -- exact inverse, §2
  }

data BackwardMode = Off | On

data SegResult
  = SegResolved (Set NID)                    -- frontiers met; meeting NIDs
  | SegGap (Set NID) [Direction] (Set NID)   -- fwd reached, gap, bwd reached

resolveSegment
  :: Monad m
  => StepInterp m -> BackwardMode
  -> Set NID       -- forward frontier
  -> [Direction]   -- the linear run
  -> Set NID       -- backward frontier; "open" = unconstrained
  -> m SegResult
```

Forward pass: fold `stepForward` over the longest prefix keeping the
frontier non-empty. Backward pass (mode `On` only): fold `stepBackward`
over the longest suffix likewise. `SegResolved` iff every step was
consumed *and* the forward landing set meets the backward frontier; a
forward pass that consumes all steps but misses the backward frontier is
**not** progress — the run failed as a whole (e.g. `@[a]` in `[@1>"a">@2]`
is unchanged, not deconstructed). Otherwise `SegGap fwd middle bwd`.

A consumption that leaves an empty `middle` between *disjoint* frontiers
is invalid — there must be at least one step left to bridge them. Roll the
last backward step back so the gap is non-empty: in `@[a /| (b & c)]` with
`[@1>"a">@2, @3>"b">@1]`, consuming `b` backward would leave a zero-step
gap between `{@2}` and `{@3}`, so `b` stays unconsumed and the residual
branch is `[@2<b]>@1` (likewise `@[a/|~c/|~b]` in
`[@1>"a">@2, @1>"b">@2, @2>"c">@3]` rolls back `~c` to yield
`[@2<~c]>@2`).

Mode `Off` (resolve/create): no backward pass; open backward frontier
accepts any landing; loop frontier `{N}` requires returning to `N`.

The three operations on the shared machinery:

| | `stepForward` | `stepBackward` | mode |
|---|---|---|---|
| `resolveNPath` | match edges | unused | `Off` |
| `createNPath` | mint edge | unused | `Off` |
| residual | match edges | inverse match | `On` |

Folding `resolveNPath` onto this is a clean win. Fold `createNPath` on
only if it falls out naturally (it shares the structural walk, not
`resolveSegment`).

### 4.2 Layer 1: structural recursion

Pure; walks the grammar, calling `resolveSegment` on linear runs and
recursing at boundaries. A *linear run* is a chain of single directions
whose intervening midpoints are unanchored, loop-free, and non-branching
(`a/|c/|~b` flattens to `[a, c, ~b]`) — the whole run goes to
`resolveSegment` in **one** call; bidirectional meeting needs the entire
run at once. A *boundary* forces recursion: an intersection, an anchored
midpoint, or a midpoint with loops.

- **Union**: residual of each member independently; drop `%never` results.
- **Pointlike** `{anchor, loops}`: resolve the anchor per §5 (note:
  `Specific` jumps even for a top-level pointlike — `@2[a]` from
  `startNid = @1` resolves at `@2`). No loops ⇒
  `%never` if the node exists, unchanged otherwise. Each loop is a segment
  with forward and backward frontier `{N}`; gaps become deconstructed-loop
  residuals (§6).
- **Rooted**: for each `(root, branchSet)`: resolve `root` (a root is
  itself a `DeterministicPath` — delegate), then resolve the intersected
  `branchSet` from the root frontier toward the target. All branches must
  resolve for full resolution; the residual contains only failed/partial
  branches. Partially-consumed branches resume from **Specific frontier
  NIDs** — emitting those concrete resume points is required structure,
  not anchor pinning in the §5 sense.
- **Sequence**: `bs1` (intersection) from the current frontier to the
  midpoint (resolve midpoint loops there), then `bs2` onward. A partial
  midpoint loop advances a forward endpoint; its unresolved suffix becomes
  a deconstructed loop, after which `bs2` continues
  (`[a /@[x/|y]| b]` in `[@1>"a">@2, @2>"x">@3]` ⇒ `[@3<y /@2| b]`).

## 5. Anchor rules

| anchor | as root | as midpoint / target |
|---|---|---|
| `Specific N` | **jump**: traversal starts at `N` (even when `N ≠ startNid`) | **assert**: traversal must arrive at `N`; never rewritten |
| `JoinPoint` | unifies with `startNid` (respecting exclusions) | unifies with the arrival NID; intersected branches must agree |
| `Unanchored` | unifies with `startNid` | unifies with anything |

**Pinning in the residual**:

- `Unanchored` is **never pinned** — it stays `Unanchored` in the
  residual, in every position. (Consequence: `[@<a & @<b]` in
  `[@1>"a">@2]` ⇒ `[@1<b]`, not `[@1<b]>@2`.)
- `JoinPoint` **pins to its unified NID iff the containing deterministic
  path made any progress** (some branch fully or partially resolved); if
  nothing resolved, the whole path is returned unchanged. This applies to
  roots too: `@<` pins to `startNid` once a sibling resolves
  (`[@<a & @<b & @<c]>@` in `[@1>"a">@2]` ⇒ `[@1<b & @1<c]>@2`), but
  `[@<a & @<b]>@` in `[]` stays as written. A `JoinPoint` target pins to
  the NID the resolved branches agreed on; if resolved branches disagree,
  the join is unsatisfied — treat those branches as unresolved and do not
  pin.
- An excluding `JoinPoint` that cannot unify (`[a]>!{@2}` when traversal
  reaches only `@2`) leaves the path unchanged.

When intersected branches partially resolve to *different* frontier NIDs,
split the residual per frontier (`@[(a & b) /| c]` with `a→@2, b→@3` ⇒
`[@2<c & @3<c]>@1`) — this falls out of frontiers being sets.

## 6. Residual shape preferences (best-effort)

Unit-tested preferences, not load-bearing for the invariant:

- Prefer one connected deterministic path over several: `[@[a/|b]<c]` in
  `[@1>"a">@2]` ⇒ `[@2<b /@1| c]`, not `[@2<b]>@1 + [@1<c]`. Disconnection
  is allowed when unavoidable (`[@[a/|b/|c/|d]<e]` in
  `[@2>"d">@1, @1>"a">@3]` ⇒ `[@3<b/|c]>@2 + [@1<e]`).
- Prefer inverting steps (`~b`) over disconnecting; prefer the original
  step orientation otherwise.
- Loop gaps: express the gap from the forward frontier with original
  orientation, targeting the backward frontier (`@[a/|b]` in `[@1>"a">@2]`
  ⇒ `[@2<b]>@1`). When the forward pass consumed nothing, the existing
  tests express the gap from the backward frontier with inverted steps
  targeting the loop root (`@[a/|b/|c]` in `[@2>"c">@1, @3>"b">@2]` ⇒
  `[@3<~a]>@1`); both forms mint identical edges — follow the tests.

## 7. Generator

`graphResolvingPathFrom` (`Models/NormalizedPath/TestUtils.hs`) works in
two stages: `resolveAnchorNids` first maps the `NormalizedPath Anchor` to
a `NormalizedPath NID`, resolving each anchor occurrence exactly once
(`Specific` keeps its NID; root positions resolve to the start NID; every
other `Unanchored`/`JoinPoint` occurrence mints one fresh NID), then a
`traverseViaTransitions_` walk over the NID-resolved path emits an edge
per transition and a node per resolved anchor. One-NID-per-occurrence is
what makes the generated graph actually resolve the path: loop branches
close back on their pointlike's NID instead of a fresh node, and a join
point shared by several branches resolves to a single node so the
branches meet. The `edgesOf` instantiation in the §3 invariant is exactly
this function applied to the residual.

## 8. Property tests

Three properties, all generating paths via
`genDeterministicPathLiteralOnly` post-processed by `coveredPathFrom`
(exported from `TestUtils`) rather than filtered with
`suchThat`/`==>`. `coveredPathFrom startNid` makes the preconditions hold
by construction:

- a path matching `requiresUnanchoredJoin` (the §3 carve-out predicate: a
  top-level `Unanchored` target with more than one branch endpoint —
  unanchored midpoints and nested-root targets are split per branch pair
  by `leastConstrainedNormalizedPath` and need no exemption) has its
  target normalized to `JoinPoint ∅`, the spec's "append `>@`"
  normalization;
- the start NID is stripped from `JoinPoint` exclusion sets, since a root
  excluding the start NID can never resolve from it no matter which edges
  are created.

1. resolving graph ⇒ residual is `%never`;
2. removing one edge ⇒ the residual resolves in the single-edge graph;
3. the invariant directly: remove a random subset of edges, compute the
   residual, instantiate it with `graphResolvingPathFrom`, and assert
   `resolveNPath` fully resolves the original path in the combined graph
   (run purely via `runInMemoryGraphMetadataEditing`).

All three still guard on the generated graph actually resolving the path
(checked with `resolveNPath`), but as a safety net only: after
`coveredPathFrom` the guard essentially never discards. (Without the
exclusion stripping it would: `genNID`'s small 1–100 range makes a root
`JoinPoint` excluding the start NID a real occurrence, and for such paths
the correct residual is the unchanged path, not `%never`.)

## 9. Test-suite status

The expectations in `spec_pathResidualsUnresolvedBy` encode the rules
above and are the behavioral spec. The implementation is currently a stub
that returns the path unchanged (everything unresolved). Against it 26 of
the 142 tests pass — the unchanged-when-unmatched group, `%never`
preservation, and the direct-invariant property (a maximally conservative
residual satisfies it trivially); the other 116 are the rewrite's
worklist. Run with `stack test --ta '-p pathResidualsUnresolvedBy'`.

## 10. Acceptance examples

1. `[a/|b]`, `[@1>"a">@2]` ⇒ `[@2<b]`.
2. `@[a/|c/|~b]`, `[@1>"a">@2, @1>"b">@3]` ⇒ `[@2<c]>@3` — one segment
   from `{@1}` back to `{@1}`; forward consumes `a`, backward consumes
   `~b`, gap `c` between `@2` and `@3`. Requires the whole run in one
   `resolveSegment` call.
3. `@[(a & b)/|c]`, `[@1>"a">@2, @1>"b">@3]` ⇒ `[@2<c & @3<c]>@1` —
   intersection boundary; split per frontier NID.
4. `[a /@[b]| c]`, `[@1>"a">@2, @2>"c">@3]` ⇒ `@2[b]` — midpoint
   `JoinPoint` pins to `@2`; only the midpoint loop is unresolved.

## 11. Pointers

- Implementation + tests: `src/Models/NormalizedPath/UnresolvedResiduals.hs`
- Traversal core shared with resolve/create:
  `src/Models/NormalizedPath/Traversals.hs` (`traverseViaTransitions`;
  note the `isRoot` flag and `unifyFullyAnchored` in
  `Models/NormalizedPath/Anchor.hs` — the root-jump behavior lives there)
- Resolver / creator: `src/Graph/Resolve.hs`, `src/Graph/Create.hs`
- Generator + carve-out predicate: `src/Models/NormalizedPath/TestUtils.hs`
- Anchor explosion semantics: `src/Models/NormalizedPath/FullyAnchor.hs`
- Run the suite: `stack test --ta '-p pathResidualsUnresolvedBy'`
  (tasty patterns; see `docs/Testing.md`, `docs/MyPrelude.md`)
