# `pathResidualsUnresolvedBy` — Rewrite Design Spec

This document specifies a from-scratch rewrite of `pathResidualsUnresolvedBy`
(currently ~1865 lines in `Models/NormalizedPath/UnresolvedResiduals.hs`).
It captures the design decisions made during review and is meant to be
implemented from directly.

> Status: the existing implementation is being replaced. Its remaining test
> failures are documented in the appendix as guidance, not as a fix list.

---

## 1. Purpose and core invariant

`pathResidualsUnresolvedBy startNid graph path` computes the **residual**:
the portion of `path` not already satisfied by `graph` when starting from
`startNid`.

**Core invariant.** Instantiating the residual's edges and adding them to
the graph must make the original path fully resolvable:

```
let r = pathResidualsUnresolvedBy startNid graph path
in  pathResidualsUnresolvedBy startNid (graph <> edgesOf r) path == %never
```

**How `edgesOf` works.** The residual is a `NormalizedPath Anchor` and its
edges are obtained with `graphResolvingPathFrom`/`createNPath` semantics:
`Specific` anchors map to their NID, and `Unanchored`/`JoinPoint` anchors
**mint fresh nodes**. So a residual that contains unanchored components is
legitimate — creating it adds fresh nodes. This couples the residual
definition to the test-graph generator, which is why fixing the generator
(§7) is part of this work.

<!-- COMMENT: I'm not sure how coupled the generator actually is. Because unanchored/joinpoint nodes also match any NID, the specific choice of random NID shouldn't matter -->

**Minimality is best-effort, not a correctness condition.** Correctness =
the invariant above. The connectedness/orientation *preferences* (§5.5) are
strong goals encoded as unit-test expectations, not absolute laws. The
property tests are therefore the real specification.

---

## 2. Design decisions (summary)

1. **Two-layer architecture** (§4): a small bidirectional graph-touching
   primitive `resolveSegment`, plus a pure structural layer that recurses
   the grammar and builds the residual AST.
2. **Opportunistic reuse**: build the machinery for the residual; fold
   `resolveNPath` onto it (clean win); fold `createNPath` only if it falls
   out naturally (it's the degenerate case).
3. **Generator fix in scope** (§7): `graphResolvingPathFrom` must close
   loops so the property tests can pass end-to-end.
4. **Diff-target rule**: when branches of an intersection reach different
   NIDs, **split per target if the target is unanchored**, but **unify to a
   single anchor if the target is anchored** (disagreement on an anchored
   target ⇒ no concretization).
5. **Root anchors are not concretized**; targets/midpoints are (§5.5).
6. **Minimality best-effort**; preferences placed wherever simpler (§5.5).
7. **Two areas are derived from tests rather than specified up front**:
   the backward-matching orientation of `~t` steps (§4.1), and the
   multi-unanchored-sequence semantics (§6).

---

## 3. The path grammar

```
NormalizedPath = Set DeterministicPath              -- union (+)
DeterministicPath = Pointlike | Rooted
Pointlike   = { anchor, loops :: Set Branch }       -- a node + cycles back to it
Rooted      = { rootBranches :: Map (root::DeterministicPath) (Set Branch),
                target :: Pointlike }
Branch      = Single Direction
            | Sequence (Set Branch) Pointlike (Set Branch)   -- bs1 → midpoint → bs2
Direction   = Outgoing t | Incoming t               -- written `t` and `~t`
Anchor      = Unanchored | JoinPoint {excluding} | Specific NID
```

The grammar is mutually recursive (`Branch ↔ Sequence ↔ Pointlike ↔
Branch`). That recursion is intrinsic; no architecture removes it.

---

## 4. Architecture

Two layers. **Layer 0** (`resolveSegment`) is the *only* code that touches
the graph. **Layer 1** is a pure structural recursion over the grammar.

### 4.1 Layer 0: `resolveSegment`

A bidirectional matcher for one **linear run** of directed steps, between a
forward frontier and a backward frontier.

```haskell
data StepInterp m = StepInterp
  { stepForward  :: NID -> Direction -> m (Set NID)  -- match existing edges, or mint one
  , stepBackward :: NID -> Direction -> m (Set NID)  -- predecessors via existing edges
  }

data BackwardMode = Off | On

data SegResult
  = SegResolved (Set NID)                    -- forward & backward met; meeting NIDs
  | SegGap (Set NID) [Direction] (Set NID)   -- fwd reached, unmatched middle, bwd reached

resolveSegment
  :: Monad m
  => StepInterp m -> BackwardMode
  -> Set NID            -- forward frontier (start)
  -> [Direction]        -- the linear run of steps
  -> Set NID            -- backward frontier (target); "open" ⇒ unconstrained
  -> m SegResult
```

**Semantics.**
- *Forward pass*: from the forward frontier, fold `stepForward` over the
  longest **prefix** of steps that keeps the frontier non-empty. Call the
  resulting set `Ff` and the consumed count `k`.
- *Backward pass* (only when `BackwardMode = On`): from the backward
  frontier, fold `stepBackward` over the longest **suffix** of the remaining
  steps that keeps the frontier non-empty. Call the resulting set `Bf`.
- *Result*: if the whole run was consumed and `Ff` meets the backward
  frontier → `SegResolved`. Otherwise `SegGap Ff middle Bf`, where `middle`
  is the steps neither pass consumed.

**Backward mode toggle.**
- `Off` (resolve/create): no backward pass. Success = forward reached the
  (possibly open) backward frontier. For an **open** backward frontier (a
  rooted branch with an unanchored target) any forward landing succeeds; for
  a **loop** (backward frontier `{N}`) success = forward returned to `N`.
  This reproduces today's `filter (== nid')` loop semantics exactly.
- `On` (residual): runs the backward pass to shrink the gap.

**The subtle part — backward matching of `~t` (derive from tests).** For an
outgoing step `t` (edge `pred-t->end`) the backward predecessor is clearly
`getIncomingSources end t`. For an **incoming** step `~t` the strict
directional inverse and the behavior the unit tests require diverge: the
current code matches `~t` against t-edges *incident to* the endpoint (it
tries both `getIncomingSources` and `getOutgoingTargets`). The worked
example `@[a/|c/|~b]` in `[@1-a->@2, @3-b->@1] → [@2<c]>@3` only works under
that permissive matching. **Do not pin a from-first-principles formula
here**; define `stepBackward` so it reproduces the *edge cases → loop paths
handling from both ends* unit tests and satisfies the core invariant.

**The three operations as `StepInterp` + mode:**

| | `stepForward` | `stepBackward` | mode | uses result |
|---|---|---|---|---|
| `resolveNPath` | match outgoing/incoming edges | unused | `Off` | build `ResolvedPath` + missing nodes; `SegGap` ⇒ branch didn't match |
| `createNPath` | mint an edge, return target | unused | `Off` | none — side effects only; every step resolves by construction |
| residual | match edges | match reverse edges (above) | `On` | keep `SegGap`s → assemble residual AST |

### 4.2 Layer 1: structural recursion

Pure (no graph access). Walks the grammar, calling `resolveSegment` on
linear runs and recursing at boundaries.

**Linear runs vs. boundaries.** A *linear run* is a chain of single
directions whose intervening midpoints are unanchored and loop-free and
whose `bs1`/`bs2` are singletons — e.g. `a/|c/|~b` flattens to `[a,c,~b]`.
The whole run goes to `resolveSegment` in **one** call (essential —
bidirectional meeting needs the entire run at once). A *boundary* is
anything that pins or branches the structure:
- an intersection (`&`, i.e. `|bs1|>1` or `|bs2|>1`),
- an anchored midpoint (`/@2|`),
- a midpoint with loops (`/@[..]|`).

Boundaries split a run into segments and force recursion.

**Per grammar node:**

- **Union** (`NormalizedPath`): residual of each `DeterministicPath`
  independently; collect the non-`%never` results into a union.
- **Pointlike** `{anchor, loops}`: resolve `anchor`→`N` (jump for `Specific`
  root, unify for unanchored/joinpoint). No loops ⇒ the node reference is
  fulfilled by reaching `N` ⇒ `%never` (the only "unchanged" case is an
  anchor that genuinely can't be reached, e.g. an excluding `JoinPoint`).
  Each loop is a **cycle**: call down with forward frontier `{N}` *and*
  backward frontier `{N}`. A `SegGap (f, steps, b)` becomes a
  deconstructed-loop residual connecting `f` to `b`.
- **Rooted** `{rootBranches, target}`: for each `(root, branchSet)`, recurse
  to resolve `root`→`R` (root resolution **delegates to the Pointlike/DP
  behavior** — a root is itself a `DeterministicPath`), then resolve the
  intersected `branchSet` from `{R}` toward the target frontier. Apply target
  concretization (§5.5).
- **Branch**: `Single`/linear ⇒ flatten + `resolveSegment`. `Sequence` with
  a boundary ⇒ resolve `bs1` (intersection) from the current frontier to the
  midpoint frontier `M` (constrained if anchored; resolve `M`'s loops
  first), then resolve `bs2` from `M`. Intersections resolve each member and
  **unify endpoints** per the diff-target rule (§5.4).

---

## 5. Behavior spec by path type

### 5.1 Empty / unions
- `%never` ⇒ `%never`.
- `path1 + path2` ⇒ residual of each independently, unioned, fully-resolved
  paths dropped.

### 5.2 Pointlike (anchor + optional loops)
- Resolve the anchor (`Specific` root jumps; unanchored/joinpoint unify with
  `startNid`, subject to exclusions).
- No loops ⇒ `%never` (jumping to the node fulfils it). Only an
  unreachable anchor (excluding `JoinPoint`) returns unchanged.
- With loops ⇒ resolve each loop (§5.6). All resolve ⇒ `%never`. Some
  unresolved ⇒ residual of only the unresolved loops, anchored to the
  concrete NID if any loop resolved (else preserve the original anchor).

### 5.3 Rooted (`[root<branches]>target`)
- Each `(root, branchSet)` resolves independently; root resolution delegates
  to the Pointlike behavior (jump/unify + resolve root loops), then follow
  branches from the resolved root.
- Intersection: **all** branches must succeed for full resolution.
- Partial intersection: residual contains only the failed branches.
- **Backward / partial-from-the-back resolution** applies here too: a
  branch that can't complete forward from the root but whose edges exist
  pointing into the (concretized) target is matched in reverse, with the
  gap as residual. This is the same `resolveSegment` bidirectional behavior,
  not a special case.

### 5.4 Diff-target rule (intersection endpoint unification)
When intersected branches reach different NIDs:
- **Unanchored target** ⇒ **split per target**: emit a residual branch per
  distinct reached NID. E.g. `@[(a&b)/|c]` with `a→@2, b→@3` ⇒
  `[@2<c & @3<c]>@1`.
- **Anchored target** ⇒ the landings must **unify** to that single anchor;
  disagreement ⇒ no concretization (target stays as written / unchanged).

The same "split if unanchored" logic applies to multiple backward sources
(e.g. two `c`-edges into a node ⇒ a union of two residuals). This falls out
of frontiers being **sets**; no special code.

### 5.5 Anchor concretization rules
- **Do not concretize the root anchor.** A root `@<` / `@N<` *starts a path
  from* that location and carries no constraint to pin — leave it as
  written. The same anchor *is* concretized when it appears later as a
  **midpoint or destination** (there it asserts where traversal arrived; cf.
  the `isRoot` distinction in §8).
- Concretize the **target** when any branch fully resolves to a destination.
- Preserve original anchoring (`Unanchored`/`JoinPoint`) when nothing
  resolves.
- Minimize concretization: prefer `[@<b]>@3` over `[@1<b]>@3`.
- **Connectedness/orientation preferences** (best-effort): prefer
  `[@2<b /@1| c]` over `[@2<b]>@1 + [@1<c]`; prefer inverting edges (`~b`)
  over disconnecting deterministic paths; prefer the original edge
  orientation. Placed wherever simpler in the implementation (likely a small
  pure post-normalization pass; not load-bearing for correctness).

### 5.6 Loop and sequence resolution
A loop is a `resolveSegment` call with backward frontier `{N}`. A sequence
splits at its midpoint:
- `bs1` resolves from the current frontier to the midpoint `M` (anchored ⇒
  constrained; resolve `M`'s loops before continuing).
- `bs2` resolves from `M` toward the target.

**Forward/backward are the two ends of one segment, not two passes.** The
deterministic recipe: forward-chain the longest prefix from the start;
backward-chain the longest suffix from the return target; the leftover
middle is the gap (a deterministic path from the forward endpoint to the
backward endpoint, inverting transitions consumed by the backward pass).
Fully resolved ⇔ the two ends meet with no gap.

**Partial midpoint loops** are handled identically: a midpoint loop that
partially resolves advances a forward endpoint, and the unresolved suffix
becomes a deconstructed loop from that endpoint back to the midpoint, after
which `bs2` continues. E.g. `[a /@[x/|y]| b]` in `[@1-a->@2, @2-x->@3]` ⇒
`[@3<y /@2| b]`.

---

## 6. Deliberately unspecified: multi-unanchored sequences

The semantics of sequences with **multiple unanchored components** (e.g.
`[a & b]` where only `a` resolves, and longer unanchored chains) is left
open on purpose. Decision: `pathResidualsUnresolvedBy` **preserves original
anchoring**, and callers like `mk` append `/@` to enforce anchoring (which
still requires concretizing the appended `/@`). The exact residual shapes
for complex unanchored cases will be **pinned by the property tests against
a working implementation**, not specified up front. A doc comment on
`pathResidualsUnresolvedBy` must call out that unanchored behavior is
dubious and can produce inconsistent results.

---

## 7. Generator fix: `graphResolvingPathFrom` loop closure

`graphResolvingPathFrom` currently emits a *fresh*-NID target for a loop's
closing step (e.g. `@[~a]` produces `freshNid >"a"> startNid` instead of the
self-loop `startNid >"a"> startNid`), so the "resolving" graph doesn't
actually resolve the loop. The rewrite must **close loops**: a loop branch's
final transition must target the loop's root NID, not a fresh NID. Without
this the property tests (§Appendix) cannot pass regardless of the residual
logic.

If `graphResolvingPathFrom` is re-expressed via the §8 "resolve to
`NormalizedPath NID` then `createNPath`" approach, the loop-closure fix must
live in the anchor-resolution pass (the loop-closing anchor resolves back to
the loop root NID).

---

## 8. How `resolveNPath` / `createNPath` fold on (opportunistic)

The override behavior that distinguishes a **root** `@N` (jump to `N`) from a
**midpoint/destination** `@N` (assert we arrived at `N`) lives in the
*unification function*, not in any one caller:

- `resolveNPath`'s `unifyFullyAnchored` distinguishes `isRoot`: `FSpecific`
  jumps at a root, asserts elsewhere.
- `createNPath` uses `\_ _ _ -> Just ()` (`b = ()`); the override question
  is moot — NIDs come straight from the literal anchors.
- `graphResolvingPathFrom`'s `unifyAnchor` ignores `isRoot` (always jumps for
  `Specific`), correct for graph generation.

In the new design:
- `resolveNPath` = Layer 1 + `resolveSegment` with `BackwardMode Off`, a
  read-matching `StepInterp`, an open backward frontier for unanchored
  targets / `{N}` for loops, interpreting `SegGap` as "didn't match." **Clean
  win** — re-express it on the shared machinery.
- `createNPath` = the degenerate case (concrete input, every step resolves,
  no frontier/gap exercised). What it actually shares is the *structural
  walk*, not `resolveSegment`. **Fold on only if it falls out naturally.**

---

## 9. Why this is smaller than the current implementation

Not because the *problem* is easier (the grammar recursion, intersection
unification, and preferences are intrinsic), but because today's code
duplicates forward/backward/incoming chaining across loop-, root-, and
sequence-contexts. Consolidating onto one bidirectional primitive deletes
that duplication:

| Today | Becomes |
|---|---|
| `chainForward` + `chainBackward` + `loopResolve` | one `resolveSegment` |
| `checkIncomingLoopCompletion`, `checkIncomingLoopForRoot`, `tryIncomingFallback` | nothing — backward is *inside* `resolveSegment`, not a fallback |
| `processLoopCore/Branch/Branches`, `followSequence` | "resolve a (maybe intersected) branch set from F toward B" |
| `LoopResult`, `BranchOutcome`, `LoopResolution`, `LoopOutcome`, `SequenceFollowResult` | one `SegResult` |

Forward / backward / incoming-fallback / bidirectional "from both ends" stop
being four code paths and become one segment resolver distinguished only by
its frontiers. Everything outside `resolveSegment` is pure and
unit-testable without a graph.

---

## 10. Worked examples (acceptance checks)

1. `[a/|b]`, `[@1-a->@2]` ⇒ `[@2<b]` — forward `a` lands @2, `b` stuck.
2. `@[a/|c/|~b]`, `[@1-a->@2, @3-b->@1]` ⇒ `[@2<c]>@3` — one segment from
   `{@1}` back to `{@1}`; forward consumes `a`, backward consumes `~b`
   (edge `@3-b->@1` fixes the backward endpoint at @3), gap `c` between @2
   and @3. (Requires the whole run in one `resolveSegment`.)
3. `[(a&b)/|c]`, `[@1-a->@2, @1-b->@3]` ⇒ `[@2<c & @3<c]>@1` — `bs1` is a
   boundary; unanchored midpoint ⇒ split per landing.
4. `[a /@[b]| c]`, `[@1-a->@2, @2-x->@3]` ⇒ midpoint loop `b` at @2 fails
   ⇒ the unresolved midpoint loop surfaces in the residual.

---

## Appendix: current implementation status (guidance only)

130/137 tests pass. The 7 failures, for reference during the rewrite:

- **Complex roots (4).** Three are implementation bugs in target
  concretization (§5.4/§5.5): the residual must concretize the target to the
  NID a resolved branch proved, and (per §1.A's resolved semantics) a root
  `@N` starts a fresh path from `N`. One was a **wrong test expectation**,
  already corrected to `[@2<c]>@2` (just the unresolved loop — adding
  `@2>"c">@2` alone satisfies the invariant); it still fails pending the
  concretization fix.
  - **One more test expectation is wrong** and should be revised during the
    rewrite: `[@2<a & @<b]>@` in `[@1-a->@2]` currently expects `[@2<b]>@3`,
    which is underivable (`@2<a` starts at @2 with no `a`-edge; `@<b` starts
    at @1 with no `b`-edge; nothing reaches @3). Replace it with the
    invariant-derived expectation.
- **Property tests (2).** They no longer crash with `impureNonNull`; they
  falsify on loop-only pointlikes (`@[~a]`, `@[~baz & baz]`) because the
  generator doesn't close loops (§7). Fix the generator and they should pass.
