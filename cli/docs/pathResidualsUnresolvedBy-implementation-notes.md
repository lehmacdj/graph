# `pathResidualsUnresolvedBy` — Implementation Notes

Notes from the from-scratch rewrite specified in
`pathResidualsUnresolvedBy-spec.md`. Status (re-measured 2026-06-10, after
making the property generators constructive, see §3):

- the 139 unit tests and property 1 pass deterministically (property 1
  verified at 1000 cases, 0.56s);
- the "removing one edge" property (property 2 of §8) fails roughly every
  other 100-case run because some generated instances are inherently
  ambiguous (see §1);
- property 3 (the spec's §3 invariant stated directly) is **also
  flaky** — about
  1 in 8 hundred-case runs falsifies it, i.e. some generated path/graph
  shapes still genuinely violate the invariant (a captured instance is in
  §4; an earlier claim that property 3 passes at 1000+ cases did not
  reproduce). In addition, a rare case can blow up combinatorially (one
  observed instance ran 20+ minutes); property 3 now bounds each case
  with `within 10s` so such cases fail and print themselves instead of
  hanging the suite.

## 1. The property-2 ambiguity finding

Property 2 demands: after deleting one edge `e` from a graph that resolves
the path, the residual computed against `graph − e` must itself fully
resolve in the graph containing only `e`. In other words the residual must
be *exactly* the removed edge's contribution — a much stronger requirement
than the §3 invariant (which properties 1 and 3 test and which holds).

Some generated instances make this requirement unsatisfiable: `graph − e`
can contain **two structurally symmetric single-edge residuals, both
correct under the §3 invariant, of identical size, with no information
distinguishing them** — and only one of them matches the removed edge.

### Minimal counterexample (found by shrinking)

```
path:    [!{@F,@f}<(~d & ~c)/@1["e" & ~d]|e]>@[~e & bar]
start:   @a
graph−e: [@1>"c">@a, @1>"d">@1, @1>"d">@a, @1>"e">@1,
          @1>"e">@t, @t>"bar">@t]            (t is the minted join target)
removed: @t>"e">@t                           (the target's ~e loop edge)
```

(`@F` and `@f` in the root's exclusion set are inert shrinking leftovers —
they never coincide with any node involved.)

Piece by piece, what the path demands (per the spec's §2 semantics) and
which minted edge satisfied each piece:

| path piece | role | demands | minted edge | in `graph − e`? |
|---|---|---|---|---|
| `!{@F,@f}` | root join point | unifies with start `@a` | (node `@a`) | ✓ |
| `~d` | root branch | edge `X>"d">@a`, arrives at `X` | `@1>"d">@a`, arrives `@1` | ✓ |
| `~c` | root branch | edge `Y>"c">@a`, with `Y = X` | `@1>"c">@a`, arrives `@1` | ✓ |
| `@1["e" & ~d]` | midpoint + loops | `@1>"e">@1` and `@1>"d">@1` | both | ✓ |
| `e` | mid → target step | edge `@1>"e">Z`, `Z` is the join | `@1>"e">@t` | ✓ |
| `~e` | target loop | `Z>"e">Z` | `@t>"e">@t` | ✗ **removed** |
| `bar` | target loop | `Z>"bar">Z` | `@t>"bar">@t` | ✓ |

As a picture (self-loops written next to their node):

```
   start                midpoint @1                join target @[~e & bar]
                ~c, ~d                  e
   ( @a ) ◄════════════ ( @1 ) ───────────────► ( @t )
            edges c, d   loops: d, e             loops: bar, e ✂removed
                                ▲
                                └── @1>"e">@1: minted for the midpoint's
                                    "e" loop, but it ALSO matches the
                                    mid→target step e — the coincidence
```

The trouble: in `graph − e` the final step `e` from `@1` lands on **two**
nodes — `@t` via the genuine `@1>"e">@t`, and `@1` via the self-loop
`@1>"e">@1` that was minted for the midpoint's `"e"` loop (the label `e` is
reused within the path, which is what creates the coincidence). The
join-point target `@` may pin either landing, and each landing already has
exactly one of the two target loops:

| pin join at | `~e` loop | `bar` loop | residual | mints | §3 invariant | prop 2: resolves in `{@t>"e">@t}` |
|---|---|---|---|---|---|---|
| `@1` | ✓ have `@1>"e">@1` | ✗ missing | `@1["bar"]` | `@1>"bar">@1` | ✓ holds | ✗ fails |
| `@t` | ✗ missing | ✓ have `@t>"bar">@t` | `@t[~e]` | `@t>"e">@t` | ✓ holds | ✓ passes |

Both candidates mint exactly one edge and both make the original path
resolve (I verified both directions). `graph − e` is perfectly symmetric
between them; only knowing *which* edge was removed breaks the tie, and
that information is not part of the function's input. The same shape
recurs with sequence steps instead of loops: a forward self-loop match vs.
a backward-recoverable seam, e.g. `~c/|bar` where `~c` coincidentally
matches a root-loop edge `@1H>"c">@1H` while the true seam edge
`@xho>"c">@1H` was the one removed.

### Why a tie-break cannot fix it

The local situation — "the forward pass matched something, and the
backward pass also makes progress" — also occurs in unit tests, and they
demand *forward*-keeping answers; some property-2 instances demand the
opposite answer on locally identical evidence:

| instance | local evidence | required answer | required by |
|---|---|---|---|
| `[a/|b]>@4` in `[@1>"a">@2, @3>"b">@4]` | fwd: `a` matched, at `@2`; bwd: `b` matched, from `@3` | keep the fwd match, re-mint the suffix: `[@2<b]>@4`, *not* `[a]>@3` | unit test ("unchanged when unmatched") |
| `@[a/|d/|~c/|~b]` in `[@1>"a">@2, @2>"d">@2, @1>"b">@2, @2>"c">@3]` | fwd: `d` matched by a *genuine* self-loop `@2>"d">@2`; bwd: `~b` progresses | keep the self-loop fwd match: `[@2<~c]>@2` | unit test ("don't meet up in middle") |
| property-2 instance (`~c/|bar` shape above) | fwd: `~c` matched by a *coincidental* self-loop `@1H>"c">@1H`; bwd: seam recoverable | trust the bwd seam (the removed edge was the seam `@xho>"c">@1H`) | property 2 |

Rows 2 and 3 present the matcher with indistinguishable local evidence but
need opposite choices. I implemented the one discriminator that is
consistent with all unit tests (prefer the backward seam only when every
forward match was frontier-preserving *and* the backward pass was fully
rolled back, see §2.8), which fixed the most common collision class, but
the residual ambiguity above survives any deterministic rule.

### Root cause and suggestions

The coincidences come from `genTransitionLabel`'s 11-word pool: reused
labels inside one path make unrelated edges (especially self-loops minted
for pointlike loops) match other occurrences' steps. Options:

1. Weaken property 2 to the §3 invariant form: instead of requiring the
   residual to resolve in `{e}`, require `resolvesIn (graphMinusEdge <>
   instantiate residual)` — this is property 3 restricted to singleton
   removals and is unambiguous. (The current property 3 already covers
   this.)
2. Make the generator label each transition occurrence uniquely (or
   precondition property 2 on paths whose labels are distinct), removing
   the collision source.
3. Accept the flake rate (~1 per few hundred cases ⇒ a 100-case run fails
   occasionally) and fix the QuickCheck seed.

## 2. Ambiguous implementation decisions

Places where neither the spec nor the tests determine the behavior, and
what I chose. All choices are exercised by the property tests, so changing
them later should be guarded by those.

Overview — each numbered item below in one line, with a concrete example
where the unit suite has one (examples marked *prop* are only exercised by
the property tests):

| # | when it bites | rule chosen | example (graph ⊢ path ⇒ residual) |
|---|---|---|---|
| 1 | a loop consumed zero steps | deconstruct at root positions, keep attached at midpoint/target positions | root: `[@1>"b">@3, @2>"a">@3] ⊢ [@2[c]<a & @1<b] ⇒ [@2<c]>@2`; midpoint: `[@1>"a">@2, @2>"x">@3] ⊢ [a /@[x]| b] ⇒ [@2[x]<b]` |
| 2 | one gap, several frontier NIDs | forward fan-out ⇒ intersected roots of *one* path; backward multi-source ⇒ separate *union* members | fwd: `[@1>"a">@2, @1>"a">@3] ⊢ [a/|b] ⇒ [@2<b & @3<b]`; bwd: `[@2>"c">@1, @3>"c">@1] ⊢ @[a/|b/|c] ⇒ [@2<~b /| ~a]>@1 + [@3<~b /| ~a]>@1` |
| 3 | root JoinPoint excludes the start NID | drop that root's branches (creating edges can't help); unchanged if *all* roots are like that | *prop* |
| 4 | nested rooted root with Unanchored target | treat as `JoinPoint ∅` internally: branches must meet, junction may pin | *prop* (property 3 fails otherwise) |
| 5 | join target where no resolved branches agree | pin by reach, then by minimal residual weight; prefer candidates whose target loops resolve | basic agree-rule: `[@1>"a">@2] ⊢ [@<a & @<b & @<c]>@ ⇒ [@1<b & @1<c]>@2`; extensions: *prop* |
| 6 | midpoint reached at several NIDs | all fully-resolving arrivals contribute; else first arrival with progress | *prop* |
| 7 | expressing a gap backward (`[@3<~a]>@1`) | only for `EndAt` ends with a pinnable start; Unanchored-rooted gaps always forward | `[@2>"c">@1, @3>"b">@2] ⊢ @[a/|b/|c] ⇒ [@3<~a]>@1` (loop root pins); but `[@1>"a">@2] ⊢ [a & b] ⇒ [b]` (Unanchored root, never pinned) |
| 8 | forward and backward passes overlap | forward greedy, except: all fwd matches frontier-preserving ∧ bwd fully rolled back ∧ bwd-only makes progress ⇒ backward seam wins | greedy: `[@1>"a">@2, @3>"b">@4] ⊢ [a/|b]>@4 ⇒ [@2<b]>@4`; exception: *prop* |
| 9 | final run of a branch ends open/at a join | backward pass seeded with *every* graph node (recovers seams like `[a]>@m` from `m>"b">f`) | *prop* (property 2) |
| 10 | gap runs into a JoinPoint midpoint / intersection | match the tail from every node; close at the seam if the whole tail resolves, else keep one chained gap | *prop* |
| 11 | OMap/OSet insertion order in the residual | consumed gaps before re-rooted gaps; root maps in declaration order; fan-outs ascending by NID | (ordering only) |

1. **Zero-consumption loop gaps are position-dependent.** A loop that
   consumed nothing deconstructs at *root* positions (`[@2<c]>@2`, test
   line ~477) but stays attached to the pointlike at *midpoint* positions
   (`@2[b]`, `[@2[b]<c]`, lines ~147/162). The spec describes only the
   deconstruction; the position split is inferred from the tests and
   generalized to unseen combinations (e.g. target loops behave like
   midpoints).

2. **Conjunction vs. union for multi-NID gaps.** A forward fan-out gap
   becomes intersected root branches of one deterministic path
   (`[@2<b & @3<b]`, line ~212) while a multi-NID *backward* gap becomes
   separate union members (line ~251), even though the shapes are
   isomorphic. I followed the tests and additionally never merge backward
   alternatives with any other gap (the `alone` flag), since merging
   alternatives of one gap would wrongly conjoin them.

3. **Roots that can never unify are dropped, not kept.** `resolveNPath`
   resolves each root of a multi-root path independently (its result is a
   union over roots), so `resolvesIn` is true even when a root is a
   `JoinPoint` excluding the start NID. Since creating edges can never
   help such a root, its branches are omitted from the residual; a path
   consisting *only* of such roots still returns unchanged (no progress),
   which is the §8 behavior. The spec's "unchanged" wording only
   contemplates the single-root case.

4. **Nested roots with Unanchored targets are treated as join points.**
   The §3 carve-out exempts only the *top-level* target. A nested rooted
   root with an Unanchored target must make its branches meet for the
   invariant to hold, so internally it is rewritten to `JoinPoint ∅`, its
   junction may be pinned, and outer branches resume from the pinned NID.
   Justified by §5's "concrete resume points are required structure, not
   anchor pinning"; without this property 3 fails.

5. **Join pinning beyond §5's rule.** §5 pins a JoinPoint target to "the
   NID the resolved branches agreed on". Extensions needed by the
   properties:
   - *reach pinning*: a branch that gapped early but whose suffix fully
     resolves to the target reports that terminal ("reach"); when no
     branch fully lands, the join pins on agreeing reaches;
   - *loop/cost search*: when nothing lands or reaches, every admissible
     node is evaluated as the join (re-aiming all branches at it) and the
     one minimizing total residual weight wins;
   - among meet candidates, prefer one where the target's own loops
     resolve (otherwise property 1 fails when branch fan-out makes the
     meet ambiguous).
   The spec's disagreement rule (resolved branches disagree ⇒ demote, no
   pin) is preserved and takes precedence.

6. **Midpoints with several arrivals.** Stations try every arrival NID;
   all fully-resolving arrivals contribute (landings unioned), otherwise
   the first arrival showing progress is used. Explicit `/@|` join
   midpoints following an intersection get the same candidate search as
   target joins when the fast path (all sub-branches agree) fails.
   Unanchored midpoints use junction pinning: a failed sub-branch's gap
   ends at the junction a sibling resolved through; if no continuation
   resolved anywhere, the continuation is duplicated per pair (exploded
   semantics).

7. **Backward expression is restricted.** Backward-expressed gaps
   (`[@3<~a]>@1`) are produced only for `EndAt` contexts with a pinnable
   start (loops, pinned roots); gaps starting at Unanchored roots are
   always forward-expressed since their root must never pin.

8. **Forward greedy matching, with one exception.** Linear runs always
   consume the longest forward prefix (this is what the unit tests
   require, see §1), except: if every consumed forward step left the
   frontier unchanged (only self-loop matches) *and* the backward pass
   from the end was fully rolled back *and* re-running backward with no
   forward consumption makes real progress, the backward seam wins. This
   discriminates coincidental self-loop matches from genuine ones (lines
   297–312) in all tested cases.

9. **Open-backward matching.** §4.1 says an open backward frontier
   "accepts any landing" but leaves the mode-On behavior open. For the
   *final* run of a branch, an open/join end gets a backward pass seeded
   with every node of the graph; this is what lets `[a /| b]` recover the
   seam `[a]>@m` when only `m>"b">f` survives (required by property 2).
   Non-final runs do not (their boundary is recovered structurally, see
   10).

10. **Recovery behind unknown boundaries.** When a run gaps into a
    JoinPoint midpoint or an intersection (whose NID is unknowable), the
    remaining tail is matched from every graph node ("tail candidates");
    if the whole tail resolves somewhere, the gap closes at that seam,
    otherwise the structure is kept as one connected gap (chaining), which
    is safe for the invariant but not minimal. Structures inside a chained
    gap are not re-attempted.

11. **Residual assembly order.** OMap/OSet equality is insertion-order
    sensitive, so the tests pin orderings: gaps that consumed something
    precede gaps re-rooted at their start position; root maps follow
    branch declaration order; forward fan-outs are in ascending NID order.

## 3. Constructive property-test generation (2026-06-10)

The property tests originally *filtered* generated data — `suchThat (not .
requiresUnanchoredJoin)` on paths, and `==>` on "the generated graph
resolves the path". Both filters are now replaced by a constructive
rewrite, `coveredPathFrom` (in `TestUtils`), applied to every generated
path:

- a top-level Unanchored target with several terminal branches (the
  spec's §3 carve-out) is normalized to `JoinPoint ∅` — the "append `>@`"
  normalization, and exactly how `leastConstrainedNormalizedPath` already
  interprets such a target at resolution time. These paths were previously
  discarded outright; now they are exercised in normalized form;
- the start NID is stripped from every JoinPoint exclusion set, since a
  root excluding the start NID can never resolve from it no matter which
  edges are created (with `genNID`'s 1–100 range this was a real
  occurrence).

The `resolvesIn` guards (`==>`) remain as safety nets but essentially
never discard now. The properties also print the generated `path`,
`start`, and `graph` via `counterexample` on failure — previously only the
removed edges and the residual were shown, which made failures
undiagnosable after the fact.

Measured after the change: property 1 passes 1000 cases in 0.56s;
properties 2/3 run 100 cases in ~0.1–0.35s. Wall-clock speed was never
dominated by the filters — the dominant risk is the rare combinatorial
blowup case bounded by `within` (see §4).

## 4. Open issue: property 3 genuinely falsifies (pre-existing)

Property 3 (the spec's §3 invariant) falsifies roughly once per ~800
cases. A
captured instance (printed by the new counterexample annotations; there is
no seed replay, so this is the artifact to debug from):

```
path:    [@H[~"z"]<"baz"/@|("c" & (~"e" & "baz" & ~"baz")/|(~"bar" & ~"baz"))
          & @H[~"z"]<("baz" & ~"e"/@|(~"z" & ~"d"))/|(~"baz" & ~"c" & "e")/@e|("y" & ~"b")
          & @H[~"z"]<("b" & ~"e" & "a")/@X|~"d"]>@[~"e" & "d"]
start:   @1X
removed: [@H>"b">@X, @H>"baz">@eTea…, @H>"z">@H, @e>"c">@dIDB…,
          @e>"y">@mI2G…, @CYQn…>"baz">@eTea…, @CYQn…>"e">@eTea…,
          @dIDB…>"d">@MdWG…, @dIDB…>"z">@MdWG…, @eTea…>"baz">@CYQn…,
          @mI2G…>"b">@e, @mI2G…>"bar">@CYQn…, @mI2G…>"d">@mI2G…,
          @mI2G…>"e">@mI2G…]
(full graph and residual in the test output; rerun
`stack test --ta '-p creating'` in a loop to reproduce a fresh instance)
```

Note the failing path's target `@[~"e" & "d"]` is a JoinPoint-with-loops
target — a shape the old, filtering generator also produced, so this class
of failure predates the constructive generation change (the baseline
suite, before any of these changes, falsified property 3 in roughly half
of its 100-case runs). The join-pinning extensions of §2.5/§2.6 are the
prime suspect: with three intersected root branches, several explicit `/@|`
join midpoints, and a join target whose loops must also resolve, the
candidate search has many ways to pin a meet that the instantiated
residual then fails to realize.

## 5. Where things live

- Implementation: `src/Models/NormalizedPath/UnresolvedResiduals.hs`
  (layer 0: `resolveSegment`; layer 1: `resolveBranch` for branch
  structure, `residualRooted`/`resolveRoot` for path structure).
- Constructive generator post-processing: `coveredPathFrom` in
  `src/Models/NormalizedPath/TestUtils.hs`.
- Run the suite: `stack test --ta '-p pathResidualsUnresolvedBy'`.
