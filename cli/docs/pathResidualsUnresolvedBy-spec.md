# `pathResidualsUnresolvedBy` — Rewrite Spec (v2)

Spec for `pathResidualsUnresolvedBy`
(`src/Models/NormalizedPath/UnresolvedResiduals.hs`), revised 2026-07-11
after a design review that settled every previously-ambiguous decision
(the decision record is in
`pathResidualsUnresolvedBy-implementation-notes.md`). The unit tests and
property tests encode the target behavior (§8–§10); this document is
self-contained and intended to be implemented from directly.

```haskell
pathResidualsUnresolvedBy ::
  NID -> Graph Text () -> NormalizedPath Anchor ->
  NonEmpty (NormalizedPath Anchor)
```

It computes the **residual alternatives**: each element of the result is a
complete description of edges whose creation makes `path` resolvable from
`startNid` (§3). The result is a *meta-union*: **any one** alternative
suffices; the caller (the future `mk` command) picks one — the preferred
alternative is first. Within one alternative, the ordinary `NormalizedPath`
union means "create every member".

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

**Multi-root paths.** The keys of `rootBranches` ("roots") are locations a
`Rooted` path starts from independently, but resolution is conjunctive
across them: **every** root's branch set must resolve, and all landings
must unify at the shared target. (`resolveNPath` historically unioned over
roots; that was a bug, fixed 2026-07-11 with regression tests in
`Graph/ResolveSpec.hs`.)

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

Self-loop edges match steps like any other edge in these formulas — the
query language is unchanged. The residual computation merely *prefers* not
to depend on them (§5.1); it must fall back to self-loop matches whenever
they are the only source of progress or full resolution, or property 1
breaks.

## 3. Alternatives and the core invariant

```
rs = pathResidualsUnresolvedBy startNid graph path        -- NonEmpty
∀ r ∈ rs:
  g' = graph <> edgesOf startNid r
  ⇒ resolveNPath startNid (leastConstrainedNormalizedPath path)
      fully resolves in g'
```

where `edgesOf startNid r` instantiates one alternative exactly as
`graphResolvingPathFrom` does (§7): `Specific` anchors map to their NID;
`Unanchored`/`JoinPoint` root anchors unify with `startNid`; other
`Unanchored`/`JoinPoint` occurrences mint fresh NIDs, **one NID per anchor
occurrence** (shared across the branches that reference it), and a loop's
closing transition targets the loop's own root.

**Every alternative is a complete repair on its own.** Alternatives are
never partial pieces that jointly repair the path; they are *rival*
complete repairs that differ in which existing structure they reuse (and
therefore which edges they mint). They arise wherever the graph — not the
path — is ambiguous about what is missing:

- a backward pass recovering **several seams** (either seam repairs the
  path; one alternative per seam);
- a forward **fan-out** followed by a gap (the continuation is missing
  from one of the fan-out nodes; one alternative per node). Note the
  contrast with an *intersection* whose branches land on different nodes:
  intersected branches are all required, so their residuals stay
  conjoined in one alternative (§10 example 3);
- a midpoint station with **several partial arrivals** and no
  fully-resolving one (one alternative per progressing arrival);
- an unpinned join versus a **reach-pinned** join (§5.2).

The result is deduplicated. Order is deterministic: the preferred
alternative first (§6 preferences: more reuse, forward orientation);
alternatives that differ only in a graph-derived NID are in ascending NID
order.

Corollaries:

- `rs == [%never]` ⇔ `path` already fully resolves in `graph` (all union
  members, all roots). `%never` never appears among the alternatives
  otherwise, and no other alternative accompanies it.
- Idempotence, per alternative:
  `pathResidualsUnresolvedBy startNid g' path == [%never]`.

**Carve-out (per the §5 anchor rules).** The invariant is *not* required
to hold for paths containing an intersection (a branch set with more than
one member) whose target pointlike is `Unanchored`: `resolveNPath` (via
`leastConstrainedNormalizedPath`) treats a top-level unanchored target as
`FJoinPoint ∅`, i.e. the branches must meet, but the residual never pins
unanchored targets, so the instantiated residual may not make them meet.
Callers that want join semantics must write `>@` (or append it, as `mk`
will). The doc comment on `pathResidualsUnresolvedBy` must state that
behavior for unanchored intersections is dubious and may be inconsistent.
Property-test path generators normalize such paths by appending `>@`
(`coveredPathFrom`, §8).

**Unsatisfiable paths are returned unchanged.** A deterministic path that
no edge creation can make resolve — e.g. a multi-root path one of whose
roots is a `JoinPoint` excluding `startNid` — is returned unchanged (as
the sole content of a singleton alternative), mirroring the
no-progress rule of §5. Its branches are *not* silently dropped.

**Minimality is best-effort.** Correctness = the invariant, for every
alternative. The shape preferences (§6) are encoded as unit-test
expectations, not laws.

## 4. Architecture

Two layers. Layer 0 is the only code that touches the graph.

### 4.1 Layer 0: `resolveSegment`

A bidirectional matcher for one linear run of directed steps between a
forward and a backward frontier. Frontiers are **sets** — multiple matches
fan out, and multiple backward sources become *alternatives* (§3).

```haskell
data SegResult
  = SegResolved (Set NID)                    -- frontiers met; meeting NIDs
  | SegGap (Set NID) [Direction] (Set NID)   -- fwd reached, gap, bwd reached

resolveSegment
  :: Graph Text ()
  -> Bool          -- openBwd: this run ends its branch (§4.1.2)
  -> Set NID       -- forward frontier
  -> [Direction]   -- the linear run
  -> EndCtx        -- open / join (with exclusions) / at specific NIDs
  -> SegResult     -- (or a small set of candidate SegGaps, see below)
```

#### 4.1.1 Split selection: maximize consumption

Among the splits (k steps consumed forward, j steps consumed backward,
k + j ≤ length − 1 when the frontiers are disjoint — the gap between
disjoint frontiers must keep at least one step, so a backward step that
would empty it is rolled back), select by:

1. **maximize k + j** (prefer matching more over matching less);
2. ties: prefer the split whose progress does not depend on a self-loop
   edge matching a non-loop step (§5.1);
3. remaining ties: prefer the larger k (forward wins).

`SegResolved` iff every step was consumed *and* the forward landing set
meets the end context; a forward pass that consumes all steps but misses
the backward frontier is **not** progress — the run failed as a whole
(e.g. `@[a]` in `[@1>"a">@2]` is unchanged, not deconstructed).

Examples of the rollback rule: in `@[a /| (b & c)]` with
`[@1>"a">@2, @3>"b">@1]`, consuming `b` backward would leave a zero-step
gap between `{@2}` and `{@3}`, so `b` stays unconsumed and the residual
branch is `[@2<b]>@1` (likewise `@[a/|~c/|~b]` in
`[@1>"a">@2, @1>"b">@2, @2>"c">@3]` rolls back `~c` to yield
`[@2<~c]>@2`).

#### 4.1.2 Backward seeding

The backward frontier comes from the end context: `EndAt` uses its NIDs.
For the **final** run of a branch whose end is open or an unpinned join
(`openBwd`), the backward pass is seeded with *every node of the graph* —
any node might be the seam; this is what lets `[a /| b]` recover the seam
`[a]>@m` when only `m>"b">f` survives. Non-final runs are not seeded this
way (their boundary is recovered structurally, §4.2.1).

Multiple backward sources that survive to the result become one
alternative each, not a union and not a conjunction.

`resolveNPath`/`createNPath` continue to use forward-only matching (no
backward pass); folding them onto the same segment core is a clean win but
optional.

### 4.2 Layer 1: structural recursion

Pure; walks the grammar, calling `resolveSegment` on linear runs and
recursing at boundaries. A *linear run* is a chain of single directions
whose intervening midpoints are unanchored, loop-free, and non-branching
(`a/|c/|~b` flattens to `[a, c, ~b]`) — the whole run goes to
`resolveSegment` in **one** call; bidirectional meeting needs the entire
run at once. A *boundary* forces recursion: an intersection, an anchored
midpoint, or a midpoint with loops.

Because alternatives compose, layer 1 is written against a nondeterminism
structure (each sub-result is a small set of alternatives; combining
sub-results takes the cartesian product, then dedups). Independent
ambiguity sites therefore multiply — dedup aggressively and keep
enumeration lazy.

- **Union**: residual of each member independently; a member's `%never`
  drops out; the members' alternative sets combine by cartesian product.
- **Pointlike** `{anchor, loops}`: resolve the anchor per §5 (note:
  `Specific` jumps even for a top-level pointlike — `@2[a]` from
  `startNid = @1` resolves at `@2`). No loops ⇒ `%never` if the node
  exists, unchanged otherwise. Each loop is a segment with forward and
  backward frontier `{N}`. A loop that consumed **zero** steps stays
  attached to its pointlike, in every position (root, midpoint, target);
  only partially-consumed loops deconstruct into rooted gaps (§6).
- **Rooted**: resolve every root (a root is itself a `DeterministicPath` —
  delegate), then resolve each root's intersected branch set from that
  root's frontier toward the shared target. All branches of all roots must
  resolve, agreeing at the target, for full resolution; the residual
  contains only failed/partial structure, but is assembled as **one**
  multi-root `Rooted` path wherever ends allow (§6), not one union member
  per root. Partially-consumed branches resume from **Specific frontier
  NIDs** — emitting those concrete resume points is required structure,
  not anchor pinning in the §5 sense.
- **Sequence**: `bs1` (intersection) from the current frontier to the
  midpoint (resolve midpoint loops there), then `bs2` onward. A partial
  midpoint loop advances a forward endpoint; its unresolved suffix becomes
  a deconstructed loop, after which `bs2` continues
  (`[a /@[x/|y]| b]` in `[@1>"a">@2, @2>"x">@3]` ⇒ `[@3<y /@2| b]`).

#### 4.2.1 Boundaries with unknown NIDs

When a run gaps into a boundary whose NID is unknowable (a `JoinPoint`
midpoint, an intersection), the remaining tail is matched from every graph
node ("tail candidates"); if the whole tail resolves somewhere, the gap
closes at that seam, otherwise the structure is kept as one connected gap
(chaining), which is safe for the invariant but not minimal. Structures
inside a chained gap are not re-attempted.

#### 4.2.2 Midpoint stations with several arrivals

A midpoint reached at several NIDs (fan-out) tries every arrival: all
fully-resolving arrivals contribute (landings unioned, one alternative);
otherwise each arrival showing progress becomes its **own alternative**
(no arbitrary first-arrival pick).

## 5. Anchor rules

| anchor | as root | as midpoint / target |
|---|---|---|
| `Specific N` | **jump**: traversal starts at `N` (even when `N ≠ startNid`) | **assert**: traversal must arrive at `N`; never rewritten |
| `JoinPoint` | unifies with `startNid` (respecting exclusions) | unifies with the arrival NID; intersected branches (across all roots) must agree |
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
  `[@<a & @<b]>@` in `[]` stays as written.
- A `JoinPoint` **target** pins **only on agreed landings**: the NID the
  fully-resolved branches agreed on. If resolved branches disagree, the
  join is unsatisfied — treat those branches as unresolved and do not pin.
  If **no** branch landed, the safe residual leaves the join unpinned
  (instantiation mints a fresh join node all residual branches meet at —
  always invariant-sound, §5.2 adds a minimality alternative). There is
  **no** whole-graph search for a pin.
- An excluding `JoinPoint` that cannot unify (`[a]>!{@2}` when traversal
  reaches only `@2`) leaves the path unchanged; a *root* `JoinPoint` that
  can never unify with `startNid` makes the whole deterministic path
  unsatisfiable — unchanged (§3).
- Nested `Rooted` roots with `Unanchored` targets get **no special
  treatment** (no internal rewrite to `JoinPoint ∅`): `Unanchored` is
  never pinned there either, and outer branches resume from concrete
  landings only. (Instantiation's one-NID-per-occurrence already makes the
  nested branches meet in the created graph, which is stricter than
  resolution requires — safe.)

When intersected branches partially resolve to *different* frontier NIDs,
split the residual per frontier (`@[(a & b) /| c]` with `a→@2, b→@3` ⇒
`[@2<c & @3<c]>@1`) — intersected branches are all required, so this is
one alternative, not two.

### 5.1 Self-loop bias

The residual prefers consumption, seams, and pins that do **not** depend
on a self-loop edge matching a non-loop step of the path (self-loops in
generated and real graphs overwhelmingly encode explicit loop syntax, and
the path language can always demand a self-loop explicitly). The bias is
soft: self-loop matches are used whenever they are the only source of
progress or of full resolution. `resolveNPath` is unaffected (§2).

### 5.2 Reach pinning (staged; minimality alternative)

A gapped branch whose trailing suffix resolved (via the backward pass) has
a known terminal — its **reach**. When no branch landed but reaches agree
on `@n`, emit — *in addition to* the safe unpinned residual — an
alternative with the target pinned at `@n`, reusing the existing
suffixes. This alternative is required for property 2's exactness (the
removed-edge repair reuses the surviving suffix); property 3 quantifying
over every alternative guards that it is only emitted when sound.
Implementation is staged: land the unpinned-only form first (property 3
green), then add reach-pinned alternatives (property 2 green, property 3
stays green).

## 6. Residual shape preferences (best-effort)

Unit-tested preferences, not load-bearing for the invariant. In priority
order:

1. **One connected deterministic path** per alternative, wherever ends
   allow: chain a gap that ends at a concrete node into the continuation
   that leaves that node (`[[@1<a & @2<b]>@3<c & @4<d]>@5` in
   `[@1>"a">@3]` ⇒ `[@2<b/@3|c & @4<d]>@5` — one multi-root path, not
   `[@2<b]>@3 + [@3<c & @4<d]>@5`). Disconnection is allowed when
   unavoidable (`[@[a/|b/|c/|d]<e]` in `[@2>"d">@1, @1>"a">@3]` ⇒
   `[@3<b/|c]>@2 + [@1<e]`).
2. **Forward expression, original orientation**, whenever the gap's start
   is expressible: a gap from pinned root `@1` to seam `@3` is
   `[@1<a]>@3`, not `[@3<~a]>@1`. Inversion is used only where it buys
   structure the forward form cannot: chaining through a loop root into a
   continuation (`[@[a/|b/|c]<d]` in `[@2>"c">@1]` ⇒
   `[@2<~b /| ~a /@1| d]`) or merging several *required* gaps into one
   path (`@[a/|b/|c & d/|e/|f]` in `[@2>"c">@1, @3>"f">@1]` ⇒
   `[@2<~b /| ~a & @3<~e /| ~d]>@1`). Prefer inverting steps over
   disconnecting; prefer the original orientation otherwise.
3. **Zero-consumption loops stay attached** to their pointlike (all
   positions); partially-consumed loops deconstruct
   (`@[a/|b]` in `[@1>"a">@2]` ⇒ `[@2<b]>@1`; but the fully-unmatched loop
   `c` in `[@2[c]<a & @1<b]` over `[@2>"a">@3, @1>"b">@3]` stays attached:
   ⇒ `@2[c]`).
4. **Ordering preserves the original path**: root-map keys and branch sets
   follow the declaration order of the branches that produced them;
   ascending NID order applies only to genuinely graph-derived sets
   (fan-out members, seam alternatives) and to the alternative list
   itself where alternatives differ only by NID.

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
point shared by several branches (across all roots) resolves to a single
node so the branches meet. The `edgesOf` instantiation in the §3
invariant is exactly this function applied to **one alternative**.

## 8. Property tests

Three properties, all generating paths via
`genDeterministicPathLiteralOnly` post-processed by `coveredPathFrom`
(exported from `TestUtils`). `coveredPathFrom startNid` makes the
preconditions hold by construction:

- a path matching `requiresUnanchoredJoin` (the §3 carve-out predicate)
  has its target normalized to `JoinPoint ∅` — the "append `>@`"
  normalization;
- the start NID is stripped from `JoinPoint` exclusion sets, since a root
  excluding the start NID can never resolve from it no matter which edges
  are created.

The transition-label pool is shared (11 words; `genTransitionLabel`), so
label collisions — self-loop coincidences, duplicate-label fan-outs — are
generated on purpose; the alternative machinery is what handles them.

1. **Resolving graph** ⇒ the result is exactly `[%never]`.
2. **Removing one edge** ⇒ **some** alternative resolves in the graph
   containing only the removed edge (or the path still resolves without
   it, i.e. the result is `[%never]`). Exactness is satisfiable despite
   ambiguity because the true repair is one of the enumerated
   alternatives.
3. **The invariant directly**: remove a random subset of edges, compute
   the residual, and for **every** alternative assert that instantiating
   it with `graphResolvingPathFrom` on top of the partial graph makes
   `resolveNPath` fully resolve the original path (run purely via
   `runInMemoryGraphMetadataEditing`). Each case stays bounded by
   `within` so combinatorial blowups fail and print instead of hanging;
   all three properties print `path`/`start`/`graph`/`residual` via
   `counterexample`.

The `resolvesIn` guards (`==>`) remain as safety nets but essentially
never discard.

**Acceptance bar** for the rewrite: all three properties pass 1000 cases
per run across ~10 consecutive runs with fresh seeds, no `within`
timeouts. The suite default stays at 100 cases for day-to-day speed.

## 9. Test-suite status

`spec_pathResidualsUnresolvedBy` encodes the rules above and is the
behavioral spec. As of 2026-07-13 the rewrite is implemented and the
focused suite is green. The acceptance bar in §8 was verified with ten
fresh runs of 1000 cases for each of the three properties, with no
timeouts; the complete CLI suite is also green.

## 10. Acceptance examples

1. `[a/|b]`, `[@1>"a">@2]` ⇒ `[@2<b]` (one alternative).
2. `@[a/|c/|~b]`, `[@1>"a">@2, @1>"b">@3]` ⇒ `[@2<c]>@3` — one segment
   from `{@1}` back to `{@1}`; forward consumes `a`, backward consumes
   `~b`, gap `c` between `@2` and `@3`. Requires the whole run in one
   `resolveSegment` call.
3. `@[(a & b)/|c]`, `[@1>"a">@2, @1>"b">@3]` ⇒ `[@2<c & @3<c]>@1` —
   intersection boundary; both branches are required, so the split per
   frontier NID is conjoined in **one** alternative.
4. `[a/|b]`, `[@1>"a">@2, @1>"a">@3]` ⇒ two alternatives `[@2<b]` and
   `[@3<b]` — fan-out of one branch's matching; either repair suffices.
5. `[a /@[b]| c]`, `[@1>"a">@2, @2>"c">@3]` ⇒ `@2[b]` — midpoint
   `JoinPoint` pins to `@2`; only the midpoint loop is unresolved (and
   stays attached).
6. `[[@1<a & @2<b]>@3<c & @4<d]>@5`, `[@1>"a">@3]` ⇒
   `[@2<b/@3|c & @4<d]>@5` — one connected multi-root alternative:
   the nested root's unresolved branch chains through its pinned target
   into the outer branch.

## 11. Pointers

- Implementation + unit/property tests:
  `src/Models/NormalizedPath/UnresolvedResiduals.hs`
- Decision record + worklist:
  `docs/pathResidualsUnresolvedBy-implementation-notes.md`
- Traversal core shared with resolve/create:
  `src/Models/NormalizedPath/Traversals.hs` (`traverseViaTransitions`;
  note the `isRoot` flag and `unifyFullyAnchored` in
  `Models/NormalizedPath/Anchor.hs` — the root-jump behavior lives there)
- Resolver / creator: `src/Graph/Resolve.hs` (+ `Graph/ResolveSpec.hs`
  multi-root regression tests), `src/Graph/Create.hs`
- Generator + carve-out predicate: `src/Models/NormalizedPath/TestUtils.hs`
- Anchor explosion semantics: `src/Models/NormalizedPath/FullyAnchor.hs`
- Run the suite: `stack test --ta '-p pathResidualsUnresolvedBy'`
  (single-word tasty patterns only; see `docs/Testing.md`). On this
  machine stack needs `--ghc-options='-framework Foundation'` (TH linker
  workaround).
