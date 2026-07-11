# `pathResidualsUnresolvedBy` — Decision Record & Worklist

Status (2026-07-11). A design review settled every ambiguity recorded by
the previous version of this document. Work is staged:

- **Stage 0 — done, committed**: `resolveNPath` multi-root union bug fixed
  in `traverseRooted` (`Traversals.hs`); regression tests in
  `Graph/ResolveSpec.hs`.
- **Stage 1 — done**: `pathResidualsUnresolvedBy-spec.md` rewritten (v2)
  to encode all decisions; this document rewritten as the decision record.
- **Stage 2 — done**: unit/property tests updated to the new behavior; the
  API changed to `NonEmpty (NormalizedPath Anchor)` with the old
  implementation wrapped as a singleton alternative. Tests whose
  expectations changed are red and form the worklist (§3).
- **Stage 3 — pending**: implement the spec (intended for a fresh agent).
- **Stage 4 — pending**: property burn-in (1000 cases × ~10 runs, all
  three properties, no `within` timeouts).

## 1. Decisions (with rationale)

1. **Multi-root resolution is conjunctive.** Roots (keys of
   `rootBranches`) start independently, but every root's branch set must
   resolve and all landings must unify at the shared target. The
   union-over-roots behavior in `resolveNPath` was a bug (fixed, stage 0).
   Consequence: a root that can never unify (a `JoinPoint` excluding
   `startNid`) makes the whole deterministic path unsatisfiable — the
   residual returns it **unchanged** (previously that root's branches were
   silently dropped).
2. **Meta-union API.** The residual returns
   `NonEmpty (NormalizedPath Anchor)`: each alternative is a *complete*
   repair satisfying the §3 invariant on its own; any one suffices; the
   preferred one is first; dedup required. The inner `NormalizedPath`
   union keeps meaning "create every member" (union members of the input
   are each completed — `[a] + [b]` in `[@1>"a">@2]` ⇒ `[b]`).
   Rationale: distinguishes the two union levels that were previously
   conflated — graph-derived *alternatives* (multi-seam, fan-out, arrival
   choices, reach-vs-unpinned joins), where only one repair is needed,
   versus path-derived unions, where everything must exist. It also makes
   property 2 satisfiable: for ambiguous instances the true repair is
   *among* the alternatives even though no deterministic single-valued
   function could pick it (the removed edge is not an input).
3. **Self-loop soft bias, residual only.** Prefer consumption/seams/pins
   that don't depend on a self-loop edge matching a non-loop step; fall
   back when they're the only source of progress or full resolution.
   `resolveNPath` is untouched (a hard rule would be a query-language
   change: `[a]` would stop traversing `@1>"a">@1`, breaking the
   `@[a /@| ~a]` unit tests and real reflexive-edge queries).
4. **Maximize-consumption split selection.** Among (k forward, j backward)
   splits of a run: maximize k+j; ties prefer non-self-loop progress, then
   forward. Replaces the old greedy-forward rule and its narrow
   "frontier-preserving ∧ fully-rolled-back" exception. Verified against
   the key unit tests: `[a/|b]>@4` and `@[a/|~c/|~b]` keep their answers
   (ties → forward); the `@[a/|d/|~c/|~b]` family flips to the
   backward-seam answer (see §3).
5. **Join pinning simplified, staged.** Targets pin only on agreed
   landings (disagreement demotes; both unchanged from spec §5). With no
   landings the safe residual leaves the join unpinned — instantiation
   mints a fresh join node, always invariant-sound. The whole-graph
   minimal-weight pin search (`searchPin`) and the loops-resolve
   preference are **deleted** (prime suspects for the old property-3
   failures: every pin without a landing is a bet that all branches,
   re-aimed gaps, and target loops assemble consistently around the chosen
   node). Reach pinning returns in a second step as an *additional
   alternative* (needed for property 2's exactness, guarded by property 3
   over every alternative).
6. **Fan-out gaps are alternatives, not conjunctions.** `[a/|b]` in
   `[@1>"a">@2, @1>"a">@3]` ⇒ alternatives `[@2<b]` | `[@3<b]` (one chain
   suffices). Distinguish from intersections landing on different nodes
   (`@[(a & b)/|c]` ⇒ `[@2<c & @3<c]>@1`): intersected branches are all
   required, so those stay conjoined in one alternative.
7. **Backward multi-source seams are alternatives** (same reasoning), each
   forward-expressed.
8. **Midpoint stations with several arrivals**: fully-resolving arrivals
   union into one alternative; otherwise each progressing arrival is its
   own alternative (replaces the arbitrary first-progress pick).
9. **Zero-consumption loops stay attached** to their pointlike in every
   position; only partially-consumed loops deconstruct. (Removes the old
   position-dependent root/midpoint split.)
10. **Orientation: forward, original direction, unless inversion buys
    connectivity** (chaining through a loop root into a continuation, or
    merging several required gaps into one path). E.g. `@[a/|b/|c]` in
    `[@2>"c">@1, @3>"b">@2]` ⇒ `[@1<a]>@3` (was `[@3<~a]>@1`), but
    `[@[a/|b/|c]<d]` in `[@2>"c">@1]` keeps `[@2<~b /| ~a /@1| d]`.
11. **Connectedness / intersection preservation.** Per-root residual
    fragments merge into one multi-root `Rooted` path wherever ends allow;
    gaps ending at a concrete node chain into continuations leaving that
    node: `[[@1<a & @2<b]>@3<c & @4<d]>@5` in `[@1>"a">@3]` ⇒
    `[@2<b/@3|c & @4<d]>@5`. Union-splitting only when unavoidable.
12. **Ordering preserves the original path.** Root maps/branch sets follow
    the declaration order of the branches that produced them; ascending
    NID only for genuinely graph-derived sets (fan-out members, seams) and
    for alternatives differing only by NID. (Replaces "consumed gaps
    before re-rooted gaps" and blanket ascending-NID.)
13. **Nested `Rooted` roots with `Unanchored` targets get no special
    rewrite** (the old internal `JoinPoint ∅` treatment is dropped). Its
    recorded justification — property 3 failing — predates the stage-0
    conjunction fix and decision 5; if property 3 reproduces the failure,
    re-add narrowly with the counterexample as a regression test.
14. **Kept as-is**: open-backward all-nodes seeding for a branch's final
    run (property 2 needs seam recovery), and tail-candidate recovery /
    connected chaining behind unknowable boundaries.
15. **Properties.** Prop 1: result `=== [%never]`. Prop 2: shared 11-word
    label pool, **some** alternative resolves in the removed-edge graph.
    Prop 3: **every** alternative satisfies the invariant; keep `within`
    bounds and `counterexample` printing. Acceptance: 1000 cases × ~10
    fresh-seed runs, no timeouts; suite default stays 100.

### Historical context: the property-2 ambiguity finding

The old implementation hit instances where `graph − e` contained two
structurally symmetric repairs of identical size — e.g. a path whose
midpoint loop `@1["e"]` minted `@1>"e">@1`, which coincidentally also
matched the mid→target step `e`; after deleting the target's `~e` loop
edge, pinning the join at `@1` (residual `@1["bar"]`) and at `@t`
(residual `@t[~e]`) were both complete repairs, but only one minted the
removed edge. No deterministic single-valued rule can choose (rows of
locally-identical evidence demanded opposite answers in unit tests vs.
property instances). Decisions 2–4 dissolve this: the soft bias prefers
the non-coincidental reading, and where the graph is genuinely symmetric
the meta-union returns both repairs and property 2 accepts either.
Duplicate-label fan-outs (`[a/|b & a/|c]` minting two `a`-edges) are the
same story via decision 6.

## 2. Code changes already made (stages 0/2)

- `Traversals.hs` `traverseRooted`: cross-root combinations via `choices`
  over per-root alternatives; `ensureSameAnchors` spans the whole
  combination; root map rebuilt with `unionsWith (<>)`.
- `Graph/ResolveSpec.hs`: `spec_resolveNPath` — 8 multi-root agreement
  cases. (Separate module: importing `Utils.Testing` into `Graph.Resolve`
  pulls ObjC bindings into TH modules' dependency closure and breaks TH
  linking.)
- `UnresolvedResiduals.hs`: signature changed to return
  `NonEmpty (NormalizedPath Anchor)`; the existing computation is wrapped
  as `pure (…)` — a compile shim so unchanged tests stay green and changed
  tests are red. Test helpers: `testResiduals` compares against a single
  parsed alternative (`pure expected`); `testResidualsAlts` compares
  against an explicit list of alternatives in order.

## 3. Stage-3 worklist: red tests and what they encode

Expectations changed (old ⇒ new), all in
`spec_pathResidualsUnresolvedBy`:

| test (path ⊢ graph) | old | new | decision |
|---|---|---|---|
| `[@<(a /| b) & @<(c /| d)]` ⊢ `[@1>"c">@2]` | `[@2<d & @1<(a /| b)]` | `[@1<(a /| b) & @2<d]` | 12 (ordering) |
| `[a/|b]` ⊢ `[@1>"a">@2, @1>"a">@3]` | `[@2<b & @3<b]` | alts `[@2<b]`, `[@3<b]` | 6 |
| `[a/|b/|c]` ⊢ `[@1>"a">@2, @2>"b">@3, @2>"b">@4]` | `[@3<c & @4<c]` | alts `[@3<c]`, `[@4<c]` | 6 |
| `@[a/|b/|c]` ⊢ `[@2>"c">@1, @3>"c">@1]` | `[@2<~b /| ~a]>@1 + [@3<~b /| ~a]>@1` | alts `[@1<a /| b]>@2`, `[@1<a /| b]>@3` | 7, 10 |
| `@[a/|*]` ⊢ `[@2>"x">@1]` | `[@2<~a]>@1` | `[@1<a]>@2` | 10 |
| `@[a/|b/|c]` ⊢ `[@2>"c">@1, @3>"b">@2]` | `[@3<~a]>@1` | `[@1<a]>@3` | 10 |
| `@[a/|d/|~c/|~b]` ⊢ `[@1>"a">@2, @2>"d">@2, @1>"b">@2, @2>"c">@3]` | `[@2<~c]>@2` | `[@2<d]>@3` | 3, 4 |
| `[@[a/|d/|~c/|~b]<e]` ⊢ same graph | `[@2<~c]>@2 + [@1<e]` | `[@2<d]>@3 + [@1<e]` | 3, 4 |
| `[@[a/|(d/|~c/|~b & f)]<e]` ⊢ same graph | `[@2<~c]>@2 + [@2<f/@1|e]` | `[@2<d]>@3 + [@2<f/@1|e]` | 3, 4 |
| `[@[a/|(d/|~c/|~b & f & g)]<e]` ⊢ same graph | `[@2<~c]>@2 + [@2<(f & g)/@1|e]` | `[@2<d]>@3 + [@2<(f & g)/@1|e]` | 3, 4 |
| `[@2[c]<a & @1<b]` ⊢ `[@2>"a">@3, @1>"b">@3]` | `[@2<c]>@2` | `@2[c]` | 9 |

New decision-pinning tests (red until stage 3 where noted):

- `[[@1<a & @2<b]>@3<c & @4<d]>@5` ⊢ `[@1>"a">@3]` ⇒
  `[@2<b/@3|c & @4<d]>@5` (decision 11; red).
- `[!{@1}<a & @<b]` ⊢ `[@1>"b">@2]` ⇒ unchanged (decision 1; red —
  the old code drops the excluded root's branches).
- `[!{@1}<a]` ⊢ `[]` ⇒ unchanged (decision 1).

Unchanged-on-purpose (verified against the new rules during the review —
do not "fix" these): `[a/|b]>@4` ⊢ `[@3>"b">@4, @1>"a">@2]` stays
`[@2<b]>@4` (tie → forward); `@[a/|~c/|~b]` ⊢
`[@1>"a">@2, @1>"b">@2, @2>"c">@3]` stays `[@2<~c]>@2` (tie → forward, no
self-loop involved); `@[a/|d/|~c/|~b]` ⊢ the graph with `@2>"c">@2` stays
`%never` (bias falls back for full resolution);
`@[a/|b/|c & d/|e/|f]` ⊢ `[@2>"c">@1, @3>"f">@1]` stays
`[@2<~b /| ~a & @3<~e /| ~d]>@1` (inversion merges required gaps);
`[@[a/|b/|c]<d]` ⊢ `[@2>"c">@1]` stays `[@2<~b /| ~a /@1| d]` (inversion
chains); intersection splits like `@[(a & b) /| c]` stay conjoined.

Properties (updated in stage 2, red until stage 3):

- prop 1 compares to `pure %never`;
- prop 2: `residual === pure %never` `.||.` some alternative's residual in
  the single-edge graph `=== pure %never`;
- prop 3: `conjoin` over all alternatives, each `within`-bounded, each
  instantiated independently.

Implementation guidance for stage 3: the spec (v2) is normative. Expect to
restructure layer 1 around a nondeterminism/alternatives structure
(cartesian composition + dedup); delete `searchPin`, the loops-resolve pin
preference, the `alone`-flag machinery (subsumed by alternatives), and the
`AttachZeroLoops`/`DeconstructZeroLoops` split (attach everywhere). Keep
`resolveSegment`'s frontier-trail core but replace split selection with
maximize-consumption + tie-breaks. Risks: alternative blowup (dedup
aggressively; the `within` bound catches the rest) and the old
property-3 failure class (three intersected roots + several `/@|` join
midpoints + a loops-carrying join target) — if it reproduces, suspect
decisions 5/13 interactions first and capture the instance as a
regression test.

## 4. Environment notes

- `stack` on this machine needs `--ghc-options='-framework Foundation'`
  (GHC TH loader otherwise fails with
  `symbol not found in flat namespace '_OBJC_CLASS_$_NSArray'`).
- tasty patterns through `stack test --ta '-p …'` must be single words.

## 5. Where things live

- Implementation + tests: `src/Models/NormalizedPath/UnresolvedResiduals.hs`
- Spec (normative): `docs/pathResidualsUnresolvedBy-spec.md`
- Multi-root resolve regression tests: `src/Graph/ResolveSpec.hs`
- Generator post-processing: `coveredPathFrom` in
  `src/Models/NormalizedPath/TestUtils.hs`
- Run the suite: `stack test --ta '-p pathResidualsUnresolvedBy'`
