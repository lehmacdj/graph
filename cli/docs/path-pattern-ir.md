# Route patterns: the path compilation target

Status: implemented. This document is the spec
and the work tracker; TODOs are checked off as they land.

Surface `Path`s compile to **route patterns**. A route pattern is the
normal form of a path and the single intermediate representation used for
resolution, residuation (`mk`), and edge selection (`rn`, `mv`, `cp`, `cl`,
preview, tags). It replaces `NormalizedPath` and everything experimented
with on `codex/path-normal-form` (the `Plan` ADT, `Expression*`, `Legacy/*`).
That bookmark is left untouched as an archive; nothing from it is rebased.
It is a reference for surface and command semantics only.

## 1. Representation

```haskell
newtype VarId  = VarId Int
newtype EdgeId = EdgeId Int

data PVertex = Current | Fixed NID | Var VarId
data NodeTest = Exactly NID | Except (Set NID)  -- Except ∅ = any node

data Matcher = MLiteral Text | MWild | MRegex CheckedRegex
data Direction = Out | In                   -- traversal direction

data Hop = Hop
  { edge :: EdgeId, direction :: Direction, matcher :: Matcher
  , target :: PVertex }
data Route = Route { root :: PVertex, hops :: [Hop] }

data PatternPath = PatternPath
  { tests  :: Map PVertex NodeTest
  , routes :: NonEmpty Route }              -- written order

newtype PathQuery = PathQuery (Set PatternPath)  -- `+` alternatives
```

- **Routes are the normal form.** Each route is one conjunct after
  distribution: a walk from its root to one output. Sharing (joins) is
  expressed by vertex identity: two routes that reach the same `PVertex`
  meet there. There is no tree structure (no Fork/Merge/Guard/Parallel).
- **Outputs** are route endpoints. There is no separate output mark.
- **Vertices.** `Current` is bound by the caller. `Fixed n` is an identity,
  so every `@n` is one vertex. `Var` is every other position. The
  compiler never produces a `Var` root today, but roots stay typed as
  `PVertex` so `%unanchored` can return (§8).
- **Tests.** Only constraining tests are stored: a `Fixed n` vertex
  implicitly tests `Exactly n` and an absent entry accepts any node
  (`vertexTest` gives the effective test). Joining two vertices takes the
  meet of their tests:
  `Except a ⊓ Except b = Except (a ∪ b)`; `Exactly n ⊓ Except s` is
  `Exactly n` unless `n ∈ s`; `Exactly n ⊓ Exactly m` requires `n == m`.
  A contradiction drops the alternative. Merging a `Var` with `Fixed n`
  yields `Fixed n`. `Exactly` survives only as a test on `Current` (for
  example, a relative path joined with `@5`).
- **EdgeId.** One per syntactic step occurrence. A witness chooses one
  concrete edge per `EdgeId`; that matters only for `*`/regex matchers,
  where several edges can connect the same two nodes. Routes that share a
  prefix (`*/@/(b & c)`) reuse its `EdgeId`s, so they make one choice.
  Independent occurrences (`(* & *)/@`) get distinct ids and choose
  independently. Labels, selection, patches, and the label callback are all
  keyed by `EdgeId`.
- **Derived pattern graph.** `Graph' PVertex PEdge PAug` with
  `PEdge { id :: EdgeId, matcher :: Matcher }` (so parallel pattern edges
  stay distinct) and `PAug { test :: NodeTest }`. It is used for adjacency
  during matching and residuation. The routes are authoritative.
- **Canonical form.** Renumber `VarId`s and `EdgeId`s in order of first
  appearance along the routes. Canonical `PatternPath`s compare as
  syntactic equality.
- **Test syntax.** There is no quasiquoter. Small Haskell helpers
  (`pat`, `patWith`, `route`, `current`, `fixed`, `var`, `out`, `inc`,
  `wild`, …) build patterns. `Show` prints the same helper expression after
  canonical renumbering, so failing output can be pasted back as an
  expectation.

## 2. Compilation (`Path` → `PathQuery`)

1. Handle directives as today (`handleDirectivesWith`).
2. Apply `~` on the surface, using `main`'s semantics (a true inverse):
   `~(p/q) = ~q/~p`, `~(p & q) = ~p & ~q`, `~(p + q) = ~p + ~q`; node
   specs are unchanged and each step's direction flips.
3. **Distribute** unjoined sequencing over conjunction. `a/(b & c)`
   becomes `a/b & a/c` with independent copies of `a`, and
   `(a & b)/(c & d)` becomes four conjuncts in `p`-major order.
   Distribution stops at joins: after `/@`, `@N`, or `!{…}` the prefix is
   shared by every continuation. This is the branch's
   `sequenceExpr`/`prefix` behavior.
4. **Glue.** Each step occurrence gets a fresh `Var` target and a fresh
   `EdgeId`. `/` identifies the left output with the right input. A join
   merges outputs (taking the meet of their tests). Relative paths are
   rooted at `Current`, and `@N` at the root is `Fixed N`.
5. Drop alternatives with contradictory tests, and canonicalize.

The `@` semantics follow the branch: at a sequence boundary it joins the
outputs of the preceding expression; inside a conjunct it stays at that
conjunct's input; a join inside one branch does not constrain outer
siblings. `%float` and `%unanchored` are **not** supported.

**Command policy (from the branch).** Query commands append `/@` before
compiling. Construction (`mk`) does not. The engine never adds defaults
itself.

## 3. Selection

| consumer | derived from |
|---|---|
| `rn`, `mv`, `cp`, `cl` target edge | last hop of route 0 |
| preview / leftmost path | all hops of route 0 |
| tags, FileType ("final edges") | last hop of each route |
| targets | route endpoints |

- If route 0 has zero hops (`rn (@ & a) c`), it is an error, as in the
  branch's goldens.
- The Guard special case is gone. `x/(@ & a)/@` has routes `[x, x·a]`:
  rename targets `x`, and final edges are `{x, a}`. `x/(a & @)/@` has routes
  `[x·a, x]`, so rename targets `a`. The unjoined `/` copies `x` into both
  routes (two independent `x` edges); with `x/@/(@ & a)/@` the `x` edge is
  shared and `a` is a loop on its target.
- `(a & b)/@ & c` has routes `C -a-> J`, `C -b-> J`, `C -c-> v`. As a query
  (`/@` appended), `v` merges into `J`.

## 4. Resolution

**Load (effectful).** Starting from `Current`/`Fixed`, propagate candidate
sets along pattern edges over `GraphMetadataReading`, reading and caching
each node once into a `Graph Text IsThin`, until a fixpoint (with arc
consistency pruning). Reads are local to neighbourhoods of candidates.
There is never a whole-graph snapshot.

**Match (pure).** Backtracking over the loaded thin graph. Matching is a
**homomorphism**: distinct variables may bind the same node.

**Factoring.** Split the pattern at bound vertices into **components**:
connected groups of variables, plus the hops that touch them. A hop between
two bound vertices is its own variable-free component. Components are
independent, so the set of witnesses is the product of per-component
solutions. That product is stored, not expanded.

```haskell
data Component = Component { vars :: Set VarId, edges :: Set EdgeId }
data Partial = Partial
  { nodes :: Map VarId NID, labels :: Map EdgeId Text }
data Resolution = Resolution
  { patternPath :: PatternPath                -- `pattern` is a keyword
  , current    :: Maybe NID
  , components :: [(Component, [Partial])] }  -- empty list ⇒ no match
data ResolvedPath = ResolvedPath
  { resolutions      :: [Resolution]     -- one per matching alternative
  , graph            :: Graph Text IsThin
  , nonexistentNodes :: Set NID }

targets        :: Resolution -> Set NID
selectedRoutes :: Resolution -> Set [ConcreteHop]        -- route 0
routeLastEdges :: Resolution -> Map Int (Set (Edge Text))
witnesses      :: Resolution -> [Witness]                -- lazy product
```

- **Order.** Components are ordered by first appearance along the routes.
  Partials within a component are ordered lexicographically by their
  concrete hops in route order. `witnesses` enumerates component-major.
  The first witness equals the lexicographic minimum of the full ordering.
  Alternatives are ordered by their minimum witness (this pins
  `tag-union-route-order`). Commands use only "first" or sets.
- A route through a `Fixed` vertex spans components. Its concrete hops
  combine only those components' partials.
- Per-witness `finalNonLoopEdges` is replaced by `routeLastEdges`.
  `leftmostConnects` is replaced by `selectedRoutes`.

## 5. Residuation

A **residual** is a total binding of the pattern plus labels. The patch is
derived from it.

```haskell
data Binding = Existing NID | Fresh Int
data Residual = Residual
  { pattern :: PatternPath
  , sigma   :: Map PVertex Binding
  , labels  :: Map EdgeId Text
  , patch   :: Patch }          -- nodes + edges to create
```

**Patch.** Allocate the fresh nodes, and create any missing `Fixed` nodes.
Map each `EdgeId` through σ and its label. It is in the patch iff the
resulting edge is absent from the graph. The patch is a set, so two
`EdgeId`s that land on the same concrete edge collapse. A fresh node has
no existing edges.

**Candidates.**
1. `Current` and `Fixed` are bound.
2. **Evidence rule.** A `Var` may be bound to an existing node only if at
   least one of its incident pattern edges exists under σ with the other
   endpoint bound. Evidence may come from either side of the vertex.
3. Otherwise the variable is fresh.
4. **Fresh identification.** Two fresh vertices may be identified iff that
   makes two pattern edges the same patch edge (same label, orientation,
   and other endpoint). Identifications from the **left** (the same hop
   from the same vertex) are always made; identifications from the
   **right** (the same hop into the same vertex) are alternatives, like
   right evidence. Both run to a fixpoint. Examples: `a & a` gives one
   edge; `a & b` gives two nodes; `a/b & a/c` gives a shared `a` node;
   `a/b/@5 & c/b/@5` is ambiguous.
   *Deviation:* the agreed wording ("may be identified, minimality
   chooses") made `a/b & a/c` ambiguous, because the shared and separate
   patches are incomparable modulo renaming; making left identification
   mandatory matches every example and the "right is weaker" decision.
5. **Labels.** A `*`/regex `EdgeId` whose edge must be created gets its
   label from the caller's callback only; there is no graph-wide reuse.
   `mk` supplies no labels, so it errors, naming the edge.

**Left-justified.** `Current` and `Fixed` are left-justified. So is any
variable reached from a left-justified vertex by an existing edge taken in
route traversal direction (transitively; a join bound by one route's left
walk counts). Every other evidence-backed binding is right-justified.

**Selection.**
1. If any alternative already resolves, the result is the no-op.
2. **Left dominance.** Drop σ if another candidate extends it and every
   additional binding is left-justified. Right-justified bindings never
   dominate the fresh alternative.
3. Drop candidates whose patch is a superset of another's, modulo renaming
   of fresh nodes. Deduplicate equal patches modulo renaming.
4. **Order the survivors:** a left-only candidate (no right-justified
   bindings) first; then by patch size (created nodes + created edges);
   then a deterministic tie-break on concrete routes.

**`mk`.**
- One survivor: apply it.
- More than one: error without changing anything. List the survivors in
  the order above, each as its **resolved routes** (existing nodes as
  `@N`, created nodes as `newK`) plus its patch, and suggest pinning a
  node with `@N` or excluding one with `!{…}` in the path.
  *Deviation:* the agreed design printed a disambiguating surface path per
  alternative, but surface syntax cannot name a created node shared by
  several routes (only `/@` joins can), so a faithful surface path does not
  always exist.
- **Orphans.** If the patch creates a weakly-connected component with no
  edge to a pre-existing node (for example, `mk @5/a` with 5 missing),
  error, naming the component. An override flag allows it.
- The label chooser is a function of the matcher (`Matcher -> Maybe
  Text`); it is consulted for every edge that must be created with a
  non-literal matcher.
- Residuation operates lazily over `GraphMetadataReading`.
  Candidate generation is phrased as "candidates for a variable given its
  bound neighbours". The no-bound-neighbour case is an explicit
  unreachable error until `%unanchored` returns.
- Resolution is factored; residuals are flat total σ. Fresh identification
  can merge across components, so they are not factored.

## 6. Testing

- **Oracle.** A brute-force matcher in the test suite enumerates every
  assignment of variables (and labels) over 2–3 node graphs. Port the
  branch's surface-query × small-graph corpus (dropping `%float` and
  `%unanchored` queries). Compare targets, `selectedRoutes`,
  `routeLastEdges`, and the set of witnesses against the oracle.
- **Residuation properties.** For every residual: applying it makes the
  path resolve; the result is a no-op iff the path already resolves;
  applying it twice is idempotent. Ambiguity is reported iff there are at
  least two survivors.
- **Unit tests** for each decision in this document, including every
  example given in it.
- **Goldens.** Port the branch's `test/golden/path-redesign/scenarios.json`
  scenarios to the existing `.golden` format. Extend that format with an
  optional inline base-graph section so a test does not need an
  `examples/` template. Drop `explicit-free-root-stay`,
  `explicit-unanchored-roots`, and `isolated-free-root-reuse`. Re-derive
  expectations that the semantics above change (Guard final edges,
  residuation choices, and error messages), and note each change in the
  commit message.
- Build/test: `stack test --fast --ghc-options '-framework Foundation'`
  from `cli/`, with single-word `-p` patterns.

## 7. Work plan

All work goes on top of `3bebf036` (Generalize Graph over the node
identifier type), in about four commits. Each commit builds and passes the
full test suite.

### Commit 1: route patterns, compiler, matcher
- [x] This document, in `cli/docs/`
- [x] `Models.PathPattern` types, canonicalization, `Show` as helpers, and
      the test helpers
- [x] Derived pattern graph (`Graph' PVertex PEdge PAug`)
- [x] Compiler: `~`, distribution, glue, tests meet, contradictions
- [x] Load phase (candidate propagation over `GraphMetadataReading`)
- [x] Pure matcher, factoring into components, `Resolution`, and
      projections
- [x] Brute-force oracle, ported corpus, and unit tests for §1–§4
- Modules: `Models.PathPattern` (types, helpers, canonical form, derived
  graph), `Models.PathPattern.Compile`, `Models.PathPattern.Match` (pure
  matching and projections), `Graph.PathPattern.Resolve` (load + match).
  Besides the brute-force matcher, `CompileSpec` checks every corpus query
  on every corpus graph against a direct interpreter of the distributed
  expressions, which shares no code with gluing or matching.

### Commit 2: residuation
- [x] Candidate enumeration: evidence rule and fresh identification
- [x] Label chooser for non-literal edges
- [x] Left-justification, dominance, subset minimality modulo renaming,
      and dedup
- [x] Survivor ordering and rendering (see the deviation in §5)
- [x] Orphan detection
- [x] Property tests and unit tests for §5
- Module: `Models.PathPattern.Residuate` (generic over the node lookup
  monad; `chooseRepair` gives the command-level choice). `ResiduateSpec`
  checks the properties for every corpus query on every corpus graph and
  that the corpus actually produces repairs, ambiguity and right evidence.

### Commit 3: commands
- [x] Port the branch's command policy (`/@` for queries) and the
      `rn`/`cl`/`cp`/`mv` diagnostics
- [x] Switch `Graph.Command`, `Graph.Paths`, `Graph.Complete`, the
      completion executable, `Graph.Create`, `Tags`, `FileType`,
      `Timestamps`, and `Preview` to the new API
- [x] `mk`: residuate, ambiguity error, orphan error, and override flag
- [x] Golden format: inline base-graph section
- [x] Port the goldens and update changed existing goldens
- `Graph.PathPattern.Execution` holds the command policy. Commands read
  nodes through `ReadGraphDataless`, the same effects they write through.
  `Graph.Paths` needed no change. `Graph.Complete` (an `undefined` stub)
  and `Graph.Create` (which only created `NormalizedPath NID`s) are not
  switched but deleted in commit 4, since nothing else uses them.
- `rn`/`cl`: the destination is resolved as `destination/@endpoint` with
  its last pattern edge dropped, and the edge recreated from the node
  before it. *Deviation:* that last hop must be a literal (the branch
  allowed matching existing labels for a wildcard, which picks an
  arbitrary label).
- The orphan override is `mk --allow-orphans`.
- Golden format: an optional first section `==========graph==========`
  with lines `0 -a-> 1` (edges between `smallNID`s; node 0 is the origin)
  and `1 data "text"` (node data, a Haskell string literal). Tests with a
  base graph also render a trailing `==========data==========` section
  listing every node's data, since the graph dump omits data. The ported
  scenarios live in `test/golden/ge/path/`; `spec-*.golden` cover the
  command examples of this document.
- The existing `mg`/`mg-complex` goldens set up with `mk a + b`, which is
  now an ambiguity error; their input became `mk a & b`, which creates the
  same two edges, so their expected output is unchanged.

### Commit 4: removal
- [x] Delete `Models.NormalizedPath*`, `Models.ResolvedPath`'s old
      projections, `Graph.Resolve`'s old resolver, `UnresolvedResiduals`,
      `ResidualsBench`, `LegacyPathMaterialization` (if unused), and
      `docs/pathResidualsUnresolvedBy-*`
- [x] Update `cli/CLAUDE.md` and docs references
- Also deleted: `Graph.Create`, `Graph.Complete`, `Models.DPath`, the
  `residuals-bench` executable, and `Graph.LegacyPathMaterialization`
  (unused once commands moved). `Models.LegacyPartialPath` stays: it is the
  completion parser, not an engine.

### Review
- [ ] An independent subagent, with a fresh context rather than a fork,
      reviews the finished commits against this spec for completeness and
      test sufficiency. Findings are fixed or explicitly deferred with the
      user's agreement.

## 8. Deferred and out of scope

- **`%unanchored`.** When it returns: a `Var` root, root candidates from
  `nodeManifest`, the no-bound-neighbour case in candidate generation, and
  an isolated vertex binding to any node that passes its test.
- **Positive node predicates** (for example, "tagged `#foo`") on variables
  would make right evidence necessary for completeness. Revisit evidence
  rules if they are added.
- **Picking among ambiguous repairs** (`mk --choose N`).
- **A label → edges index** for performance.
