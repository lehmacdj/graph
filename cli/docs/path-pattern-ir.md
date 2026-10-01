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
  (`pat`, `patWith`, `route`, `current`, `fixed`, `fixedN`, `var`, `out`,
  `inc`, `outW`, `inW`, `outM`, `inM`, `except`, `exactly`) build patterns.
  `Show` prints the same helper expression after canonical renumbering, so
  failing output can be pasted back as an expectation. The exception is a
  regex matcher, which prints as `MRegex regex:"…"` and must be written with
  `[re|…|]`.

## 2. Compilation (`Path` → `PathQuery`)

1. Expand directives (`handleDirectivesWith`; see *Directives* below).
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
   rooted at `Current`, and `@N` at the root is `Fixed N`. `@N` is a root
   only when nothing is sequenced before it in its alternative: in `@N/a`
   and `a & @N` it is `Fixed N`, but `@/@N` and `(@ + a)/@N` test `Current`
   instead (see below).
5. Drop alternatives with contradictory tests, and canonicalize.

The `@` semantics follow the branch: at a sequence boundary it joins the
outputs of the preceding expression; inside a conjunct it stays at that
conjunct's input; a join inside one branch does not constrain outer
siblings. An `@` that is one alternative of a union directly at a sequence
boundary (on either side of `/`, however unions nest within the union) is
such a join too: it keeps its join meaning when `/` distributes over `+`.
So `(@ + a)/@N` is `@/@N + a/@N`, which tests that the current node is `N`
rather than jumping to `N`; `p/(@ + a)` is `p/@ + p/a`; and in
`x/(@ + a)/(b & c)`, which is `x/@/(b & c) + x/a/(b & c)`, the `@`
alternative shares `x` between the conjuncts. An `@` inside a conjunct,
as in `((@ + a) & b)/@N`, is not at the boundary and stays at its input.
This also applies to a `%targets(…)` expansion (`%never + @N + …`) that
follows an `@` alternative, as in `(@ + b)/%targets(@N)`. `%float` and `%unanchored` are **not** supported.

**Command policy (from the branch).** Query commands append `/@` before
compiling. Construction (`mk`) does not. The engine never adds defaults
itself.

**Directives.** Every command that takes a path (`cd`, `ls`, `mk`,
`mk --allow-orphans`, `mg`, both arguments of `tag`/`t`, `rm`, `rmnf`,
`at`, `mv`, `cp`, `rn`/`cl` with or without `--all-parents`) accepts
directives. `dd`, `text` and `desc` take a label, not a path. A command
keeps its paths unexpanded until it runs, then expands each one
(`Graph.Command.expandDirectives`), left to right, before it does anything
else:

- `%history(n)` / `%last` become `@N` for the location `n` steps back in the
  history *at that moment*, so in `cd a; cd %last` (one line) the `%last`
  is the location before `cd a`.
- `%targets(p)` becomes `%never + @N1 + @N2 + …` for the current targets of
  `p`, queried from the current location. It is a union, so in `mk` each
  target is one alternative: with `0 -p-> 1`, `mk x/%targets(p)` creates
  `0 -x-> 1`, but with `0 -p-> 1` and `0 -p-> 2` it is ambiguous (two
  repairs, nothing changed). `mk #t/""/%targets(q)` and
  `tag #t %targets(q)` tag `q`'s target. Likewise, as an `rn`/`cl`
  destination prefix, `%targets(p)` with several targets is a `+` union,
  so `rn a %targets(p)/z` reports several alternatives rather than
  `SeveralParents`. When `p` has no targets the expansion is just
  `%never`; if the command then fails, its error ends with a note naming
  the empty `%targets(…)` and its position.
- A URL (`https://…`) imports the resource and becomes its `@N`. The import
  happens while the command's paths are expanded, so it stays even if the
  command then fails (e.g. an ambiguous `mk`). Each distinct URL is fetched
  at most once per command line, however many times it is expanded (in
  `at`, repeated across `;`, or by `wget`); later uses reuse the first
  import's `@N` while that node still exists (`mk x/URL ; rmnf x ;
  mk y/URL` imports it again). A URL extends to the next space, so one
  followed by `;` (or `)`) needs a space before it: `mk x/URL ; ls`.
  A command line that fails to parse never runs, so it never imports
  anything.
- `%{…}` splices only work in the path quasiquoter. A command line
  containing one is rejected when it is parsed (`SpliceInCommand`, naming
  the splice and its position), so nothing on it runs: `mk z; mk q/%{foo}`
  does not create `z`.

`at p cmd` expands `p` once, then runs `cmd` at each target with the
location set to that target, so `cmd`'s directives are expanded per
target: `at * mk z/%targets(@)` adds a `z` loop on every child. The
history is not changed by `at`: it does not record the targets, so
`%history(0)` inside `at` is the outer location, not the target, and
`%last` is the location before it. Each target starts from the history
`at` was given, and `at` restores it afterwards, so a `cd` inside
`at (a + b) {cd k; mk y/%last}` neither affects the other target's `%last`
nor the location after `at`. Only completion (`Models.LegacyPartialPath`) still rejects
directives at parse time (`IllegalDirective`).

## 3. Selection

| consumer | derived from |
|---|---|
| `rn`, `mv`, `cp`, `cl` target edge | last hop of route 0 |
| preview / leftmost path | all hops of route 0 |
| tags, FileType ("final edges") | last hop of each route |
| targets | route endpoints |

- If route 0 has zero hops (`rn (@ & a) c`), it is an error, as in the
  branch's goldens.
- The `rn`/`cl` destination creates one edge: its prefix (everything
  before the final label) must resolve to a single parent node. With
  `0 -c-> 3`, `0 -c-> 4`, `rn a c/z` is an error (`SeveralParents`) that
  names the parents and changes nothing; pin one (`c/@3/z`) or pass
  `--all-parents` (`rn --all-parents a c/z`) to create the edge under
  each parent, deleting the original once. `mv`/`cp` likewise need a
  destination that resolves to one node.
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
   `a/b/@5 & c/b/@5` is ambiguous. Edges are compared in stored
   orientation, so `l & k/~l/@1` shares the one new node both `l` hops
   need (`1 -l-> x`). Such a mixed pair (one hop taken from the shared
   end, one taken into it) counts as from the right.
   *Deviation:* the agreed wording ("may be identified, minimality
   chooses") made `a/b & a/c` ambiguous, because the shared and separate
   patches are incomparable modulo renaming; making left identification
   mandatory matches every example and the "right is weaker" decision.
5. **Pins.** A fresh vertex may also be identified with an existing node
   (the current node, a `Fixed` node, even one `mk` creates, or a
   variable's evidence binding) iff that makes two pattern edges that must both be created
   the same patch edge, compared as in 4. This **pin** is always an
   alternative, from either side: the candidate without it is kept. A pin
   is allowed only if the node passes the test of every variable in the
   fresh vertex's class, so `a/!{@2} & a/@2` never pins. A pinned
   variable is bound to the node; evidence (2) continues from it, and the
   identifications in 4 are redone. A candidate is kept only if every pin
   still makes a created hop the same patch edge as one at its node
   (directly, or through a pin justified that way), since the
   identifications that led to it may not be redone. If the shared edge
   already exists, the binding is ordinary evidence instead. Examples, with node 2 existing
   and no edges: `mk a & a/@2` is ambiguous between creating `C -a-> 2`
   and `C -a-> new1` or only `C -a-> 2`; `mk a/b & a/@2/c` is ambiguous
   between hanging `b` under a new `a` node or under 2; `mk a/~a` is
   ambiguous between `C -a-> new1` with `new2 -a-> new1`, or only
   `C -a-> new1` (pinning the second source to `C`). With `2 -b-> 3`,
   `mk a/b & a/@2` offers creating only `C -a-> 2` (the pin, then left
   evidence for `b`) besides the unpinned repair.
6. **Labels.** A `*`/regex `EdgeId` whose edge must be created gets its
   label from the caller's callback only; there is no graph-wide reuse.
   `mk` supplies no labels. A label the chooser does not give (or gives
   but the matcher rejects) is **missing**. Selection runs with a missing
   label as a stand-in, `Missing matcher`, distinct from every real label
   and equal for every edge with the same matcher. This models a chooser
   that returns one fresh label per matcher (the label callback itself is
   called per edge and per binding, so it need not behave that way).
   If the path already resolves, the result is the no-op as usual.
   Otherwise, if any **survivor** of selection uses a missing label, it is
   an error and nothing is changed: `mk` does not fall back to the other
   survivors or alternatives. The error names the matcher and lists the
   survivors (at most five, as for ambiguity) as resolved routes, with the missing label shown as its
   matcher (`-*->`), and which survivors need it; with two or more
   survivors it also says that the path is ambiguous even with a label.
   So `mk a + *` and `mk * + a` error rather than create `a`, and so does
   `mk a/*/@5` when `a` leads to a node without a `*` edge to 5, rather
   than taking right evidence.
   A candidate that needs a missing label is still eliminated like any
   other, and then the label never matters: `mk a + a/*` creates `a`
   (the `a/*` patch is a strict superset), and `mk * & a` from a node with
   an edge creates only `a` (left dominance).
   A missing label never makes a label-free candidate lose, so label-free
   candidates are selected exactly as with a fresh label:
   - *Dominance.* If c' extends c, every edge c' must create, c must too:
     an edge between vertices bound in both is the same, and an edge at a
     vertex c leaves fresh is new in c. So if c' needs a label, so does c.
   - *Minimality and dedupe.* A stand-in edge is in no label-free patch,
     so a label-needing patch is never contained in, or equal to, a
     label-free one.
   The rule is: without labels, `mk` either does exactly what it would with
   a fresh label, or errors when that repair would use the label. A
   chooser that supplies labels is unaffected. The test suite checks this
   against a fresh-label chooser over the whole corpus.

**Left-justified.** `Current` and `Fixed` are left-justified. So is any
variable reached from a left-justified vertex by an existing edge taken in
route traversal direction (transitively; a join bound by one route's left
walk counts). Every other evidence-backed binding is right-justified. A pin
is never left-justified (like right evidence), but a variable it reaches by
an existing edge is left-justified relative to it.

**Selection.**
1. If any alternative already resolves, the result is the no-op.
2. **Left dominance.** Drop σ if another candidate extends it and every
   additional binding is left-justified. Right-justified bindings and pins
   never dominate the fresh alternative.
3. Drop candidates whose patch is a strict superset of another's, modulo
   renaming of fresh nodes, unless the other has a pin this one lacks: a
   pin never eliminates a repair without it. Deduplicate equal patches
   modulo renaming. The exception is what keeps the unpinned repair: a
   pin turns a fresh node into an existing one, usually leaving a subset
   of the unpinned patch (`mk a & a/@2`: `{C -a-> 2}` ⊂
   `{C -a-> 2, C -a-> new1}`). It holds whatever else differs, so other
   evidence bindings (`mk a & b & b/@3` with `1 -a-> 1`, `1 -a-> 2`),
   left evidence (`mk b/a & b/@2/a` with `1 -b-> 1`) and other
   alternatives (`mk (b/a & b/@2/a) + (b & b/@2/a)`) all stay ambiguous.
   Right evidence needs no such exception, since existing nodes are never
   renamed. The cost is some redundancy: `mk (a & a/@2) + a/@2/b` also
   lists `a/@2/b`, a superset of the pinned `{C -a-> 2}`. Unpinned
   candidates eliminate as usual: `mk a/@2 + (a & a/@2)` creates only
   `C -a-> 2`.
4. **Order the survivors:** a left-only candidate (no right-justified
   bindings, pins, or right identifications) first; then by patch size
   (created nodes + created edges); then a deterministic tie-break on
   concrete routes.

**`mk`.**
- One survivor: apply it.
- More than one: error without changing anything. List the survivors in
  the order above, each as its **resolved routes** (existing nodes as
  `@N`, created nodes as `newK`) plus its patch, and suggest pinning a
  node with `@N` or excluding one with `!{…}` in the path. Only the first
  five are listed, followed by how many more there are: independent pins
  multiply (`mk a/~a & b/~b & c/~c` has 8 repairs).
- **Suggested paths.** Each listed repair is followed by `use: mk <path>`
  (or the `tag` command) when a path that selects it is found
  (`Models.PathPattern.Suggest`). The candidate keeps only the `+`
  alternative the repair came from. `compileAlternatives` compiles the
  path once more, as `compilePath` does, but keeping with each expression
  a choice of one side of every `+` that gives it, so the choices for all
  listed repairs come from that one compilation (stopping once they are
  found).
  It then rewrites each step (with `~`
  pushed to the steps) from the repair's binding of the step's output:
  bound to an existing node `N` (by evidence or a pin), the step becomes
  `step/@N`; created, it becomes `step/!{@N…}` excluding the nodes other
  repairs of the same alternative bind that output to. Rewrites that turn
  out not to be needed are dropped one at a time, in written order.
  Sequences are written left-associated, labels unquoted where possible,
  `@<tags>/t` as `#t`, a regex between quotes it does not contain, and
  NIDs in full. A candidate is
  shown only if its text parses back to it and residuating it with the
  same node lookup gives exactly one repair, equal to the listed one modulo
  renaming of created nodes. Otherwise the
  repair is listed without one. Examples: with `0 -a-> 1`, `0 -a-> 2`,
  `mk a/b` suggests `a/@1/b` and `a/@2/b`; `mk a + b` suggests `a` and `b`;
  with `6 -b-> 5`, `mk a/b/@5` suggests `a/!{@6}/b/@5` and `a/@6/b/@5`;
  with node 1, `mk a & a/@1` suggests `a/!{@1} & a/@1` and `a/@1 & a/@1`.
  The suggestion is a command to type at the prompt afterwards, so it does
  that repair there. A repair that creates orphans is suggested as
  `mk --allow-orphans …` (`tag` has no override, so it gets none). When the
  command ran at another node than the prompt's (inside `at`), the
  suggestion is prefixed with `at @N`: `at n mk a/b` suggests
  `at @3 mk a/@1/b`. (The one exception is a block that `cd`s to the `at`
  target itself before `mk`, since that is indistinguishable from running
  at the prompt.)
  There is no suggestion when the repairs differ only in identifying
  created nodes (`mk a/b/@5 & c/b/@5`: surface syntax cannot name a created
  node shared by several routes, nor forbid sharing one), or when
  distribution copies a step into routes that bind its output differently
  (`a/(a & b)`), since pinning the one surface step pins every copy. These
  are the deviation from the agreed design, which printed a disambiguating
  path for every repair. Over the test corpus, 4049 of 4109 listed repairs
  get one. The cost is bounded: one compilation of the path per error,
  which costs about as much as the one residuation did (compiling is
  exponential in the number of written `+` choices either way), and for
  each of the five listed repairs at most 16 residuations of paths
  without `+` (rewrites beyond that are kept rather than tried for
  removal). With `0 -a-> 1`, `0 -a-> 2`,
  `mk a/b/(@1 + @2 + @)/…` with 12 such factors takes about 8s with
  suggestions, against about 3s without (`ls` of it takes about 3s).
  The label error (6) lists no suggestions.
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
- A `rn`/`cl` destination prefix that resolves to several parent nodes is
  an error (`SeveralParents`) unless `--all-parents` is given, in which
  case the edge is created under every parent (the fan-out that used to be
  the default; §3).
- The orphan override is `mk --allow-orphans`.
- `rm` errors on a zero-hop route 0, like `rn`/`cl`/`mv`/`cp`.
- A `rn`/`cl` destination that matches through several `+` alternatives
  is an error rather than creating an edge for each.
- `tag p q` (as on the branch) links the outputs of the first witness of
  `p` to the target; other witnesses are not linked.
- `mk` also creates the current node if it does not exist (it is a bound
  vertex like `Fixed`).
- Golden format: an optional first section `==========graph==========`
  with lines `0 -a-> 1` (edges between `smallNID`s; node 0 is the origin)
  (the label may be a quoted string, `0 -"a b"-> 1`), `2` (a node without
  edges) and `1 data "text"` (node data, a Haskell string literal). Tests with a
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
- [x] An independent subagent, with a fresh context rather than a fork,
      reviewed the finished commits against this spec for completeness and
      test sufficiency. Its eight findings (mixed-direction identification,
      `rm` on a zero-hop route, label errors naming the step, the
      unreachable-variable error, `Show` syntax, fetch tests, weaker tests,
      and nits) were fixed in "Address the independent review", and a
      second fresh reviewer confirmed the fixes; its follow-ups (mixed
      small/large NID printing, equal union destinations, three more tests)
      are fixed in the same commit. A mixed-direction pair that does not
      shrink the patch stays an alternative (`l/m & k/~l/@1` is ambiguous).

## 8. Deferred and out of scope

- **`%unanchored`.** When it returns: a `Var` root, root candidates from
  `nodeManifest`, the no-bound-neighbour case in candidate generation, and
  an isolated vertex binding to any node that passes its test.
- **Positive node predicates** (for example, "tagged `#foo`") on variables
  would make right evidence necessary for completeness. Revisit evidence
  rules if they are added.
- **Picking among ambiguous repairs** (`mk --choose N`).
- **A label → edges index** for performance.
