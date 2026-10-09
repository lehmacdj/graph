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

1. Expand directives and lower `+` (`handleDirectivesWith`; see
   *Directives* and *Unions* below). The compiled `Path` (the `Prenormal`
   phase) has only `|` and `++`: `+` is unrepresentable there.
2. Apply `~` on the surface, using `main`'s semantics (a true inverse):
   `~(p/q) = ~q/~p`, `~(p & q) = ~p & ~q`, and `~` distributes over `|` and
   `++`; node specs are unchanged and each step's direction flips.
3. **Chains.** Every shared union (`++`) is chosen first, once for the
   whole path; what follows is done for each choice. The rest of the path
   is read as *chains*. A chain is a sequence at one nesting level. Its
   segments are steps, joins (`@`, `@N`, `!{…}`), groups (`p & q & …`,
   each conjunct a chain of its own) and relational unions (`p | q`, each
   side a chain that is read in the union's place once it is chosen).
   Parentheses without `&` make no chain of their own, `(p & q) & r` is
   one group of three conjuncts, and joins right after one another are
   one join with both tests.
4. **Routes.** A route goes through a chain choosing one conjunct of
   every group it comes to. What two routes share is decided by the copy
   of each step and join they go through (*Sequencing* below): every
   step and join on a route has a name, made of where it is written and
   of the choices that copy it.
5. **Glue.** Steps with the same name are one edge, with one `EdgeId`,
   to one fresh `Var`. The routes that go through joins with the same
   name meet there (taking the meet of their tests). Relative routes
   start at `Current`, and a route that starts with an `@N` starts at
   `Fixed N`.
6. Drop alternatives with contradictory tests, and canonicalize.

**Sequencing.** This is the definition of `/`, `&` and the joins
(*chain-scoped joins*); `annotate` in `Models.PathPattern.Compile` is
its implementation.

0. *Normal form first.* `+` is lowered and every `++` chosen before
   anything below applies. `&` associates: nested groups are one group.
   A conjunct that forks without a join of its own is the conjuncts
   distribution makes of it: `a/(b & c) & d` is `a/b & a/c & d`. A join
   in its own chain is the only thing that keeps a conjunct together as
   one chain, which is then copied whole (1); a leading `@` is such a
   join (2). Not every path that has a normal form is run: one in which
   the product (1) would copy a meeting point is refused (*Meeting
   points* below).
1. *Adjacent groups are their product.* When groups follow one another
   in a chain with only steps between them (a *stretch*: a chain is cut
   into stretches by its joins), each choice of a conjunct in every one
   of them is a path written apart:
   `(a & b)/(c & d) = a/c & a/d & b/c & b/d`, four `a`-or-`b` steps and
   four `c`-or-`d` steps, and `a/(b & c) = a/b & a/c` with two
   independent `a`. Every conjunct is copied whole, with the joins and
   the `|` unions it has: `(x/@/w & b)/(c & d)` is
   `x/@/w/c & x/@/w/d & b/c & b/d`, with two `x`. Each term is a chain
   like any other, made of the stretch with each group replaced by the
   chosen conjunct: the joins of the conjuncts cut it, and the groups in
   its stretches are multiplied out again.
   This is literal for a conjunct without a step too. A conjunct that
   is only `@` is the join it is in `@/…` and `…/@`: `(a & @)/(@1 & d)`
   is `a/@1 & a/d & @/@1 & @/d`, where `@/@1` tests that the current
   node is `1`. So such a conjunct can find less than it did before
   chain-scoped joins: the product of `q/(@ & b)/((c & d)/@ & e)` has
   the term `q/@/(c & d)/@`, in which `c` and `d` leave from one `q`
   target, where each had a `q` of its own. (That path itself is now
   refused, since `(@ & b)` would copy `(c & d)/@`, where `c` and `d`
   meet; its product written out is run. See *Meeting points*.) There
   is no way to write a conjunct without a step that does not join.
2. *A join governs the chain it is written in.* A join is `@`, `@N` or
   `!{…}`.
   - Arriving: the routes that arrive along the join's chain meet at
     it. `(p & q)/x/@/w` has one `x` target for both. A join inside a
     conjunct is in a chain of its own for every copy of the conjunct,
     and merges only there: `(p & q)/(x/@/w & y)` is
     `p/x/@/w & p/y & q/x/@/w & q/y`, two joins, each with one route.
   - Leaving: a join ends the stretch. What is before it is not copied
     by a group after it in the same chain, so every continuation
     leaves from the one join: `x/@/(c & d)` and `x/@/w/(c & d)` have
     one `x` (and, in the second, two `w`). A join inside a conjunct of
     an earlier group does not do that, since the conjunct is copied
     with it (1); a join of the chain between that group and the later
     one does: `(x/@/w & b)/k/@/(c & d)` has one node that `x/@/w/k` and
     `b/k` meet at and `c` and `d` leave from. To have one `x` for two
     continuations without merging `b` into it, write the join in the
     chain: `x/@/w/(c & d) & b/(c & d)`.
   - `@N` is a root only when nothing is sequenced before it in its
     written-out term: in `@N/a` and `a & @N` it is `Fixed N`, but in
     `@/@N`, `(@ | a)/@N` and `(a & @)/(@N & d)` it tests the node the
     route is at.
   - A leading `@` is a join like any other and is never left out: at
     the root of the path, right after another join, at the start of a
     conjunct or of a side. Where the input is one node already it
     merges nothing, but it is a join of its chain, so the conjunct it
     starts is one chain, copied whole, and the groups in it are not
     groups of the stretch around it (0).
     After a prefix that shows: `q/(@/(a & b) & c)` is
     `q/@/(a & b) & q/c`, one `q` for `a` and `b`, where
     `q/(a & b & c)` has three. With `0 -q-> 1 -a-> 9`, `0 -q-> 2 -b-> 9`
     and `0 -q-> 3 -c-> 9`, `ls` finds nothing for the first and `9` for
     the second. Where another group stands next to the conjunct's
     group, as in `(@/(a & b) & c)/(@/f & e)`, the product would copy
     the `@` that `a` and `b` leave, and the path is refused (*Meeting
     points*); its product written out,
     `@/(a & b)/@/f & @/(a & b)/e & c/@/f & c/e`, is run, and in its
     first term `a` and `b` meet before `f`, where
     `(a & b & c)/(@/f & e)` has `a/@/f` and `b/@/f` apart. Where
     nothing forks between the `@` and the next join of its chain it
     changes nothing: `@/a` is `a`, also as a conjunct, and
     `@/(a & b)/(c & d)` at the root is `(a & b)/(c & d)`.

The routes of a path are listed as its product written out lists them:
conjunct by conjunct, the first choice varying slowest
(`(a & b)/(c & d)`: `a/c`, `a/d`, `b/c`, `b/d`), the routes through a
join being every route up to it with every route from it on.

**Meeting points.** The product (1) copies a conjunct whole, with the
joins in it. Where such a join is a node that several walks meet at or
leave from, the copies have no clear meaning (do the walks of one copy
meet those of the other?), so a path in which that would happen is
*refused*: every command (`ls`, `cd`, `mk`, `rm`, `mv`, `cp`, `rn`,
`tag`, `at`, `mg`, and the query of a `%targets(…)`) fails before it
does anything with the path, with an error that names the conjunct, the
join and the group that would copy it, and says what to write instead
(`CopiedMeetingPoint`). The whole path is refused, not only the part of
it that is unclear. What has happened by then stays: the directives of
the path are expanded before it is checked, so a URL in it is imported
already, and the commands before this one on the line have run (the
error says that this command was not run, not that nothing was). `checkCopies` in `Models.PathPattern.Compile` is the
check, and `Graph.Command.checkPath` runs it where a command expands its
path; `compilePath` itself still compiles every path, which is what the
laws below are about.

The rule is about the product written out (equivalently, about the
routes of each alternative the path compiles to):

- A `/@` or `!{…}` that is written once has a *copy* for every term of
  the product it comes to stand in: `(x/@/w & b)/(c & d)` is
  `x/@/w/c & x/@/w/d & b/c & b/d`, two copies of the `@`.
- A copy is a *meeting point* when more than one walk comes into it or
  more than one goes out of it. In `(a & b)/((c & y)/@/e & d)` =
  `a/(c & y)/@/e & a/d & b/(c & y)/@/e & b/d`, `c` and `y` come into
  each copy of the `@`. A walk that an `@N` has already made one is one
  walk: in `(c & y)/@4/w/@/e` one walk comes into the `@` and one goes
  out.
- A path is refused when some `/@` or `!{…}` written once has two
  copies or more and at least one of them is a meeting point, in any
  alternative: any way for the copies of its `|` unions to take their
  sides, any choice of its `++` unions.
- An `@N` is never one: all its copies are node `N`.

By hand: write the product out, or picture it. If an `@` or `!{…}` that
you wrote once appears more than once, and in some copy more than one
walk comes into it or goes out of it, the path is refused.

Copies are counted as they are written, not as they end up: the copies
of an `@` that starts a stretch are all the node the stretch starts at,
and `(@/(a & b) & c)/(d & e)` is refused all the same (the designer's
ruling; counted as nodes of the compiled pattern it would be run, and
it is the one kind of path on which the two ways to count differ: an
`@` whose copies are one node because the stretch starts there, or
because an `@N` stands right next to it).

How `checkCopies` decides it, without writing the product out. When is
a `/@` or `!{…}` copied, and when do several walks come into a copy?

- *Copied.* A conjunct is copied when its group has another group as a
  neighbour in its stretch: only steps and unions between the two, no
  join of their chain (`@`, `@N` or `!{…}`). Each conjunct of either
  group is then copied once for every conjunct of the other, with
  everything in it, also the conjuncts inside it. A conjunct without a
  join of its own is no chain apart (0), so the groups in it stand where
  its own group stands: in `((p & q)/x & z)/(K & d)` both `(p & q)` and
  the group it is in are neighbours of `(K & d)`, and in
  `(m/(K & z)/(e & f) & d)` the group `(e & f)` is a neighbour of
  `(K & z)`.
- *Several walks, from its own conjunct.* The conjunct has an `&` at its
  own level in the stretch right before the join or right after it,
  with no other join of the conjunct between the two (not inside a
  conjunct of that `&`, but possibly inside a side of a union, read in
  the union's place). In `(c & y)/@/e` the walks `c` and `y` come in;
  in `x/@/(w & v)` and `@/(a & b)` two go out; in `c/@/w/(e & d)` two
  go out through `w`.
- *Several walks, from the conjunct it is copied for.* The join is the
  first join of its conjunct (nothing of the conjunct keeps what is
  before it out), and a conjunct of the neighbouring group before it
  has a join of its own and an `&` after its last join: it holds
  several walks together and brings them along. In
  `(@1/(c & e) & b)/(x/@/w & d)` the product has the terms
  `@1/(c & e)/x/@/w` and `b/x/@/w`: two copies of the `@`, and in the
  first `c/x` and `e/x` come into it. Likewise for the last join of a
  conjunct and a conjunct after it with an `&` before its first join:
  in `(a/@ & b)/((c & e)/@1/f & d)`, `c` and `e` go out of the copy of
  the `@` in `a/@/(c & e)/@1/f`. A conjunct without a join of its own
  brings nothing along: `q/(c & e)` is `q/c & q/e`, and each of its
  walks has a copy of its own.

The common case, as a quick test (a shortcut, not the definition):

1. Find two `&` groups with nothing but steps and unions between them:
   no `/@`, `@N` or `!{…}` of the chain they are written in. (A group
   inside a conjunct that has no join of its own counts as standing
   where that conjunct's group stands. A side of a union with an `&` at
   the end that faces the other group counts as a group; a join of the
   side between its `&` and that end keeps it from counting.) No such
   pair: the path is run.
2. In either group, find a conjunct with a `/@` or `!{…}` and an `&`
   next to it (in the same conjunct, at its level, no join between
   them). Found: the path is refused.
3. Not found: the path is run, unless a conjunct of one group holds
   walks together behind an `@N` and a conjunct of the other has a `/@`
   or `!{…}` facing it, which is the case the quick test misses:
   `(@1/(c & e) & b)/(x/@/w & d)`. Write those out to see.

With `K = (c & y)/@/e` (the expanded forms are the product written out,
which is what the compiler makes of the path and what the error says to
write):

| path | | why |
|---|---|---|
| `(a & b)/(K & d)` = `a/K & a/d & b/K & b/d` | refused | the `@` of `K`, where `c` and `y` meet, is copied for `a` and for `b` |
| `(x/@/(w & v) & b)/(c & d)` = `x/@/(w & v)/c & x/@/(w & v)/d & b/c & b/d` | refused | its mirror image: `w` and `v` leave the `@`, copied for `c` and `d` |
| `(@/(a & b) & c)/(@/f & e)`, `(x & y)/(@/(a & b) & c)` | refused | two walks leave the leading `@` |
| `(a/@/(b & c) & d)/(e & f)`, `(p & q)/(a/@/(b & c) & d)` | refused | same |
| `(@/(a & b) & c)/(d & e)` | refused | same, although every copy of this `@` is the node the path starts at; this could be allowed later, as `@N` is |
| `(a/@/(b & c) & d)/@`, `(a/@/(b & c) & d)/@/(e & f)`, `(p & q)/@/(a/@/(b & c) & d)` | run | a join of the chain stands between the groups: nothing is copied |
| `z/(K & d)`, `q/(@/(a & b) & c)` | run | one group: the meeting point is not copied. The second is `q/@/(a & b) & q/c` |
| `(x/@/w & b)/(c & d)` = `x/@/w/c & x/@/w/d & b/c & b/d` | run | `x/@/w` is one walk; its `@` does nothing, and there are two `x` |
| `(p & q)/(x/@1/w & y)`, `(p & q)/(x/@1/(w & v) & y)` | run | `@N` is no meeting point |
| `(x/@ & b)/(c & d)` = `x/c & x/d & b/c & b/d`, `(a & b)/(@/c & d)` | run, with a warning | the `@` begins or ends a conjunct that is a plain walk: it does nothing, and looks as if it pinned |
| `(a & @)/(@1 & d)`, `((x \| y)/@ & z)/(c & d)` | run | no warning: a conjunct that is only `@` cannot be written without it, and a conjunct with a union is not a plain walk (below) |
| `(@1/(c & e) & b)/(x/@/w & d)` = `@1/(c & e)/x/@/w & @1/(c & e)/d & b/x/@/w & b/d`, `(q/@1/(c & e) & b)/(x/@/w & d)` | refused | `x/@/w` is one walk, but in its copy for `@1/(c & e)` the walks `c/x` and `e/x` meet at the `@` |
| `(a/@ & b)/((c & e)/@1/f & d)`, `(a/!{@7} & b)/((c & e)/@1/f & d)` | refused | the mirror image: in the copy for `(c & e)/@1/f`, `c` and `e` leave the `@` (or the `!{@7}`, which is no filter there) |
| `(@1/(c & e) & m)/((a & b) \| z)/(x/@/w & d)` | refused | the same through a union |
| `(q/(c & e) & b)/(x/@/w & d)`, `(@1/(c & e)/@2 & b)/(x/@/w & d)`, `(@1/(c & e) & b)/(x/@2/w/@/v & d)` | run | `q/(c & e)` holds nothing together (it is `q/c & q/e`); a join stands after the `&`; the `@` is not the first join of its conjunct |
| `(a & b)/((c & y)/@4/w/@/e & d)` | run | `c` and `y` are one walk after `@4`: one walk comes into each copy of the `@` |
| `(a & b)/(c/@/w/(e & d) & d)` | refused | two walks go out of each copy of the `@`, through `w` |

What looking only inside the conjunct missed (run on a build that did
only that). With
`1 -c-> 2`, `1 -e-> 3`, `2 -x-> 4`, `3 -x-> 5`, `4 -w-> 9`, `5 -w-> 9`,
`0 -b-> 6`, `6 -x-> 7`, `7 -w-> 9`, `6 -d-> 9`, `2 -d-> 9` and
`3 -d-> 9`, `ls (@1/(c & e) & b)/(x/@/w & d)` found nothing and
`ls (@1/(c & e) & b)/(x/w & d)` found `9`: the `@` on one walk was no
no-op. With `0 -a-> 2`, `0 -a-> 3`, `2 -c-> 1`, `3 -e-> 1`, `1 -f-> 9`,
`0 -b-> 4`, `4 -c-> 1`, `4 -e-> 1`, and `d` from each of `2`, `3` and
`4` to `9`, `ls (a/@ & b)/((c & e)/@1/f & d)` found nothing, with a
warning that the `@` did nothing, and `ls (a & b)/((c & e)/@1/f & d)`
found `9`. And `(@1/(c & e) & m)/((a & b) | z)/(x/@/w & d)` was the
question the rule exists to close (which copy is a walk at that took no
branch), on a path that was run.

What to write instead of a refused path: a join between the two groups
if the walks of the first should meet before the second,
`(a & b)/@/(K & d)`; or the copies written out, each conjunct for each
pair, `a/K & a/d & b/K & b/d`, where every `K` is in a group that stands
alone. The two differ: with `0 -a-> 1`, `1 -b-> 2`, `1 -c-> 2`,
`0 -d-> 3`, and `e` and `f` from each of `2` and `3` to `9`, the refused
`(a/@/(b & c) & d)/(e & f)` written out,
`a/@/(b & c)/e & a/@/(b & c)/f & d/e & d/f`, finds `9`, and
`(a/@/(b & c) & d)/@/(e & f)` finds nothing (the two ends are `2` and
`3`). The error gives both for the path at hand where the two groups
stand in one chain (one of them may be written inside a conjunct of a
group of that chain, as in `(p & q)/(x/(K & d) & b)`, for which it gives
`(p & q)/@/(x/(K & d) & b)` and `p/x/(K & d) & p/b & q/x/(K & d) & q/b`);
the written-out form only when the path has no union and few routes.

Unions. The rule is about what the product would copy, however it is
written, so a union is read every way it can be taken (the operators
are defined in *Unions* below):

- A `|` union is each of its sides written in its place, and also one
  step: two routes that chose different sides pass it as a step (rule 3
  there), so a `|` never keeps two groups apart, whatever joins its
  sides have. The path is refused when some way of reading its `|`
  unions is: `((a & b) | z)/(K & d)`, `(p & q)/(x/@ | s)/(K & d)`,
  `(x/@ | (p & q)/s)/(K & d)`, `((a & b) | (u & v))/(K & d)`,
  `((a & b) | @3)/(K & d)` and `(x/@/(m & n) & q)/((a & b) | z)` for a
  side in place; `(p & q)/(x/@ | w/@)/(K & d)` and
  `(p & q)/(@1 | @2)/(K & d)` for the union as a step (so
  `(p & q)/%targets(t)/(K & d)` is refused when `t` has two targets or
  more, and run when it has one). Equal conjuncts are not merged:
  `((a & a) | z)/(K & d)` is refused. Neither are sides: a side
  written twice is two sides (*Unions*, 6), so
  `(p & q)/(x/@ | x/@)/(K & d)` is refused as
  `(p & q)/(x/@ | w/@)/(K & d)` is (`p` and `q` may take one each),
  where `(p & q)/x/@/(K & d)` is run.
- Run: `(a | z)/(K & d)` and `(x/@ | s)/(K & d)` (no group next to
  `K`'s); `a/(@1 | @2)/(x & y)`, each copy choosing its node;
  `(x/@ | zz)/(c & d)`; `(p & q)/(x/@ | s)/(m/@/e & d)`, where `m/@/e`
  is one walk; `((a & b)/@ | z)/(K & d)`, where the side's own join
  stands between its `&` and `K`; and
  `((x | y)/@ & z)/(c1 & … & cn)`, with its `2^n` alternatives.
- A `++` union is the side chosen: the path is refused when one choice
  of its `++` unions is, since otherwise that choice would vanish
  without a word. `((a & b) ++ z)/(K & d)` is refused;
  `(p & q)/(x/@ ++ w/@)/(K & d)` is run.
- A `+` is checked as what it is lowered to for the command. Where it
  is `|` the path is refused exactly when it would be with `|` written;
  where it is `++`, as in `mk`, it is checked as `++`. The error shows
  the union as it is lowered and says so where the union it names was
  written `+` ("written with +, which is | in this command"), with `++`
  and `%override({plus = shared}, …)` as the way out where they are one;
  a `+` elsewhere in the path is not mentioned. Where the `+` is inside
  an `%override({plus = …}, …)` already, as in
  `mk %override({plus = relational}, (p & q)/(x/@ + w/@)/(K & d))`, the
  error says that the override makes it what it is ("which is | by the
  %override({plus = …}, …) around it") and that the way out is to say
  `plus = shared` in that override, not to wrap the path in another.
  Nothing falls
  back from one to the other: `ls (p & q)/(x/@ + w/@)/(K & d)` is
  refused and `mk` of the same path is not.
- So "adding a side never removes a result" (*Unions*) has an
  exception: adding a side can make a path refused.

An `@` that does nothing. A `/@` in a copied conjunct that is one walk
is run and does nothing, since each copy has its own walk through it:
`(x/@ & b)/(c & d)` has two `x`, where `x/@/(c & d)` has one. Where such
an `@` begins or ends the conjunct it looks like the pin it is in
`x/@/(c & d)` and `(a & b)/@/c`, and the command warns on stderr before
it runs (`JoinDoesNothing`). The warning says that the `@` can be left
out, so it is given only where that is certain, whatever is written
around the conjunct. Exactly when all of these hold:

1. The path is run (not refused).
2. The `@` is the first or the last segment of a conjunct of a group,
   and that conjunct is a *plain walk*: at its own level it has nothing
   but steps and `@`, and at least one step. So no `@N`, no `!{…}`, no
   union and no group in the conjunct. (`@/x`, `x/@`, `@/x/@`, `x/@/w/@`
   are plain walks; in the last only the `@` at the end is warned
   about.)
3. A product copies the conjunct: its group has another group next to
   it in its stretch (steps and unions between them, no join of their
   chain), in some way of reading the unions between them. A group in a
   conjunct without a join counts as a group of the stretch that
   conjunct's group is in (*Sequencing* 0), so the `@` of
   `(b & c)/(a/(@/x & b) & w)` is warned about.
4. No `|` union anywhere in the path has a join in a side (at the
   side's own level, through unions written in the side): no
   `(m/@ | n)`, no `(@1 | @2)`. A `++` union with a join in a side is
   no obstacle.

Why each condition is there (each excludes something that is no no-op,
or that is not known to be one):

- For 2: an `@` before an `@N` makes the `@N` a test of the node the
  route is at, where it would be a root (*Sequencing* 2), and a side of
  a union may begin with an `@N`. With `1 -b-> 5`, `5 -c-> 9`,
  `5 -d-> 9`, `0 -a-> 6`, `6 -c-> 9` and `6 -d-> 9`,
  `ls ((@/(x | @1/b) | y) & a)/(c & d)` finds nothing and
  `ls (((x | @1/b) | y) & a)/(c & d)` finds `9` (the warning was given
  for this path while it had fewer conditions). A conjunct that is only
  `@` (`(a & @)/(@1 & d)`) is a
  join that cannot be written out of the path. A conjunct with a group
  is copied whole only because of its join.
- For 4: where routes chose different sides of a `|` union with a join
  in a side, the routes of the alternative are listed in an order that
  goes by the joins they pass (*Unions*, 7), and it is not shown that
  an `@` that joins nothing never changes that order. The order is
  what a command acts on (§3), so no warning is given.
- In a plain walk of a path that is run and has no such union, the `@`
  can be left out: the check has made sure that no copy of the `@` has
  more than one walk coming in or going out; the steps of a plain walk
  are copied by the choices of its stretch with the `@` and without it;
  and routes are listed by the conjuncts they choose, which the `@`
  does not change.

The test for it (`spec_compileCopiedMeetingPoints`, "an @ that is
warned about does nothing"): on random paths with unions, `@N` and
`!{…}` anywhere, for every path that is warned about and every `@` in
it whose removal takes a warning away, the path without that `@`
compiles to the same alternatives, routes in the same order.

The warning names the conjunct and the group that copies it, and says
what to write for one shared node (`x/@/(c & d) & b/(c & d)`) where the
two are written in one chain and the `@` faces the group. It is given
once for a path in a command line (also when `at` runs the command at
many nodes), for every command, and not while completing with TAB (a
refused path completes to nothing). Not warned about, though they do
nothing either: an `@` strictly inside a walk (`(x/@/w & b)/(c & d)`);
an `@` whose conjunct is not copied itself but lies inside a copied
one (`(p & q)/(m/@1/(x/@ & z) & y)`); and most of the `@` that
conditions 2 and 4 leave out (`((x | y)/@ & z)/(c & d)`,
`(x/@ & b)/(m/@ | n)/(c & d)`). A `!{…}` is a filter and is never
warned about. Warning about every `@` that does nothing is left for
later.

What the check reads. It reads the path as written, with no union
chosen and no route listed: each group looks along its chain in both
directions, through the sides of the unions on the way, and into the
conjuncts of the groups it meets. So it takes at most the length of the
path times its depth for each group, whatever the path would compile
to: a path is refused without anything of it being compiled, however
large it is. It follows the path as the compiler reads it in two
respects: node tests written right after one another are one join
(`@/@1` is one join, which tests that the node is `1`, and is no
meeting point), and a side that is `%never` or has such tests that
contradict each other (`@1/@2`) is no side; every other side that is
written is a side of its own, also one written twice (*Unions*, 6). So
the check and the compiler read the same unions.

Refused though no meeting point would be copied apart (each is
something the designer may relax; none of them runs a path that should
be refused):

- The leading `@` of a forking conjunct whose group starts its stretch
  (`(@/(a & b) & c)/(d & e)`), or an `@` right next to an `@N` reached
  through a union side
  (`(p & q)/((m & n)/(x/@1 | y/@2)/@ & d)`): every copy is one node
  (above). That second case comes from reading a `|` union in a
  conjunct as one step, which is needed: with a step between the `@N`
  and the `@`, as in `(p & q)/((m & n)/(x/@1/w | y/@2/v)/@ & d)`, the
  routes through `m` and `n` may take a side each, and then `w` and `v`
  meet at the `@`, which is copied for `p` and for `q`. No side read in
  place shows that (the `@N` stands between the `&` and the `@` in
  each), and what the compiler makes of the path has that copied
  meeting point (a test).
- A side that cannot be chosen because its tests contradict something
  further away still counts as a side, and a conjunct that can never
  match (`@1/…/!{@1}`) is still read: where tests contradict each
  other the check refuses more than is compiled (in the tests: on
  paths without such tests the check and the compiled routes agree
  exactly; with them, what is compiled as a copied meeting point is
  always refused).
- A `|` union of exactly two sides, both with a join, between a
  conjunct that ends with an `@` and a group with a conjunct whose
  walks an `@N` holds together:
  `(p/@ & q)/(x/@ | w/@)/((c & y)/@1/e & f)` is refused, with
  `((c & y)/@1/e & f)` named as what would copy `p/@`, though the
  compiler lists 64 alternatives for it (62 once equal ones are
  merged) and in none of them is `p/@` two copies with a fork at one
  (a test, `spec_compileCopiedMeetingPoints`). The check reads the `|`
  as one step, and past one step the group does copy `p/@`. The
  review that found this reasoned, and did not prove, that with only
  two sides the route through `f` has to share a side with the route
  through `c` or the one through `y`, which the `@1` holds together,
  so that `p/@` stays one copy. With a third joined side,
  `(x/@ | w/@ | v/@)`, the copy is there (729 alternatives listed,
  the same test), so the refusal cannot simply be dropped for such
  unions. It is the same with a side written twice,
  `(p/@ & q)/(x/@ | x/@)/((c & y)/@1/e & f)`: refused, and nothing
  copied in what compiles. With a `++` for the `|`, or with one side
  (`x/@`, a join of the chain), the path is run.

Measured (the scratch commands are in `cli/tmp/impl-mp2/h/`, which is
not part of the repository: `runall.sh` and `runev.sh` run them and
`measured.py` prints the numbers. `t` is a tool built against this
compiler and `hb/tb` the same tool built against the state before
every written side became a side of its own and before the warning was
narrowed, commit `31ebc6db`. The path sets are the design's,
`cli/tmp/impl-mp/qpaths.txt`, `k0paths.txt`, `k1paths.txt` and
`k2paths.txt`, and the second review's, `cli/tmp/review-mp2/h/sets/`.)
"Before" is that earlier state:

| paths | refused before | refused | warned before | warned |
|---|---|---|---|---|
| 590 distinct paths of the tests of the compiler, the matcher, residuation and suggestions, as they were before the rule | 16 | 16 (2.7%) | 38 | 13 (2.2%) |
| 13,819 random without unions | 1,349 | 1,349 (9.8%) | 1,587 | 1,199 (8.7%) |
| 35,958 random with a `\|`, no node tests | 2,446 | 2,451 (6.8%) | 3,989 | 530 (1.5%) |
| 15,491 random with a `\|` and node tests | 1,658 | 1,658 (10.7%) | 2,025 | 161 (1.0%) |
| 355,994 random of the second review, nine profiles, stretch-heavy | 61,872 | 61,928 (17.4%) | 90,240 | 39,154 (11.0%) |
| 299,992 of that review with a meeting point planted | 160,858 | 160,858 (53.6%) | 71,791 | 29,416 (9.8%) |
| 33,632 of that review made for sides (unions of a side written several ways) | 2,838 | 2,848 (8.5%) | 6,983 | 247 (0.7%) |

(`hb/tb tab` and `t tab` on each set. Random paths are far more tangled
than written ones.)

- Verdicts against before (`measured.py`, with `t4 dup` to tell which
  paths have a union with a side written twice): every path that was
  refused is refused. 71 paths that were run are refused now (5, 56
  and 10 in the third, fifth and seventh rows), and each of them has a
  union with a side written twice, which the check read as one side
  (as in `(p/@ & q)/(c/@ | c/@)/((d & e)/@ & f)`). No other verdict
  changed.
- Warnings against before: every path that is warned about was warned
  about before, but for one, `(w/@ & @3)/((@3 ++ @3)/@1 & *)/@`, which
  compiles to nothing with the `@` and without it (the two `@3` were
  one side, right before an `@1` that contradicts it, and the path
  had no chain to read).
- The verdict is the same for a path and its reverse on all 292,129
  paths it was tried on (`t5 tabr`: the first four sets and seven of
  the review's).
- The check and the rule worked by writing the product out
  (`handRefuses` in the tests) agree on 7,000 random and planted paths,
  of which 1,770 are refused (`cost/hand 20261102 3500`). The quick
  test above misses 1,098 of those and refuses 146 that are run; these
  generators are made to hit the rule and say nothing about how often
  that happens.
- The check and the compiled routes (same command; `copiedJoins` in the
  tests: every `/@` and `!{…}` marked, a join flagged when the routes
  of an alternative go through it under two names or more and at one
  of them arrive by different steps or leave by different steps): of
  the 6,299 of those paths that were within a limit of 300 alternatives
  (the limit on alternatives that there was then,
  `docs/path-cost-limits.md`), 1,356 are flagged, and the check differs on 39, each refused and not
  flagged; those looked at have node tests that contradict each other
  (`@1/…/!{@1}`), so that the alternative the check reads is not
  compiled. No path that is run is flagged.
- The second review's two evaluators, which share no code with the
  check (`ev chk 300`: its own product expander, each union read with
  a side in place; and the compiled routes with every `/@` and `!{…}`
  marked): of that review's 688,000 paths (388,000 random in nine
  profiles and 300,000 with a meeting point planted), 222,832 are
  refused and 465,168 are run, and neither evaluator flags a path that
  is run. For 70,552 of them the compiled routes are over that
  evaluator's limit of 300 alternatives, and only the first spoke
  (45,241 before: sides written alike now multiply the ways to
  choose); 25,921 of those are run. (With the limit at 1,500, tried
  on 7,111 of those: 3,098 compile within it and none is flagged; the
  run was stopped there.) Of the refused paths within the limit, 1,836
  are flagged by neither evaluator; each has an `@N`, as the
  over-refusals listed above do.
- A warned `@` does nothing (`t idle`, all the sets above and the
  24,609 paths that review found warned): every `@` whose removal takes
  a warning away was taken out, 106,107 times. 101,465 times the path
  compiles to the same alternatives in the same order, 174 times to
  the same alternatives listed in another order (the routes of each in
  the same order; in those looked at the `@` was in a side of a union,
  and sides are taken in an order that goes by what they are made of),
  and never to anything else; 4,468 were over a limit of 400
  alternatives and were not compared. With the conditions the warning had
  before, 6 of the 24,609 compiled to something else, each with a
  union in the conjunct of the `@`.
- What condition 4 of the warning leaves out (`t2 idlex`: the paths
  that would be warned about without it), on sixteen of the sets: an
  `@` that only condition 4 keeps a warning from was taken out 28,399
  times. 20,530 times the path compiles to the same alternatives, and
  never to anything else; 7,869 were over the limit of 400 and were
  not compared. So no path is known on which such an `@` does
  something. The condition stands because the argument for the warning
  does not cover the order of routes there, not because of a
  counterexample.
- An earlier measure (`ref sound` of `cli/tmp/impl-mp/`: no alternative
  puts together two routes that chose alike beyond what the sides in
  place give) was not run again. As measured then it held on all
  31,820 paths with a `|` and no node test that are run, and failed on
  19 of the 12,678 with node tests: all through an `@N`, as in
  `((a & b) | z)/((c & y)/@1/e & d)`, where the `e` after `@1` is one
  step for the copies made for `a` and for `b`. No `/@` or `!{…}` is a
  copied meeting point in them; whether such a path can find something
  that the copies written apart do not is not known.
- Cost (`cost/cost`, optimised build, a machine doing other work): the
  check takes 4 to 16 microseconds a path on the first four sets and
  on the first review's 46,000, the same as before (for instance 15.7
  against 16.7 on the 46,000). Every path with a group is looked at the
  same way; there is no slower path to fall into, and a path without a
  group is only read. The work is at most quadratic in the length of
  the path: the worst case found is `n` groups in one stretch, each
  with a conjunct `x/@` (each looks along the whole stretch): about
  50 ms for 320 groups (960 steps and joins). Forty `++` unions of
  joined sides before a meeting point (`2^40` choices), 320 `|` unions
  along one route, and nesting 320 deep each take under 20 ms.
  Compiling no longer compares sides, so nothing is compiled before a
  command but what the command runs.

Laws of sequencing (for paths without `|`; each is a test in
`Models.PathPattern.CompileSpec`). They are laws of what `compilePath`
makes, also of a path that is refused; where one of the paths in a law
is refused, the law says what its product written out means:

- *`/` associates*: a sequence is its flat chain, whatever its
  parentheses.
- *Product* (`spec_compileProduct`): `(A & B)/(C & D)` is
  `A/C & A/D & B/C & B/D` for conjuncts with joins, with `|` unions and
  without a step, anywhere in a path. It is stated after 0: a conjunct
  that forks without a join is several conjuncts first (`&` associates,
  and `a/(b & c)` in a group is `a/b & a/c`), and a `++` is chosen
  before. The routes are in the same order when no conjunct has several
  routes. It holds for a conjunct that starts with `@` and forks, since
  the `@` is kept (2): `(@/(c & d) & a)/(@/e & y)` has
  `@/(c & d)/@/e`, which merges `c` and `d`. For a conjunct with a
  meeting point the left side is refused, and the law says that the
  right side, which the error tells to write, is what it compiled to;
  it is a statement about paths a command runs only for conjuncts
  without meeting points.
- *Reversal* (`spec_compileReversal`): for a relative path `P` without
  an `@N`, node `t` is among the targets of `ls P` from node `c`
  exactly when `c` is among the targets of `ls ~P` from `t`. It is a
  statement about whole queries, which are sealed at both ends: every
  route of a relative path starts at the current node, as if `@/` were
  written before the path, and `ls` appends `/@`; reversing swaps the
  two. It does not hold with an `@N` (which is a root one way and a
  test the other: `ls @1` from `0` finds `1`, and `ls ~@1` from `1`
  does not find `0`), and it is not a statement about pieces of a path
  with their leading `@` left out: the reverse of a conjunct that forks
  and then joins, `(a & b)/@`, starts with an `@` and forks, and is the
  converse only because that `@` is kept (2). With `0 -a-> 1`,
  `0 -b-> 3`, `0 -c-> 2`, and `q` from each of `1`, `2` and `3` to `9`,
  `((a & b)/@ & c)/q` finds nothing from `0` (`a` and `b` end apart),
  and `~q/(@/(~a & ~b) & ~c)` finds nothing from `9`; without its `@`,
  `~q/(~a & ~b & ~c)` finds `0`. A path is refused exactly when its
  reverse is (`spec_compileCopiedMeetingPoints`), so the law loses no
  path on one side only: `(x & c/@)/((c & d)/@ & a)` and
  `(@/(~c & ~d) & ~a)/(~x & @/~c)` are both refused.
- *A conjunct written twice* (`spec_compileRepeatedConjunct`): writing
  a conjunct twice changes nothing when both paths are sealed. To seal
  a path is to write `@/` before it and `/@` after it: then nothing
  around it copies into its joins, and its joins hold nothing around it
  together. `@/(q & q)/@` and `@/q/@` are interchangeable everywhere:
  for any relative `q`, in any path, the compiler makes of the one what
  a query finds with the other. (Where one of the two is run and the
  other refused there is nothing to compare: in a group that another
  group copies, `@/(q & q)/@` is a meeting point and `@/q/@`, for a `q`
  that is one walk, is not.) A whole query is sealed already, as under
  *Reversal*, so `ls (q & q)` finds what `ls q` finds.
  Without the seal the two can differ only when `q` has an `@` or
  `!{…}` in its own chain (or in the own chain of a side of a union
  there, as `q = (@/c | d)` has) and a group stands next to it:
  `P/(q & q)` reaches what `P/q` reaches when `q` has no such join, or
  `P` has one output. Otherwise `P/q` merges the outputs of `P` at that
  join and `P/(q & q)` does not (2): `(a & b)/@/c` and
  `(a & b)/(@/c & @/c)` differ, and sealed, `(a & b)/@/@/c/@` and
  `(a & b)/@/(@/c & @/c)/@`, they do not. With `0 -a-> 1 -b-> 9` and
  `0 -a-> 2 -c-> 9`, `(a/@ & a/@)/(b & c)` finds `9` (and warns) and
  `a/@/(b & c)` nothing; sealed, `@/(a/@ & a/@)/@/(b & c)` and
  `@/a/@/@/(b & c)` both find nothing.
- *Not a law:* left distribution `(A & B)/C = A/C & B/C` when `A`
  contains a join and `C` forks. `(x/@ & b)/(c & d)` has two `x`, and
  `x/@/(c & d) & b/(c & d)` one. It was given up for the product.

Before chain-scoped joins (up to commit `c64b4b1e`) a join stopped the
copying of what was before it wherever it was written, also from inside
a conjunct of an earlier group. Paths without `|` compile exactly as
they did then (`spec_compileUnionFree` compares with that compiler, kept
in the test suite as a reference) outside two classes, and route 0 has
the same steps in them too:

1. *A join in a conjunct of a group that a later group copies.* The
   conjunct has a join (`x/@`, `x/@/w`, `x/@N/w`, `x/!{…}`, also a
   leading one, as in `@/(c & d)`), or is only `@` (which is a join,
   1); its group is followed in its chain, with no join of that chain
   in between, by another group. Then the join was shared by the copies and is now
   copied with its conjunct. `(x/@/w & b)/(c & d)` had one `x`: with
   `0 -x-> 1 -w-> 2 -c-> 9`, `0 -x-> 3 -w-> 4 -d-> 9`, `0 -b-> 5`,
   `5 -c-> 9` and `5 -d-> 9`, `ls` found nothing and now finds `9`. For
   an `@N` only the edge ids differ. For a conjunct that is only `@`
   the difference shows after a prefix, before a conjunct that forks
   and then joins: `q/(@ & b)/((c & d)/@ & e)` has the term
   `q/@/(c & d)/@`, with one `q` where there were two. A conjunct that
   starts with `@` and then forks was its conjuncts (the `@` was left
   out) and is now one chain: `(@/(c & d) & a)/(@/e & y)` merges `c`
   and `d` before `e`. (These two examples are refused now, *Meeting
   points*: what is said of them holds for their products written out.
   Of the paths in this class, those whose conjunct with the join also
   forks are refused; those where it is one walk are run, and warn when
   the `@` begins or ends it.) The class is wide for such conjuncts: of 480
   paths made of them, 400 compile to something else than they did
   (all in the class, and all to what the product written out gives);
   of 9,883 random paths without `|`, 20 do.
2. *A conjunct that is only `@` before an `@N`.* In
   `(a & @)/(@1 & d)` the `@1` after the `@` conjunct was a root (the
   path jumped to node 1) and is now a test. A conjunct that is only
   `@M` or `!{…}` was a join already and is in class 1 only.

Consequences for commands, inside class 1: a path matches more. With
the graph above, `mk (x/@/w & b)/(c & d)/@` was ambiguous (six repairs)
and is now a no-op; with `0 -x-> 1 -c-> 9`, `0 -x-> 2 -d-> 9`,
`0 -b-> 3`, `3 -c-> 9` and `3 -d-> 9`, `rm (x/@ & b)/(c & d)` matched
nothing and now removes `1 -c-> 9`. On an empty graph `mk` creates what
it did for a join after a step (hops created from one node are
identified, §5), but not for a conjunct that starts with `@` and
forks: `mk @/(a & b)/@/f & @/(a & b)/e & c/@/f & c/e` has to end `a`
and `b` at one node, since they meet before `f` (written as the group
`(@/(a & b) & c)/(@/f & e)` it is refused). Goldens:
`test/golden/ge/path/chain-scoped-*.golden`, and for the refusal and
the warning `copied-meeting-point.golden` and
`join-does-nothing.golden`.

The `@` in a union: an `@` that is one side of a union is the same join
written in the union's place, so `(@ + a)/@N` is `@/@N + a/@N`, which
tests that the current node is `N` rather than jumping to `N`;
`p/(@ + a)` is `p/@ + p/a`; and in `x/(@ + a)/(b & c)` the `@`
alternative shares `x` between the conjuncts. This also applies to a
`%targets(…)` expansion (`%never + @N + …`) that follows an `@`
alternative, as in `(@ + b)/%targets(@N)`. `%float` and `%unanchored`
are **not** supported.

**Unions.** There are three union operators, all at the lowest
precedence. A chain of one of them associates to the left; mixing two
different ones without parentheses (`a + b | c`) is a parse error
(`MixedUnionOperators`), so `(a + b) | c` must be written.

| operator | name | chooses | keeps | loses |
|---|---|---|---|---|
| `p \| q` | relational | separately in each copy sequencing makes of it | `p/(q & r) = p/q & p/r` and `(p & q)/r = p/r & q/r` for unjoined `p`, `r`; `a \| b` agrees with `regex:"^(a\|b)$"`; `*` is the `\|` of all labels | `p/(q \| r) = p/q \| p/r` when `p` has several outputs, `(p \| q)/r = p/r \| q/r` when `r` has several inputs or `p`, `q` join (the copies that choose a joined side share its join, which `p/r \| q/r` chooses once) |
| `p ++ q` | shared | once for the whole path; copies share the choice | `p/(q ++ r) = p/q ++ p/r` and `(p ++ q)/r = p/r ++ q/r` | `p/(q & r) = p/q & p/r` when `p` contains `++` |
| `p + q` | contextual | as `\|` or `++`, by context | | |

Why: a group copies what is next to it in its stretch, and the laws
differ in where the choice sits relative to the copy. Relationally `/` is
composition and `\|` union; copying is contraction, and `\|` puts the
choice inside the copy, `!(A ⊕ B)`, which contraction duplicates into
independent choices (as `*` and regexes always did). `++` puts it
outside, `!A ⊕ !B`: the choice is made first and every copy has the
same side. A union that is not copied (no `&` after or before it) means
the same either way.

`+` is a directive: it is lowered while directives are expanded, after
they expand (so a `%targets(…)` result, which is a `+` union, is lowered
like a `+` written there), to `|` in a query and to `++` in a
construction:

| context | `+` is |
|---|---|
| `ls`, preview, `cd`, `rm`, `rmnf`, `mv`/`cp` (both paths), `rn`/`cl` (both), every `mg` argument, `at`'s path, `tag`'s target (second argument), completion, a `%targets(…)` argument | `\|` |
| `mk`, `mk --allow-orphans`, the path `tag` constructs (first argument) | `++` |

`%override({plus = shared}, p)` and `%override({plus = relational}, p)`
lower every `+` in `p` to `++` or `|` instead, in any command; the
innermost override wins, `|` and `++` written in `p` are unaffected, and
an override inside `%targets(…)` applies to its query
(`%targets(%override({plus = shared}, …))`). Its settings record is
`{key = value, …}`, with the keys `plus` (`shared` or `relational`)
and `max-repairs` (a positive number or `unlimited`; the limit on the
candidate repairs of a construction, §5). Settings combine
(`{plus = shared, max-repairs = 50000}`); an
unknown key or value, or a key given twice, is a parse error naming the
accepted ones (`InvalidOverrideSetting`). `max-alternatives`, which was
a key while compiling had a limit (*No limit on what a path compiles
to*, below), is an unknown key like any other. The
path quasiquoter rejects `+`, since a quoted path has no command; tests
write `|` or `++` (or an `%override`). Suggestions (§5) are rendered with
`|` and `++` only, so they mean the same in every command.

Relational unions in detail. A `|` union is its sides read in place,
route by route (`Models.PathPattern.Compile`: `resolve` lets every
route choose, `sharedBy` says what two routes share and whether they can
be routes of one alternative, `sharing` puts that together):

1. *A union is copied like a step.* A union is a segment of its chain,
   and has the copies a step written in its place would have (*Sequencing*
   1 and 2): `(x | y)/(c & d)` has two, `(p & q)/(x | y)/(a & b)` four,
   `x/@/(a | b)/@/(c & d)` one.
2. *Each copy chooses a side*, independently of the others, and the
   routes through the copy go on through that side read where the union
   is written: its segments are segments of the union's chain. So for the
   routes that chose a side `S` the path is exactly what `S` written in
   place of the union gives for those routes:
   - A join in `S`'s own chain is a join of the chain the union is in.
     It cuts that chain like any other, and all the routes that chose `S`
     (in one copy of that chain) go through that one join. With
     `J = x/@`, `(J | y)/(c & d)` has the four alternatives `J/(c & d)`
     (one `x` for both), `J/c & y/d`, `y/c & J/d` and `y/c & y/d`; and
     `a/(@1 | @2)/(x & y)` has four, including `a/@1/x & a/@2/y`, so
     `%targets(…)` in a query lets each copy choose its target (in `mk`,
     where `+` is `++`, there are two).
   - What is not joined is copied as sequencing copies it: after
     `x/@/w` the `w` is copied by a later group, so `(x/@/w | zz)/(c & d)`
     with both copies choosing the first side is `x/@/w/(c & d)`, one `x`
     and two `w`.
   - A join inside a conjunct of `S` is in that conjunct, copy by copy
     (*Sequencing* 1): `((x/@ & z) | y)/(c & d)` with both copies
     choosing the first side is `(x/@ & z)/(c & d)`, two `x`.
3. *Two routes that chose different sides.* For two routes that chose
   different sides of a union, that union is a segment neither has
   chosen: it cuts nothing and chooses nothing, and the two share around
   it what they would share around a step written there (`sharedBy`).
   - So they share what no side is part of. In
     `(J | s)/((c & y)/@/e & d)` the conjunct `(c & y)/@/e` is one chain
     whichever side leads to it: when the copies before `c` and `d`
     choose `J` and the one before `y` chooses `s`, the routes are
     `x/c`, `s/y` and `x/d` with one `x`, the first two meeting at the
     conjunct's join. With `0 -x-> 1 -c-> 5`, `0 -s-> 3 -y-> 5`,
     `5 -e-> 9`, `0 -x-> 2` and `2 -d-> 9`, `ls` finds nothing, and `9`
     with the `d` edge at `1`.
   - And the joins inside the sides they chose do not put them in one
     copy of what follows. In `(a & c)/(a/@ | b/@)/(c/@/e & d)` the route
     through `a` then `a/@` and the one through `c` then `b/@` are apart
     from the first group on, as they would be around a step, so their
     `c/@` are two joins, each with one route: with `0 -a-> 1 -b-> 2`,
     `2 -c-> 5 -e-> 9`, `2 -d-> 9`, `0 -c-> 3 -a-> 4`, `4 -c-> 6 -e-> 9`
     and `4 -d-> 9`, `ls` finds `9`, as it does without the `/@` after
     `c`.
   - That is so whatever the sides are: a side with a group and a join
     of its own after it, `(a & b)/@`, is one walk after its join as
     `x/@` is, so `((a & b)/@ | z)/((c & y)/@/e & d)` finds what
     `(x/@ | z)/((c & y)/@/e & d)` finds where `x` leads to where `a`
     and `b` meet. (The compiler had a case of its own for a side with
     a group anywhere in it, by which the first found more.) A side
     whose group faces the conjunct, as in `((a & b) | z)/((c & y)/@ & d)`,
     would copy the conjunct for the routes that chose it and not for
     the others, which has no clear meaning: such a path is refused
     (*Meeting points*), and so is every path in which two routes that
     chose different sides could be at two copies of one meeting point.
   - What a route shares with a second and the second with a third is
     one step or join for all three.
4. *An alternative is a way for every route to choose.* Every route of
   the path (a conjunct of each group) chooses its unions on its own: a
   side of each, and where a side has a group, a conjunct of it for each
   of the routes it then is, which choose the unions that are left on
   their own in turn, in any order. Such a choice for every route is an
   alternative when no two routes go through one copy of a union with
   different sides of it: when any two that chose different sides of a
   union are told apart by something else in its stretch, the union
   taken as not chosen (3).
   `((a & b) | x)/((c & d) | e)` has eight alternatives: each union
   chooses once, or one of them chooses its conjunction and the other
   chooses for each conjunct. Each copy does that in its own order:
   `(p & q)/(k & y | m)/(d | (b & c))` has 8 × 8, as
   `p/… & q/…` has, among them the one in which the copy after `p` takes
   `k & y` and chooses the second union for each conjunct, while the
   copy after `q` takes `b & c` and chooses the first for each.
   `(c & d)/(x/@ | y)/(w/@ | z)/(a & b)` has 152: of the 256 ways for
   its four routes to choose, those in which two routes through the same
   `c` or `d` that both chose `w/@` chose the same side of the first
   union (they are one copy of it, cut off from `a & b`), and likewise
   for the second union after `x/@`. Two unions cannot each be the
   other's only reason for two copies: in
   `(x & c)/k/@/(c/@ | d)/(b/@ | a)` the two routes are one after `k/@`,
   and cannot choose `c/@` then `a` for one and `d` then `b/@` for the
   other.
5. *A copy of a conjunct has unions of its own.* A union in a conjunct
   is copied with the conjunct (*Sequencing* 1), and each copy of the
   conjunct has its own union: its copies choose apart from those of the
   other, and two that choose the same joined side are two walks.
   `(p & q)/((J | y) & z)` is `p/(J | y) & p/z & q/(J | y) & q/z`, four
   alternatives, none with one `x` for `p` and `q`. So a union before a
   join in a conjunct of a group that a later group copies chooses in
   every copy: `((x | y)/@ & z)/(c1 & … & cn)` has `2^n` alternatives
   (it had 2 before chain-scoped joins), where `(x | y)/@/(c1 & … & cn)`
   has 2; and `((x | y)/@ & z)/(m/@ | n)/(c & d)` has 56, among them the
   one in which the route through `c` takes `x` and `m/@` and the route
   through `d` takes `y` and `n`. (In both the `@` after `(x | y)` ends
   a conjunct that is one walk: it does nothing there. No warning says
   so, since the conjunct has a union, *Meeting points*. For one choice
   of `x` or `y`, write `(x | y)/@/(c1 & … & cn) & z/(c1 & … & cn)`.)
6. *Every side that is written is a side of its own.* A union has as
   many sides as are written in it, for `|` and for `++`. Nothing makes
   two sides one: not being written the same (`a/@ | a/@` is two
   sides), not compiling to the same thing in the union's place, not
   differing only in an `@` that does nothing there (`a/@ | @/a/@`,
   `a/@ | a/@/@`). A union is not a set of its sides: `p | p` is not
   always `p`.
   - What this gives. A route picks a side. Routes that pick the same
     side of one copy of the union share that side's joins (2), and
     routes that pick different sides share nothing of them (3). So
     for a join written once in each side, a union of `n` sides has at
     most `n` nodes, however many routes pick them. With `0 -a-> 1`,
     `0 -a-> 2`, `1 -d-> 9` and `2 -e-> 9`:

     | command | finds | why |
     |---|---|---|
     | `ls a/@/(d & e)` | nothing | one `a` node for `d` and `e` |
     | `ls (a/@ \| a/@)/(d & e)` | `9` | the routes through `d` and `e` pick one side each: two `a` nodes |
     | `ls (a/@ \| @/a/@)/(d & e)` | `9` | the same; the leading `@` does nothing here, and its side is a side all the same |
     | `ls (a/@ ++ a/@)/(d & e)` | nothing | a `++` is chosen once for the whole path, and either choice is `a/@/(d & e)` |

     `(a/@ | a/@)/(d & e)` has two alternatives: `a/@/(d & e)` (both
     pick the same side) and `a/@/d & a/@/e` (one each). With a third
     route, `(a/@ | a/@)/(d & e & f)` finds nothing where `d`, `e` and
     `f` each need an `a` node of their own (`0 -a-> 1`, `0 -a-> 2`,
     `0 -a-> 3`, `1 -d-> 9`, `2 -e-> 9`, `3 -f-> 9`), and `9` where two
     `a` nodes are enough; with three sides, `(a/@ | a/@ | a/@)`, it
     finds `9` in both. It is the same for sides written differently:
     `(a/@ | b/@)/(d & e & f)` finds nothing when `d` and `e` need two
     different `a` nodes.
   - A union written as a side is its sides: `p | (q | r)` and
     `(p | q) | r` are the three sides `p`, `q`, `r`, and nothing is
     merged. `%never` is no side, and neither is a side whose node
     tests contradict each other right where they are written
     (`x/@1/@2`): `p | %never` is `p`.
   - The order sides are written in never matters, and neither does
     how they are grouped: the path compiles to the same alternatives,
     each with its routes in the same order (a test, on random paths,
     and on every order and grouping of the examples below).
   - A side is kept as written: a leading `@` is still literal in what
     the path compiles to (*Sequencing* 2), `@/@1` (the current node is
     `1`) is not the root `@1`, and `c & *` and `* & c` are two sides
     with their routes in two orders.
   - When `p | p` is `p`. Where no group stands in the union's
     stretch, so that one route goes through each copy of the union,
     the two compile to the same alternatives: `q/(a/@ | a/@)/c`,
     `(p & q)/@/(a/@ | a/@)/@/(d & e)`. With groups in the stretch,
     where `p` has no join of the chain the union is written in and no
     `++` union, the two compile to the same alternatives but for one
     case that is known, and in that case they still find the same
     nodes. The same alternatives: `(a | a)/(d & e)` is `a/(d & e)`
     (the routes share nothing of `p` whichever side each picks; a
     join inside a conjunct of `p` is no join of that chain). The
     case: `p` forks, and a group in the stretch has a conjunct in
     which walks that fork on the union's side meet at an `@N` and go
     on from it as one walk (`(c & y)/@1/e` after the union,
     `e/@1/(c & y)` before it). Then `p | p` has one alternative more:
     `((a & b) | (a & b))/((c & y)/@1/e & d)` has 2 alternatives where
     `(a & b)/((c & y)/@1/e & d)` has 1. In the one more, the routes
     through `c` and through `y` took different sides of the union,
     and the `e` after `@1` is one step for the copy of `(c & y)/@1/e`
     made for `a` and the copy made for `b`; in the other, which is
     the alternative of `p` alone, each copy has an `e` of its own.
     The one more finds nothing the other does not find, as far as
     was tried: the review that found the pair compared what the two
     paths find and found the same targets on every graph it tried,
     and `mk` of either created the same one edge. (Tests,
     `spec_compileEverySide`: the counts 2 and 1 for `p` being `a & b`
     and `a/a & b`; the alternative of `p` is one of `p | p`; the two
     find the same nodes on four graphs; and for a `p` that does not
     fork, `a`, `a/b` and `a | b`, the two compile the same there
     too. Whether another such case exists is not known: on 12,760
     random paths with no group in the stretch and 1,327 with a `p`
     without joins the same review found the two identical.)
     Otherwise it may not be: `(a/@ | a/@)/(d & e)` above;
     `(p & q)/(a/@ | a/@)`, in which `p` and `q` need not meet; and
     `((x ++ y) | (x ++ y))/(d & e)`, which has `x/d & y/e`, since a
     `++` written twice is two unions, each chosen once, and one may be
     `x` and the other `y` (`(x ++ y)/(d & e)` is `x/(d & e)` or
     `y/(d & e)`). Sides that are one fixed node are one node whichever
     is picked: `(@1 | @1)/(d & b)` is `@1/(d & b)`.
   - `p ++ p` is `p`, whatever `p` is: the side is chosen once for the
     whole path, either choice is `p`, and equal alternatives are one
     alternative (below). So for a `++`, a side being a side of its own
     changes nothing a command can see.
   - The check for meeting points reads the same sides (*Meeting
     points*): a side written twice is two sides there too, so the
     union is also read as one step.
   - The compiler lists the ways to choose before it knows which of
     them come to the same alternative, so a row of unions of sides
     written alike costs more than its alternatives suggest:
     `(x/@ | @/x/@)/` five times before `(a/@ | @/a/@)/(d & e)` has 7
     alternatives, which take about a tenth of a second to compile
     (an unoptimized build; four times it has 6, in a third of that).

   (Before, sides that compiled to the same thing in place were one
   side, within bounds on how much was compiled to find out. That made
   `(a/@ | @/a/@)/(d & e)` find nothing above, and it made results
   depend on the order of sides, on their grouping, on a side being
   added, and on how many unions followed. Graph `0 -n-> 1`,
   `1 -c-> 2`, `1 -c-> 3`, `2 -e-> 4`, `3 -e-> 5`, `4 -f-> 9`,
   `5 -g-> 9`: `ls (m/@ | n)/(c/@ | c/@/@ | @/c/@)/(d/@ | e)/(f & g)`
   found nothing and found `9` with the sides of the second union in
   another order; it finds `9` now in every order and grouping. On the
   graph of the table, `ls (a/@ | @/(a/@ | c))/(d & e)` found `9` and
   `ls (a/@ | c | @/(a/@ | c))/(d & e)` nothing; both find `9`. And
   with `9 -x-> 9` added, `ls (a/@ | @/a/@)/(d & e)/@` followed by
   `/(x | y)` four times found nothing and followed by five found `9`;
   it finds `9` whether none follows or eight (with nine the path has
   1024 alternatives). These are tests,
   `spec_compileEverySide`.)
7. *The routes of an alternative, and its spelling.* Rules 1 to 6 say
   what the routes of an alternative are and what they share; they fix
   no order for them beyond that of a path without unions. The order
   matters for what a command acts on (the first route, §3) and for
   whether the alternative is, routes in order, what some path without
   unions compiles to.
   The alternatives themselves have no order that a path decides. A
   query is the set of its alternatives: alternatives that compile to
   the same pattern (same routes in the same order) are one, so the
   four ways the two routes of `(a/@ | a/@)/(d & e)` pick sides are two
   alternatives. Commands go through the set in the order of the
   compiled patterns, which has nothing to do with how the sides are
   written, and none of them takes "the first alternative" for
   anything. (`compileAlternatives`, which `mk` uses to find a path
   that spells a given repair, lists them as they are made, and an
   alternative may be listed more than once there; it is searched for
   the alternative wanted, not read from the front.)
   The routes of one alternative are listed by the conjuncts they
   choose, as the routes of a path without unions are, so the first
   route is the first route of every side written alone, and the order
   does not depend on the order of the sides. When all
   routes chose alike that is the order of the path with those sides.
   When routes chose different sides, the routes that are connected by
   the joins they share are put next to each other, where the first of
   them is, and those that all go through one join are listed as that
   join lists them (every route up to it with every route from it on,
   both parts listed likewise), since a path without unions can only
   list them so (`together`). The alternative of
   `(c & d)/@/(w/@ | z)/(a & b)` in which the copy before `a` chose
   `w/@` and the one before `b` chose `z` is listed `c/w/a`, `c/z/b`,
   `d/w/a`, `d/z/b`, as `(c & d)/@/(w/@/a & z/b)` lists it. This is a
   choice of the implementation for the sake of spelling, not a
   consequence of the rules.
   Where two routes that chose a joined side differ both before and
   after its join, the side written there also has the routes that
   cross over (the route up to the join of each with the route from it
   on of the other), and so has the alternative: when in
   `(p & q)/(J | y)/(a & b)` the copies before `a` after `p` and before
   `b` after `q` choose `J` and the other two `y`, the alternative is
   `(p & q)/J/(a & b) & p/y/b & q/y/a`. The routes `p/J/b` and `q/J/a`
   are made of steps the alternative has already, so they change nothing
   a query finds or `mk` creates. They are left out where one of them
   would end with a step that no route ends with (when the route from
   the join on has no step, as in `(@ & c)/(@ | a)/(@1 & y)`), since the
   last step of each route is what tags and file types read (§3).
   Not every alternative is a path without unions: not one in which a
   walk is used by a route inside a joined conjunct and by a route
   outside it (the alternative of `(J | s)/((c & y)/@/e & d)` in 3), nor
   one whose crossing routes are left out. `compileAlternatives` gives a
   path for an alternative when it finds one that compiles to it,
   routes in order (it writes the routes out and compiles the result);
   an ambiguous `mk` lists the repairs of the others without a `use:`
   line (§5). It finds one for 132 of the 152 alternatives of
   `(c & d)/(x/@ | y)/(w/@ | z)/(a & b)` (4), for 6 of the 8 of
   `(J | s)/((c & y)/@/e & d)`, and for all four of
   `(c & d)/@/(w/@ | z)/(a & b)`. How many of the rest are some path
   without unions in another order of routes is not known; those
   looked at share a walk as described.

These laws hold for paths with unions (each a test in
`Models.PathPattern.CompileSpec`, on random paths with joins). Like the
laws of sequencing they are laws of the compiler, tested on every path
it compiles, also on those no command runs:

- *`/` associates* on the alternatives, and not only on what they
  reach: `(c & e)/(x & y)/(d | @)` and `(c & e)/((x & y)/(d | @))` are
  the same alternatives (`spec_compileUnionMonotoneRandom`).
- *Sequencing distributes over a conjunction of paths without joins*,
  whatever unions they have: `(p & q)/r = p/r & q/r` and
  `r/(p & q) = r/p & r/q` (`spec_compileCopies`).
- *A union is the choices of its routes* (`spec_compileUnionIsItsChoices`):
  the alternatives of `C[p | q]` in which no route chose `q` are exactly
  those of `C[p]` (up to the order of their routes; the test finds them
  in the same order). There is no further
  reading: a side "written alone" is the alternative in which every copy
  chose it. (Before chain-scoped joins it was a reading of its own,
  which gave `(p & q)/((J | y) & x)` an alternative with one walk of `J`,
  and `(a | b | c & d | e & f)/(a & b)` two alternatives more than its
  16 choices.)
- So *adding a side never removes an alternative* (so never a target),
  up to the order of its routes, for unions of any number of sides, and
  for `|` and `++`
  (`spec_compileSubUnions`, `spec_compileUnionMonotoneRandom`). This is
  a law of the compiler. For commands it has one exception: adding a
  side can make the path refused (*Meeting points*), as `z/(K & d)` is
  run and `((a & b) | z)/(K & d)` is not.
- *The order and the grouping of sides never matter*: a union with its
  sides in another order, or with `p | (q | r)` for `(p | q) | r`,
  compiles to the same alternatives, each with its routes in the same
  order (`spec_compileEverySide`, `spec_compileUnionMonotoneRandom`).
- *`p | %never = p`.*
- *`p | p = p` is not a law.* `p | p` has every alternative of `p`, and
  may have more: `(a/@ | a/@)/(d & e)` has the alternative
  `a/@/d & a/@/e`, which `a/@/(d & e)` has not (6, with the graph on
  which it finds more). It is `p` where no group stands in the union's
  stretch. Where `p` has no join of the chain the union is written in
  and no `++` union it compiles to the same alternatives as `p`, but
  for one known case in which it has one alternative more and still
  finds the same nodes: a `p` that forks, next to a conjunct in which
  walks meet at an `@N` and go on as one (6, with
  `((a & b) | (a & b))/((c & y)/@1/e & d)`; `spec_compileEverySide`,
  `spec_compileUnionMonotoneRandom`). `p ++ p = p` always.
- *Conjunctions may be written out*: replacing adjacent segments without
  unions or joins by their product changes no alternative
  (`spec_compileSharedWalks`): `(x/*/@ | y)/(c & d)/(a & b)` is
  `(x/*/@ | y)/(c/a & c/b & d/a & d/b)`.
- *Product*, for conjuncts with unions (`spec_compileProduct`).
- *Reversal*, for paths with unions too (`spec_compileReversal`).
- *Prefixes that compile the same are the same prefix*: two paths
  without unions that compile to the same pattern alone, before each
  side of the unions that follow, and before a continuation that forks
  and joins (`(m & n)/@`, `@/m & n`, `m/@ & n`), give the same
  alternatives before those unions, up to the order of their routes and
  with the same first routes (`spec_compileInterchangeablePrefixes`):
  `(a & b) & c` and `a & (b & c)`, or `x/(a & b)` and `x/a & x/b`,
  before `(y | d/@)/e`. A leading `@` before a fork makes another prefix
  (`@/(a & b) & c` is not `a & b & c` before `@/m & n`; that is a path
  no command runs, since `@/m & n` would copy `@/(a & b)`), and a conjunct
  it keeps together has its routes next to each other in every
  alternative, which is where the order can differ. The exception this
  law had, a conjunct that is only `@` before an `@N`, is gone
  (*Sequencing* 1), and so is its limit after a union:
  `(J | s)/((a & b)/@ & c)/@` and `(J | s)/(a & b & c)/@` are the same
  alternatives, as the two continuations are the same after any path
  without unions.
- *`*` is the `|` of all labels* (`spec_compileWildIsUnion`).

**How many alternatives.** Alternatives multiply: a `k`-way `|` copied
into `n` conjuncts gives `k^n` alternatives (`(a | b | c)/(d & e & f & g)`
has 81), deduplicated by canonical equality. A chain of unions
multiplies too, with or without a conjunction, and with `++` as with
`|` (`k` unions of `n` sides are `n^k` alternatives; a chain of ten
`(a | b)` has 1024), and a long one of wide unions is slow: four unions
of 64, 64, 64 and 2 sides are 524,288 alternatives and take about ten
seconds to compile.

Alternatives that glue to the same pattern are one, and ways to choose
that contradict each other are none: `(@1 | … | @31)/(a & b)` has 961
alternatives and with 32 nodes 1024; sides of other shapes among the
targets, as in
`(@1 | … | @25 | a | b | c & d | e & f | x/@ & d | y/@ & d)/(e & f)`
(31 sides, 961), make no difference.

Ways to choose that contradict each other are dropped as they are made,
not listed and then counted: two routes at one joined node cannot
choose different `@N` there, so the second is not tried with a node the
first did not choose. This is what a `%targets(…)` after a join costs:
`x/@/(@1 | @2)/(a & … & j)` has 2 alternatives and
`x/@/(@1 | … | @5)/(a & b & c & d & e)` has 5 (found in a millisecond;
the ways to choose are `2^10` and `5^5`), and
`(c & d)/@/(@1 | … | @k)/(a & b)` has `k` (0.02s for 60).
`(a & b)/(@1 | … | @64)/@1` has 4096 ways and one alternative, found in
0.01s.

Nor is a route tried with others before it is known to have a way to
choose at all: a conjunct that cannot be satisfied, as `(@1 | @2)/@3`
or `@/(@1 & c)/@2`, makes the path compile to nothing at once wherever
it is written among twenty unions joined by `&`. And a side that some
route through a copy cannot choose is not tried for the other routes
of that copy (`narrowed`). These are tests,
`spec_compileContradictions`; with nothing limiting what a path may
compile to (below), they are what keeps such paths from taking as long
as their choices are many. One kind of dead end is known that they do
not see: two routes that must choose two unions alike by one of them
and differently by the other; a path with it took 11.5s to compile to
nothing (measured by the fourth review of the limit, before it was
removed).

A join of the chain is gone through by every route, so the stretches
before and after it choose apart, and only the routes of one stretch
are compared two by two: `(a & b & c)/…/@/(x | y)/(a & b)` with seven
groups before the join has 4 alternatives of 4374 routes each (0.6s).
`q & (a & (c | @1))/(@ & d/@1)/((@ | @1) & !{@1})/(~a | @1)/(* & (a | c))`
has no alternative at all: every way to choose contradicts itself.

Unions cost about what their alternatives cost. Nine `(a | b)` joined
by `&`, or in a chain before `& z`, are 512 alternatives (0.02s);
`(a | b)/(c | d)/(e | f)/(g | h)/(i | j)/(m & n)` has 1024 (0.06s), and
4096 with a sixth union (0.3s). Unions with joined sides in one chain
are no dearer:
`(p & q)/(x/@ | y)/(w/@ | z)/(u/@ | v)/(s/@ | t)` has 146 alternatives
(0.01s), with a fifth such union 454 (0.04s), and with a sixth 1394
(0.15s). Eight `(p/@ | q)` joined by `&` are 256 (0.01s). With
`u = (@1 | @2 | @3 | a | (a | b) & (c | d) | (c & d)/(a | b) | (a & b | c)/@ | (a | b/@)/(c & d))`,
`(u & u)/c` has 324 alternatives (0.02s). (Before chain-scoped joins
the four-union path had 256 alternatives.) Alternatives of many routes
cost their routes: `(x | y) & (a & b & c)/…` with seven such groups
has 2 alternatives of 2188 routes each (0.4s), and with eight groups 2
of 6562 (1.4s). (The times in this section are the processor time of
`compilePath` alone, in an unoptimized build, on a machine doing other
work; a command also loads and matches what it compiled, §4.)

One kind of path has more alternatives than it had before chain-scoped
joins: a union before a join in a conjunct of a group that a later
group copies chooses in every copy of the conjunct (*Relational unions
in detail*, 5). `((x | y)/@ & z)/(c1 & … & c9)` has 512 alternatives
(it had 2), and 1024 with a tenth conjunct (0.3s).
`(x | y)/@/(c1 & … & c10)`, or the same with `++`, has 2.

**No limit on what a path compiles to.** Nothing refuses a path for
being large. A command compiles its path however many alternatives it
has and however many routes they have, for as long as that takes, and
some paths that are short to write do not finish:
`(x | y)/(a & b & c)/…` with seven such groups has one union that 2187
routes choose on their own, `2^2187` ways. The only paths a command
does not run are those that would copy a meeting point (*Meeting
points*), which is found by reading the path and costs nothing.

There was a limit. A path with a `|` union and a conjunction could
compile to at most 1000 alternatives, counted before the command ran,
within bounds on the work of counting; beyond them the command failed
with `TooManyAlternatives`, `TooLargeToCount` or `TooManyRoutes`, and
`%override({max-alternatives = N}, p)` moved the limit. It was removed.
`docs/path-cost-limits.md` says what it was, why it was removed, where
the time of a path command goes, what is known about limiting it, and
what removing it costs, with the measurements. In short: a path the
limit accepted compiles to exactly what it did; and of 149 paths
sampled from those that only the limit refused, 132 compile within 20
seconds of processor time (79 within one second), 107 finish an `ls`
and 76 an `mk` within 20 seconds, and the others run until the user
stops them (an unoptimized build, a busy machine).

A command that is compiling can be stopped, and so can the next one:
any number of them in one session. At the prompt Ctrl-C brings the
prompt back (in about a tenth of a second when tried) and nothing has
been changed, since a command compiles its paths before it writes;
`ge -e` ends at an interrupt, by the signal as an interrupted program
does, and says nothing of its own. A command that is reading node files
when the interrupt arrives is stopped in the same way, and is not
reported as a command that failed. Either way the damage that the line
had read past is still warned of.

TAB completion compiles the prefix it completes, lowering `+` as `|`
like a query, and reads the node files that the prefix leads to.
Neither holds the prompt: when the candidates are not ready after about
half a second (`Executable.GraphEditor.Completion.completionAllowance`)
completion gives up and offers nothing, and the prompt goes on as after
any TAB that finds nothing. The work is done in a thread of its own,
which is told to stop then and is not waited for: work that computes
stops at once, and work that waits for the system to let it read a file
(another program is writing it, or it is not downloaded yet) stops when
it has been let, after that one read. Ctrl-C during the half second
gives a new prompt at once. That is a bound on completion, which is
advice, and on no command. A prefix that no command runs offers no
completions.

What `mk` makes is limited: its work is in the candidate repairs of
each alternative (residuation), which depend on the graph, and they
have a limit, `max-repairs` (§5), the one limit there is. Measured with
`ge` in an unoptimized (`--fast`) build, each including about 0.06s of
startup: on a graph with none of the path's edges, where each
alternative has one candidate, `mk (a | b)/(c & … & j)` (256 repairs)
takes 0.20s, `mk (a | b)/(c & … & k)` (512) 0.35s and
`mk (a | b | c)/(d & … & i)` (729) 0.30s. On a graph that already has
an `a` and a `b` edge every copy of the prefix may bind or not, so each
alternative of `mk (a | b)/(c & … & g)` has 32 candidates: 1024 in all,
under the default limit of 5000, and its 32 repairs are listed in about
0.2s. The 729- and 256-repair paths above have far more (the latter
65,536) and are refused in about 0.6s and 0.8s. In `mk` a `+` is `++`,
so this only arises for a `|` written there. (These figures and those
of §5 are approximate, from single runs on a machine that was not
always idle.)

`%override({max-repairs = N}, p)` is a directive, expanded before
compilation, so the setting is carried explicitly: expansion leaves a
`WithLimits limits p` node around the expanded `p` (a constructor that
only exists in the `Prenormal` phase), which residuation reads
(`pathMaxRepairs`, §5). It does not otherwise change what the path
means: wherever such nodes are, and however many are nested, the path
compiles to what it compiles to without them (`withoutLimits`).

Implementation (`Models.PathPattern.Compile`): `structures`
writes the path as chains, one for each choice of its shared unions, in
a normal form (`normal`: joins written right after one another are one
join, and the sides of a union stand in a fixed order, every side that
is written being a side of its own). `routesOf` lists the routes of a chain with its unions not
chosen, and `resolve` lets each of them choose its unions
(`chosen`) and takes every way to do so for all of them in which any two
routes can be together, depth first, dropping a way whose routes
contradict each other as it is made (`gluesWith`). It does so for each
stretch of the chain between the chain's own joins, and an alternative
of the chain is one of each stretch. `annotate` names every step and
join on a route by its copy: it is the one place that says what
sequencing shares, and what two routes share that chose the same sides.
`sharedBy` is the same for two routes at once, a union they chose
different sides of being a segment that is not chosen; it says whether
the two can be together and what they share. `sharing` numbers the steps
and joins of an alternative by that, `listedBy` and `together` put the
routes in order, and `glueRoutes` makes the pattern, steps and joins
with the same number being the same edge and vertex. Nothing is
identified by being written the same way, and no walk is put together
after the fact. The compiler before relational unions, which rewrote
terms, is kept in `CompileSpec` as the reference for paths without `|`
(`referenceCompile`), with an interpreter of its terms.

The `@` examples above hold for either union.

**Command policy (from the branch).** Query commands append `/@` before
compiling. Construction (`mk`) does not. The engine never adds defaults
itself.

**Directives.** Every command that takes a path (`cd`, `ls`, `mk`,
`mk --allow-orphans`, every argument of `mg`, both arguments of
`tag`/`t`, `rm`, `rmnf`, `at`, `mv`, `cp`, `rn`/`cl` with or without
`--all-parents`) accepts directives. `dd`, `text` and `desc` take a
label, not a path. A command keeps its paths unexpanded until it runs,
then expands each one (`Graph.Command.expandDirectives` for a query,
`expandConstruction` for a constructed path), left to right, before it
does anything else; expanding also lowers `+` (see *Unions*):

- `%override({plus = shared}, p)` / `%override({plus = relational}, p)`
  expand `p` with `+` lowered to `++` / `|`, and
  `%override({max-repairs = N}, p)` sets the limit on the candidate
  repairs of a construction (§5).

- `%history(n)` / `%last` become `@N` for the location `n` steps back in the
  history *at that moment*, so in `cd a; cd %last` (one line) the `%last`
  is the location before `cd a`.
- `%targets(p)` becomes `%never + @N1 + @N2 + …` for the current targets of
  `p`, queried from the current location (a `+` in `p` is a query's, `|`,
  unless an `%override` inside `p` says otherwise). The result is a `+`
  union, lowered where `%targets` is: `++` in `mk`, so
  `mk a/%targets(p)/(x & y)` chooses one target for the whole path, and
  `|` in a query, where each copy that distribution makes may choose a
  different target: `ls a/%targets(p)/(x & y)` also finds an `x` under
  one target with a `y` under another. For the per-target reading in a
  query, write `%override({plus = shared}, a/%targets(p)/(x & y))`, or
  run the command at each target with `at %targets(p) …`
  (`%override({plus = relational}, …)` gives the per-copy reading in
  `mk`). Many targets before a conjunction multiply (`k` targets copied
  into `n` conjuncts are `k^n` alternatives): with 42 targets,
  `ls %targets(*)/(a & b)` has 1764, where the per-target reading has
  42, and nothing limits that number (*No limit on what a path compiles
  to*). Targets that nothing copies do not multiply, whatever else the
  path has: `ls x & %targets(*)/a` has one alternative for each target
  (`test/golden/ge/directives/targets-many.golden`, which runs all
  three). In
  `mk` each target is one alternative: with `0 -p-> 1`, `mk x/%targets(p)` creates
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
- If the path `rm`, `mv` or `cp` takes its edges from (the first
  argument of `mv`/`cp`) matches nothing, so that there is no route 0 at
  all, the command changes nothing and prints a notice naming the path as
  it was written, e.g. `the path of rm ("zz") matched nothing; nothing was
  changed.` (`test/golden/ge/no-match/`). It is a warning, not an error:
  the rest of a `;` sequence runs, and `at p cmd` goes on to its other
  targets, where the path may match (there the notice also names the
  target, `… matched nothing at @N; …`). A path of which some `+` side
  matches is a match, and prints nothing. The other refusals are
  unchanged and come without the notice: a zero-hop route 0 (`rm @`), a
  `mv`/`cp` destination that is not one node (checked first, whatever the
  source matches), a parse error. `rn`/`cl` still treat a source that
  matches nothing as an error.
- The `rn`/`cl` destination creates one edge: its prefix (everything
  before the final label) must resolve to a single parent node. With
  `0 -c-> 3`, `0 -c-> 4`, `rn a c/z` is an error (`SeveralParents`) that
  names the parents and changes nothing; pin one (`c/@3/z`) or pass
  `--all-parents` (`rn --all-parents a c/z`) to create the edge under
  each parent, deleting the original once. `mv`/`cp` likewise need a
  destination that resolves to one node.
- `mg p1 p2 …` merges nodes, one per argument: each argument's targets
  must be exactly one node (otherwise an error naming the argument, like
  `cd`'s). `--keep` before at most one argument (`mg a --keep b c`; a
  second `--keep` is a parse error) marks the survivor, which keeps its
  NID and its data; the others' edges are moved onto it (edges between
  merged nodes become self loops) and their data is discarded with a
  warning naming those nodes. Without `--keep` the first argument's node
  survives, and at most one of the nodes may have data, which the
  survivor takes; if several have data nothing changes and the error
  lists them and suggests `--keep`. Empty data counts as no data. A
  system node (the origin, the system root, `tags`, …) is never merged
  away: without `--keep` it is the survivor, and `--keep` on another node
  or merging two system nodes is an error that changes nothing. Arguments
  that resolve to the same node count once; fewer than two distinct nodes
  is an error. Afterwards
  the location is the survivor. (`mg` used to take one path and merge all
  its targets, silently keeping the last node's data.)
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

Rules 2 and 3 are stated over every pair of candidates, and
`selectReference` applies them that way; it is kept as the reference
that the tests compare `select` with (over the corpus with every chooser
used on it, symmetric patches, and random paths and graphs). `select`
gives the same survivors in the same order without comparing every pair:
- *Dominance.* c' dominates c iff c's binding is that of c' without one
  or more variables that are left-justified in c'. The bindings dominated
  by leaving out one variable are collected in a set, so most dominated
  candidates are found by a lookup; for the rest, the candidates that
  bind two or more variables more are gone through, grouped by the set
  of variables they bind.
- *Supersets.* A patch that contains another modulo renaming, and is not
  contained in it, has more nodes or edges: were they as many, the
  renaming would be onto, and its inverse would contain it in turn. So a
  patch is compared only with the strictly smaller patches of candidates
  whose pins are among its own, each two distinct patches once, with one
  search for a renaming instead of two. The search is skipped where the
  smaller patch has a feature the larger lacks: an edge with some label
  and ends, or two such edges at one fresh node (fresh ends alike).
- *Equal patches.* Patches equal modulo renaming have the same shape:
  the same created fixed nodes, as many edges of each label and ends, and
  the same edges at each fresh node. A patch is compared only with the
  kept patches of its shape, again by one search. A shape is not a
  canonical form (patches of one shape need not be equal), so nothing is
  merged or kept apart on its account: the search for a renaming still
  decides, as it does in the reference.

**Limit on candidates.** Candidates multiply. A step that matches `n`
existing edges is bound to one of them or left to be created, so `k`
such steps are `(n + 1)^k` bindings; a chain of `d` existing steps can
be matched up to any of its steps, so `k` chains continued by something
new (`mk a1/…/ad/x & b1/…/bd/y & …`) are `(d + 1)^k`, although only the
binding that matches every chain in full survives; independent pins and
identifications multiply likewise. So residuation counts the candidates
as it makes them, and stops as soon as there are more than the path's
limit, 5000 by default (`Models.PathPattern.Residuate.defaultMaxRepairs`).
What is counted is every way of matching the path's steps to existing
nodes that has to be considered: every binding with every identification
of its fresh vertices (4), in every alternative. These are the
candidates before selection, including those that are then dropped
because a pin is not justified (5). Dominated and redundant candidates
count, since telling which they are is the work the limit bounds; so a
path with one repair can have many candidates (ten existing conjuncts
and a new one are 1024, four chains of five 1296, five chains of three
1024, seven chains of two 2187: all under the default, and done in
about 0.2s, 0.4s, 0.3s and 0.5s with `ge`), and with enough existing
steps it is over the limit (thirteen existing conjuncts and a new one
are 8192, refused in 0.3s). Pinning a step with `@N` leaves it one way to match, and so
divides the count.
Then:
- Up to the limit nothing differs: the same survivors in the same order,
  five listed, the exact count.
- Beyond it the command fails without changing anything, and without
  listing repairs, since which candidates survive is not known:
  "Working out how to construct this path would take too long: its
  steps can be matched to existing nodes, or left to be created, in more
  ways than the limit of 5000, and each way has to be considered. Nothing
  was changed. Pin a step to a node with @N or exclude nodes with !{@N}
  so that fewer match, or raise the limit with
  %override({max-repairs = …}, …).
  (TooManyRepairs)". This comes before the label error (6) and before
  "cannot be constructed", which both need every candidate.
- A path that already resolves is the no-op whatever the limit (1). Up to
  the limit that is found as before, by the candidate with an empty
  patch. When the limit is passed first, that candidate may not have been
  made, so each alternative is matched instead, as a query matches it
  (`loadPattern`, `matchPatternWithin`), requiring that every matched
  node exists. That is the same condition: a candidate's binding is
  total and its patch empty exactly when it is such a match, and the
  evidence rule reaches every match.

`%override({max-repairs = N}, p)` (or `unlimited`) changes the limit:
`N` is a positive number;
the innermost override around a part of the path is that part's limit;
and since the candidates are those of the whole path and cannot be
attributed to a part, the largest limit among the path's steps and nodes
applies to all of it (`pathMaxRepairs`). So a low limit around one
conjunct does not constrain a path with parts outside the override
(which have the default); to lower the limit, wrap the whole path. The
setting combines with `plus` (`{plus = shared, max-repairs = 5000}`)
and is carried by a `WithLimits` node (§2). Commands that construct
nothing (`ls`, `cd`, `rm`, …, a `%targets(…)` query, `tag`'s target)
accept it and ignore it. The paths suggested for an ambiguous `mk`
(below) are written without it, and are confirmed within the default
limit.

The limit bounds a count, not time. What one counted candidate costs
grows with the pattern, by a factor of a hundred and more between the
paths below, and almost all of it is in making the candidates, not in
selecting. Refusing a path costs what making the candidates up to the
limit costs, and then the matching pass that tells whether the path
resolves. Measured in an unoptimized (`--fast`) build; "alone" is
residuation alone over a graph in memory, and `ge` adds about 0.06s of
startup, compiling, reading the nodes (each once for a command) and,
for an ambiguous path, the suggestions below. The figures are
approximate (see §2):
- Small patterns. 625 candidates of four steps with four matches each
  (`mk regex/x & regex/y & regex/z & regex/w`, 256 repairs) take 0.03s
  alone, and so do the 511 of a chain of eight steps down a binary
  tree. With seven matches each (4096 candidates, 2401 repairs) the
  four steps take 0.3s alone and 0.5s with `ge`; with eight (6561) the
  path is refused, in 0.16s with `ge`.
- Independent pins. `mk a/~a & … & k/~k` (2048 candidates, all of them
  repairs) takes 0.9s alone and 1.4s with `ge`; with a twelfth (4096)
  2.5s alone and 3.7s with `ge`; with a thirteenth it is refused, in
  1.6s.
- Many pins in a larger pattern.
  `mk (a & b & c & d)/~a/(a & b & c & d)/~b` (64 pattern edges, 768
  counted, 576 candidates after unjustified pins, 27 repairs) takes
  0.65s alone and 2.0s with `ge`.
  `mk (a & b & c & d & e)/~a/(a & b & c & d)/~b` (100 edges, 3072
  counted, 54 repairs) takes 4s to 4.6s alone and 5.7s with `ge`: the
  slowest path of this kind found under the default.
  `mk (a & b & c & d & e & f)/~a/(a & b)/~b` (12,288) is refused in
  2.4s with `ge`, and
  `mk (a & b & c & d)/~a/(a & b & c & d)/~b/(a & b & c)` in 19s; without
  a limit that last path did not finish in 90 minutes.
- Large patterns are not bounded by the count at all.
  `mk (a & a & c & b & b)/~b/(b & c & b & a)/~b/(c/@ & ~a/b/b/b & c & a/a)`
  on an empty graph distributes to 480 pattern edges and has only 45
  candidates and 10 repairs: it takes 8s to 10s alone, all of it making
  the candidates, and 16s with `ge`.
  `mk (b/~b & c/~c & a/~a & b/~b & b/~b & a/~a & c/~c & b/~b)/(a/~a/b & c/~c & c/@N)`,
  with `N` the current node, is refused with `ge` after 29s on an empty
  graph, 8s on one with `a/b` and `c`, and 1.8s on one with `a`, `b`
  and `c`.
- Without a limit, the 6561 candidates (4096 repairs) of four steps with
  eight matches each take 0.6s alone (34s before selection stopped
  comparing every pair) and 0.7s with `ge`.

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
  path once more and gives, with each alternative, a path without unions
  that compiles to just it: the path with one side of every union kept
  when every copy chose alike, otherwise the routes written out
  (`a/c & b/d` for `(a | b)/(c & d)`); the choices for all listed repairs
  come from that one compilation (stopping once they are found). An
  alternative that no path without unions compiles to (§2, *Relational
  unions in detail*, 7) has none, and its repairs are listed without a
  `use:` line (`test/golden/ge/path/chain-scoped-mk-no-spelling.golden`).
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
  removal). Residuating a candidate costs little when its rewrites pin
  most steps, and as much as the residuation that gave the repairs when
  they pin none, so two things spare residuations without changing what
  is suggested (`suggestPathsReference` residuates every candidate, and
  the tests compare the two):
  - A candidate is residuated once, whichever of the listed repairs it
    is tried for: its repair, if it has exactly one, is kept by what the
    candidate compiles to.
  - A candidate that compiles to just one alternative that two or more
    of the repairs are of is not residuated. Alone that alternative has
    the candidates it had among the path's, a candidate is dominated
    only within its alternative, and a survivor stays minimal among
    fewer candidates, so it has those repairs still and cannot have just
    one. This is the case of a repair none of whose steps can be
    rewritten: its candidate is the path itself.

  `mk` also reads each node once for the residuation and the
  suggestions together, as nothing is written while a repair is chosen.
  Measured as in *Limit on candidates*, the suggestions for
  `mk (a & b & c & d & e)/~a/(a & b & c)/~b` (60 repairs, none of which
  a path selects) took 10s for a residuation of 2.4s, and take 0.9s;
  for the 100-edge path there 13s, now 1.0s. What is left is one
  candidate, the path with `!{@N}` after its last step, which is over
  the limit on candidates and costs about 0.9s to refuse, whatever the
  residuation cost: for `mk (a & b & c)/~a/(a & b & c)/~b` that is
  0.95s after a residuation of 0.07s. Where every listed repair gets a
  suggestion the step was cheap already: 0.16s for 2048 independent
  pins (residuation 1.0s), 0.03s for four steps with seven matches
  each, and a millisecond or two for `mk a/b` with two `a` edges or
  `mk a & a/@N`. With `0 -a-> 1`, `0 -a-> 2`,
  `mk a/b/(@1 + @2 + @)/…` with 12 such factors takes 0.07s with `ge`.
  The label error (6) lists no suggestions.
- More candidates than the limit: the `TooManyRepairs` error above.
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
  Besides the brute-force matcher, `CompileSpec` checks every corpus
  query on every corpus graph against a direct interpreter of the terms
  of the compiler before relational unions, which it keeps as a
  reference and which shares no code with the compiler, gluing or
  matching. For a query with unions that is checked for each
  alternative that is a path without unions, through that path; an
  alternative that is none is not covered by it.

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
  same two edges, so their expected output is unchanged. (Since `mg`
  takes one node per argument, §3, they use `mg a b`; `mg.golden` merges
  two separately made nodes, and `test/golden/ge/mg/` covers the rest.)

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
