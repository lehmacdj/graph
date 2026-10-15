# Route patterns: how a path is compiled and run

Surface `Path`s compile to **route patterns**. A route pattern is the
normal form of a path and the single intermediate representation used for
resolution, construction (`mk`, `tag`), and edge selection (`rn`, `mv`,
`cp`, `cl`, preview, tags). This document is for people who change the
compiler or the engine: the representation, how a path is lowered to it,
how commands select from it, how it is resolved against the graph, how
`mk` builds what is missing, and how all of that is tested.

What a path means (the operators, the product rule, joins, unions, the
paths that are refused or warned about, the laws) is in
[path-language.md](path-language.md), and what each command does with a
path is in [commands.md](commands.md). This document states those rules
only in the compiler's terms, where a person changing the compiler needs
them, and says where each rule itself is; where the two differ,
path-language.md is the one that is meant to be right.

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

newtype PathQuery = PathQuery { alternatives :: Set PatternPath }
```

- **Routes are the normal form.** Each route is one conjunct after
  distribution: a walk from its root to one output. Sharing (joins) is
  expressed by vertex identity: two routes that reach the same `PVertex`
  meet there. There is no tree structure (no Fork/Merge/Guard/Parallel).
- **Outputs** are route endpoints. There is no separate output mark.
- **Vertices.** `Current` is bound by the caller. `Fixed n` is an identity,
  so every `@n` is one vertex. `Var` is every other position. The
  compiler produces no `Var` root; roots are typed as `PVertex`, which
  leaves room for one (§8).
- **Tests.** Only constraining tests are stored: a `Fixed n` vertex
  implicitly tests `Exactly n` and an absent entry accepts any node
  (`vertexTest` gives the effective test). Joining two vertices takes the
  meet of their tests:
  `Except a ⊓ Except b = Except (a ∪ b)`; `Exactly n ⊓ Except s` is
  `Exactly n` unless `n ∈ s`; `Exactly n ⊓ Exactly m` requires `n == m`.
  A contradiction drops the alternative. Merging a `Var` with `Fixed n`
  yields `Fixed n`. `Exactly` survives only as a test on `Current` (for
  example, a relative path joined with `@5`).
- **EdgeId.** One per copy of a step. A step that is written once has
  one `EdgeId` for each copy the product rule makes of it (the `a` of
  `a/(b & c)` is two edges), and routes that share a copy share its id.
  A *witness* is one complete match of a pattern against the graph: a
  node for every `Var` and a label, and so a concrete edge, for every
  `EdgeId` (`Witness` in `Models.PathPattern.Match`, §4). That it
  chooses an edge per `EdgeId` matters only for `*`/regex matchers,
  where several edges can connect the same two nodes. Routes that share a
  prefix (`*/@/(b & c)`) reuse its `EdgeId`s, so they make one choice.
  Independent occurrences (`(* & *)/@`) get distinct ids and choose
  independently. Labels, selection, patches, and the label callback are all
  keyed by `EdgeId`.
- **Derived pattern graph.** `patternGraph` gives a
  `Graph' PVertex PEdge PAug` with
  `PEdge { edgeId :: EdgeId, matcher :: Matcher }` (so parallel pattern
  edges stay distinct) and `PAug { test :: NodeTest }`. Nothing outside
  `Models.PathPattern` and its tests uses it: matching and `mk` go by
  the routes, which are authoritative.
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

**Chains.** Between the surface `Path` and the routes, the compiler reads
a path as *chains* (`Models.PathPattern.Compile`):

```haskell
data Seg
  = SStep Int Direction Matcher  -- a step
  | SJoin Int NodeTest Bool      -- `@`, `@N`, `!{…}`; True for an `@N`,
                                 -- a root when nothing precedes it
  | SGroup Int [Chain]           -- `p & q & …`, each conjunct a chain
  | SUnion Int [Chain]           -- `p | q | …`, each side a chain
type Chain = [Seg]
```

A chain is a sequence at one nesting level. The `Int` is the place the
segment is written at in the path, and it is the segment's *name*. One
written segment can stand in several routes; in each it is a *copy*,
told apart by the conjuncts that the route chose in the groups that copy
it. Two routes go through the same step or join when they go through the
segment of that name in the same copy (*Sequencing* below). A *stretch*
is the part of a chain between two joins written in that chain, or
between such a join and an end of the chain. A join in a side of a
union ends the stretch only for the routes that chose that side
(*Relational unions in detail*, 2 and 3).

## 2. Compilation (`Path` → `PathQuery`)

1. Expand directives and lower `+` (`handleDirectivesWith`; see
   *Directives* and *Unions* below). The compiled `Path` (the `Prenormal`
   phase) has only `|` and `++`: `+` is unrepresentable there.
2. Apply `~` on the surface; it is a true inverse:
   `~(p/q) = ~q/~p`, `~(p & q) = ~p & ~q`, and `~` distributes over `|` and
   `++`; node specs are unchanged and each step's direction flips.
3. **Chains.** Every shared union (`++`) is chosen first, once for the
   whole path; what follows is done for each choice. The rest of the path
   is read as chains (§1). The segments of a chain are steps, joins (`@`, `@N`, `!{…}`), groups (`p & q & …`,
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

**Sequencing.** What `/`, `&` and the joins mean, with examples run on
graphs, is in
[path-language.md](path-language.md#24-sequencing-a-fork-the-product-rule)
(2.4 to 2.6). `annotate` in `Models.PathPattern.Compile` implements it,
and is the one place that says what sequencing shares. In the compiler's
terms:

0. *Normal form first.* `+` is lowered and every `++` chosen before
   anything below applies. `&` associates: nested groups are one group.
   A conjunct that forks without a join of its own is the conjuncts
   distribution makes of it (`a/(b & c) & d` is `a/b & a/c & d`), so a
   group inside it is a group of the stretch that its own group is in.
   A join in its own chain is the only thing that keeps a conjunct
   together as one chain, which is then copied whole (1); a leading `@`
   is such a join (2). `compilePath` compiles every path. A *meeting
   point* is a copy of an `@` or `!{…}` that more than one route comes
   into or goes out of; a path in which the product (1) would copy one
   is refused by a separate check (*Meeting points* below).
1. *The groups of a stretch are their product.* A route chooses one
   conjunct of every group of the stretch, and each such choice is a
   copy of the stretch: `(a & b)/(c & d)` is the four routes `a/c`,
   `a/d`, `b/c`, `b/d`, with four `a`-or-`b` edges and four `c`-or-`d`
   edges, and `a/(b & c)` is `a/b & a/c` with two independent `a`.
   Every conjunct is copied whole, with the joins and the `|` unions it
   has. Each copy is a chain like any other, made of the stretch with
   each group replaced by the chosen conjunct: the joins of the
   conjuncts cut it, and the groups in its stretches are multiplied out
   again. This is literal for a conjunct without a step too. A conjunct
   that is only `@` is the join it is in `@/…` and `…/@`:
   `(a & @)/(@1 & d)` is `a/@1 & a/d & @/@1 & @/d`, where `@/@1` tests
   that the current node is `1`. There is no way to write a conjunct
   without a step that does not join.
2. *A join governs the chain it is written in.* A join is `@`, `@N` or
   `!{…}`.
   - Arriving: the routes that arrive along the join's chain meet at
     it. `(p & q)/x/@/w` has one `x` target for both. A join inside a
     conjunct is in a chain of its own for every copy of the conjunct,
     and merges only there: `(p & q)/(x/@/w & y)` has two joins, each
     with one route.
   - Leaving: a join ends the stretch. What is before it is not copied
     by a group after it in the same chain, so every continuation
     leaves from the one join: `x/@/(c & d)` and `x/@/w/(c & d)` have
     one `x` (and, in the second, two `w`). A join inside a conjunct of
     an earlier group does not do that, since the conjunct is copied
     with it (1); a join of the chain between that group and the later
     one does.
   - `@N` is a root only when nothing is sequenced before it in the
     term of the written-out product that it stands in (1): in `@N/a`
     and `a & @N` it is `Fixed N`, but in
     `@/@N`, `(@ | a)/@N` and `(a & @)/(@N & d)` it tests the node the
     route is at.
   - A leading `@` is a join like any other and is never left out: at
     the root of the path, right after another join, at the start of a
     conjunct or of a side. It is a join of its chain, so the conjunct
     it starts is one chain, copied whole, and the groups in that
     conjunct are not groups of the stretch around it (0):
     `q/(@/(a & b) & c)` is `q/@/(a & b) & q/c`, one `q` for `a` and
     `b`, where `q/(a & b & c)` has three. It changes nothing in two
     cases, which are different cases. Where one route arrives at it
     and one leaves, it merges nothing: `@/a` is `a`, also as a
     conjunct. And at the root of the path every route is at the
     current node already: `@/(a & b)/(c & d)` at the root is
     `(a & b)/(c & d)`.

The routes of a path are listed as its product written out lists them:
conjunct by conjunct, the first choice varying slowest
(`(a & b)/(c & d)`: `a/c`, `a/d`, `b/c`, `b/d`), the routes through a
join being every route up to it with every route from it on.

**Meeting points.** A path in which the product (1) would copy a meeting
point is refused, and an `@` that a product copies and that does nothing
is warned about. The rule, a quick test for it, the table of examples,
how unions are read, what the error and the warning say and what to
write instead are in
[path-language.md](path-language.md#3-refused-and-warned-paths) (3.1 and
3.2). `checkCopies` in `Models.PathPattern.Compile` is the check, and
`Graph.Command.checkPath` runs it where a command expands its path,
after the directives are expanded. Completion runs it too: a prefix
that is refused completes to nothing. `compilePath` itself still
compiles every path, which is what the laws below are about.

How `checkCopies` decides it, without writing the product out. When is
a `/@` or `!{…}` copied, and when do several walks come into a copy?

- *Copied.* A conjunct is copied when its group has another group as a
  neighbour in its stretch: only steps and unions between the two, no
  join of their chain (`@`, `@N` or `!{…}`). Each conjunct of either
  group is then copied once for every conjunct of the other, with
  everything in it, also the conjuncts inside it. A conjunct without a
  join of its own is no chain apart (0), so the groups in it stand where
  its own group stands: in `((p & q)/x & z)/((c & y)/@/e & d)` both `(p & q)` and
  the group it is in are neighbours of `((c & y)/@/e & d)`, and in
  `(m/((c & y)/@/e & z)/(e & f) & d)` the group `(e & f)` is a neighbour of
  `((c & y)/@/e & z)`.
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
be refused; path-language.md section 5 lists them for the user):

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
  as one step, and past one step the group does copy `p/@`. It
  is reasoned, and not proved, that with only
  two sides the route through `f` has to share a side with the route
  through `c` or the one through `y`, which the `@1` holds together,
  so that `p/@` stays one copy. With a third joined side,
  `(x/@ | w/@ | v/@)`, the copy is there (729 alternatives listed,
  the same test), so the refusal cannot simply be dropped for such
  unions. It is the same with a side written twice,
  `(p/@ & q)/(x/@ | x/@)/((c & y)/@1/e & f)`: refused, and nothing
  copied in what compiles. With a `++` for the `|`, or with one side
  (`x/@`, a join of the chain), the path is run.

The warning. `checkCopies` returns the `@` to warn about (`IdleJoin`),
under the four conditions of
[path-language.md](path-language.md#32-a-join-that-does-nothing-warning).
`Graph.Command` prints them on the `Warn` channel before the command
runs and keeps the texts it has shown for the command line
(`ShownWarnings`), so a path warns once in a line, also when `at` runs
the command at many nodes. Completion does not print them.

Why each condition is there (each excludes something that is no no-op,
or that is not known to be one):

- For 2: an `@` before an `@N` makes the `@N` a test of the node the
  route is at, where it would be a root (*Sequencing* 2), and a side of
  a union may begin with an `@N`. With `1 -b-> 5`, `5 -c-> 9`,
  `5 -d-> 9`, `0 -a-> 6`, `6 -c-> 9` and `6 -d-> 9`,
  `ls ((@/(x | @1/b) | y) & a)/(c & d)` finds nothing and
  `ls (((x | @1/b) | y) & a)/(c & d)` finds `9`. A conjunct that is only `@`
  (`(a & @)/(@1 & d)`) is a join that cannot be written out of the
  path. A conjunct with a group
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

That an `@` at the edge of a conjunct lying inside a copied conjunct past
a join of its own (`(p & q)/(m/@1/(x/@ & z) & y)`) is not warned about is
a choice of this implementation, not a ruling: the check reads only the
conjuncts of the group that is copied.

Measured on 2026-10-09 with the compiler of the commit 6e962c22600e
("Refuse a path in which a product would copy a meeting point"), by
scratch tools and path sets that are not part of the repository, so
these figures cannot be taken again from it:

- The verdict is the same for a path and its reverse on all 292,129
  paths it was tried on.
- The check and the rule worked by writing the product out
  (`handRefuses` in the tests) agree on 7,000 random and planted paths,
  of which 1,770 are refused. (How often the quick test of
  path-language.md 3.1 is wrong was not measured: it is a shortcut that
  errs both ways, and that section lists the kinds of path it gets
  wrong.)
- Two evaluators that share no code with the check (a product expander
  of their own, and the compiled routes with every `@` and `!{…}`
  marked) flag no path that is run, among 688,000 paths of which
  222,832 are refused.
- A warned `@` does nothing: every `@` whose removal takes a warning
  away was taken out, 106,107 times. The path then compiled to the same
  alternatives in the same order 101,465 times, to the same alternatives
  listed in another order 174 times (the routes of each in the same
  order), and never to anything else; 4,468 were too large to compare.
- The check takes 4 to 16 microseconds a path on those sets (optimised
  build, a machine doing other work), and at most quadratic time in the
  length of the path: about 50 ms for 320 groups in one stretch.

**Unions.** The three union operators, how a side is read and what each
law keeps or loses are in
[path-language.md](path-language.md#29-unions) (2.9, and 4.2 to 4.9);
which command reads `+` how is in
[commands.md](commands.md#1-which-commands-take-paths). For the compiler,
`+` is a directive. It is lowered while directives are expanded, after
they expand (so a `%targets(…)` result, which is a `+` union, is lowered
like a `+` written there): to `|` in a query and to `++` in a
construction, or to what the innermost `%override({plus = …}, …)` around
it says. An override around a `%targets(…)` does not reach its argument,
which is a query of its own. The compiled `Path` (the `Prenormal` phase)
has only `|` and `++`. The path quasiquoter rejects `+`, since a quoted
path has no command; tests write `|` or `++` (or an `%override`).
Suggestions (§5) are rendered with `|` and `++` only, so they mean the
same in every command.

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
     (in one copy of that chain) go through that one join.
     `(x/@ | y)/(c & d)` has the four alternatives `x/@/(c & d)`
     (one `x` for both), `x/@/c & y/d`, `y/c & x/@/d` and `y/c & y/d`; and
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
     `(x/@ | s)/((c & y)/@/e & d)` the conjunct `(c & y)/@/e` is one chain
     whichever side leads to it: when the copies before `c` and `d`
     choose `x/@` and the one before `y` chooses `s`, the routes are
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
     and `b` meet. A side
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
   `(p & q)/((x/@ | y) & z)` is `p/(x/@ | y) & p/z & q/(x/@ | y) & q/z`, four
   alternatives, none with one `x` for `p` and `q`. So a union before a
   join in a conjunct of a group that a later group copies chooses in
   every copy: `((x | y)/@ & z)/(c1 & … & cn)` has `2^n` alternatives,
   where `(x | y)/@/(c1 & … & cn)`
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
     for a join written once in the own chain of each side, a union of
     `n` sides has at most `n` nodes, however many routes pick them, in
     each copy of a conjunct that it stands in (5). A join inside a
     conjunct of a side is not such a join: it is copied with that
     conjunct (2), so `((a/@ & z) | (a/@ & z))/(d & e & f)` has an `a`
     node for each of `d`, `e` and `f`. With `0 -a-> 1`,
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
   - When `p | p` is `p`. The law, its conditions and its
     counterexamples are in
     [path-language.md](path-language.md#42-side-order-never-matters-a-side-written-twice-is-two-sides)
     (4.2), and are not stated again here. What the tests hold of it
     is exact equality of the alternatives for fixed examples
     (`spec_compileEverySide`): for `p` one of `a/@`, `@/a/@`, `a/@/b`
     and `(a & b)/@`, alone, before `c`, in `q/…/c` and in
     `(p & q)/@/…/@/(d & e)`, where one route goes through the union;
     and for `p` one of `a`, `a/b`, `a & b`, `a/(b & c)`, `x/@/w & y`
     and `a | b` before `(d & e)`, after `(p & q)`, between the two,
     and between `q/@` and `(d & e)`, where the routes share nothing of
     `p` whichever side each picks (a join inside a conjunct of `p` is
     no join of the chain the union is written in). Neither is a
     general statement: a union in a conjunct that a product copies is
     copied with it, whatever stands in its own stretch (5). One case
     is known in which a `p` without a join of that chain and without
     `++` compiles to more written twice. The
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
     Whether the one more can find a node that the other does not is
     not known: the two paths found the same targets on the four
     graphs of the test. (Tests,
     `spec_compileEverySide`: the counts 2 and 1 for `p` being `a & b`
     and `a/a & b`; the alternative of `p` is one of `p | p`; the two
     find the same nodes on four graphs; and for a `p` that does not
     fork, `a`, `a/b` and `a | b`, the two compile the same there
     too.) Whether another such case exists is not known.
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
     changes no alternative and nothing a command does. It can change
     what a warning says: `checkCopies` reads both sides, and counts an
     `@` that does nothing once for each
     ([path-language.md](path-language.md#42-side-order-never-matters-a-side-written-twice-is-two-sides)).
   - The check for meeting points reads the same sides (*Meeting
     points*): a side written twice is two sides there too, so the
     union is also read as one step.
   - The compiler lists the ways to choose before it knows which of
     them come to the same alternative, so a row of unions of sides
     written alike costs more than its alternatives suggest:
     `(x/@ | @/x/@)/` five times before `(a/@ | @/a/@)/(d & e)` has 7
     alternatives.

   These are tests (`spec_compileEverySide`, `spec_compileSameSides`),
   together with examples whose result must not depend on the order of
   the sides, on their grouping, on a side being added, or on how many
   unions follow.
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
   alternatives. The set has no order that means anything. The order
   in which a command goes through the alternatives that match is made
   at resolution, by their smallest witness, and is stated once, in §4
   (*Order*): it depends on the graph, and has nothing to do with how
   the sides are written. (`compileAlternatives`, which `mk` uses to find a path
   that spells the alternative of a given possibility (§5), lists them as they are made, and an
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
   `(p & q)/(x/@ | y)/(a & b)` the copies before `a` after `p` and before
   `b` after `q` choose `x/@` and the other two `y`, the alternative is
   `(p & q)/x/@/(a & b) & p/y/b & q/y/a`. The routes `p/x/@/b` and `q/x/@/a`
   are made of steps the alternative has already, so they change nothing
   a query finds or `mk` creates. They are left out where one of them
   would end with a step that no route ends with (when the route from
   the join on has no step, as in `(@ & c)/(@ | a)/(@1 & y)`), since the
   last step of each route is what tags and file types read (§3).
   Not every alternative is a path without unions: not one in which a
   walk is used by a route inside a joined conjunct and by a route
   outside it (the alternative of `(x/@ | s)/((c & y)/@/e & d)` in 3), nor
   one whose crossing routes are left out. `compileAlternatives` gives a
   path for an alternative when it finds one that compiles to it,
   routes in order (it writes the routes out and compiles the result);
   an ambiguous `mk` lists the possibilities of the others with "(no
   command was found that creates just this)" in place of a `use:` line
   (§5). It finds one for 132 of the 152 alternatives of
   `(c & d)/(x/@ | y)/(w/@ | z)/(a & b)` (4), for 6 of the 8 of
   `(x/@ | s)/((c & y)/@/e & d)`, and for all four of
   `(c & d)/@/(w/@ | z)/(a & b)`. How many of the rest are some path
   without unions in another order of routes is not known; those
   looked at share a walk as described.

**Laws and the tests that pin them.** The laws are stated, each with its
condition and its counterexamples, in
[path-language.md](path-language.md#4-laws) (section 4). The tests are
about what `compilePath` makes, also of paths no command runs: where
one of the paths in a law is refused, the law says what its product
written out means. The table says for each law what its tests do, which
is often less than the law. To read it:

- The tests are in `Models.PathPattern.CompileSpec` unless the row
  names `LawsSpec`, which is `Models.PathPattern.LawsSpec`.
- *Random* is a fixed sample drawn from a seeded generator, the same
  paths at every run, less the paths that `fewAlternatives` finds too
  large (§6). There is no one generator, and what a test can find
  depends on what its generator makes, so the row names it:
  - `genPath` of `spec_compileUnionMonotoneRandom`: the steps `a`, `b`,
    `~a` and `*`, `@`, `@1`, `!{@1}`, with `/`, `&`, `|`, `++` and `~`
    of a sub-path.
  - `genPath` of `spec_compileRepeatedConjunct` and that of
    `spec_compileUnionIsItsChoices`: the same without `~` of a
    sub-path.
  - `genPath` of `spec_compileCopies`: the steps `a`, `b` and `c` with
    `/`, `&` and `|`; no join, no `@N`, no `++`, no `~`.
  - `genRelative` of `spec_compileReversal`: the steps `a`, `b`, `c`,
    `~a` and `*`, `@`, `!{@1}`, with `/`, `&`, `|`, `++` and `~` of a
    sub-path; no `@N`. `genJoined` there: two groups of two or three
    conjuncts from a fixed pool with `@` and forks, in five draws of
    eight with a step before or after them, `c/@` before, `@/c` after,
    or a `~` around them.
  - `genScoped`: every step has a label of its own (so no two sides of
    a union are alike) and is sometimes reversed; `@`, `!{@1}` and,
    with its argument, `@1`; groups and `|` and `++` unions of two or
    three; `~` of a chain; no `*`.
  - `genStretchy`: eight labels and, rarely, `*`; `@`, `@1` to `@3`,
    `!{@7}`; `~` of a step or of a group. Four draws in five (before a
    path of more than 16 steps and joins is drawn again) are two or
    three groups in one chain, each sometimes a side of a union.
    After each group may come a step (two times in five), then a
    union, `|` or `++` (one in five), then a join (about one in twelve), each
    drawn on its own; a join or a step may stand before the first
    group, and a step after the last. The fifth draw is a plain
    sequence of one to three segments, each a step, a join, a group or
    a union.
  - `genPlanted`: one of thirteen kernels, wrapped up to three times in
    a conjunct, a side of a union or a sequence, with a group next to
    it or not. Six kernels are one conjunct with a fork and an `@` or
    `!{@2}` in its chain (in two of them an `@1` stands between the
    two). Six are two neighbouring groups, a conjunct of one holding
    several walks together with an `@1` and a conjunct of the other
    having an `@` or `!{@2}` (in three of them in a group a level
    deeper). One is two neighbouring groups with an `@` or `!{@2}` on
    one walk and nothing held together, which is no meeting point.
  - `genPath` of `LawsSpec`: the steps `a`, `b`, `c` and their
    reverses, `*`, `@`, `@1`, `!{@1}`, with `/`, `&`, `|`, `++` and `~`
    of a sub-path. `genHoled` there is a *context*, a path with one
    place to write another path in: a random one (3 draws in 24), or
    that place in a chain (4), in a conjunct of a group (3), in a side
    of a `|` or `++` union (4), or in a conjunct of a group in such a
    side (2), or a small context made of steps in which just one thing
    copies the place (`genCopying`, 8). In the four kinds in the
    middle, next to the place on either side, in the chain all that
    stands in, is any of nothing, a step, a group, a step and then a
    group, a join and then a group, a union with a join in a side and
    then a group, a union with a group in a side, and a join and then
    such a union; and one time in six a `~` is written around the
    whole. In a side, between the place and each end of the side
    stands nothing, a step, a join, or a step and a join in either
    order: a join there cuts the place off from a fork outside the
    union on that end. A `genCopying` context has one of: a fork next
    to the place; a fork next to the group the place is in a conjunct
    of, or a step away from it; a union with a fork in a side next to
    the place, or a step away; and a fork beyond a union with a join
    in a side. `genFollowed` is the context of the properties of 4.6,
    4.7 and 4.8: nothing (1 draw in 8), a step behind the place (4),
    a fork of two steps behind it (1), or a random context (2).
- *Fixed* is examples written in the test. *Corpus* is the 107 queries
  and 265 graphs of `Models.PathPattern.TestCorpus`; those graphs have
  the nodes `1` and `2` and the labels `a` and `b` only.
- *Exact* compares what `compilePath` gives: the same alternatives,
  each with its routes in the same order. *Up to route order*
  disregards the order of the routes in an alternative. *Targets*
  compares only the nodes reached on some graphs.
- In `LawsSpec`, *targets* is the nodes `ls` finds for the two paths
  of a pair in two places. One is from every node of two random
  graphs on four nodes with the labels `a`, `b` and `c` (of sixty,
  with two to five tenths of the edges they can have); the test fails
  when fewer than the percentage named of these comparisons find a
  node on either side, and also when fewer than 95% of the comparisons
  of the other place do. The other is on the graphs that the two paths
  themselves describe (`modelOf`: a node for every vertex and an edge
  for every hop of an alternative, for up to four alternatives of
  each path, and for the first also with the current node or a
  variable made node `1`), from the node the path is run at there: an
  alternative finds a node on its own graph, and a second path that
  finds the same everywhere has to find it too. A comparison whose
  matches could be more than 8,000 is left out (`matchesBound`), and
  the test fails when those left out are more than a fifth of the
  comparisons that were made (a sixth of all that were tried).
- In `LawsSpec`, the condition of a law is read off the path as it is
  written (`written`, `uncopied`, `productWithin`, `heldTogether`,
  `leftWithin`, `rightWithin`, `relative`), by code of its own. Of the
  compiler it uses `pushBackwards`, which applies every `~`, so that a
  chain is read in the order it is walked; and where a test asks
  whether commands run a path it asks the compiler's check (`runs`,
  3.1). A property with a condition is a `Law`
  there: its cases are the first so many that a seeded generator gives
  within the condition. *Each clause dropped* names the clauses of the
  condition, and means this for every one of them: the same property
  is run with that one clause left out of the condition, on the cases
  the same generator and seed then give and as many of them, and it
  has to fail in the first half of the cases (`needsEveryClause`).
  What counts as failing is a pair of paths that commands run and for
  which `ls` finds different nodes on one of the graphs
  (`firstToldApart`), or, for the bound of 2.9, a path over the bound.
  A pair that only compiles to different routes does not count: a join
  that is one thing in one path and two in the other does not show at
  the very end of a query, where `ls` joins the walks anyway.
  The generators are aimed, so that without a clause some twenty
  cases of a sample fail (eight at the least, on eight seeds tried on
  2026-10-11). *Fixed* in those rows is the
  counterexamples of path-language.md, each on its graph, with what
  `ls` finds for both paths.
- "Not pinned" means no test was found that states the law; examples
  of it may still pass through other tests.

| law (path-language.md) | test | paths | compared |
|---|---|---|---|
| 4.1 `/` regroups: `p/(q/c)` is `(p/q)/c` | `spec_compileUnionMonotoneRandom` | random (its `genPath`), up to 400 triples, not placed in a context | exact |
| 4.2 the order and grouping of sides never matter | `spec_compileEverySide`: every union of the path, `\|` and `++`, reversed, rotated and regrouped. Random: 700 paths of `genScoped` with `@1` and 700 of `genStretchy`, each also with every label written `a` (a `*`, which `genStretchy` makes, stays a `*`); and the first 500 of those 1,400 as they were drawn, which are `genScoped` paths and not those copies, also with the first side of every union written again after the others. Of all these, the paths that have a union and are within the size limit are compared: over 600 together, over 100 of them paths with the side written again. And five fixed examples in every order and grouping. `spec_compileUnionMonotoneRandom`: five comparisons of one `\|` union, each of its 400 cases in one context drawn from seven (none, `x/c`, `c/x`, `x & c`, `c/x/d`, `(x & c)/d`, `c/(x & d)`, for two random paths `c` and `d`), with `other` the union `c \| d`: `p \| (other \| p)`, `(p \| p) \| other` and `(other \| p) \| p` against `(p \| other) \| p`; `other \| p` against `p \| other`; and `p \| p'` against `p \| p`, where `p'` is `p` with the two sides of every union in it, `\|` or `++`, swapped (random, its `genPath`). `spec_compileSubUnions`: the sides of one `\|` union reversed (random from fixed pools) | random; fixed | exact |
| 4.2 a side written twice is two sides | `spec_compileSameSides`, `spec_compileEverySide` (fixed examples: exact, and counts of alternatives, 2 against 1). `spec_compileEverySide` also writes a side of every union twice in over 100 random paths and requires that more than 20 of them compile to something else than without the twin | fixed; random | exact; a count of paths that differ |
| 4.2 `p \| p` is `p` where nothing copies the union | `spec_lawSideTwice` in `LawsSpec`: `C[p \| p]` against `C[p]` for 250 pairs, `C` a `genHoled` context in which nothing copies the union (`uncopied`) and `p` a random path, in three draws of five one that has a join of its own chain or a `++` (half of those a small one: a walk with a join, or two steps with `++`). In at least 20 of the 250 the union stands in a side of another union and only a join written in that side cuts it off from a fork outside (`cutOffInSide`). Each clause dropped, four: no fork in the union's stretch; no fork in the stretch of a group or union it is written inside; a union with an `&` in a side is a fork; a join in a side of another union does not end the stretch. Also `spec_compileEverySide`: four joined `p` in four contexts with one route through the union (*Relational unions in detail*, 6), exact | random; fixed (the document's counterexamples, and pairs for a join in the side the union stands in and for an `&` in a side of another union); the older test fixed only | targets, 40%; the older test exact |
| 4.2 `p \| p` is `p` where the union is copied and `p` has no join of its own chain and no `++` | `spec_compileUnionMonotoneRandom`: `C[p \| p]` against `C[p]` for those of its 400 cases in which `p` has no join of its own chain and no `++`, over 60, each in the one context it was drawn with (see the row above), so that something copies the union in some of them only; none is the known exception. The exception and its counts in `spec_compileEverySide` | random (its `genPath`); fixed | exact; targets on four graphs for the exception |
| 4.2 `p ++ p` is `p` | `spec_lawSideTwice` in `LawsSpec`: `C[p ++ p]` against `C[p]` for over 180 pairs, any `genHoled` context and random `p`; the same pairs written with `\|` have to differ within the first 40. `spec_compileEverySide`: `(a/@ ++ a/@)/(d & e)` against `a/@/(d & e)`, and `((x \| y)/@ ++ (x \| y)/@)/(d & e)` against `(x \| y)/@/(d & e)` | random; fixed, two examples | exact |
| 4.3 `p \| %never` is `p` | `spec_lawEmptySide` in `LawsSpec` (below); `spec_compileUnionMonotoneRandom` (both orders, each case in its one context), `spec_compileEverySide` | random (`genPath` of `LawsSpec` in `genHoled` contexts; the `genPath` of `spec_compileUnionMonotoneRandom`); fixed (`spec_compileEverySide`) | exact |
| 4.3 `p ++ %never` is `p` | `spec_lawEmptySide` in `LawsSpec`: `C[p ++ %never]`, `C[%never ++ p]`, `C[p \| %never]` and `C[%never \| p]` against `C[p]`, over 150 pairs in `genHoled` contexts; and `C[p ++ q]` against `C[p]` has to differ within the first 5, which shows that the comparison can fail (the law has no condition to drop) | random; fixed (the document's `(a \| b \| %never)/(c & d)` and `(a \| %never)/(c & d)` on its graph, and `%never` as a step) | exact; the fixed paths also targets |
| 4.4 adding a side never removes an alternative | `spec_compileUnionMonotoneRandom` (`\|` and `++`, over 400 pairs), `spec_compileSubUnions` (`\|`, sides and contexts from fixed pools), `spec_compileUnionMonotone` (`\|` and `++`) | random (the `genPath` of `spec_compileUnionMonotoneRandom`); random from fixed pools; corpus | up to route order, and targets on 10 graphs; up to route order; targets |
| 4.5 `*` is the `\|` of all labels | `spec_lawWild` in `LawsSpec`: 40 paths with a `*` or a `~*` written in a `genHoled` context (which may have more of them), against the same paths with every `*` written `a \| b \| c`, the labels of the graphs. The law has no condition; that the comparison can fail is shown by the same paths with `a ++ b ++ c`, of which a pair has to differ within the first 20. Older: `spec_compileWildIsUnion` (5 pairs on 128 graphs with the labels `a` to `d`), `spec_compilePerCopyChoice` (5 paths, one graph) | random; fixed (one graph); the older tests fixed | targets, 40%; the older tests the routes matched, and targets |
| 4.6 the product identity for two groups written next to each other | `spec_compileProduct` | random: conjuncts from fixed pools (with `@`, `@1`, `!{@1}`, `\|` and forks; no `++`), in one of 7 contexts (none, which is drawn twice as often as each of the six others), over 400 pairs | up to route order; exact where every conjunct is one route without a union |
| 4.6 with steps between the groups | `spec_lawProduct` in `LawsSpec` (next row): 80 of its pairs have one step or two between the groups and nothing else, and 80 nothing. `spec_compileUnionFree`: `(x/@/w & b)/k/(c & d)` against its four walks written out | random; fixed, one example | up to route order; exact |
| 4.6 with a `\|` union between the groups: that it holds without a join of its own chain, an `&` or a `++` in a side, and fails with one; and that it fails with a join between the groups | `spec_lawProduct` in `LawsSpec`: two groups and what is between them, against every conjunct of the one continued by every conjunct of the other, in a `genFollowed` context. 320 pairs within the condition (`productWithin`), each path within the size limit `fewAlternatives 100`: the first 160 drawn that have a `\|` union between the groups, the first 80 with steps only and the first 80 with nothing, the three kinds taking turns. Half the draws are groups of two or three conjuncts with joins and `\|` unions, and between them nothing or one or two of a step and a union of two or three sides. The other half are small: two groups of two conjuncts, each one or two steps, often with a join, and between them nothing, a step or a union of two sides. The small draws also make, mostly one at a time, what the clauses exclude: a join, an `&` or a `++` in a side, a conjunct `x/(y & z)` or `x ++ y`, a join or a `++` between the groups (the larger draws seldom fit the limit with these: the written-out path of the test's pair with a `++` in a side, `a/((x ++ y) \| z)/c & …`, has 81 alternatives). Within the condition a conjunct that forks has a join written in the chain the fork is written in (`heldTogether`, next row), and a side is a walk of steps and `*`, a `\|` union of such walks, or `%never`. Each clause dropped, six: no join of its own chain in a side; no `&` in a side; no `++` in a side; a conjunct that forks has a join; no `++` in a conjunct or between the groups; no join between the groups | random; fixed (the document's pairs for the identity and for each way it fails; and two pairs of the test's own, one with a `++` in a side, and `(a & b)/@/(c & d)` for a join between the groups) | up to route order; the fixed pairs targets |
| 4.6 for a conjunct that forks in a side of a union: that it holds where a join is written in the chain the fork is written in, and fails where the join is in another side, or the fork outside the union | `spec_lawProduct` in `LawsSpec`: 200 pairs of `genSidedProduct` (two groups with a step or nothing between them, in a `genFollowed` context) in which some conjunct forks and has no join written in its own chain (`forksInSide`), within the condition, each path within the size limit `fewAlternatives 60`. Half the draws have two or three conjuncts in a group, each a walk, a walk with a join, or a union in whose sides, or next to which, forks and joins stand. The other half are small: two conjuncts in a group, one a walk with or without a join, the other that again or, half the time, a union of two sides, one of them a fork of two steps, with a join in that side, in the other side, or nowhere. The condition is `heldTogether`: every fork written in the conjunct's own chain or in a side of a union there, however deep in sides, has a join written, before it or after it, in the chain the fork is written in or in a chain around that (for a fork in a side: the side, the chain its union is written in, and so on out to the conjunct's own chain); a fork inside a conjunct of a group does not count. Commands run both paths of at least 10 of the pairs; of the others they refuse the first (3.1). Each clause dropped, two: the join is written in the chain the fork is written in or in a chain around it (in its place: anywhere path-language.md, section 4, counts a join of the conjunct's own chain); a conjunct that forks has a join | random; fixed (`(x/@ & b)/(c & (e & f \| @))`; the pair the first review of these tests found, `(~c/@ & a)/(b/a & (b & c \| @))`, with and without what was around it; and five that commands refuse, so that it is what the compiler makes of them that is compared: `(x/@ & b)/(c & (e & f)/(g/@ \| h/@))`, `(x/@ & b)/(c & (e & f)/k/(g/@ \| h/@))`, `(x/@ & b)/(c & (g/@ \| h/@)/(e & f))` and `(x/@ & b)/(c & ((e & f)/@ \| (g & h)))`, which differ from their products, and `(x/@ & b)/(c & k/@/(m/(e & f) & z))`, which compiles as its product) | up to route order; the fixed pairs targets, the last one exact |
| 4.7 `(A & B)/C = A/C & B/C` under its condition, and each way it fails | `spec_lawLeftDistribution` in `LawsSpec`: 200 pairs within the condition (`leftWithin`: no join of its own chain in a conjunct, none in `C`, no `++` in `C`), two or three conjuncts and a `C` in a `genFollowed` context. Each of them is a random path or a small one (`genDistribution`): one or two steps, such a walk with a join, a union with that in a side, and, for `C`, also a fork of two walks, a union with such a fork in a side, or two walks with `++`. The property also compares 400 pairs within the condition with nothing aimed at a clause (`unaimedLeft`): every conjunct and `C` a random path, in no context (three draws in five) or in a random one; no clause is dropped on these. Each clause dropped, four: no join of its own chain in `A` and `B`; none in `C`; no `++` in `C`; a join in the top chain of a side of a union is a join of the chain | random; fixed (the document's pairs; `(a & b)/c/@/d` on another graph, since both paths find nothing on the document's) | up to route order; the fixed pairs targets |
| 4.8 `R/(P & Q) = R/P & R/Q` under its condition, and each way it fails | `spec_lawRightDistribution` in `LawsSpec`: 200 pairs within the condition (`rightWithin`: no join of its own chain and no `++` in `R`, and no join of their own chain in `P` and `Q` when `R` has an `&` anywhere), from the same generator, `R` drawn as `C` is; and 400 more with nothing aimed at a clause (`unaimedRight`), drawn as those of the row of 4.7, on which no clause is dropped. Each clause dropped, five: no join of its own chain in `R`; no `++` in `R`; no join of their own chain in `P` and `Q` when `R` has an `&`; an `&` in a side of a union of `R` is an `&` of `R`; a join in the top chain of a side of a union is a join of the chain | random; fixed (the document's pairs) | up to route order; the fixed pairs targets |
| 4.7, 4.8 older tests | `spec_compileCopies`: `(p & q)/r` is `p/r & q/r` and `r/(p & q)` is `r/p & r/q`, for the two steps `p` and `q` and a random `r` that has at least one `\|`. Single failures elsewhere: a join in a copied conjunct against the pin written out (`spec_compileChainScoped`, targets), `(a & b)/@/c` against `(a & b)/(@/c & @/c)` (`spec_compileRepeatedConjunct`, targets), a `++` chosen before the product (`spec_compileProduct`, exact) | random (`genPath` of `spec_compileCopies`), over 100 pairs; fixed | up to route order; targets; exact |
| 4.9 `++` distributes over `/` | `spec_lawUnionsAndSequencing` in `LawsSpec`: `C[p/(q ++ r)]` against `C[p/q ++ p/r]` and `C[(p ++ q)/r]` against `C[p/r ++ q/r]`, over 200 pairs of random `p`, `q`, `r` in no context or a `genHoled` one. The same pairs written with `\|` have to be told apart by `ls` within the first 40 | random; fixed (the document's two pairs, and its two for `\|`) | exact; the `\|` pairs targets |
| 4.9 a union that nothing copies finds the same written `\|` or `++` | `spec_lawUnionsAndSequencing` in `LawsSpec`: `C[p \| q]` against `C[p ++ q]` for 150 pairs, `C` a `genHoled` context in which nothing copies the union (`uncopied`; in at least 12 of them only because a join in the side the union stands in cuts it off, as in the row of 4.2), `p` and `q` random. Each clause dropped, the four of the row of 4.2. (`+` is lowered to one of the two before a path is compiled, so there is nothing to test for it here) | random; fixed (the document's pairs, and pairs for a join in the side the union stands in, for a join in a side of another union and for an `&` in a side of another union) | targets, 40% |
| 4.9 `a \| b` finds what `regex:"^(a\|b)$"` finds | `spec_lawWild` in `LawsSpec`: `a \| b` against the regex in 40 `genHoled` contexts, on graphs that have the label `c` too. The law has no condition; that the comparison can fail is shown by `a ++ b`, which has to differ from the regex within the first 40 | random; fixed (one graph) | targets, 40% |
| 4.10 a conjunct written twice, sealed (with `@/` before it and `/@` after it) | `spec_lawConjunctTwice` in `LawsSpec`: `C[@/(q & q)/@]` against `C[@/q/@]` for 150 pairs in which commands run both paths, `C` a `genHoled` context and `q` a relative path (`relative`): in half the draws a walk with a join, in one of fourteen two steps with `++`, in one of seven a random path that has a join of its own chain or a `++`, and in two of seven any random path. With them, 80 pairs with nothing aimed at the seal (`unaimedTwice`): `C` a context of the first five kinds of `genHoled` (3, 4, 3, 3 and 1 draws in 14) and `q` a random path, in three draws of five one that has a join of its own chain or a `++`. And `q & q` against `q` as whole queries for 60 random `q`. One clause dropped: the seal (`@/` before both paths and `/@` behind). The other two clauses of the condition are not dropped, because the property passes without either: the test requires instead that no pair is found to differ among 80 with a `q` that is not relative (`genEnded` paths in which an `@1` starts a walk) and 40 of which commands run one path and refuse the other (there it compares what the compiler makes of both). Older: `spec_compileRepeatedConjunct`, random `q` without `@N` (its `genPath`) in 7 contexts on 6 corpus graphs; five of the contexts have a `c` step, which no corpus graph has, so both sides are empty there | random; fixed (the document's three blocks, and one more graph for `(a & b)/(@/c & @/c)`) | targets, 35% for the sealed pairs (sealed, all the walks have to end at one node, and fewer comparisons find one) and 40% for the whole queries |
| 4.10 `ls P/(q & q)` finds what `ls P/q` finds | `spec_compileRepeatedConjunct`, only for the pairs in which `P` has no `&` anywhere (`oneOutput`) or `q` has no `@` and no `!{…}` in its own chain or in a union side there (`joinsItsChain`); its fixed counterexample outside that is `(a & b)/@/c` against `(a & b)/(@/c & @/c)` | random (its `genPath`), over 120 pairs, 8 corpus graphs | targets |
| 4.11 `mk` makes the path hold | `spec_residuationProperties` in `Models.PathPattern.ResiduateSpec`: after a possibility is created its own binding matches the pattern, `mk` again says the path resolves, and creating it twice changes nothing; `spec_walkLaws` in `Models.PathPattern.WalkSpec`: the same, and that the result does not depend on the order of conjuncts or union sides and that `mk` fails for a written node only when it does not exist. That a query of the path then finds a node is not tested | corpus, random paths and graphs | the pure walk, no command |
| 4.12 reversal | `spec_compileReversal` | random (`genRelative` and `genJoined`), relative, without `@N`: over 550 paths on graphs of three nodes | targets from every node to every node; the path is run exactly when its reverse is |
| 4.12 a path is refused exactly when its reverse is | `spec_compileCopiedMeetingPoints` | random (`genScoped` without `@1`), 3,000 paths | the verdict (refused, warned, quiet), both ways |
| 2.9 a union of `n` sides, each with a join in its own chain, is at most `n` nodes | `spec_lawSidesAndNodes` in `LawsSpec`. A union of two or three sides (half the time all written alike), each a walk, a join (`@` or `!{@1}`) and a step `m` after it, in a `genHoled` context or, one draw in three, in one with a fork of three steps next to the union or next to the group it is in a conjunct of. Within the condition the join is written in the own chain of every side (two unions in five are drawn with every side a group that has the join in a conjunct), and forks may copy the union but nothing copies a conjunct it stands in (`uncopied` without its first clause): in every alternative the `m` steps leave from at most `n` vertices (200 paths; in over 50 more routes go through the union than it has sides, and in over 70 the `n` vertices are reached). The same bound is required of 200 more unions with nothing aimed at a clause (`unaimedBounded`): the context is one of the first five kinds of `genHoled` (3, 4, 3, 3 and 1 draws in 14), and one side in four, each drawn by itself, is a group that has the join in a conjunct; no clause is dropped on these. Each clause dropped, two: the join of each side is written in the side's own chain; no product copies a conjunct that the union stands in. And `X/(S1/@ \| … \| Sn/@)/(c1 & … & ck)` against the `\|` of every way to give each continuation a side, those with the same side leaving from its one join (40 pairs); with the path added in which each continuation has a join to itself, `ls` has to tell the two apart within the first 40 | random; fixed (the document's examples, and one more pair for a join inside a conjunct of a side) | the number of vertices in what `compilePath` gives; up to route order; the fixed pairs targets |
| 2.10 a relative path starts as if `@/` were written before it | `spec_lawRelative` in `LawsSpec`: `P` against `@/P` for 200 relative paths (`relative`, which reads the path with every `~` applied), over 60 of them with an `@N` and a `~` around more than a step (`genEnded`: random paths, mostly with an `@1` at an end of a walk and often a `~` around it). Each clause dropped, two: `P` is relative; where a walk starts is read with every `~` applied (in its place: off the path as it is written) | random; fixed (`a & @1/b`, `~(a/@1)`, `~(b/@2)`, and what `relative` says of six paths) | targets, 30% |
| §4 the alternatives that match are gone through by their smallest witness | `spec_lawOrderOfAlternatives` in `LawsSpec`: for random paths on the random graphs, in over 600 cases with several alternatives that match, the resolutions are in the order of the smallest of all their witnesses (in over 70 that is not the order of the patterns); fixed: `b \| regex:"^a$"` on `0 -a-> 1`, `0 -b-> 2`, sides in either order, where the pattern with `b` sorts first and the alternative through `a` comes first. Goldens `path/union-witness-order` (`ls` lists `a` first) and `path/tag-union-witness-order` (`tag (b \| regex:"^a$") t` links node `1`); `path/tag-union-route-order` has `tag (b + a)`, which either order answers alike | random; fixed; three goldens | the order of `resolveQuery`'s resolutions; what `ge` prints and does |
| 3.1 the check agrees with the rule worked by writing the product out (`handRefuses`) | `spec_compileCopiedMeetingPoints` | random (`genScoped` with `@1`, 2,500 paths) and planted (`genPlanted`, 2,000) | the verdict, both ways |
| 3.1 the check agrees with the compiled routes (`copiedJoins`) | `spec_compileCopiedMeetingPoints`, in three parts. (a) 1,500 random paths with `@1` (`genScoped`) and 1,500 planted ones (`genPlanted`): no path that is run has, in any alternative, a written join that is two vertices with a fork at one (over 1,500 run and within the size limit). (b) 1,500 other random paths without `@N` and the same planted ones (over 2,400 within the limit; the source's condition is that no node tests contradict each other): the path has such a join, copies counted as written, exactly when it is refused. (c) the first 1,500 of (a), which are the random ones, less those over the size limit: what has such a join is refused | random and planted, 4,500 paths | (a) and (c) one way, so the check may refuse more there (*Meeting points*); (b) both ways |
| 3.2 a warned `@` can be left out | `spec_compileCopiedMeetingPoints` | random (`genStretchy`, `genScoped` with `@1`, `genPlanted` and `genIdle`), over 400 warned paths | exact, with and without the `@`. One way only: nothing says that an `@` that can be left out is warned about |
| 3.2 a command line prints each warning text once | goldens `path/warning-once-same-path` (the same path twice on one line: one warning), `path/warning-once-same-text` (two paths whose warnings read the same: one) and `path/warning-twice-two-texts` (two paths whose warnings differ: two); `path/join-does-nothing` has a path run at two nodes by `at` among its lines | fixed, each one `ge -e` line | what `ge` prints on stderr |

One law that the tests state has no number in path-language.md (2.9
has it in words: a query finds what any alternative finds):

- *A union is the choices of its routes* (`spec_compileUnionIsItsChoices`,
  random, over 300 cases, exact): the alternatives of `C[p | q]` in
  which no route took `q` are those of `C[p]`.
  `spec_compileGridOracle` (50 random shapes of two prefixes, a `|`
  union and two continuations, on three graphs each, targets) checks
  that such a path reaches what its alternatives, each written by hand
  as a path without unions, reach together.

Two more have no counterpart on the surface:

- *Conjunctions may be written out* (`spec_compileSharedWalks`):
  replacing adjacent segments without unions or joins by their product
  changes no alternative. `(x/*/@ | y)/(c & d)/(a & b)` is
  `(x/*/@ | y)/(c/a & c/b & d/a & d/b)`.
- *Prefixes that compile the same are the same prefix*
  (`spec_compileInterchangeablePrefixes`): two paths without unions that
  compile to the same pattern alone, before each side of the unions that
  follow, and before a continuation that forks and joins (`(m & n)/@`,
  `@/m & n`, `m/@ & n`), give the same alternatives before those unions,
  up to the order of their routes and with the same first routes:
  `(a & b) & c` and `a & (b & c)`, or `x/(a & b)` and `x/a & x/b`,
  before `(y | d/@)/e`. A leading `@` before a fork makes another prefix,
  and a conjunct it keeps together has its routes next to each other in
  every alternative, which is where the order can differ.

`CompileSpec` has a second compiler for paths without `|`, which
rewrites terms (`referenceCompile`). `spec_compileUnionFree` compares
with it: a path without `|` compiles to
what the reference makes of its product written out, and to exactly what
the reference makes of the path itself outside two classes (a join in a
conjunct of a group that a later group copies, and a conjunct that is
only `@` before an `@N`), in which route 0 still has the same steps.

**How many alternatives.** Alternatives multiply: a `k`-way `|` copied
into `n` conjuncts gives `k^n` alternatives (`(a | b | c)/(d & e & f & g)`
has 81), and a chain of unions multiplies too, with or without a
conjunction, and with `++` as with `|` (`k` unions of `n` sides are `n^k`
alternatives). Alternatives that glue to the same pattern are one, and
ways to choose that contradict each other are none:
`(@1 | … | @31)/(a & b)` has 961 alternatives and with 32 nodes 1024.

Where the counts in this document come from. Tests assert these:
`(@1 | … | @12)/(a & b)` has 144 and with 20 nodes 400
(`spec_compileManyJoinedSides`); six to nine unions joined by `&` have
64 to 512 (`spec_compileIndependentUnions`);
`((a & b) | x)/((c & d) | e)` has 8 and
`(c & d)/(x/@ | y)/(w/@ | z)/(a & b)` 152, of which
`compileAlternatives` spells 132 (`spec_compileIndependentUnions`,
`spec_compileChainScoped`); `(p & q)/(k & y | m)/(d | (b & c))` has 64
and `((x | y)/@ & z)/(m/@ | n)/(c & d)` 56 (`spec_compileCopies`);
`(a & b)/(@1 | … | @64)/@1` has one, and
`(c & d)/@/(@1 | … | @k)/(a & b)` has `k`, asserted for 46 nodes
(`spec_compileContradictions`); `a/(@1 | @2)/(x & y)` has four
(`spec_compileJoinedSides`);
the 2 against 1 and the 7 of *Relational unions in detail*, 6
(`spec_compileSameSides`, `spec_compileEverySide`); and the 64 and 729
ways listed for the two paths of *Meeting points*
(`spec_compileCopiedMeetingPoints`). The other counts were counted
with a scratch tool on 2026-10-11 (a program linked against the
compiler that prints the number of alternatives of `compilePath`), and
are not pinned by a test: 81; 961 and 1024; the 146, 454 and 1394 of
`(p & q)/(x/@ | y)/(w/@ | z)/(u/@ | v)/(s/@ | t)` and its longer
forms (the 146 that `spec_compileCopies` asserts is of another path,
`(a & b | x)/(c & d | e)/(f & g | h)`); 4 of 4374 routes; 512 against
2; the four of `(x/@ | y)/(c & d)`; the 2 of
`x/@/(@1 | @2)/(a & … & j)` (the tests have the like with five nodes
and five conjuncts, 5); and the 62.

Ways to choose that contradict each other are dropped as they are made,
not listed and then counted: two routes at one joined node cannot choose
different `@N` there, so the second is not tried with a node the first
did not choose. This is what a `%targets(…)` after a join costs:
`x/@/(@1 | @2)/(a & … & j)` has 2 alternatives out of `2^10` ways to
choose, `(c & d)/@/(@1 | … | @k)/(a & b)` has `k`, and
`(a & b)/(@1 | … | @64)/@1` has 4096 ways and one alternative.

Nor is a route tried with others before it is known to have a way to
choose at all: a conjunct that cannot be satisfied, as `(@1 | @2)/@3`
or `@/(@1 & c)/@2`, makes the path compile to nothing at once wherever
it is written among 22 unions joined by `&`. And a side that some
route through a copy cannot choose is not tried for the other routes
of that copy (`narrowed`). These are tests,
`spec_compileContradictions`; with nothing limiting what a path may
compile to, they are what keeps such paths from taking as long as their
choices are many. One kind of dead end is known that they do not see:
two routes that must choose two unions alike by one of them and
differently by the other.

A join of the chain is gone through by every route, so the stretches
before and after it choose apart, and only the routes of one stretch
are compared two by two: `(a & b & c)/…/@/(x | y)/(a & b)` with seven
groups before the join has 4 alternatives of 4374 routes each. Unions
with joined sides in one chain have fewer alternatives than ways to
choose: `(p & q)/(x/@ | y)/(w/@ | z)/(u/@ | v)/(s/@ | t)` has 146, with
a fifth such union 454, and with a sixth 1394. A
union before a join in a conjunct of a group that a later group copies
chooses in every copy of the conjunct (*Relational unions in detail*,
5): `((x | y)/@ & z)/(c1 & … & c9)` has 512 alternatives, where
`(x | y)/@/(c1 & … & c10)`, or the same with `++`, has 2.

**No limit on what a query compiles to.** No query refuses a path for
being large. A query compiles its path however many alternatives it
has and however many routes they have, for as long as that takes, and
some paths that are short to write do not finish:
`(x | y)/(a & b & c)/…` with seven such groups has one union that 2187
routes choose on their own. The only paths a query does not run are
those that would copy a meeting point. What the user can do about a
command that runs long (Ctrl-C, and TAB completion giving up after
`Executable.GraphEditor.Completion.completionAllowance`, half a second)
is in
[commands.md](commands.md#51-what-a-path-compiles-to);
what was tried as a limit on alternatives, why it went, and the
measurements are in [path-cost-limits.md](path-cost-limits.md).

What `mk` and `tag` make is limited twice (§5). Before compiling,
`Models.PathPattern.Admit.checkExpansion` refuses a path whose size,
estimated in one pass over the path as written (`expansionEstimate`),
is more than `expansionLimit`, 256 route steps over all alternatives
(`ExpansionTooLarge`). The estimate counts every union as if each copy
of it chose a side of its own, `++` and `+` included, so it counts
high, often far above what the path compiles to (no path was found
for which it counts low): `mk (a | b)/(c & … & j)`, with 256
possibilities, is refused by it. And the
walks of each alternative, which depend on the graph, are limited by
the option `--max-repairs` of `mk` and `tag`. Measured with `ge` in an
unoptimized (`--fast`) build, on a machine that was not idle, two runs
each: `ge -e nid` takes 0.05 s, and `mk (a | b)/(c & d & e & f)` on a
graph with none of its edges, which lists 16 possibilities, 0.06 s.

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
after the fact. (`chosen` and `narrowed` are local to `resolveStretch`;
the reference compiler of the tests is described after the law table
above.)

**Directives.** What each directive means to the user is in
[path-language.md](path-language.md#1-syntax) (the table of section 1)
and [commands.md](commands.md#26-targetsp) (2.6, 2.7 and 5). For the
compiler: a command keeps its paths unexpanded until it runs, then
expands each one (`Graph.Command.expandDirectives` for a query,
`expandConstruction` for a constructed path, both through
`Models.Path.handleDirectivesWith`), left to right, before it does
anything else; expanding also lowers `+` (*Unions* above), and the
expanded path is then checked (`checkPath`).

- `%override({plus = …}, p)` expands `p` with `+` lowered accordingly.
  It has no other setting: `max-repairs`, which it once had, is an
  option of `mk` and `tag` (`Make MaxRepairs ParsedPath` and
  `Tag MaxRepairs ParsedPath ParsedPath` in `Models.Command`), parsed
  before the path, and an `%override` that sets it is refused when it
  is parsed (`InvalidOverrideSetting`).
- `%history(n)` and `%last` become `@N` for the location in the history
  at that moment.
- `%targets(p)` becomes `%never + @N1 + @N2 + …` for the current targets
  of `p`, queried from the current location with `+` as a query's. The
  result is a `+` union, lowered where the `%targets` stands. When `p`
  has no targets the expansion is just `%never`, and if the command then
  fails, its error ends with a note naming the empty `%targets(…)` and
  its position. Many targets before a conjunction multiply like the
  sides of any `|` (`test/golden/ge/directives/targets-many.golden`).
- A URL imports the resource and becomes its `@N`, at most once per
  command line for each distinct URL.
- `%{…}` splices only work in the path quasiquoter. A command line
  containing one is rejected when it is parsed (`SpliceInCommand`), so
  nothing on it runs.

`at p cmd` expands `p` once, then runs `cmd` at each target with the
location set to that target (a `Reader` override, which is why a `cd`
inside the block does not move the commands after it), so `cmd`'s
directives are expanded per target. Each target starts from the history
`at` was given, and `at` restores it afterwards. Only completion
(`Models.LegacyPartialPath`) rejects directives at parse time, with
`IllegalDirective`, which the parser helpers it shares raise
(`Utils.Parsing`).

## 3. Selection

| consumer | derived from |
|---|---|
| `rn`, `mv`, `cp`, `cl` target edge | last hop of route 0 |
| preview / leftmost path | all hops of route 0 |
| tags, FileType ("final edges") | last hop of each route |
| targets | route endpoints |

What each command then does with what it selected, and its messages, is
in [commands.md](commands.md#3-the-edge-commands-rm-mv-cp-rn-cl);
`Graph.PathPattern.Execution` holds that policy. Query commands append
`/@` before compiling, so that every route ends at one vertex;
construction (`mk`) does not. The engine never adds defaults itself.

- If route 0 has zero hops (`rn (@ & a) c`, `rm @`), it is an error.
- If the path `rm`, `mv` or `cp` takes its edges from matches nothing,
  there is no route 0 at all: the command changes nothing and prints a
  notice, which is a warning and not an error
  (`test/golden/ge/no-match/`). `rn`/`cl` treat a source that matches
  nothing as an error.
- The `rn`/`cl` destination creates one edge. It is resolved as
  `destination/@endpoint` with its last pattern edge dropped, and the
  edge is recreated from the node before it. That last hop must be a
  literal: matching an existing label for a wildcard would pick an
  arbitrary one. The prefix must resolve to a single parent node
  (`SeveralParents` otherwise; `--all-parents` creates the edge under
  each), and a destination whose alternatives come to different edges
  is an error rather than an edge for each (`prospectiveEdges` compares
  the edges of the alternatives that match; when they all come to the
  same edges, as in `rn a (b/z | c/z)` where `b` and `c` lead to one
  node, there is no error).
- `tag p q` links the outputs of the first witness of the first
  matching alternative of `p`, in the order of §4, to the target; other
  witnesses are not linked.
- `x/(@ & a)/@` has routes `[x, x·a]`: rename targets `x`, and final
  edges are `{x, a}`. `x/(a & @)/@` has routes `[x·a, x]`, so rename
  targets `a`. The unjoined `/` copies `x` into both routes (two
  independent `x` edges); with `x/@/(@ & a)/@` the `x` edge is shared
  and `a` is a loop on its target.
- `(a & b)/@ & c` has routes `C -a-> J`, `C -b-> J`, `C -c-> v`. As a
  query (`/@` appended), `v` merges into `J`.

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
data Component = Component
  { vars  :: Set VarId
  , edges :: [EdgeId] }      -- in order of first appearance along the routes
data Partial = Partial
  { nodes :: Map VarId NID, labels :: Map EdgeId Text }
data Resolution = Resolution
  { patternPath :: PatternPath                -- `pattern` is a keyword
  , current    :: Maybe NID
  , components :: [(Component, [Partial])] }  -- no list of partials is empty
data ResolvedPath = ResolvedPath
  { resolutions      :: [Resolution]     -- one per matching alternative
  , graph            :: Graph Text IsThin
  , nonexistentNodes :: Set NID }

data Witness = Witness                   -- a complete match
  { nodes :: Map VarId NID, labels :: Map EdgeId Text }

targets        :: Resolution -> Set NID
selectedRoutes :: Resolution -> [ConcreteRoute]          -- route 0, with its
                                                         -- root, in witness order
routeLastEdges :: Resolution -> Map Int (Set (Edge Text))
witnesses      :: Resolution -> [Witness]                -- lazy product
```

- **Order.** Components are ordered by first appearance along the routes.
  Partials within a component are ordered lexicographically by their
  concrete hops in route order. `witnesses` enumerates component-major.
  The first witness equals the lexicographic minimum of the full ordering.
  The alternatives that match are ordered by that first witness
  (`resolveQueryWith` in `Graph.PathPattern.Resolve`:
  `sortOn firstWitnessKey`, the concrete routes of the first witness
  compared as lists). This is the one order in which commands go
  through alternatives, and it depends on the graph, not on how the
  sides of a union are written or on the order of the compiled
  patterns. What the commands take from it: `ls` lists the selected
  routes resolution by resolution in that order
  (`Graph.Command`, `Preview`); `tag` takes the first witness of the
  first resolution (`constructTag` in `Graph.PathPattern.Execution`);
  `rm`, `mv`, `cp`, `rn` and `cl` take the selected routes of all
  resolutions as a set (`selectedConcreteRoutes`), and targets are
  sets. On `0 -a-> 1`, `0 -b-> 2`, `0 -t-> 9`,
  `tag (b | regex:"^a$") t` links node `1`: the witness through the
  label `a` sorts first, though the pattern with the literal `b`
  sorts before the one with the regex. The golden
  `tag-union-route-order` has `tag (b | a) t`, which both orders
  answer alike.
- A route through a `Fixed` vertex spans components. Its concrete hops
  combine only those components' partials.
- A pattern that does not match has no `Resolution` at all
  (`matchPattern` gives `Nothing`), and neither has one of whose
  components some has no solution. A `Resolution` without a component is
  a match: it is that of a path without a step, such as `ls @`.

## 5. Construction: what `mk` creates

`mk p` makes `p` resolve by creating nodes and edges, in three modules:
`Models.PathPattern.Admit` refuses the paths it does not build and puts
the steps of each other path in order, `Models.PathPattern.Residuate`
walks the path in that order and chooses among the walks, and
`Graph.PathPattern.Execution` (`planConstruction`) runs these steps and
applies or reports the result. Nothing is decided from the far end of a
step: a step looks at the node it stands on and at nothing else. No
step waits for another or is taken twice. (The module and function names still say
"residuate" and "repair", from a search that this replaced; nothing in
them searches.)

A **walk** takes every step of every route of one alternative, with one
node chosen wherever steps had edges to several nodes to follow. What a
walk creates is a **possibility**: the pattern with every vertex at a
node and every step along an edge, and the nodes and edges that have to
be created for that.

```haskell
data Binding = Existing NID | Fresh Int
data Residual = Residual
  { pattern  :: PatternPath
  , sigma    :: Map PVertex Binding   -- where every vertex is
  , labels   :: Map EdgeId Text       -- the edge every step goes along
  , patch    :: Patch                 -- nodes + edges to create
  , branches :: [Branch] }            -- where there were several nodes to go to
data Branch = Branch { vertex :: PVertex, taken :: Binding, others :: [Binding] }
```

The nodes of a patch are all `Fresh`: no node is created at an id that
is given (*Written nodes* below).

**Refused shapes** (`Admit`). Before anything else, and before the
graph is read, a path is refused for its shape
([commands.md](commands.md#48-paths-that-mk-refuses-for-their-shape)
has them for the user, with what to write instead). A refused path is
refused whatever the graph holds, also where it already resolves.

- **X**, `ExpansionTooLarge` (`checkExpansion`, on the path as written,
  before it is compiled): `expansionEstimate`, one pass over the path,
  is more than `expansionLimit`, 256 route steps over all
  alternatives. The estimate is deliberately high: every union is
  counted as if each conjunct next to it had a copy of its own, also a
  `++`, whose copies the compiler shares, so `(a ++ b)/(c & d & e & f &
  g)` is refused and `a/(c & d & e & f & g) ++ b/(c & d & e & f & g)`,
  which compiles to the same, is not. `shrinking` works out which
  rewrites the message may offer (moving the `+` and `++` unions out,
  keeping one side of each).
- The other four are found in the compiled routes of each alternative
  (`refusals`), in this order, and the first alternative that has one
  refuses the whole path, even where other alternatives could be
  built:
  - **R**, `ReturnsToMeetingPoint`: a route comes back to an unnamed
    `Var` it has visited (`mk a/@/(@ & a)/@`).
  - **M**, `WildcardJoin`: a `*` or regex step is one of several steps
    that place one unnamed node (`mk (* & a)/@`).
  - **V**, `SeveralWildcards`: an alternative has more than one `*` or
    regex step (`mk */a & */c`, and `mk */(a & b)`, whose `*` the
    product copies).
  - **C**, `CircularSuppliers`: the operations of the plan (below) that
    must each come after another of them form a circle
    (`mk (a & b)/@ & a`, `mk a/!{@1}/b & a/c`, `mk a & ~a`).

  R takes one pass over the hops of each route, M one over the
  operations and V one over the steps; C is the plan, at most
  O(m (m + L)) time and O(m²) space for m steps and L hops. The
  current node counts as a written node, so whether C refuses
  `mk (a & b)/@ & @5/a` depends on whether it runs at node 5. The
  messages (`renderRefusal` in `Graph.PathPattern.Execution`) name the
  written steps, a step that the product copies once with the number
  of its copies (`writtenSteps` in `Models.PathPattern.Suggest`), and
  suggest a join after such a step only where that is the user's whole
  path and is itself admitted.

**The plan** (`planOf`). An **operation** places an unnamed node, by
all the steps into it at once, or connects two known nodes (written,
or placed before) by one step. Its **distance** is how far its steps
are from the start of their routes, counted from 1, the furthest
counting: of the routes that share a step, and of the steps into one
node. An operation comes after the steps before its own on their
routes, after the placing of the node it starts from, and after every
possible **supplier**: an operation one of whose steps has a literal
label that a step of the first accepts, is no further from its route's
start than the first (same pace, rule 4 below), and could, as far as
anything written says, create an edge from where the first's step
stands to where it may go. A connection with a literal label follows
nothing and has no suppliers. Operations that are the same ordinary
request (one step each, into an unnamed node, with the same literal
label, direction, test and distance) may be each other's suppliers and
are done together (`mk a & a` creates one edge). The strongly
connected components of "comes after" are put in **batches**, each
after those it comes after; a component that is a circle, and not one
such request, is refused (C).

**The rules** (`walksOf`). Each alternative (each choice of union
sides) is walked on its own, its batches in order, each batch once for
each walk, all its operations looking at the same state: the graph and
what earlier batches created.

1. **A step.** Standing on a node, a step with a label and a direction
   looks at the edges with that label and direction at that node whose
   other end passes the step's test (`!{…}`): the edges of the graph,
   and the edges this command created in earlier batches that count
   for it (rule 4). If there is none, the step creates a new node and
   the edge to it. If they all lead to one node, it goes there. If they
   lead to several nodes, each is tried in a walk of its own, and the
   choices of the operations of a batch multiply; an edge created in
   one walk is not seen in another. No edge is preferred, neither an
   existing one over a created one nor the other way round.
2. **A step to a known node.** A step between two known nodes (followed
   by `@N`, or into a join already placed) with a literal label creates
   its edge unless that edge was there when its batch began. A `*` or
   regex step goes along an edge between its two nodes if one counts
   for it, also one its own batch created, and needs a label otherwise
   (*Labels* below).
3. **A join.** The steps into a join (`/@`, `/!{…}`) are one operation.
   The nodes that any of them could follow an edge to are the choices;
   with none, one node is created, with an edge for each step. A step
   with no edge to the node taken creates one. The edge of one step of
   a join is never followed by another step of the same join, so
   `mk (* & a)/@` would need a label for the `*` (it is refused, M).
4. **Which created edges count: the same pace.** The steps that found
   an edge missing in one batch and created it are its creators. An
   edge that this command created counts for an operation if one of its
   creators is no further from the start of its route than the
   operation's distance (the furthest of its steps): the steps into a
   join follow edges created that far or nearer, and an edge that one
   of them creates counts for another operation at the distance of
   that step. A step that goes along an edge created in an earlier
   batch is not one of its creators. So in `mk a & b/~b/a/@2` the first
   `a` (distance 1) does not see the `0 -a-> 2` that the last creates
   (distance 3), and in `mk x/~x/a & a/@2` the `a` at distance 3 sees
   the one created at distance 1.
5. **One node per place.** Operations of one batch that create from the
   same node with the same label and direction create the same node.
6. **`*` and regex.** Such a step follows every edge whose label
   matches and that counts for it, existing or created. If it has to
   create an edge, the label comes from the caller; `mk` gives none
   (*Labels* below).

Examples, with node 0 current and node 2 existing
(`spec_walkExamples` in `Models.PathPattern.ResiduateSpec`):

| command | graph | creates |
|---|---|---|
| `mk a & a` | | `0 -a-> new1` (one request) |
| `mk a/b & a/c` | | one `a` node with `b` and `c` under it (5) |
| `mk a/a` | | `0 -a-> new1 -a-> new2`: the second `a` stands on `new1` |
| `mk a & a/@2` | | `0 -a-> 2` only: both steps are at distance 1 (4) |
| `mk a/b & a/@2/c` | | `0 -a-> 2`, and `b` and `c` under 2 |
| `mk a/~a` | | `0 -a-> new1` only: `~a` walks back along it (4) |
| `mk a/~a/b` | | `0 -a-> new1`, `0 -b-> new2` |
| `mk a/b/@2 & c/b/@2` | | two nodes: `0 -a-> new1 -b-> 2`, `0 -c-> new2 -b-> 2` |
| `mk (a & c)/@/b/@2` | | one node: `0 -a-> new1`, `0 -c-> new1`, `new1 -b-> 2` (3) |
| `mk a/b/@2` | `1 -b-> 2` | `0 -a-> new1 -b-> 2`: nothing is taken from the far end |
| `mk a/@1/b/@2` | `1 -b-> 2` | `0 -a-> 1` |
| `mk (a & c)/@` | `0 -a-> 1` | `0 -c-> 1` (3) |
| `mk x/~x/a & a/@2` | | `0 -x-> new1`, `0 -a-> 2`: the `a` at distance 3 follows the edge created at distance 1 (4) |
| `mk a & b/~b/a/@2` | | `0 -a-> new1`, `0 -b-> new2`, `0 -a-> 2`: the `a` at distance 1 does not see the edge created at distance 3 (4) |
| `mk a & x/@0/a/@2` | | `0 -a-> new1`, `0 -x-> 0`, `0 -a-> 2`: the distance is counted along the route, also past a written node (4) |
| `mk (x/y & a)/@ & a/@2` | | `0 -a-> 2`, `0 -x-> new1`, `new1 -y-> 2`: the join is at distance 2 and follows the edge created at distance 1 (4) |
| `mk (x/y/z & w)/@ & w` | | refused (C): the join's `w` and the other `w` might each follow the other's edge |
| `mk a/~a/a/@2` | | `0 -a-> new1` and `0 -a-> 2`: the first `a` does not see the edge the last creates at distance 3 (4) |
| `mk (a/x & a)/!{@5}` | `0 -a-> 5` | `5 -x-> new1`, `0 -a-> new1`: the first `a` goes along `0 -a-> 5`, since the join, whose `x` starts where that `a` goes, comes after it |
| `mk * & a` | | `0 -a-> new1` only (6) |
| `mk a/~*` | | `0 -a-> new1` only (6) |
| `mk */@2 & a/@2` | | `0 -a-> 2` only (6) |
| `mk */@2/~b/@0` | | needs a label: the `b` edge is created one step further along (2, 6) |
| `mk a/!{@1}` | `0 -a-> 1` | `0 -a-> new1`: the edge to an excluded node does not count |
| `mk ~a/@2` | | `2 -a-> 0` (2) |

**Written nodes.** No node is created at an id that is given. An
alternative that stands on a node which does not exist (a `Fixed`
vertex, or the current node) is not walked: it is left out (`LeftOut`).
If no alternative is left and some were left out for that, the result
is `MissingNodes` with those nodes; `mk` and `tag` then fail and change
nothing. Otherwise `mk` goes on with the others, and says which it left
out ("Note: mk left out `a/@7`, which needs @7: …", `leftOutNotes` in
`Graph.PathPattern.Execution`), after what it created, also when the
path already resolves, and after the list of an ambiguous `mk` or a
label error; not with `MissingNodes`, `TooManyRepairs` or a refused
shape. A node inside a test (`!{…}`) is no vertex and need not exist.
So a patch never needs a node file at a chosen id, and every created
node hangs by an edge from a node that exists (`mk` once had an option
`--allow-orphans` for nodes that would not; it is gone, and the parser
says so).

**Selection** (`select`).

- If some walk creates nothing, the path already resolves and nothing
  is done. A walk follows every edge that exists, so every way the path
  resolves is a walk.
- Otherwise a walk that creates everything another walk creates and
  more (up to the names of new nodes; an existing node is never
  renamed, `patchSubsumes`) is dropped, and walks that create the same
  count once (`selectReference` states this over every two walks and
  the tests compare the two). The walks of different alternatives are
  compared like those of one.
- One possibility left: it is created (`chooseRepair` gives `Apply`).
- Several: nothing is created; they are listed, the one that creates
  least first (then in a fixed order of their text).
- None: `MissingNodes` if an alternative was left out for a node that
  does not exist, and otherwise "This path cannot be constructed: each
  of its alternatives has a test that a node already there fails …"
  (`Unsatisfiable`).

With `0 -a-> 1`, `0 -a-> 2`, `1 -b-> 3`: `mk a/b` does nothing;
`mk a/b/c` lists two (c under 3, or b and c under 2); `mk a/b & a/c`
has four walks and lists two (c under 1, c under 2: the two walks that
also create a `b` under 2 create more).

**Labels.** A `*` or regex step that has to create an edge gets its
label from the caller's callback, which is asked once for each matcher;
there is no graph-wide reuse. `mk` supplies no labels (`noLabels`). A
label the chooser does not give (or gives but the matcher rejects) is
**missing**. The walk goes on with a stand-in, `Missing matcher`,
distinct from every real label and equal for every edge with the same
matcher; a step with the same matcher follows an edge with that label,
and so does `*`. If the path already resolves, the result is the no-op
as usual. Otherwise, if a possibility that is left uses a missing
label, it is an error (`UnsupportedLabel`) and nothing is changed:
`mk` does not fall back to the other possibilities. The error names
the matcher and lists the possibilities (at most five) with the
missing label shown as its matcher (`-*->`), and says which need it.
So `mk a + *` and `mk * + a` are errors, and `mk a/*/@5` with
`0 -a-> 2` is one too (`2 -*-> 5`). A possibility that needs a label
and is dropped anyway does not matter: `mk a + a/*` creates `a`. Over
the test corpus `mk` without labels does exactly what it does with a
label that no other step has, or fails where that label would be used.
A `*` step given a label by a chooser is no supplier in the plan; the
command line never gives one.

**Limit on walks.** A step with edges to several nodes multiplies the
walks, and every walk is made before any is dropped: `n` edges at each
of `k` independent steps are `n^k` walks. `mk` counts the walks of
every alternative, under way and finished, and stops as soon as there
are more than the command's limit, 5000 by default
(`Models.PathPattern.Residuate.defaultMaxRepairs`), or what the option
`--max-repairs N` of `mk` and `tag` says (`unlimited` for none; the
command carries it as `MaxRepairs`, `Nothing` for the default):
"Working out what this path creates takes too long: … there are more
combinations than the limit of 5000. This does not mean that the path
cannot be created. … (TooManyRepairs)". Each walk is made once, so a
limit equal to the number of walks is always enough. A path that
already resolves is the no-op whatever the limit: when the limit is
passed, each alternative is matched instead, as a query matches it
(`loadPattern`, `matchPatternWithin`), requiring that every matched
node exists.

**Laws.** Tested over the corpus, random paths and graphs, and paths
built so that steps follow each other's edges
(`Models.PathPattern.WalkSpec`), with the refusals as the filter:

- After `mk p` creates a possibility (the one it creates, or any it
  lists), `p` resolves.
- `mk p` again then does nothing.
- A path that resolves, and only such a path, does nothing.
- Neither the result nor whether the path is refused depends on the
  order of conjuncts or of union sides, and a conjunct that has nothing
  to do with the rest changes nothing (but for X, whose estimate it can
  tip).
- `mk` fails with `MissingNodes` only for nodes that the path writes
  and that do not exist, and never when they all exist; and it creates
  only new nodes. A path with literal labels, existing written nodes
  and tests that agree is never answered "cannot be constructed".

*Staging is not a law.* "`mk p` and then `mk p/q` leaves what `mk p/q`
alone leaves" holds when `q` begins with a step (not with `@`, `@N` or
`!{…}`), `mk p` and `mk p/q` alone both decide without asking, and the
path writes no label twice (`a` and `~a` are the same label) and has no
`*` or regex, but for one documented exception, the last case below
(`spec_walkStaging` states the law with that exception). Outside that
class it fails in these ways:

- *A cut just before `@`, `@N` or `!{…}`.* What follows the cut says
  where the part before it ends. With node 2 and no edges,
  `mk a/@2/(a & b)` creates `0 -a-> 2` and `a` and `b` under 2;
  `mk a` first creates `0 -a-> new1`, which is then left over.
- *The first command adds an edge beside one that a step of its own
  follows.* With `0 -a-> 0` and node 3, `mk a/a/@3/a` creates
  `0 -a-> 3` and `3 -a-> new1`. `mk a/a/@3` creates `0 -a-> 3`; after
  it the first `a` has two edges to follow, and `mk a/a/@3/a` asks
  (`3 -a-> 3`, or `3 -a-> new1`).
- *The first command creates an edge beside the node that a step of
  its own creates, further along than that step is.* With node 4,
  `mk b & c/~c/b/@4` creates `0 -b-> new1`, `0 -c-> new2` and
  `0 -b-> 4` (rule 4). `mk (b & c/~c/b/@4)/b` alone puts a `b` under
  `new1` and one under node 4; after the first command its first `b`
  has two edges to follow, the way along `0 -b-> 4` creates less, and
  nothing is put under `new1`.
- *`*` or regex.* With `0 -c-> 0`, `mk */a & c/b` creates `0 -a-> new1`
  and `0 -b-> new2`, and then the `*` of `mk (*/a & c/b)/d` has three
  edges to follow; alone it has one, the loop, and decides.

And the exception inside the class: after `mk p`, `mk p/q` may ask
where alone it decides. It then lists the way the law asks for (with
what `mk p` created, it creates what `mk p/q` alone creates) and, beside
it, only ways that alone are dropped: each of them, with what `mk p`
created and that command's nodes counted as new again, creates
everything `mk p/q` alone creates and more, up to the naming of new
nodes. Such a way is listed after the first command only because an
existing node is never renamed (`patchSubsumes`), and the nodes `mk p`
created exist by then. With `0 -x-> 0` and `0 -x-> 5`,
`mk (x & @)/i/(l & @)/@` creates `0 -i-> new1` and `new1 -l-> new1`,
and `mk (x & @)/i/(l & @)/@/b` alone creates those and `new1 -b-> new2`;
but after the first command it asks between `N -b-> new1` (N the node
the first created) and a way in which the `x` goes to 5, the `i` from
there creates a node and the join goes there. Alone, that larger way
contains the smaller one up to the naming of new nodes and is dropped.
The cases found take a step that the product copies so that one copy
goes into a join and the other past it (`i/(l & @)/@`), a conjunct `@`
beside it, and a step with several edges or a union. Over seeds 1 to
30,000,000 of the staging property's generator, 1 of 10,198,031 cuts
was this exception. The designer has ruled (2026-10-15) that `mk`
stays as it is and that this is an exception to the law, not a fault:
making the second command decide would take changing selection
(4.2 of commands.md, what is listed) or the choices of a join (rule 3),
both documented rules.

**Costs.** As plainly as the benefits, counted (`WalkCost` is
`{walks, batches}`; `spec_walkCostCounted`):

- A step with edges to several nodes multiplies the walks, and a path
  that resolves in many ways is walked in every way until the limit
  hands it to the matcher. Writing `@N` after a step leaves it one node
  and divides the count. `k` independent choices of two take `2^k`
  walks.
- No plan has more batches than operations, and the batches done are
  at most the walks times the operations. A relay of steps that follow
  each other's created edges takes one walk and three batches at every
  length tried, from 2 to 64 (at 128 it is refused, X).
- `*` and regex steps see every created edge that matches and counts.
  Beside two steps that create from the same node, a `*` step has two
  more edges to follow: `mk a & b & */c` asks. `mk c & */b` with
  `0 -a-> 1` asks too (b under 1, or under the new `c` node).
- Each `use:` line of an ambiguous `mk` costs one more walk, of a path
  in which every choice is written.

**`mk`** (`Graph.PathPattern.Execution`, `constructPath`; `tag` is
`constructTag`, which does the same for its first path and then links
each end of the first match to the target with the label `""`).

- A refused shape: the refusal (`renderRefusal`), and nothing changes.
- The path already resolves: nothing is done, and `mk` says
  `created 0 new nodes and 0 new edges`.
- One possibility: it is created (`applyResidual`: a `freshNID` for
  each `Fresh` binding, then the edges), and `mk` says how many nodes
  and edges, e.g. `created 1 new node and 2 new edges`. With
  `ge --dry-run` it says `would create …`.
- Several: an error that changes nothing. It lists the possibilities,
  each as its routes with every vertex resolved (existing nodes as
  `@N`, created ones as `newK`) and what it creates. Only the first
  five are listed (`shownPossibilities`), followed by how many more
  there are.
- **Suggested paths.** Each listed possibility is followed by
  `use: mk <path>` (or the `tag` command) when a path can be written
  that creates exactly it (`Models.PathPattern.Suggest`), and by "(no
  command was found that creates just this)" otherwise. The line is
  constructed, not searched for: the path keeps only the `+`
  alternative the possibility came from (`compileAlternatives` gives a
  path without unions for an alternative, where there is one); after
  each written step that took an existing node where it had several to
  go to, `/@N` is written, and after each that took the created one,
  `/!{…}` with the other nodes. There is no line when another of the
  nodes is a created one (`mk a & b & */c`: neither new node has a
  name), when the copies of a written step went different ways
  (`mk a/(b & c)` with two `a` edges: what is written after the one
  `a` applies to both copies), when the alternative has no path
  without unions, or when the line cannot be typed or is a path that
  no command runs. A constructed line is walked once, within the
  command's limit, which applies the refusals R, M, V and C to it (not
  X: the size estimate is made only of the path the user wrote), and is
  shown only if it creates exactly the possibility; with
  `--max-repairs N` given, the line gives it too. That check is what
  makes a shown line right, and it is needed: what is written after a
  step changes which edges other steps have to follow (with `0 -a-> 0`
  and `0 -a-> 2`, the line `(a/@0 & b)/a/@0` for a possibility of
  `mk (a & b)/(@ + a)` would create less than the possibility it is
  for). Sequences are written left-associated, labels unquoted where
  possible, `@<tags>/t` as `#t`, and NIDs in full. When the command ran
  at another node than the prompt's (inside `at`), the line is
  prefixed with `at @N`.
- More walks than the limit: the `TooManyRepairs` error above.
- A written node that does not exist: the `MissingNodes` error ("This
  path needs @7, which does not exist, and a node with a written id is
  never created: mk gives every new node a new id. Nothing was
  changed."); `tag`'s says `tag`. When it is the current node that no
  longer exists (deleted after it became the current node), the message
  calls it "the current node @1", not a written node.
- The walk reads nodes through the command's transaction, each once
  for the command: on a damaged graph, reading past damage and then
  writing is refused for the whole command, as for every command. A
  walk that follows an edge to a node without a file creates nothing
  there: the write is refused as damage.

## 6. Testing

- **Oracle** (`spec_matchOracle`, `spec_compileOracle`). A brute-force
  matcher in the test suite enumerates every assignment of variables
  (and labels) over small graphs, for a corpus of surface queries
  (`Models.PathPattern.TestCorpus`). Targets, `selectedRoutes`,
  `routeLastEdges` and the set of witnesses are compared with it.
  `CompileSpec` also checks every corpus query on every corpus graph
  against a direct interpreter of the terms of the reference compiler
  (`referenceCompile`, §2), which shares no code with the compiler,
  gluing or matching. For a query with unions that is checked for each
  alternative that is a path without unions, through that path; an
  alternative that is none is not covered by it.
- **Laws of the compiler**: the table in §2.
- **Random paths.** The tests on random paths leave out those that are
  too large to compile in a test with `fewAlternatives` in
  `Models.PathPattern.TestCorpus`: a rough count read off the path,
  then a lazy count of the alternatives up to a bound. It reads no
  clock and is a filter for tests, not a bound on anything.
- **What `mk` creates** (`spec_residuationProperties`,
  `Models.PathPattern.WalkSpec`). For every possibility: creating it
  makes the path resolve, and `mk` again does nothing; `mk` does
  nothing iff the path already resolves; it asks iff at least two
  possibilities are left; neither the result nor whether the path is
  refused depends on the order of conjuncts or union sides;
  `MissingNodes` only for written nodes that do not exist; staging
  over its class, with its exception, which is pinned on the cases it
  was found on (`spec_walkStaging`); what walking
  takes, in counted walks and batches (`spec_walkCostCounted`); the
  examples of §5 (`spec_walkExamples`), and the designer's table of
  paths built and refused (`spec_designExamples`); that the steps are
  walked at the same pace. `select` is compared with `selectReference`
  (`spec_selectMatchesReference`), and every shown `use:` line creates
  exactly its possibility (`Models.PathPattern.SuggestSpec`). The
  messages of the refusals are a golden,
  `test/Graph/PathPattern/ExecutionSpec/refusal-messages`.
- **Goldens.** `test/golden/ge/path/` runs `ge` on small inline graphs
  ([Testing.md](Testing.md#cli-golden-tests) has the format):
  `chain-scoped-*.golden` for the product rule and joins,
  `copied-meeting-point.golden` and `join-does-nothing.golden` for the
  refusal and the warning, `union-*.golden` for unions, `spec-*.golden`
  for the command examples, `mk-*.golden` and `walk-*.golden` for
  `mk`; `test/golden/ge/no-match/` and
  `test/golden/ge/directives/` for those.
- Tests that guard against a search that does not end
  (`spec_compileContradictions` and others) have an allowance of a
  minute for work that takes a fraction of a second; the allowance
  measures nothing.
- Build and test: `stack test --fast --ghc-options '-framework Foundation'`
  from `cli/`; [Testing.md](Testing.md#tasty) says how to give a `-p`
  pattern.

## 7. Modules

- `Models.PathPattern`: the types of §1, canonical form, the derived
  pattern graph, and the test helpers.
- `Models.PathPattern.Compile`: `compilePath`, the check for copied
  meeting points and idle joins (`checkCopies`), and
  `compileAlternatives`, which spells alternatives as paths for `mk`.
- `Models.PathPattern.Match`: pure matching, factoring into components,
  and the projections of §3 and §4.
- `Graph.PathPattern.Resolve`: loading the neighbourhood of a pattern
  from the graph, then matching.
- `Models.PathPattern.Admit`: the shapes `mk` refuses (the size
  estimate and `checkExpansion`, and `refusals`) and the order of the
  operations of an alternative (`planOf`), from the path and its
  routes alone (§5).
- `Models.PathPattern.Residuate`: the walk, selection and the limit on
  walks (§5), generic over the node lookup monad; `chooseRepair`
  gives the command-level choice.
- `Models.PathPattern.Suggest`: the `use:` paths of an ambiguous `mk`.
- `Graph.PathPattern.Execution`: the command policy on top of all of
  these (what a query appends, which edge a command selects, `mk` and
  `tag`).
- `Graph.Command`: expands directives, lowers `+`, runs the check and
  prints its error or warnings.
- `Models.LegacyPartialPath`: the parser for TAB completion. It is not
  an engine.

What stops a command, for whoever changes how commands write (what the
user sees is in [commands.md](commands.md#51-what-a-path-compiles-to)):

- Every change of a file of the graph goes through `DAL.RawGraph`: a
  node file written or deleted, a data file, what `fix` copies and
  removes. `changingGraphFiles` writes the files of one change under a
  lock with exceptions from outside masked, and `changingGraphFile`
  counts each change in `GraphWrites` as it is made;
  `changingGraphFileOutside` counts the data file that the editor of
  `vi` changed. A new place that changes graph files must go through
  these, or an interrupt can cut it in half and the notes of an
  interrupted line will not know of it.
- The notes of an interrupted line (`notingWhatInterruptLeaves` in
  `Error.Warn`) come from the count: `untoldGraphChanges`, and
  `unnamedAtTargetChanges` for the targets of `at`.
- `Effectful.Readline` (in `effectful-readline/`):
  `installHandlerUnheld` installs the handlers of the interrupt signal,
  SIGTERM and SIGHUP so that the runtime does not hold them during a
  garbage collection (one that fell into a collection while a line was
  read used to be handled only at the next line); `OutputFailed` is
  output of the line editor that cannot be written, and `SessionEnded`
  a session of the line editor that has ended.
- `Executable.GraphEditor`: `endingBySignal` ends `ge` at SIGTERM and
  SIGHUP, within `endingAllowance` (`--test-only-ending-allowance` for
  the tests), after `noMoreGraphChanges` has waited for the change in
  progress; `startedIgnoring` asks, without changing anything, whether
  a signal was ignored when `ge` started (`nohup`);
  `sayingThatOutputFailed` reports output that cannot be written.
- `System.ControllingTerminal` (`awaitTerminalOutput`,
  `discardTerminalOutput`): what `ge` does when it leads its session
  and ends.
- `runToEnd` in `Effect.IOWrapper.Editor` runs the editor so that an
  interrupt that stops it comes out as itself, also when stopping the
  editor's process fails.
- `app/ge/standard-descriptors.c` opens a standard input, output or
  error that is closed when `ge` starts on `/dev/null`, before the
  runtime starts.

## 8. Deferred and out of scope

- **`%unanchored`.** What it would take: a `Var` root, root candidates
  from `nodeManifest` for the matcher, and for `mk` a first step that
  has no node to stand on until its root is chosen, and an isolated
  vertex binding to any node that passes its test. `%float` and
  `%unanchored` are not supported now.
- **Positive node predicates** (for example, "tagged `#foo`") on
  variables: `mk` would have to choose or create a node that passes
  them, which it never does now (a new node passes every test but that
  of being a given node).
- **Picking among the possibilities of an ambiguous `mk`** (`mk --choose N`).
- **A label → edges index** for performance.
- **Warning about every `@` that does nothing**, and relaxing the
  refusals that go further than the rule needs
  ([path-language.md](path-language.md#5-known-rough-edges-and-what-may-change)).
