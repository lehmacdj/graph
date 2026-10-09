# Limits on what a path costs: what was tried, and why there is none

A path command (`ls`, `mk`, `rm` and the others) compiles its path to
alternatives, each a set of routes, and then matches them against the
graph (`cli/docs/path-pattern-ir.md`). A path that is short to write can
compile to a great deal, and then the command takes long.

For a while `ge` refused such paths before running them. That check, the
alternatives limit, has been removed, and nothing replaces it: a path
compiles for as long as it takes. This note records what the check was,
why it was removed, where the time of a path command actually goes, the
best design known for a check if one is wanted again, what is still
limited, and what removing the limit costs.

The decision is the language designer's:

> I think we can shelve preflight work for now; I will revisit it if
> commands taking much longer than expected ends up being a problem in
> practice. Remove the existing checks and document what was done/tried
> and what the current best approaches we were thinking of were.

And on the separate limit of `mk`: "keeping max repairs is probably a
good idea".

How to read the numbers. Every time below says how it was measured.
"Unoptimized" is a build with `-O0`, which is what `cli/stack.yaml`
gives (`stack build --fast` and `stack test --fast` likewise);
"optimized" is `-O1`. All of them were taken on a machine that was
doing other work at the time, with a load average between 17 and 160 on
18 cores, so processor time is given where it was recorded, wall-clock
times are inflated, and ratios are more reliable than absolute values.
Graphs are small throwaway graphs, described where they matter; no real
graph was used.

## 1. What existed

### The limit

A `k`-way `|` union that a conjunction copies into `n` routes is chosen
by each route on its own, so it makes up to `k^n` alternatives:
`(a | b | c)/(d & e & f & g & h & i & j)` has `3^7 = 2187`. A `+` is a
`|` in queries, so this arises without the user writing `|`.

The limit was on that number. A path with a `|` union and a conjunction
could compile to at most 1000 alternatives
(`Models.PathPattern.Compile.defaultMaxAlternatives`). It was checked by
`Models.PathPattern.Compile.checkAlternatives`, which
`Graph.Command.checkPath` ran for every path of every command, after the
check for copied meeting points and before the command did anything.
TAB completion ran it too and offered nothing for a prefix over the
limit. A path without a `|` union, or without a conjunction, was not
checked at all.

The limit came with relational unions (the commit "Add relational | and
shared ++ unions, and lower + by command") and was reworked in "Scope
joins to their chain and finish the relational union", which added the
third error below.

### How it counted

The check compiled the path. The compiler was written so that it could
be stopped: every function that makes chains, routes or alternatives
took a record of two callbacks (`Spend`: `made`, called with the size
of something before it is made, and `found`, called with the routes of
each alternative as it is found) and ran in a monad. `compilePath` ran
it with callbacks that do nothing; `checkAlternatives` ran it with
callbacks that count and stop. So a path that passed was compiled
twice, once by the check and once by the command.

The sizes were charged by hand, at about ten places in the compiler,
each with a formula of its own.

### The three errors

- `TooManyAlternatives`: the alternatives, counted as the compiler
  makes them (after contradictory ones are dropped and those that glue
  to the same pattern are merged), came to more than the limit. The
  count was exact and stopped at the first alternative beyond the
  limit.
- `TooLargeToCount`: the check ran out of budget before it knew the
  count. The budget was 400 units for each alternative allowed
  (`checkSteps`) plus twice (`checkOnce`) what the routes of the path
  come to when listed once with no union chosen; at the default limit
  that is at most 400,000 plus twice 150,000, 700,000 units. Charged
  against it: every chain (one for each choice of the `++` unions) by
  its steps and node specs; the routes of each stretch of a chain
  between its joins, by their segments; every way a route chooses a
  union, by the routes it then is; every two ways that are compared, by
  the length of the route, four times (`checkPair`); every way of a
  route that is tried with those of the routes before it, by one; every
  two routes that chose different sides of a union and are compared, by
  the length of a route, four times; every way to choose that is found
  for a stretch, by its routes, four times (`checkShare` and one); and
  every alternative of a chain with joins of its own, by its routes.
- `TooManyRoutes`: the routes of the path, listed once with no union
  chosen, came to more than 150,000 segments (`checkRoutes`). Such a
  path was refused before anything was made, whatever number the limit
  was set to.

The messages, as `Graph.Command.checkAlternativesLimit` printed them
(`N` is the limit):

```
This path would make more than N alternatives, since each copy distribution makes of a | union chooses its side. Write ++ for a union whose side every copy shares, wrap the path in %override({plus = shared}, …), narrow it, or raise the limit with %override({max-alternatives = …}, …). (TooManyAlternatives)

This path's unions make too much to count its alternatives within the limit of N: each copy distribution makes of a | union chooses its side, and what is made on the way is limited too, however few alternatives it comes to. Write ++ for a union whose side every copy shares, wrap the path in %override({plus = shared}, …), narrow it, or raise the limit with %override({max-alternatives = …}, …). (TooLargeToCount)

This path has too many routes to count the alternatives its | unions make (the limit is N, but its conjunctions alone multiply to more routes than are listed to count, so no higher number helps). Write ++ for a union whose side every copy shares, wrap the path in %override({plus = shared}, …), narrow it, or lift the limit with %override({max-alternatives = unlimited}, …), which compiles it however long that takes. (TooManyRoutes)
```

### `max-alternatives`

`%override({max-alternatives = N}, p)` set the limit for the `|` unions
in `p` to a positive number `N`, or to `unlimited`. The innermost
override around a union was that union's limit, and since alternatives
multiply across unions and cannot be attributed to one of them, the
largest limit among the `|` unions of a path applied to the whole path.
The setting was carried by the same `WithLimits` node of the expanded
path that still carries `max-repairs`.

It is no setting any more. Writing it gets the error that any unknown
setting gets:

```
Unknown setting max-alternatives in %override; the settings are plus and max-repairs at line 1, columns 15-37. (InvalidOverrideSetting)
```

### Tests

The limit had a test group of its own (`spec_compileAlternativesLimit`
in `cli/src/Models/PathPattern/CompileSpec.hs`, 16 cases). Nine of
them ran the check under `timeout`, with allowances of 0.3 to 3
seconds, to show that it refuses or passes quickly, and 17 cases of two
other groups (`spec_compileManyJoinedSides`,
`spec_compileIndependentUnions`) timed the check in the same way. It
had two golden tests (`union-alternatives-limit` and
`union-too-large-to-count` under `cli/test/golden/ge/path/`). And the
tests on random paths elsewhere in the suite used the check as their
filter, in fifteen places, to leave out the random paths that are too
large to compile in a test.

## 2. Why it was removed

### A count of alternatives does not predict time

The limit bounded a count. What the user cares about is time, and the
two have little to do with each other.

Paths with few alternatives that are slow:

- `ls x & (a & b & c)/(a & b & c)/(a & b & c)/(a & b & c)/(a & b & c)/(a & b & c)/(a & b & c)`
  has no union, one alternative, 2,187 routes and 15,310 steps. The
  limit never looked at it. It took 129.8 s of processor time
  (unoptimized `ge`, a one-node graph with the labels `a`, `b`, `c` and
  `x`, before the matcher fixes of section 3).
- `(x | y) & (a & b & c)/…` with eight such groups has 2 alternatives of
  6,562 routes each. The limit passed it after counting for most of a
  second (0.9 s was the slowest check that passed, measured by the
  author of the check in an unoptimized build). Compiling it alone
  takes 1.4 s of processor time (`compilePath` forced, unoptimized,
  measured for this note), and the command then compiles it again and
  matches it.
- On a complete graph of two nodes, `ls (a & b)/(a & b)/(a & b)`, one
  alternative of 8 routes, allocates 1.6 GB (0.68 s), and on three
  nodes more than 3 GB: the matcher lists every match (section 3).

Paths with many alternatives, or refused for the work of counting, that
are fast (measured for this note with the build that has no limit,
unoptimized, `ls` on a graph where every step matches once; processor
time of the whole `ge` process):

- `(a | b | c)/(d & e & f & g & h & i & j)`, 2,187 alternatives, refused
  as `TooManyAlternatives`: 1.0 s, of which compiling is 0.17 s.
- `(a | b)/(c | d)/(e | f)/(g | h)/(i | j)/(m & n)`, 1,024
  alternatives, refused as `TooManyAlternatives`: 0.41 s.
- `(x/@ | @/x/@)/` five times before `(a/@ | @/a/@)/(d & e)`, 7
  alternatives, refused as `TooLargeToCount`: 0.36 s.

And in bulk, from the investigation that section 4 reports (20,683
generated paths with unions, `ls` on a one-node graph, unoptimized,
with the matcher fixes): the limit refused 3,803 of them, of which 261
have at most 1000 alternatives and 76 finish in under 0.25 s; it
accepted 16,880, of which the slowest took 5.2 s after a check of up to
2.3 s, and 21 more did not finish within 3 GB of allocation in one
phase. On a complete three-node graph (18,125 of the paths) it accepted
15,596, of which 1,653 did not finish within 3 GB a phase.

### Each repair of the bounds moved the failures

The exact count alone was not enough, because counting is compiling: a
path with too many alternatives takes too long to count. So the check
got bounds on its own work, and those bounds were then repaired in
three rounds of independent review of the commit "Scope joins to their
chain and finish the relational union". Each round fixed what the
review before had found and broke something else.

1. The first of these reviews found that the bound on size refused
   queries with few alternatives, for example
   `ls x/@/(@1 | @2)/(a & b & c & d & e & f & g & h & i & j)`, which
   has 2, and that the time to refuse grew with the size of the path
   (4.5 s at the largest size it tried, which its notes give as 500).

   The repair made the compiler drop contradictory choices as they are
   made and resolve a chain stretch by stretch between its joins, and
   gave the check its two bounds (150,000 segments up front; a budget
   of at most 700,000 units).

2. The next review found:
   - Paths refused that used to run, or that have almost no
     alternatives: `(x | y) & (a & b & c)/…` with nine groups (2
     alternatives), `(a & b & c)/…/@/(x | y)/(a & b)` with eight groups
     before the join (4 alternatives; the build before accepted it),
     and `((x | y)/@ & z)/(c1 & … & c10)` (1024 alternatives; the build
     before accepted it, and the designer had ruled that the `2^n`
     alternatives of such a path are fine).
   - A check that itself took exponential time: with a conjunct that
     cannot be satisfied written after twenty unions,
     `ls (a0 | b0) & … & (a19 | b19) & (@1 | @2)/@3` took 4.4 s, where
     the build before refused it in 0.3 s. Dead ends of the search were
     not charged to the budget.
   - A budget that counted a route through a group as one segment,
     however long the route is, so that `ls` could run for minutes
     after the check had passed.
   - A `TooLargeToCount` message that advised raising a limit which
     could not help, constants of the bounds that no test pinned, and a
     timing test with an allowance of 20 s.

   The repair added `TooManyRoutes` with a message of its own, charged
   routes by their steps and joins, and added the pruning that is still
   in the compiler (`narrowed` and the early exit in `resolveStretch`
   of `cli/src/Models/PathPattern/Compile.hs`).

3. The review after that found:
   - 173 of 16,560 random paths that the build before accepted were now
     refused; 119 of them by the size bound alone though they have at
     most 1000 alternatives, some as few as 9 to 36; 93 of the 173 were
     new in that round, from the new charging.
   - A kind of dead end that the pruning does not see (two routes that
     have to choose two unions alike by one of them and differently by
     the other). For a path with it the check passed after 11.5 s and
     `ls` then took 31 s; the build before refused it in 0.5 s.
   - The clock tests failed when the machine was three times
     oversubscribed (their allowances were 1 to 3 s).
   - Wrong messages: a path with 1,394 alternatives was told it was too
     large to count; the `TooManyRoutes` message was wrong when a `++`
     union made the count, and following its advice ran for more than
     45 s; at 700 conjuncts the two bounds gave advice that
     contradicted each other.
   - Constants of the charging that no test pinned, and an assertion
     without a timeout that could hang the suite.

No fourth repair was made. The clock tests went on failing in full
runs of the suite on a loaded machine: "gives up on a path that would
take too long to count" (allowance 2 s) and "a conjunct that cannot be
satisfied costs nothing, wherever it is written" (0.3 s) are the two
that the notes of later work name, the second in a run that passed
1,391 of 1,392 tests.

The pattern behind the three rounds is that the budget was a model of
the compiler's cost kept by hand next to the compiler. Whenever the
compiler changed, or a path found a cost the model did not have, the
model was wrong in one direction or the other: it refused what was
cheap, or it passed what was dear.

### The decision

Given that, the designer shelved the work (quoted at the top). The
checks are removed. Section 4 has what would be built instead if the
problem shows up in practice.

## 3. Where the time actually goes

The investigation that followed the last of those reviews profiled
`ge` instead of adjusting the budget again. Its main finding is that the slowest
commands were slow for a reason that no limit should have been hiding.

### Two inefficiencies in the matcher

Profile of `ls x & (a & b & c)/…` with six groups (729 routes, 4,375
steps) on a one-node graph, unoptimized, with cost centres:
`patternEdges`, entered 4,375 times from `loadPattern`, 89.3% of the
time; `patternComponents` 4.9%.

- `loadPattern` in `cli/src/Models/PathPattern/Match.hs` put the edges
  of the pattern in order with a lookup in `patternEdges p`, a map of
  every edge, written inside a list comprehension. Without optimization
  the map was built again for every edge, which is quadratic in the
  edges. With `-O1` the compiler moves it out of the loop and the cost
  disappears, which is why an optimized build looked different.
- `patternComponents` in the same module grouped the edges by
  component with `Map.fromListWith (flip (++))`, which appends each
  edge to the end of its component's list: quadratic in the edges of a
  component, in every build. `ls p` joins all routes at their end, so
  the whole pattern is one component. In an optimized `ge` this one
  line was 73% of the six-group command.

They are fixed in the commit "Group and load pattern edges in linear
time", with a follow-up, "Index pattern edges to components and look
literal steps up", which also stops a step from scanning every edge of
the node it leaves when its label is literal. Those commits are on
another line of work than the one that removed the limit; the
measurements that section 6 reports for this note were taken with a
build that does not have them yet, and say so.

`ge` end to end, one-node graph with the labels `a`, `b`, `c`, `x`,
processor seconds, measured by the investigation (the fix here is the
first commit's two changes):

| `ls x & (a & b & c)/…` | routes | steps | unoptimized, before | unoptimized, fixed | optimized, before | optimized, fixed |
|---|---|---|---|---|---|---|
| six groups | 729 | 4,375 | 6.9 | 0.29 | 0.49 | 0.20 |
| seven groups | 2,187 | 15,310 | 129.8 | 0.88 | 4.6 | 0.59 |
| eight groups | 6,561 | 52,489 | over 100 (stopped) | 3.2 | not run | 1.8 |

After the fix, going from seven to eight groups (3.4 times the steps)
costs 3.6 times as much: resolution is linear in the steps of the
pattern. The follow-up, as its author measured it: `ls` at a node with
811 labels 1.90 s to 0.37 s, and 2,000 one-hop routes 1.74 s to 0.87 s.

### Three costs that no number read off the path predicts

With the matcher fixed, three costs remain that cannot be read off the
path before running it (all measured by the investigation, unoptimized
with the matcher fixes unless said otherwise):

1. **The compiler's search among the choices of unions** is not
   proportional to what it produces. The path of the last review with
   the dead end that the pruning does not see took 23 s to be found to
   have no alternative at all. The time is spread over the function that
   combines the ways the routes of a stretch can choose
   (`resolveStretch`, its `combine`). Nobody has tried to make that
   search cleverer.
2. **Matching on a graph where steps match several edges.** The matcher
   lists every match of a connected part of the pattern, though `ls`
   prints only distinct routes. `ls (a & b)/(a & b)/(a & b)` (8 routes)
   on a complete graph of two nodes allocates 1.6 GB, and on three
   nodes more than 3 GB. `ls a/a/a/a/a/a/a/a/a/a/a/a` exceeds 3 GB when
   every node has four `a` edges. How many matches a pattern has
   depends on the graph and multiplies across independent routes. (A
   matcher that does not multiply independent branches seems possible
   for these tree-shaped patterns; that is reasoning, not something
   that was run, and it would be a redesign of resolution.)
3. **`mk`.** On an empty graph: `mk x & (a & b)/…` with five groups
   (161 steps) takes 0.27 s, with six (385 steps) 1.4 s, with seven
   (897 steps) 14 s; `mk x & (a & b & c)/…` with five groups (1,216
   steps) 33 s. That is roughly the 2.6th power of the steps. On a
   graph where the path already exists,
   `mk x & (a & b & c)/(a & b & c)/(a & b & c)` (28 routes) takes 7 s:
   the one node is read from disk 18,548 times (41% of the time; `mk`
   does not cache reads as `ls` does), and candidates are listed up to
   `max-repairs` before the path is found to resolve already.

How well a number predicts time, measured on 20,683 generated paths
with unions and 9,951 random paths without (unoptimized, matcher
fixed, `ls` on a one-node graph; time is compiling, loading and
matching together):

| what is measured | when it is known | fit | measured over predicted, 5% to 95% of paths | worst under-estimate |
|---|---|---|---|---|
| steps read off the path, paths without unions or `*` (6,659) | before anything runs | 13.6 µs a step | 0.07 to 1.83 | 3.0 times |
| the same, paths with `*` (3,292) | | | does not hold: 33 paths under 300 steps took over 0.1 s, 22 exceeded 3 GB | |
| steps of all alternatives, paths with unions | after compiling | 22.5 µs a step | 0.39 to 4.76 | unbounded: 2.3 s for a path with no alternative |
| megabytes allocated, all paths | while running | 0.49 ms a MB | 0.69 to 1.12 | 4.1 times |

So the size of a path predicts its time within a factor of three only
for paths without unions and without `*`, and the one thing that
follows time closely is work counted while the command runs.

## 4. The best approach known, if this is revisited

### A check in two parts

The investigation proposed, and prototyped in `ge` for `ls` and `mk`, a
check in two parts.

1. **Size, before anything is made.** Read the routes and steps off
   the path, taking the smaller side of each union so that the number
   is a lower bound for every alternative. More steps than a limit
   (`max-steps`) is refused at once, with exact numbers
   (`TooManySteps`). It is linear in the length of the path; a refused
   command took 0.08 s including the startup of `ge`. It also covers
   paths without unions, which the alternatives limit never checked.
   The size read off a path without unions is exact: on 9,946 random
   paths it equals what the compiler makes. With unions the lower
   bound held on all 6,273 paths it could be checked on; that it is a
   lower bound in general was reasoned, not proved.
2. **Work, while the command runs.** Compiling, loading, matching and,
   for `mk`, the planning of the repair run once, inside one budget
   (`max-work`). Nothing in the code does the charging: the runtime
   counts the bytes the thread allocates and raises an exception at the
   limit (`GHC.Conc.enableAllocationLimit`), which the command catches
   and reports (`TooMuchWork`). Nothing has been written at that point;
   for `mk` only the planning is inside the budget, and the graph
   directory was byte-identical after a refused `mk`.

There is no separate check to pay for: an accepted command has done its
work once. And the hand-kept model of section 2 is gone: every unit of
work is counted because nothing has to remember to count it, including
in code that is not written yet.

The message that goes with it does not suggest a rewrite, it runs one:
each candidate (the heaviest `|` written `++`; every `|` written `++`)
goes through the read-only part of the same command with a quarter of
the budget, and is named only if it finished. So the advice is true by
construction, at the cost of up to half a budget more time for a
refusal. Two messages of the prototype, verbatim:

```
This path is too large to run quickly, so it was not run. The limit is 20,000 steps (max-steps).
  It has 6,562 routes of 52,489 steps in all.
  Routes multiply across the groups written one after the other: 3 x 3 x 3 x 3 x 3 x 3 x 3 x 3 = 6,561.
  To run it, leave out conjuncts or groups until the product is under the limit, or raise the limit to at least 52,489. (TooManySteps)

This path takes too much work to run, so it was stopped; nothing was changed. The limit is 1,000 units of work (max-work).
  Each alternative has at least 16 routes of 32 steps in all.
  The largest group is c1 & c2 & c3 & c4 &  … 13 & c14 & c15 & c16 (16 conjuncts).
  The union a | b (2 sides) is chosen separately by up to 16 routes: up to 65,536 ways to choose, each a candidate alternative.
  This runs within the limit (it was tried, and took 7 units): write ++ for the | of a | b (one side for the whole path):
    ls (a ++ b)/(c1 & c2 & c3 & c4 & c5 & c6 & c7 & c8 & c9 & c10 & c11 & c12 & c13 & c14 & c15 & c16)
  Or raise the limit (max-work). (TooMuchWork)
```

Its tests read no clock. They assert on megabytes allocated, as
ratios: 3.6 times the steps is at most 5 times the work (it was 12.4
times before the matcher fixes, 3.83 after), so such a test catches
both matcher defects and gave the same result on the loaded machine.

### How accurate it is

Measured on the 20,683 generated paths with unions (`ls`, one-node
graph, unoptimized, matcher fixed). A unit of `max-work` is a megabyte
allocated.

| | the alternatives limit | two-part check, `max-work` 1500 | two-part check, 500 |
|---|---|---|---|
| paths measured | 20,683 | 20,672 | 18,011 |
| accepted | 16,880 | 17,181 | 15,391 |
| slowest accepted | 5.2 s, plus up to 2.3 s for the check; 21 more did not finish within 3 GB | 1.36 s; 2 over 1 s, 146 over 0.5 s | 0.33 s |
| refused | 3,803 | 60 by size, 3,431 by work | 60 by size, 2,560 by work |
| time to refuse | up to 2.3 s | median 0.69 s; 95% within 1.02 s; 75 over 2 s; 9 over 5 s; worst 7.65 s | median 0.23 s; worst 1.11 s |
| refused though it finishes in under 0.5 s | 76 finish in under 0.25 s; 261 have at most 1000 alternatives | none (116 finish in under 1 s) | 354 |

On the complete three-node graph, where the alternatives limit let
through 1,653 paths that did not finish, the work budget stopped them,
and the slowest accepted command took 0.86 s.

### What it costs

Stated as plainly as the benefits:

- **Some refusals are slow.** Work follows time within a factor of
  about four for commands that finish, but 9 of 3,431 refusals at a
  budget of 1500 took more than five seconds to arrive, the worst
  7.65 s. Some work allocates little for the time it takes; which work
  that is was not found. A tighter promise needs a clock as well, or
  that finding.
- **The unit depends on the build.** The same command used more than
  999 units in the unoptimized build and 489 in the optimized one. A
  default is calibrated for one build, and `cli/stack.yaml` builds
  without optimization.
- **What a budget means in seconds depends on the machine.** The
  budget counts work, which is the same on every machine: two fresh
  runs of one binary agreed to the last digit under different load.
  The time a unit takes is not: the 0.49 ms a megabyte of section 3 is
  this machine's, under the load it had. A budget that keeps commands
  under a second here keeps them under some other time elsewhere.
- **It is not exactly repeatable inside one process.** Running the same
  path twice in one process gave 460.3 and then 458.3 MB in the
  optimized build, because values computed once are reused. A path
  just under the limit can be refused the first time and accepted
  later in the same session.
- **The verdict depends on the graph.** The same path can run at one
  node and be stopped at another. For matching that is the point, but
  the verdict is then no property of the path.
- **Feedback is not instant** for what the size check does not catch:
  a refusal arrives when the budget is spent.
- **It was a prototype.** Not built: commands other than `ls` and `mk`
  (they would get the size check only), `%override` settings for the
  two limits, TAB completion, `%targets(…)` queries. The prototype
  forced results inside the budget by hand; anything left lazy would be
  evaluated outside it, and a real version needs that audited.
- The default was not chosen. At 500, accepted commands take at most
  0.33 s and refusals arrive within 1.1 s, but 354 of 20,683 paths are
  refused that finish in under half a second. At 1500 none is, and
  accepted commands take up to 1.4 s and refusals up to 7.7 s.

### The other options that were on the table

- **Repair the hand-charged budget a fourth time.** Not chosen: three
  rounds each moved the failures (section 2), and the cause is the
  design, a cost model kept by hand.
- **Keep only the exact count, without bounds on the work of
  counting.** Not chosen: counting is compiling, so a path with too
  many alternatives, or with a dead end in the search, takes as long
  to refuse as to run.
- **A limit on size alone (`max-steps`).** It is the first part above.
  Alone it is instant and exact and covers paths without unions, but
  it refuses a large path that a contradiction would have made cheap,
  and it sees nothing of the three costs of section 3.
- **A clock.** A limit on processor time, alone or as a backstop a few
  times above the target, would cap the slow refusals. Its verdicts
  near the limit depend on the machine and on its load, which is what
  made the timed tests of the alternatives limit fail. It was not
  prototyped.
- **Building with optimization.** About half the processor time on
  these commands, and one of the two matcher defects could not have
  existed. The designer's answer was that the installed build is
  optimized already and that tests use whichever is faster overall.
  (`cli/stack.yaml` sets `-O0` for the packages of the project.)
- **Repairs that remove a reason to hit any limit**, each a piece of
  work of its own: looking literal steps up instead of scanning a
  node's edges (done in the follow-up commit of section 3); caching
  node reads in `mk`; testing "already resolves" before listing
  repairs; the cost of merging in `mk`; a matcher that does not list
  every match. The `mk` repairs were approved as separate work.
- **Retiring `max-repairs` together with `max-alternatives`** once a
  work budget exists, since both bound a count and not time. The
  designer first agreed, for the case that the budget is built; with
  the budget shelved, `max-repairs` stays.

## 5. What remains bounded today

One thing: the candidates `mk` considers.

`mk` works out how to construct a path by residuation: it lists the
ways the steps of the path can be matched to existing nodes or left to
be created (candidates), and selects among them. Candidates multiply
with the matches of each step. `Models.PathPattern.Residuate` stops
after 5000 of them by default (`defaultMaxRepairs`), counted over all
alternatives of the path, and the command fails without changing
anything:

```
Working out how to construct this path would take too long: its steps can be matched to existing nodes, or left to be created, in more ways than the limit of 5000, and each way has to be considered. Nothing was changed. Pin a step to a node with @N or exclude nodes with !{@N} so that fewer match, or raise the limit with %override({max-repairs = …}, …). (TooManyRepairs)
```

`%override({max-repairs = N}, p)`, with `N` a positive number or
`unlimited`, changes it; the largest limit among the parts of a path
applies to the whole path. A path that already resolves is a no-op
whatever the limit. When a path has several repairs, `mk` lists five
of them and changes nothing.

It bounds a count, not time: what one candidate costs grows with the
pattern (`cli/docs/path-pattern-ir.md`, section 5, has the
measurements, and section 3 above has the cost of `mk` on large
patterns). And it bounds only `mk`'s planning. Compiling the path, for
`mk` as for every other command, is not bounded by anything.

Two other things stop a path, neither of them for its size. A path in
which a product would copy a meeting point is refused by reading it
(`CopiedMeetingPoint`). And a path with joins that contradict each
other where they are written compiles to nothing at once.

## 6. What removing the limit costs

Measured for this note. The build is the one that has no limit:
unoptimized, and without the matcher fixes of section 3, which are on
another line of work, so loading and matching are slower here than they
will be once the two lines are together. Each command is a fresh `ge`
process on a small scratch graph. The machine had a load average
between 40 and 70 on 18 cores while these ran, so the measure is the
processor time of the `ge` process (all its threads, which includes
about 0.1 s for starting up), and wall-clock times were up to two and
a half times that.

### Paths the limit accepted: nothing changes

The compiler without the counting gives the same result as before on
every path the limit accepted that was tried:

- The two compilers, before and after, linked into one program, on
  44,500 generated paths (three generators that the reviews of the
  meeting-point rule wrote: random stretches of groups and unions in
  nine profiles, 34,500 paths; shapes planted around a copied join,
  5,000; unions whose sides are variants of one walk, 5,000): the
  limit accepted 43,158 of them, and for
  every one the two compilers give the same list of alternatives (the
  same routes in the same order, the same steps and joins shared), the
  same query, the same verdict and message of the meeting-point check,
  and the same paths for `mk` to spell its repairs with. No path
  differed and none took over the 20 s allowed.
- The old and the new `ge`, run on the same input and compared byte
  for byte (standard output, standard error, and every file of the
  graph afterwards). Each session is a copy of a small graph and, for
  each of ten paths `p` in turn, `ls p`, `mk p`, `ls p`, so that a
  path is tried on whatever the `mk` before it left. On a graph with
  almost no edges: 21,000 paths that the limit accepted, of which
  20,951 gave identical sessions, none a different one, and 49 did not
  finish within 60 s in either build. On a graph where every step
  matches once: 600 paths, 586 identical, none different, 14 unfinished
  in both. On a sparse random graph of nine nodes: 600 paths, 593
  identical, none different, 7 unfinished in both; and `ls` alone of
  all 21,000 on that graph, identical for every one. (Of the 21,000
  paths, 17,742 are run, 1,924 of them with a warning about an `@`
  that does nothing, and 3,258 are refused by the meeting-point check,
  with the same message from both builds.)
- The golden tests of `cli/test/golden/ge` that do not mention the
  limit, 117 of them: unchanged, and they pass with both builds.

### Paths the limit refused: most now run, some for a long time

Of the 44,500 generated paths the limit refused 1,342 (1,340 as
`TooLargeToCount`, 2 as `TooManyAlternatives`). The meeting-point check
refuses 694 of those anyway, and did so first, so the limit alone
stopped 648 of them, 1.5% of the generated paths. These generators are
made to stress the compiler; they say nothing about how often a person
writes such a path.

A sample of 149 of the 648, drawn at random, each run with a limit of
20 s of processor time:

| | finished within 20 s | stopped at 20 s | of those that finished |
|---|---|---|---|
| compiling alone (`compilePath`, forced; a program linked against the library, not `ge`) | 132 | 17 | half within 0.63 s, nine in ten within 5.4 s, the slowest 19.6 s; 79 under a second |
| `ls`, on a graph where every step matches once | 107 | 42 | half within 2.8 s, nine in ten within 12.2 s, the slowest 18.9 s; 12 under a second, 75 under five |
| `mk`, on a graph with almost no edges (below) | 76 | 73 | half within 9.9 s, the slowest 20.6 s; 2 under a second, 20 under five; 34 of the 76 ended with `TooManyRepairs` |

So about nine in ten of the paths the limit refused compile within
20 s, most of them within a second; seven in ten finish an `ls` within
20 s; and half finish an `mk`. The ones that do not finish are stopped
only by the user.

(The graph for `mk` has the nodes 0, 1, 2, 3 and 7 and was meant to
have no edges. The sixth `mk` of the table below succeeded and wrote
what it created, a chain of seven edges from node 0, into the graph,
though `ge` was run without `--no-dry-run-writing`: at this commit
that flag does not keep `mk` from writing, with or without the limit.
Every `mk` after it, which is all of the sample, ran on the graph with
that chain.)

Three more things the sample shows:

- The limit refused paths that are small. Of the 132 that finished
  compiling, 75 have at most 1000 alternatives (8 have at most 100),
  and were refused for the work of counting; the largest has 16,384.
- Compiling is not where most of the time goes. 25 of the 149 compile
  within the limit and still do not finish an `ls` within it: the time
  is in loading and matching what was compiled, which is what the
  matcher fixes of section 3 are about.
- Memory grows with the time: the largest `ls` process reached 1.5 GB
  (the median was 167 MB), the largest `mk` 469 MB.

The paths that earlier text of the specification used as examples of
each error, with the same build (`ls` on the graph where every step
matches once and `mk` on the graph with almost no edges, each with
120 s of wall-clock time, which under the load was 50 to 100 s of
processor time; compiling alone with 120 s of processor time):

| path | what the limit said | alternatives | compiling alone | `ls` | `mk` |
|---|---|---|---|---|---|
| `(a \| b \| c)/(d & e & f & g & h & i & j)` | `TooManyAlternatives` | 2,187 | 0.16 s | 1.0 s | 5.2 s: 2,187 repairs to choose from |
| `(a \| b)/(c \| d)/(e \| f)/(g \| h)/(i \| j)/(m & n)` | `TooManyAlternatives` | 1,024 | 0.06 s | 0.41 s | 1.4 s: 1,024 repairs |
| `(p & q)/(x/@ \| y)/(w/@ \| z)/(u/@ \| v)/(s/@ \| t)/(m/@ \| n)/(k/@ \| l)` | `TooManyAlternatives` | 1,394 | 0.15 s | 0.71 s | 2.8 s: 2,360 repairs |
| `((x \| y)/@ & z)/(c1 & … & c10)` | `TooLargeToCount` | 1,024 | 0.24 s | 1.9 s | 33 s, then `TooManyRepairs` |
| `(x/@ \| @/x/@)/` five times, then `(a/@ \| @/a/@)/(d & e)` | `TooLargeToCount` | 7 | 0.08 s | 0.36 s | 0.30 s |
| `(x0 \| y0) & … & (x8 \| y8) & (a & b)/…/s/…/s`, six groups and ten steps | `TooLargeToCount` | 512, of 73 routes each | 3.0 s | not in 120 s | not in 120 s |
| `(a & b & c)/(a & b & c)/(a & b & c)/(a & b & c)/(a & b)/@/((e \| f) & g)/(a & b & c)/(a & b & c)` | `TooLargeToCount` | 512, of 2,916 routes each | 14.7 s | not in 120 s | not in 120 s |
| `(c & d)/(@1 \| … \| @29)/(a & b)/@` | `TooLargeToCount` | 707,281 | 91 s | not in 120 s | not in 120 s |
| `(x \| y) & (a & b & c)/…`, nine groups | `TooManyRoutes` | 2, of 19,684 routes each | 5.1 s | not in 120 s | not in 120 s |
| `(a & b & c)/…/@/(x \| y)/(a & b)`, eight groups before the join | `TooManyRoutes` | 4, of 13,122 routes each | 1.9 s | not in 120 s | not in 120 s |
| `(x \| y)/(a & b & c)/…`, seven groups | `TooLargeToCount` | `2^2187` ways to choose | not in 120 s | not in 120 s | not in 120 s |
| `(a0 & … & a169)/(x \| y)/(b0 & … & b169)` | `TooManyRoutes` | `2^28900` ways to choose | not in 120 s | not in 120 s | not in 120 s |

The last two never finish: no machine lists `2^2187` alternatives.
The limit did not protect against slow paths in general, though. The
path with the dead end of section 2, which the limit accepted, takes
17 s of processor time to compile to nothing with this build, and a
path without unions was never checked.

### What the user can do about a command that runs long

Tried with the new build on a path that never finishes compiling
(`ls (x | y)/(a & b & c)/…` with seven groups), in a terminal emulated
by a program, eight seconds into the command:

- **At the prompt of the REPL, Ctrl-C stops the command.** `^C` is
  echoed and the prompt is back 0.11 s later; the next command works.
  Nothing was changed: a command compiles its paths before it writes
  anything. The memory the process had taken (about 300 MB after
  eight seconds) was still held five seconds later.
- **`ge -e 'COMMAND'` ends at an interrupt.** The process is gone
  0.04 s after the signal, killed by it, with nothing printed.
- **TAB completion cannot be interrupted.** TAB after that path and a
  `/` compiles the prefix to find what could follow. The prompt stays
  frozen; Ctrl-C typed there is not seen (the terminal is in raw mode
  while a line is being edited), and neither is an interrupt signal
  sent to the process from outside; what is typed afterwards is not
  read. Only killing `ge` ends it. With the limit, TAB offered nothing
  for such a prefix and the prompt stayed usable. This is the one
  place where removing the limit makes `ge` worse in a way the user
  cannot get out of, and it is the first thing to look at if long
  commands become a problem: completion should be interruptible
  whatever else is decided.

### What it costs the tests

The tests on random paths used the limit as their filter (section 1).
They now use one of their own, `fewAlternatives` in
`cli/src/Models/PathPattern/TestCorpus.hs`: a rough count of the ways
to choose, read off the path as written, and then the alternatives
counted as the compiler lists them, lazily, up to a bound. It reads no
clock. It is a filter for tests and nothing more: the count read off
the path is not a bound on anything, and was set by trying it on 3,700
paths of the generators (listing alternatives lazily alone was held up
for seconds, or for good, by 28 of them).

The tests that pinned how the compiler prunes its search stayed, as
`spec_compileContradictions` in
`cli/src/Models/PathPattern/CompileSpec.hs`. With nothing limiting a
path, that pruning is all that keeps a path with a contradiction in it
from taking as long as its choices are many. Each of those tests has an
allowance of a minute for work that takes a fraction of a second, so
that a search that lists what it should drop fails the test instead of
hanging the suite; the allowance measures nothing.
