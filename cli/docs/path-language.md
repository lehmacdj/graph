# The path language

A path describes walks through the graph: from the node the command runs
at, along edges chosen by their labels, to the nodes the walks end at.
The commands that take a path are `ls` (`preview`), `cd`, `mk`, `rm`,
`rmnf`, `mv`, `cp`, `rn`, `cl` (`clone`, `alias`), `tag` (`t`), `at` and
`mg`. This document is the language: its syntax,
what each operator means, which paths are refused or warned about and
what to write instead, and the laws that hold. What each command does
with a path is in [commands.md](commands.md); how a path is compiled and
run is in [path-route-ir.md](path-route-ir.md).

Every example in this document was run on the build it is checked in
with. An example block names a graph and then shows commands and what
`ge` printed. **Every `$` line runs on a fresh copy of the block's
graph**: nothing an earlier `$` line did carries over. Where an example
depends on an earlier command, the commands are written on one `$` line
with `;` between them.

```ge
graph: 0 -a-> 1, 1 -b-> 2
$ ls a/b
a/b @2
$ cd a; ls b
b @2
```

Node `0` is the node the command runs at. Nodes are named by their NID,
twelve base62 characters; the examples use nodes whose NIDs are small
numbers padded with zeros, written and shown as `@2`, and a node that a
command creates gets a fresh NID, shown in full. Output is shown as
printed, without colours and without the timestamp column of `ls`;
errors and warnings (printed on stderr) follow the output. Lines
beginning with `+` or `-` list the edges a command created or deleted.
Long lines are wrapped with a four-space indent.

## 1. Syntax

A path is built from steps and node tests with the operators below,
listed from the one that binds tightest to the loosest.

| form | meaning | example |
|---|---|---|
| `label` | a step along an edge with this label, out of the node (1.1) | `a`, `"a b"` |
| `*` | a step along any edge | `*/b` |
| `regex:"…"`, `regex:'…'` | a step along an edge whose label matches the regex | `regex:"^a"` |
| `#t` | the node of the tag `t`: short for `@<tags>/t`, where `<tags>` is the node that holds the tags. The nodes tagged `t` are one step further, `#t/""` (2.6) | `#todo`, `#"to do"` |
| `@` | stay: the node the walk is at; where several walks arrive, one node for all | `x/@/(c & d)` |
| `@N` | the node with NID `N`: a root at the start of a walk, a test of the node reached elsewhere | `@000000000002/a`, `a/@000000000002` |
| `!{@N, @M}` | any node but these (a trailing comma is allowed, an empty list is not) | `a/!{@000000000001}/b` |
| `%never` | matches nothing | `a \| %never` |
| `~p` | the reverse of `p`: every step against its edge, in the opposite order (binds tightest; `~~a` is a parse error) | `~a`, `~(a/b)` |
| `p/q` | sequence: `q` from where `p` ends (left associative) | `a/b/c` |
| `p & q` | fork: both, from the same node (left associative; `(p & q) & r` is one fork of three) | `a & b/c` |
| `p \| q` | union: every walk that goes through it takes one side, each on its own (2.9) | `(a \| b)/(c & d)` |
| `p ++ q` | union: one side is taken, the same for the whole path (2.9) | `(a ++ b)/(c & d)` |
| `p + q` | union, read as `\|` or `++` by the command | `ls a + b` |
| `(p)` | grouping | `a/(b & c)` |

The three union operators have the lowest precedence. A chain of one of
them associates to the left; two different ones cannot be mixed without
parentheses (`a + b | c` is a parse error, 3.3; write `(a + b) | c`).
Whitespace is allowed around every operator and inside `!{…}`; two paths
side by side (`ls a b`) are a parse error. So `~a/b & c | d` is
`((~a)/b & c) | d`: reversal binds to the step, `/` before `&`, `&`
before the union.

Directives are expanded when the command runs, relative to the node and
the history at that moment:

| form | expands to |
|---|---|
| `%targets(p)` | the nodes `ls p` finds, as a `+` union of `@N` (`%never` when there are none; the command's error then says so). `p` is a query of its own: a `+` in it is `\|` unless an `%override` written inside `p` says otherwise (2.9) |
| `%override({key = value, …}, p)` | `p` with these settings changed inside it: `plus = shared` or `plus = relational` (how `+` is read), and `max-repairs = N` or `unlimited` (the limit on the walks `mk` makes); both are in [commands.md](commands.md#5-settings-and-the-one-limit). `%override({}, p)` is `p` |
| `%history(n)` | `@N` for the node `n` steps back in the history; `%history(0)` is the current node, a negative `n` goes forward (after `back`), and an `n` beyond either end of the history gives the node at that end |
| `%last` | `%history(1)` |
| `https://…`, `http://…` | imports the resource and becomes its node; a URL extends to the next space ([commands.md](commands.md#27-history-and-urls)) |
| `%{…}` | a splice; only in Haskell source, refused on the command line (3.3) |

### 1.1 Labels, quoting and regexes

A label is written as a *plain word*: letters, digits and the
characters `-`, `.`, `_` and `'`, with nothing else. So `a.b`, `12` and
`it's` are labels, and `'a'` is the label with the apostrophes in it.
Any other label is written between double quotes, where the escapes of
a Haskell string are interpreted (`"a\tb"` has a tab); `""` is the
empty label, which `tag` uses. A regex is written `regex:"…"` or
`regex:'…'`: either quote may delimit it, backslashes inside are kept
as written for the regex, and the regex cannot contain its own
delimiter. It is a Perl-compatible regex (PCRE) and it is searched for
in the label: `regex:"a"` matches every label with an `a` in it, and
`regex:"^a$"` the label `a` alone. The parser's message for a regex that does not compile is
unhelpful (`regex:"["` fails with "unexpected ':'"), as is the one for
an `@` followed by more than twelve base62 characters (the last example
below has thirteen).

```ge
graph: 0 -a-> 1, 0 -'a'-> 2, 0 -a.b-> 3, 0 -"a b"-> 4, 0 -12-> 5, 0 -""-> 6
$ ls 'a'
'a' @2
$ ls a.b
a.b @3
$ ls 12
12 @5
$ ls "a b"
"a b" @4
$ ls ""
"" @6
$ ls ~~a
<interactive>:1:5:
  |
1 | ls ~~a
  |     ^^
unexpected "~a"
expecting "!{", "%history(", "%last", "%never", "%override(", "%targets(",
    "%{", "http://", "https://", '#', '(', '*', '@', <transition>, @<nid>, or
    regex:"<regex>"
$ ls regex:"["
<interactive>:1:9:
  |
1 | ls regex:"["
  |         ^
unexpected ':'
expecting "++", '&', '+', '/', '|', alphanumeric character, or end of input
$ ls @abcdefghijklm
<interactive>:1:5:
  |
1 | ls @abcdefghijklm
  |     ^
unexpected 'a'
expecting "++", '&', '+', '/', '|', or end of input
```

## 2. Meaning

### 2.1 A walk

The simplest path is a sequence of steps. `a/b/c` walks an `a` edge out
of the node the command runs at, then a `b` edge out of the node
reached, then a `c` edge. A query (`ls` and the other reading commands)
finds every node such a walk can end at:

```ge
graph: 0 -a-> 1, 1 -b-> 2, 2 -c-> 9
$ ls a/b/c
a/b/c @9
$ ls a/b
a/b @2
```

`ls` prints one line for each way the walk was taken: the labels it
walked and the node it reached ([commands.md](commands.md#21-ls-and-preview)).
Where several edges carry the label, the walk may take any of them:

```ge
graph: 0 -a-> 1, 0 -a-> 2, 1 -c-> 9, 2 -c-> 9
$ ls a/c
a/c @9
a/c @9
```

Two lines, because there are two ways to walk `a/c` to `9`.

### 2.2 Reversal

`~a` walks an `a` edge backwards: from its target to its source. `~`
reverses a whole path, so `~(a/b/c)` is `~c/~b/~a`: the same steps,
backwards and in the opposite order. `at @N cmd` runs a command at
another node:

```ge
graph: 0 -a-> 1, 1 -b-> 2, 2 -c-> 9
$ at @9 ls ~c/~b/~a
c/b/a @0
$ at @9 ls ~(a/b/c)
c/b/a @0
$ ls a/~a
a/a @0
```

The last finds the nodes from which an `a` edge leads to a node that an
`a` edge from `0` reaches. `~` passes through `&` and the unions:
`~(p & q)` is `~p & ~q`, and `~(p | q)` is `~p | ~q`. A node test (`@`,
`@N`, `!{…}`) is not changed by `~`, but it takes its place in the
reversed sequence: `~(a/@1/b)` is `~b/@1/~a`. So an `@N` that ended a
path starts its reverse, where it is a root (2.6), and an `@N` that
started a path ends its reverse, where it is a test; the reversal law
(4.12) is about paths without `@N` for that reason.

```ge
graph: 0 -a-> 1, 1 -b-> 2
$ at @2 ls ~(a/@1/b)
b/a @0
$ at @2 ls ~b/@1/~a
b/a @0
$ ls a/@1
a @1
$ at @1 ls ~(a/@1)
a @0
$ at @2 ls ~(@0/a/b)
b/a @0
```

### 2.3 Forks

`a & b` is two walks from the same node: an `a` edge and a `b` edge.
What a fork requires of its walks' ends depends on the command. A query
needs all the walks of the path to end at the same node (2.10):

```ge
graph: 0 -a-> 1, 0 -b-> 1, 0 -c-> 2
$ ls a & b
a @1
$ ls a & c
no targets
$ ls (a & b) & c
no targets
$ ls a & (b & c)
no targets
```

`&` regroups: `(a & b) & c`, `a & (b & c)` and `a & b & c` are one
fork of three walks, so a fork is a flat list of *conjuncts*, which
this document calls a *group*. The order of the conjuncts is the order
of the walks, and the first walk is the one the edge commands act on
([commands.md](commands.md#3-the-edge-commands-rm-mv-cp-rn-cl)).

### 2.4 Sequencing a fork: the product rule

When a fork is followed by more path, each of its walks continues
separately. `(a & b)/c` is `a/c & b/c`: an `a` walk continued by a
`c`, and a `b` walk continued by its own `c`.

```ge
graph: 0 -a-> 1, 1 -c-> 9, 0 -b-> 2, 2 -c-> 9
$ ls (a & b)/c
a/c @9
$ ls a/c & b/c
a/c @9
$ ls (a & b)/@/c
no targets
```

The third form makes `a` and `b` meet before `c` (2.5); the first does
not, and finds `9` although `a` and `b` end at different nodes.

Two forks in a row multiply out. `(a & b)/(c & d)` is four walks
written apart,

    a/c & a/d & b/c & b/d

each a walk of its own: the `a` of `a/c` and the `a` of `a/d` are
walked independently, so they may take different `a` edges. The one
`a` that was written stands in two walks of the written-out path, and
each of the two is a *copy* of it: multiplying out copies what is
written once. This is the *product rule*: a group followed by a group,
with only steps between them, is every conjunct of the one continued by
every conjunct of the other, each pair a separately written walk. (A
`|` union between the two groups is copied into each walk as a step
would be, and each copy takes a side on its own. Whether the walks are
then still written apart depends on what the sides of that union are:
4.6 has the condition.) The graph below has no `a`
node with both a `c` and a `d`, and the path finds `9` all the same:

```ge
graph: 0 -a-> 1, 1 -c-> 9, 0 -a-> 3, 3 -d-> 9, 0 -b-> 2, 2 -c-> 9, 2 -d-> 9
$ ls (a & b)/(c & d)
a/c @9
$ ls a/c & a/d & b/c & b/d
a/c @9
$ ls (a & b)/@/(c & d)
no targets
```

A fork inside a conjunct is multiplied out the same way when the
conjunct has no `@`, `@N` or `!{…}` of its own: `a/(b & c) & d` is
`a/b & a/c & d`, three walks with two independent `a`:

```ge
graph: 0 -a-> 1, 1 -b-> 9, 0 -a-> 2, 2 -c-> 9, 0 -d-> 9
$ ls a/(b & c) & d
a/b @9
$ ls a/b & a/c & d
a/b @9
$ ls a/@/(b & c) & d
no targets
```

So, without joins, a path is simply the list of walks it multiplies out
to, and each walk is independent of the others. Joins are what make
walks share nodes. (The messages of `ge` call a walk of the written-out
path a *route*.)

### 2.5 Joins

`@` written after a step is a *join*: the walks that arrive at it along
its sequence must be at one node there, and everything that follows
leaves from that one node. The most common use is to pin a node for
several continuations. `x/@/(c & d)` is one `x` edge whose target has
both a `c` and a `d`, where `x/(c & d)` is `x/c & x/d`, two `x` edges:

```ge
graph: 0 -x-> 1, 1 -c-> 9, 0 -x-> 2, 2 -d-> 9
$ ls x/(c & d)
x/c @9
$ ls x/@/(c & d)
no targets
```

```ge
graph: 0 -x-> 1, 1 -c-> 9, 0 -x-> 2, 2 -d-> 9, 0 -x-> 3, 3 -c-> 9, 3 -d-> 9
$ ls x/(c & d)
x/c @9
x/c @9
$ ls x/@/(c & d)
x/c @9
```

Written after a fork, a join makes the walks of the fork meet:
`(a & b)/@/c` is an `a` and a `b` that end at the same node, and one
`c` from there (the graph of 2.4 found nothing for it).

**A join governs the sequence it is written in.** Call a sequence at one
level of nesting a *chain*: `x/@/w/(c & d)` is one chain of the
segments `x`, `@`, `w` and the group `(c & d)`; each conjunct of a group
is a chain of its own; parentheses without `&` make no chain (`a/(b/c)`
is the chain `a/b/c`). A join belongs to the chain it stands in, and
cuts it: what is before the join is not copied by a fork after it in
the same chain, and what arrives along the chain meets at it. So a
*join of a chain* is one written in that chain itself, not one inside a
conjunct of a group in it. The joins written in a chain cut it into
*stretches*: a stretch is the part of a chain between two joins written
in it, or between such a join and an end of the chain. Forks multiply
out within a stretch, and not across its ends.

- *Leaving.* After `x/@`, every continuation in the chain leaves from
  the one `x` target, however far away the fork is: `x/@/w/(c & d)` has
  one `x` and two `w` (the `w` is after the join, so the fork copies
  it):

  ```ge
  graph: 0 -x-> 1, 1 -w-> 2, 2 -c-> 9, 1 -w-> 3, 3 -d-> 9
  $ ls x/@/w/(c & d)
  x/w/c @9
  $ ls x/@/w/@/(c & d)
  no targets
  ```

- *Inside a conjunct.* A join inside a conjunct is a join of that
  conjunct's chain only. The product rule copies the conjunct whole,
  with its join, so each copy has a join of its own:
  `(p & q)/(x/@/w & y)` is `p/x/@/w & p/y & q/x/@/w & q/y`, with one
  `x` target for `p`'s walk and another for `q`'s. Compare
  `(p & q)/x/@/w`, where `x` is in the outer chain and `p` and `q`
  must reach one `x` target:

  ```ge
  graph: 0 -p-> 1, 0 -q-> 2, 1 -x-> 3, 3 -w-> 9, 2 -x-> 4, 4 -w-> 9, 1 -y-> 9, 2 -y-> 9
  $ ls (p & q)/(x/@/w & y)
  p/x/w @9
  $ ls p/x/@/w & p/y & q/x/@/w & q/y
  p/x/w @9
  $ ls (p & q)/x/@/w
  no targets
  ```

- *A join in an earlier group does not stop the copying.* The product
  rule copies `x/@/w` whole for `c` and for `d` in
  `(x/@/w & b)/(c & d)`, which is `x/@/w/c & x/@/w/d & b/c & b/d`: two
  `x` walks, each with a join that one walk arrives at and one leaves,
  so the `@` does nothing here. To have one `x` for both continuations
  without making `b` meet it, write the join in the chain that forks:
  `x/@/w/(c & d) & b/(c & d)`. A join of the outer chain between the
  two groups, `(x/@/w & b)/@/(c & d)`, makes `x/@/w` and `b` meet first.

  ```ge
  graph: 0 -x-> 1, 1 -w-> 2, 2 -c-> 9, 0 -x-> 3, 3 -w-> 4, 4 -d-> 9, 0 -b-> 5, 5 -c-> 9, 5 -d-> 9
  $ ls (x/@/w & b)/(c & d)
  x/w/c @9
  $ ls x/@/w/c & x/@/w/d & b/c & b/d
  x/w/c @9
  $ ls x/@/w/(c & d) & b/(c & d)
  no targets
  $ ls (x/@/w & b)/@/(c & d)
  no targets
  ```

  When the `@` that does nothing begins or ends the copied conjunct, as
  in `(x/@ & b)/(c & d)`, it looks like the pin it is in `x/@/(c & d)`
  and the command warns (3.2).

- Joins written one after another are one join: `a/@/@` is `a/@`,
  `a/@/!{@N}` is one join that also excludes `N`, and `a/@/@N` and
  `a/!{@M}/@N`, for `M` other than `N`, are one join that tests for
  node `N`, so each counts as the `@N` does (2.6, 3.1). Tests that
  contradict each other let nothing pass: `a/!{@N}/@N` finds nothing,
  and as a side of a union it is no side (4.3).

The product rule and joins together define what any path without unions
means: multiply out every group whose conjuncts have no join of their
own, copy a conjunct with a join whole, and let every join hold
together the walks of its chain that reach it.

### 2.6 Fixed nodes

`@N` names a node. At the start of a walk it is a *root*: the walk
starts there instead of at the current node. After a step it is a
*test*: the walk must have reached that node.

```ge
graph: 0 -a-> 1, 0 -b-> 2, 1 -c-> 9
$ ls @1/c
c @9
$ ls a/@1/c
a/c @9
$ ls b/@1/c
no targets
$ ls @1 & a
@1
$ ls @1 & b
no targets
```

`@1 & a` is the fixed node `1` (a walk of no steps that starts there)
and an `a` walk from the current node; the query needs them to end at
the same node. `@/@1` tests that the node the walk is at is `1`:

```ge
graph: 0 -a-> 1
$ ls @/@1
no targets
$ at @1 ls @/@1
@1
```

The product rule is literal for a conjunct that is only `@`:
`(a & @)/(@7 & d)` is `a/@7 & a/d & @/@7 & @/d`, where `@/@7` tests
the current node rather than jumping to `7`, so from `0` it finds
nothing. The `@` joins nothing here, but it is what makes `@7` a test,
so the path cannot be written without it, and no warning is given
(3.2):

```ge
graph: 0 -a-> 7, 0 -d-> 7, 7 -d-> 7
$ ls (a & @)/(@7 & d)
no targets
$ ls a/@7 & a/d & @/@7 & @/d
no targets
```

`@N` is a join like `@`: walks that arrive at it along its chain are
at node `N`, so they are at one node. It differs from `@` in one
respect for the rule of 3.1: copies of it are all the same node.

`#t` is the node of the tag `t`: `@<tags>/t`, a fixed node and one step
from it. `tag #t p` links that node to the targets of `p` with an edge
whose label is empty ([commands.md](commands.md#46-tag)), so the nodes
tagged `t` are `#t/""`:

```ge
graph: 0 -a-> 1
$ tag #todo a; ls #todo; ls #todo/""
todo @O8OhMSVtjevt
todo/"" @1 #todo
+ O8OhMSVtjevt -""-> 1
+ pbYxBO6fzBQV -todo-> O8OhMSVtjevt
```

### 2.7 Exclusion

`!{@N, @M}` passes any node but those listed. On a single walk it is a
filter; at the start of a path it tests the current node:

```ge
graph: 0 -a-> 1, 0 -a-> 2, 1 -c-> 9, 2 -c-> 9
$ ls a/!{@1}/c
a/c @9
$ ls a/!{@1, @2}/c
no targets
$ ls !{@0}/a
no targets
$ ls !{@1}/a
a @1
a @2
```

Where several walks arrive at it along its chain it is a join with a
test, like `@`: `(a & b)/!{@5}/c` needs `a` and `b` to meet, at a node
other than `5`.

```ge
graph: 0 -a-> 1, 1 -c-> 9, 0 -b-> 2, 2 -c-> 9
$ ls (a & b)/!{@5}/c
no targets
$ ls (a & b)/(!{@5}/c & !{@5}/c)
a/c @9
```

### 2.8 Any label and regexes

`*` steps along an edge with any label and `regex:"…"` along one whose
label matches (1.1 for the quoting). A label that is not a plain word
is shown quoted in the output:

```ge
graph: 0 -a-> 1, 0 -b-> 2, 0 -ab-> 3, 0 -"a b"-> 4
$ ls *
a @1
"a b" @4
ab @3
b @2
$ ls regex:"^a"
a @1
"a b" @4
ab @3
$ ls regex:'^a.$'
ab @3
$ ls "a b"
"a b" @4
```

A `*` or regex step chooses one edge; a fork copies the step and each
copy chooses on its own, so in a query `*` behaves as the `|` union of
all labels (4.5).

### 2.9 Unions

A union offers two or more *sides*; a walk through it takes one. The
three union operators differ in who chooses when the union is copied.

**`|` chooses in every copy.** In `(a | b)/(c & d)` the fork copies the
union into the `c` walk and the `d` walk, and each copy picks a side on
its own. One way to pick is `a/c & b/d`, so the path finds `9` here:

```ge
graph: 0 -a-> 1, 1 -c-> 9, 0 -b-> 2, 2 -d-> 9
$ ls (a | b)/(c & d)
a/c @9
$ ls a/c & b/d
a/c @9
$ ls (a ++ b)/(c & d)
no targets
```

Every way for the copies to pick is an *alternative* of the path:
`(a | b)/(c & d)` has four, `a/(c & d)`, `a/c & b/d`, `b/c & a/d` and
`b/(c & d)`, and a query finds what any alternative finds. Alternatives
multiply: a `|` of `k` sides that a fork copies into `n` walks has up
to `k^n` of them. Nothing limits that number (3.4).

**`++` chooses once for the whole path.** `(a ++ b)/(c & d)` is
`a/(c & d) ++ b/(c & d)`: both copies take the same side, so it found
nothing above.

**`+` is one of the two, by command.** In a query (`ls`, `preview`,
`cd`, `rm`, `rmnf`, `mv`, `cp`, `rn`, `cl`, `at`, `mg`, the target of
`tag`, a `%targets(…)`) it is `|`; in a construction (`mk`, the path
`tag` builds) it is `++`. `%override({plus = shared}, p)` reads every
`+` in `p` as `++`, and `plus = relational` as `|`, in any command:

```ge
graph: 0 -a-> 1, 1 -c-> 9, 0 -b-> 2, 2 -d-> 9
$ ls (a + b)/(c & d)
a/c @9
$ ls %override({plus = shared}, (a + b)/(c & d))
no targets
$ mk (a + b)/(c & d)
This path can be created in 2 ways; nothing was changed. To choose one, write
    @N after a step for the node it should go to, or !{@N} for nodes it should
    not, or keep one side of a +.
  1. @0 -"a"-> @1 -"c"-> @9 & @0 -"a"-> @1 -"d"-> new1
    creates: @1 -"d"-> new1
    use: mk a/(c & d)
  2. @0 -"b"-> @2 -"c"-> new1 & @0 -"b"-> @2 -"d"-> @9
    creates: @2 -"c"-> new1
    use: mk b/(c & d)
```

`mk` reads `+` as `++`, so it offers one way of creating the path per side and does not
offer to create `a/c & b/d` ([commands.md](commands.md#4-mk)). The
argument of a `%targets(…)` is a query of its own, in which `+` starts
again as `|`: an `%override` around the `%targets(…)` governs the union
of nodes it becomes, not the `+` inside its argument, which only an
`%override` written inside the argument reaches:

```ge
graph: 0 -a-> 1, 1 -c-> 9, 0 -b-> 2, 2 -d-> 9
$ ls %targets((a + b)/(c & d))
@9
$ ls %override({plus = shared}, %targets((a + b)/(c & d)))
@9
$ ls %targets(%override({plus = shared}, (a + b)/(c & d)))
no targets
```

**A side is read where the union stands.** A side's steps continue the
chain the union is in, and a join in a side is a join of that chain: in
`(x/@ | zz)/(c & d)` the copies that take `x/@` share one `x` target,
as they would with `x/@` written there. The graph below has two `x`
edges and no `zz`: `(x | zz)/(c & d)` finds `9` with two `x` walks,
`(x/@ | zz)/(c & d)` finds nothing, since the copies that take `x/@`
must take one `x`, and `(x | x/@)/(c & d)` finds `9` through the
alternative in which the `c` copy takes `x` and the `d` copy `x/@`:

```ge
graph: 0 -x-> 1, 1 -c-> 9, 0 -x-> 2, 2 -d-> 9
$ ls (x | zz)/(c & d)
x/c @9
$ ls (x/@ | zz)/(c & d)
no targets
$ ls (x | x/@)/(c & d)
x/c @9
```

A side with a fork is copied like any other side; its conjuncts are
multiplied out with the rest of the chain by the product rule, and a
fork in a side can make the path refused (3.1).

**Every side that is written is a side of its own.** A union has as
many sides as are written in it, for `|` and for `++`. Nothing makes
two sides one: not being written the same, not meaning the same where
the union stands, not differing only in an `@` that does nothing there.
So a union is not a set of its sides, and `p | p` is not always `p`
(4.2).

What a copy of a `|` takes is a side. The copies that take the same
side share that side's joins, as they would with the side written in
place of the union, and copies that take different sides share nothing
of them. So for a join written once in the own chain of each side, a
union of `n` sides gives at most `n` nodes, however many copies of that
union take them, and writing a joined side once more is how to ask for
one node more. (Two things are another matter, both below: a join
inside a conjunct of a group in the side, which is copied with that
conjunct; and a union in a conjunct that a product copies, where each
copy of the conjunct has a union of its own.) Below,
`a/@/(d & e)` needs one `a` target with both a `d` and an `e`, and
there is none; with the side written twice, the `d` copy can take one
side and the `e` copy the other, each side with an `a` target of its
own. A side that differs only by a leading `@`, which does nothing
there, is a side all the same. A `++` is chosen once for the whole
path, and either choice of `a/@ ++ a/@` is `a/@/(d & e)`:

```ge
graph: 0 -a-> 1, 1 -d-> 9, 0 -a-> 2, 2 -e-> 9
$ ls a/(d & e)
a/d @9
$ ls a/@/(d & e)
no targets
$ ls (a/@ | a/@)/(d & e)
a/d @9
$ ls (a/@ | @/a/@)/(d & e)
a/d @9
$ ls (a/@ ++ a/@)/(d & e)
no targets
$ ls (a | a/@)/(d & e)
a/d @9
```

Two sides are at most two nodes. Where three continuations each need an
`a` target of their own, two sides are not enough and three are; where
two targets are enough, two sides are:

```ge
graph: 0 -a-> 1, 0 -a-> 2, 0 -a-> 3, 1 -d-> 9, 2 -e-> 9, 3 -f-> 9
$ ls (a/@ | a/@)/(d & e & f)
no targets
$ ls (a/@ | a/@ | a/@)/(d & e & f)
a/d @9
$ ls (a/@ | (a/@ | a/@))/(d & e & f)
a/d @9
$ ls (a/@ | (a/@ ++ a/@))/(d & e & f)
no targets
```

```ge
graph: 0 -a-> 1, 0 -a-> 2, 1 -d-> 9, 1 -e-> 9, 2 -f-> 9
$ ls a/@/(d & e & f)
no targets
$ ls (a/@ | a/@)/(d & e & f)
a/d @9
```

A union written as a side is its sides: `p | (q | r)` and `(p | q) | r`
are the three sides `p`, `q` and `r` (the third example above), where
`a/@ | (a/@ ++ a/@)` has two, one of them a `++` that is chosen once.
The order in which sides are written never matters, nor how they are
grouped (4.2).

A joined side holds together every copy that takes it, whatever those
copies took before it. Below, a copy that came through `a` and one that
came through `b` both take `c/@` and must then be at one `c` target;
with that side written twice they can take one each:

```ge
graph: 0 -a-> 1, 0 -b-> 2, 1 -c-> 3, 2 -c-> 4, 3 -d-> 9, 4 -e-> 9
$ ls (a | b)/c/(d & e)
a/c/d @9
$ ls (a | b)/c/@/(d & e)
no targets
$ ls (a | b)/(c/@ | k)/(d & e)
no targets
$ ls (a | b)/(c/@ | c/@)/(d & e)
a/c/d @9
```

It is the same after a fork: `(p & q)/a/@/b` makes the walks of `p`
and `q` meet at one `a` target, and `(p & q)/(a/@ | a/@)/b` lets each
take a side.

```ge
graph: 0 -p-> 1, 0 -q-> 2, 1 -a-> 3, 2 -a-> 4, 3 -b-> 9, 4 -b-> 9
$ ls (p & q)/a/@/b
no targets
$ ls (p & q)/(a/@ | a/@)/b
p/a/b @9
$ ls (p & q)/(a/@ ++ a/@)/b
no targets
```

**What copies a union.** A union comes to stand in several walks in two
ways, and the two differ.

- *A fork in its stretch.* A group before or after the union in its
  stretch (2.5) copies it as it would copy a step written there. These
  are copies of the one union: each takes a side on its own, and those
  that take the same side share that side's joins. Everything above is
  about such copies.
- *A product that copies the conjunct it stands in.* A union in a
  conjunct is copied with the conjunct, like a join there (2.5), and
  each copy of the conjunct then has a union of its own, as if the
  conjunct had been written once for each: the copies of the one choose
  apart from those of the other, and two that take the same joined side
  are still two walks, each with a join of its own. So `n` sides are at
  most `n` nodes in each copy of the conjunct, not in all of them
  together.

Below, `(p & q)/(a/@ | k)/d` has one union, in the outer chain: the
walks of `p` and of `q` both take `a/@` (there is no `k` edge) and must
meet at one `a` target, which they do not. In
`(p & q)/((a/@ | k)/d & z)` the union stands in a conjunct that
`(p & q)` copies, so the path is
`p/(a/@ | k)/d & p/z & q/(a/@ | k)/d & q/z`, with two unions and an
`a` target for each:

```ge
graph: 0 -p-> 1, 0 -q-> 2, 1 -a-> 3, 2 -a-> 4, 3 -d-> 9, 4 -d-> 9, 1 -z-> 9, 2 -z-> 9
$ ls (p & q)/(a/@ | k)/d
no targets
$ ls (p & q)/((a/@ | k)/d & z)
p/a/d @9
$ ls p/(a/@ | k)/d & p/z & q/(a/@ | k)/d & q/z
p/a/d @9
```

Likewise two sides are not enough for the three continuations below
that each need an `a` target of their own, and as a conjunct that
`(d & e & f)` copies, `(a/@ | a/@)` is three unions of two sides. The
third path has the `@` inside a conjunct of each side: the product
copies that conjunct for `d`, for `e` and for `f`, so the copies that
take one side have three `a` targets between them, and the warning
says that the `@` joins nothing (3.2):

```ge
graph: 0 -a-> 1, 0 -a-> 2, 0 -a-> 3, 1 -d-> 9, 2 -e-> 9, 3 -f-> 9, 0 -z-> 4, 4 -d-> 9, 4 -e-> 9, 4 -f-> 9
$ ls (a/@ | a/@)/(d & e & f)
no targets
$ ls ((a/@ | a/@) & z)/(d & e & f)
a/d @9
$ ls ((a/@ & z) | (a/@ & z))/(d & e & f)
a/d @9
Warning: the @ that ends `a/@` does nothing here: `(d & e & f)` copies `a/@`
    whole, once for each of its conjuncts, so the @ joins nothing and pins
    nothing. An @ pins only in the chain it is written in, as in x/@/(c & d):
    leave this one out, or write out what it should join. The same holds for
    one more @ in this path. (JoinDoesNothing)
```

*Nothing copies* a union when no fork stands in its stretch (no group,
and no union with an `&` in a side), and no other fork stands in the
stretch of a group or union that it is written inside. So in
`q/((x | y)/k & z)` nothing copies `(x | y)`, and in
`((x | y)/k & z)/(c & d)` the group `(c & d)` does, since it stands in
the stretch of the group the union is written in. Such a union is gone
through once. It
then makes no difference whether it is written `|`, `++` or `+` (4.9),
nor whether a side of it is written twice (4.2).

**`%never` matches nothing.** As a side it is the empty side: `a | %never`
is `a`, and a `%targets(p)` that finds nothing expands to `%never`. As
a step it makes its whole walk fail, and with it every fork and
sequence it is in: `a & %never`, `a/%never` and `%never/a` find
nothing, and `(a & %never) | a` is `a`.

```ge
graph: 0 -a-> 1
$ ls a | %never
a @1
$ ls a & %never
no targets
$ ls a/%never
no targets
$ ls (a & %never) | a
a @1
```

**`%targets(p)` is a `+` union of nodes.** In a query each copy may
pick a different target; with `%override({plus = shared}, …)` the
whole path picks one; in `mk` it picks one.

```ge
graph: 0 -t-> 1, 0 -t-> 2, 1 -x-> 9, 2 -y-> 9
$ ls %targets(t)/(x & y)
x @9
$ ls (@1 | @2)/(x & y)
x @9
$ ls @1/x & @2/y
x @9
$ ls %override({plus = shared}, %targets(t)/(x & y))
no targets
```

### 2.10 What a query adds

A path is *relative* when every walk of it starts at the node the
command runs at: nothing that can start a walk elsewhere (`@N`, `#t`, a
`%targets(…)`, `%history(n)`, `%last` or a URL) stands at the start of
a walk, which is the start of the path, or of a conjunct or a union
side that itself stands there. Where a walk starts is read with every
`~` applied (2.2): `~(a/@1)` is `@1/~a`, whose `@1` starts the walk, so
it is not relative, though the `@1` is written after a step. Further on
in a walk such a thing is a test of the node reached (2.6), and the
path is relative all the same, as `a/(@1 | b)` is. A relative path
starts at
the current node as if `@/` were written before it; and a query appends
`/@`, so that all the walks of the path end at one node, which is the
target. That is why `ls a & b` finds nothing when `a` and `b` end apart,
while `mk a & b` is satisfied by an `a` edge and a `b` edge wherever
they lead:

```ge
graph: 0 -a-> 1, 0 -b-> 2
$ ls a & b
no targets
$ ls (a & b)/@
no targets
$ mk a & b
created 0 new nodes and 0 new edges
```

(`mk` printed nothing and created nothing: the path already holds.) For
a path that is not relative, writing `@/` in front is not the same
thing: it turns an `@N` that starts a walk from a root into a test
(2.6), where a query leaves it a root:

```ge
graph: 0 -a-> 2, 1 -b-> 2
$ ls a & @1/b
a @2
$ ls (a & @1/b)/@
a @2
$ ls @/(a & @1/b)
no targets
```

The two implicit joins are what the reversal law is about (4.12).

## 3. Refused and warned paths

### 3.1 A product may not copy a meeting point

The product rule copies a conjunct whole, with the joins in it. Where
such a join is a node that several walks meet at or leave from, the
copies have no clear meaning: should the walks of one copy meet those
of the other? Rather than define that, the language refuses the path.
Every command refuses it before it reads or edits the graph for it, and
the error says what to write instead. The whole path is refused, not
only the part of it that is unclear. What has happened by then stays:
the directives of the path are expanded first, so a URL in it is
imported already, and the commands before this one on a `;` line and
the earlier targets of an `at` have run
([commands.md](commands.md#6-when-a-command-fails)).

The rule is about the path with its products written out (2.4):

- An `@` or `!{…}` that is written once has a *copy* in every walk of
  the written-out path that it comes to stand in.
  `(x/@/w & b)/(c & d)` is `x/@/w/c & x/@/w/d & b/c & b/d`: two copies
  of the `@`.
- A copy is a *meeting point* when more than one walk comes into it or
  more than one goes out of it. `(a & b)/((c & y)/@/e & d)` is
  `a/(c & y)/@/e & a/d & b/(c & y)/@/e & b/d`, and `c` and `y` come
  into each copy of the `@`. Walks that an `@N` has already brought to
  one node count as one walk from there on: in `(c & y)/@4/w/@/e` one
  walk comes into the `@` and one goes out.
- A path is *refused* when some `@` or `!{…}` that is written once has
  two copies or more and at least one of them is a meeting point. With
  unions, that is so when it holds for any alternative: any way for the
  copies of the `|` unions to take their sides, and any choice of the
  `++` unions (below).
- An `@N` is never refused: all its copies are node `N`. Nor is an `@`
  or a `!{…}` written right next to an `@N` (`@/@N`, `@N/@`,
  `!{@M}/@N`), which is one join with it (2.5).

Copies are counted as they are written, not as the nodes they turn out
to be: the copies of an `@` that starts a chain are all the node the
chain starts at, and `(@/(a & b) & c)/(d & e)` is refused all the same
(section 5).

To apply the rule by hand, write the products out, or picture them. If
an `@` or `!{…}` that you wrote once appears more than once, and in
some copy more than one walk comes into it or goes out of it, the path
is refused. For the common case there is a quicker test. It is a
shortcut and not the rule, and it can be wrong either way: it lets
through some paths that are refused, and it stops at some that run.
The kinds of path it is known to get wrong are listed after it; where
it matters, write the products out.

1. Find two groups with nothing but steps and unions between them in
   their chain: no `@`, `@N` or `!{…}` written in that chain (one in a
   side of a union between them does not count). (A group inside a
   conjunct that has no join of its own counts as standing where that
   conjunct's group stands. A side of a union whose end facing the
   other group is an `&` counts as a group; a join of the side between
   its `&` and that end keeps it from counting.) No such pair: the
   shortcut says the path runs.
2. In either group, find a conjunct with an `@` or `!{…}` and an `&`
   next to it: in the same conjunct, at its own level, with no join
   between the two. Found: the shortcut says the path is refused.
3. Not found: the shortcut says the path runs.

Refused, where the shortcut says it runs:

- A conjunct of one group holds several walks together behind an `@N`,
  and a conjunct of the other group has an `@` or `!{…}` facing it, as
  in `(@1/(c & e) & b)/(x/@/w & d)`: `x/@/w` is one walk, but its copy
  for `@1/(c & e)` is `@1/(c & e)/x/@/w`, in which `c/x` and `e/x` meet
  at the `@`.
- The meeting point lies in a group nested in the conjunct, past a join
  of that conjunct, as in `(a & b)/(k/@1/(m/@/(e & f) & z) & d)`: the
  conjunct `k/@1/(m/@/(e & f) & z)` is copied for `a` and for `b` with
  everything in it, and `e` and `f` leave each copy of the `@`.
- The `&` next to the `@` is in a side of a union, as in
  `(a & b)/(((c & y) | k)/@/e & d)`.

Run, where the shortcut says it is refused:

- The `@` or `!{…}` stands right next to an `@N` and is one join with
  it, as in `(a & b)/((c & y)/@/@1/e & d)`.
- A `++` union stands between the two groups and each of its sides has
  a join, as in `(a & b)/(x/@ ++ w/@)/((c & y)/@/e & d)`: each choice
  puts a join of the chain between the groups.

```ge
graph: 0 -a-> 1, 0 -b-> 2
$ ls (a & b)/(k/@1/(m/@/(e & f) & z) & d)
This path would copy a meeting point, which has no clear meaning, so the
    command was not run. In `m/@/(e & f)` several walks meet at the `@` or
    leave it, inside `k/@1/(m/@/(e & f) & z)`, and `(a & b)` would copy it,
    once for each of its conjuncts: it stands next to the group `(k/@1/(m/@/(e
    & f) & z) & d)` with no join of their chain between the two. If the walks
    should come together between the two groups, put a join between them: `(a
    & b)/@/(k/@1/(m/@/(e & f) & z) & d)`. Otherwise write the copies out, one
    conjunct for each pair: `a/k/@1/(m/@/(e & f) & z) & a/d & b/k/@1/(m/@/(e &
    f) & z) & b/d`. (CopiedMeetingPoint)
$ ls (a & b)/(((c & y) | k)/@/e & d)
This path would copy a meeting point, which has no clear meaning, so the
    command was not run. In `(c & y | k)/@/e` several walks meet at the `@` or
    leave it, and `(a & b)` would copy it, once for each of its conjuncts: it
    stands next to the group `((c & y | k)/@/e & d)` with no join of their
    chain between the two. If the walks should come together between the two
    groups, put a join between them: `(a & b)/@/((c & y | k)/@/e & d)`.
    Otherwise write the copies out, one conjunct for each pair.
    (CopiedMeetingPoint)
$ ls (a & b)/((c & y)/@/@1/e & d)
no targets
$ ls (a & b)/(x/@ ++ w/@)/((c & y)/@/e & d)
no targets
```

The error names the conjunct, the join and the group that would copy
it, and says what one can write instead: a join between the two groups,
if the walks of the first should meet before the second, or the copies
written out, each conjunct for each conjunct of the other group. It
spells the first out when the two groups stand in one chain and no
union is read between them, and the second only when the path has no
union and few walks; otherwise it says "(/@)" and "write the copies
out". What it spells out is the part of that chain from the one group
to the other, not the whole path: for
`z/(a & b)/((c & y)/@/e & d)/q` it says to write
`(a & b)/@/((c & y)/@/e & d)`, which belongs where the two groups
stood, between `z/` and `/q` (section 5). Here `(a & b)` would copy
`(c & y)/@/e`, where `c` and `y` meet, once for `a` and once for `b`:

```ge
graph: 0 -a-> 1, 0 -b-> 2, 1 -c-> 4, 1 -y-> 4, 2 -c-> 5, 2 -y-> 5, 4 -e-> 9, 5 -e-> 9, 1 -d-> 9, 2 -d-> 9
$ ls (a & b)/((c & y)/@/e & d)
This path would copy a meeting point, which has no clear meaning, so the
    command was not run. In `(c & y)/@/e` several walks meet at the `@` or
    leave it, and `(a & b)` would copy it, once for each of its conjuncts: it
    stands next to the group `((c & y)/@/e & d)` with no join of their chain
    between the two. If the walks should come together between the two groups,
    put a join between them: `(a & b)/@/((c & y)/@/e & d)`. Otherwise write
    the copies out, one conjunct for each pair: `a/(c & y)/@/e & a/d & b/(c &
    y)/@/e & b/d`. (CopiedMeetingPoint)
$ ls (a & b)/@/((c & y)/@/e & d)
no targets
$ ls a/(c & y)/@/e & a/d & b/(c & y)/@/e & b/d
a/c/e @9
```

The two replacements differ: with the join between the groups, `a` and
`b` must end at one node (here they do not), and with the copies
written out, each copy has a `(c & y)/@/e` of its own under its own
`a` or `b` node. The written-out form is what the compiler makes of the
refused path.

The same error comes from every command (`cd`, `mk`, `rm`, `mv`, `cp`,
`rn`, `tag`, `at`, `mg`, and a `%targets(…)` whose query is refused),
and TAB completion offers nothing for a refused path:

```ge
graph: 0 -a-> 1, 0 -b-> 2, 1 -c-> 4, 1 -y-> 4, 2 -c-> 5, 2 -y-> 5, 4 -e-> 9, 5 -e-> 9, 1 -d-> 9, 2 -d-> 9
$ cd (a & b)/((c & y)/@/e & d)
This path would copy a meeting point, which has no clear meaning, so the
    command was not run. In `(c & y)/@/e` several walks meet at the `@` or
    leave it, and `(a & b)` would copy it, once for each of its conjuncts: it
    stands next to the group `((c & y)/@/e & d)` with no join of their chain
    between the two. If the walks should come together between the two groups,
    put a join between them: `(a & b)/@/((c & y)/@/e & d)`. Otherwise write
    the copies out, one conjunct for each pair: `a/(c & y)/@/e & a/d & b/(c &
    y)/@/e & b/d`. (CopiedMeetingPoint)
$ mk (a & b)/((c & y)/@/e & d)
This path would copy a meeting point, which has no clear meaning, so the
    command was not run. In `(c & y)/@/e` several walks meet at the `@` or
    leave it, and `(a & b)` would copy it, once for each of its conjuncts: it
    stands next to the group `((c & y)/@/e & d)` with no join of their chain
    between the two. If the walks should come together between the two groups,
    put a join between them: `(a & b)/@/((c & y)/@/e & d)`. Otherwise write
    the copies out, one conjunct for each pair: `a/(c & y)/@/e & a/d & b/(c &
    y)/@/e & b/d`. (CopiedMeetingPoint)
```

These paths are refused, run, or run with a warning. Where a path is
followed by `=`, what follows is the path with its products written
out:

| path | | why |
|---|---|---|
| `(a & b)/((c & y)/@/e & d)` = `a/(c & y)/@/e & a/d & b/(c & y)/@/e & b/d` | refused | the `@` at which `c` and `y` meet is copied for `a` and for `b` |
| `(x/@/(w & v) & b)/(c & d)` = `x/@/(w & v)/c & x/@/(w & v)/d & b/c & b/d` | refused | the mirror image: `w` and `v` leave the `@`, which is copied for `c` and for `d` |
| `(a/@/(b & c) & d)/(e & f)`, `(p & q)/(a/@/(b & c) & d)` | refused | the same |
| `(a & b)/(c/@/w/(e & d) & d)` | refused | two walks go out of each copy of the `@`, through `w` |
| `(@/(a & b) & c)/(d & e)`, `(@/(a & b) & c)/(@/f & e)`, `(x & y)/(@/(a & b) & c)` | refused | two walks leave the leading `@` (for the first, every copy of that `@` is the node the path starts at; section 5) |
| `(@1/(c & e) & b)/(x/@/w & d)` = `@1/(c & e)/x/@/w & @1/(c & e)/d & b/x/@/w & b/d` | refused | `x/@/w` is one walk, but in its copy for `@1/(c & e)` the walks `c/x` and `e/x` meet at the `@` |
| `(a/@ & b)/((c & e)/@1/f & d)`, `(a/!{@7} & b)/((c & e)/@1/f & d)` | refused | the mirror image: in the copy for `(c & e)/@1/f`, `c` and `e` leave the `@` (or the `!{@7}`, which is no mere filter there) |
| `(a/@/(b & c) & d)/@`, `(a/@/(b & c) & d)/@/(e & f)`, `(p & q)/@/(a/@/(b & c) & d)` | run | a join of the chain stands between the groups: nothing is copied |
| `z/((c & y)/@/e & d)`, `q/(@/(a & b) & c)` | run | one group: the meeting point is not copied; the second is `q/@/(a & b) & q/c` |
| `(x/@/w & b)/(c & d)` = `x/@/w/c & x/@/w/d & b/c & b/d` | run | `x/@/w` is one walk: its `@` does nothing, and there are two `x` |
| `(p & q)/(x/@3/w & y)`, `(p & q)/(x/@3/(w & y) & y)`, `(a & b)/((c & y)/@/@4/e & d)` | run | an `@N` is never refused, nor is an `@` written next to one |
| `(a & b)/((c & y)/@4/w/@/e & d)` | run | `c` and `y` are one walk after `@4`: one walk comes into each copy of the `@` |
| `(q/(c & e) & b)/(x/@/w & d)`, `(@1/(c & e)/@2 & b)/(x/@/w & d)` | run | `q/(c & e)` holds nothing together (it is `q/c & q/e`); in the second a join stands after the `&` |
| `(x/@ & b)/(c & d)` = `x/c & x/d & b/c & b/d`, `(a & b)/(@/c & d)` | run, with a warning | the `@` begins or ends a conjunct that is a plain walk: it does nothing and looks like a pin (3.2) |
| `(a & @)/(@7 & d)`, `((x \| y)/@ & z)/(c & d)` | run | the `@` does nothing here either, and no warning is given (3.2) |

```ge
graph: 0 -x-> 1, 1 -w-> 3, 1 -v-> 3, 3 -c-> 9, 0 -x-> 2, 2 -w-> 4, 2 -v-> 4, 4 -d-> 9, 0 -b-> 5, 5 -c-> 9, 5 -d-> 9
$ ls (x/@/(w & v) & b)/(c & d)
This path would copy a meeting point, which has no clear meaning, so the
    command was not run. In `x/@/(w & v)` several walks meet at the `@` or
    leave it, and `(c & d)` would copy it, once for each of its conjuncts: it
    stands next to the group `(x/@/(w & v) & b)` with no join of their chain
    between the two. If the walks should come together between the two groups,
    put a join between them: `(x/@/(w & v) & b)/@/(c & d)`. Otherwise write
    the copies out, one conjunct for each pair: `x/@/(w & v)/c & x/@/(w & v)/d
    & b/c & b/d`. (CopiedMeetingPoint)
$ ls x/@/(w & v)/c & x/@/(w & v)/d & b/c & b/d
x/w/c @9
$ ls (x/@/(w & v) & b)/@/(c & d)
no targets
$ ls (x/@/w & b)/(c & d)
x/w/c @9
```

```ge
graph: 0 -a-> 1, 1 -b-> 2, 1 -c-> 2, 0 -d-> 3, 2 -e-> 9, 2 -f-> 9, 3 -e-> 9, 3 -f-> 9
$ ls (a/@/(b & c) & d)/(e & f)
This path would copy a meeting point, which has no clear meaning, so the
    command was not run. In `a/@/(b & c)` several walks meet at the `@` or
    leave it, and `(e & f)` would copy it, once for each of its conjuncts: it
    stands next to the group `(a/@/(b & c) & d)` with no join of their chain
    between the two. If the walks should come together between the two groups,
    put a join between them: `(a/@/(b & c) & d)/@/(e & f)`. Otherwise write
    the copies out, one conjunct for each pair: `a/@/(b & c)/e & a/@/(b & c)/f
    & d/e & d/f`. (CopiedMeetingPoint)
$ ls a/@/(b & c)/e & a/@/(b & c)/f & d/e & d/f
a/b/e @9
$ ls (a/@/(b & c) & d)/@/(e & f)
no targets
```

A leading `@` keeps the conjunct it starts together: `q/(@/(a & b) & c)`
is `q/@/(a & b) & q/c`, one `q` target for `a` and `b`, where
`q/(a & b & c)` has three independent `q`. Nothing copies the conjunct
here, so the path runs:

```ge
graph: 0 -q-> 1, 1 -a-> 9, 0 -q-> 2, 2 -b-> 9, 0 -q-> 3, 3 -c-> 9
$ ls q/(@/(a & b) & c)
no targets
$ ls q/@/(a & b) & q/c
no targets
$ ls q/(a & b & c)
q/a @9
```

Where another group stands next to it, the same conjunct is refused:

```ge
graph: 0 -a-> 1, 0 -b-> 1, 1 -d-> 9, 0 -c-> 2, 2 -e-> 9, 2 -d-> 9, 1 -e-> 9
$ ls (@/(a & b) & c)/(d & e)
This path would copy a meeting point, which has no clear meaning, so the
    command was not run. In `@/(a & b)` several walks meet at the `@` or leave
    it, and `(d & e)` would copy it, once for each of its conjuncts: it stands
    next to the group `(@/(a & b) & c)` with no join of their chain between
    the two. If the walks should come together between the two groups, put a
    join between them: `(@/(a & b) & c)/@/(d & e)`. Otherwise write the copies
    out, one conjunct for each pair: `@/(a & b)/d & @/(a & b)/e & c/d & c/e`.
    (CopiedMeetingPoint)
$ ls @/(a & b)/d & @/(a & b)/e & c/d & c/e
a/d @9
```

An `@N` in the place of the `@` is not refused, since every copy of it
is the same node, nor is an `@` written next to it. Walks that the
`@N` has brought together are one walk at an `@` further on. A `!{…}`
on one walk only filters, while a `!{…}` where walks meet is a meeting
point like `@`:

```ge
graph: 0 -a-> 1, 0 -b-> 2, 1 -c-> 4, 1 -y-> 4, 2 -c-> 5, 2 -y-> 5, 4 -e-> 9, 5 -e-> 9, 1 -d-> 9, 2 -d-> 9, 4 -w-> 6, 6 -e-> 9
$ ls (a & b)/((c & y)/@4/e & d)
no targets
$ ls (a & b)/((c & y)/@/@4/e & d)
no targets
$ ls (a & b)/((c & y)/@4/w/@/e & d)
no targets
$ ls (a & b)/(c/!{@7}/e & d)
a/c/e @9
$ ls (a & b)/((c & y)/!{@7}/e & d)
This path would copy a meeting point, which has no clear meaning, so the
    command was not run. In `(c & y)/!{@7}/e` several walks meet at the
    `!{@7}` or leave it, and `(a & b)` would copy it, once for each of its
    conjuncts: it stands next to the group `((c & y)/!{@7}/e & d)` with no
    join of their chain between the two. If the walks should come together
    between the two groups, put a join between them: `(a & b)/@/((c &
    y)/!{@7}/e & d)`. Otherwise write the copies out, one conjunct for each
    pair: `a/(c & y)/!{@7}/e & a/d & b/(c & y)/!{@7}/e & b/d`.
    (CopiedMeetingPoint)
```

A conjunct that holds several walks together behind an `@N` brings them
along into what it is copied for, and the error then names the copy in
which the walks meet or part. Below, `x/@/w` is one walk, and yet its
`@` is no idle one: written out, `c/x` and `e/x` must meet at it, which
they do not here, and without the `@` the path finds `9`. A join after
the `&` of the first conjunct (`@1/(c & e)/@2`) leaves one walk to
bring along, and the path runs:

```ge
graph: 1 -c-> 2, 1 -e-> 3, 2 -x-> 4, 3 -x-> 5, 4 -w-> 9, 5 -w-> 9, 0 -b-> 6, 6 -x-> 7, 7 -w-> 9, 6 -d-> 9, 2 -d-> 9, 3 -d-> 9
$ ls (@1/(c & e) & b)/(x/@/w & d)
This path would copy a meeting point, which has no clear meaning, so the
    command was not run. `(@1/(c & e) & b)` would copy `x/@/w`, once for each
    of its conjuncts: it stands next to the group `(x/@/w & d)` with no join
    of their chain between the two. In the copy for `@1/(c & e)`, which holds
    several walks together, those walks meet at the `@` of `x/@/w`, and in the
    other copies they do not. If the walks should come together between the
    two groups, put a join between them: `(@1/(c & e) & b)/@/(x/@/w & d)`.
    Otherwise write the copies out, one conjunct for each pair: `@1/(c &
    e)/x/@/w & @1/(c & e)/d & b/x/@/w & b/d`. (CopiedMeetingPoint)
$ ls @1/(c & e)/x/@/w & @1/(c & e)/d & b/x/@/w & b/d
no targets
$ ls (@1/(c & e) & b)/(x/w & d)
c/x/w @9
$ ls (@1/(c & e)/@2 & b)/(x/@/w & d)
no targets
```

**Unions.** The rule is about what the product would copy, however the
path is written, so a union is read in every way it can be taken:

- A `|` union is read as each of its sides in its place, and also as a
  single step: two walks that take different sides of it pass it as a
  step would be passed, so a `|` never keeps two groups apart, whatever
  joins its sides have. The path is refused when any one of these
  readings is. `((a & b) | z)/((c & y)/@/e & d)` is refused for the
  side `(a & b)` read in place (and so are
  `(p & q)/(x/@ | s)/((c & y)/@/e & d)`,
  `(x/@ | (p & q)/s)/((c & y)/@/e & d)`,
  `((a & b) | (u & v))/((c & y)/@/e & d)`,
  `((a & b) | @3)/((c & y)/@/e & d)` and
  `(x/@/(m & n) & q)/((a & b) | z)`);
  `(a & b)/(x/@ | w/@)/((c & y)/@/e & d)` and
  `(a & b)/(@1 | @2)/((c & y)/@/e & d)` are refused for the union read
  as a step, which leaves `(a & b)` next to `((c & y)/@/e & d)`. Equal
  conjuncts are not merged: `((a & a) | z)/((c & y)/@/e & d)` is
  refused too.
- A side written twice is two sides here as everywhere (2.9), so
  writing a side again can turn a path that runs into one that is
  refused: `(a & b)/x/@/((c & y)/@/e & d)` runs, since the `x/@` is a
  join of the chain between the groups, and
  `(a & b)/(x/@ | x/@)/((c & y)/@/e & d)` is refused, since `a` and
  `b` may take a side each.
- A `++` union is read as each of its choices; the path is refused when
  one choice is, since otherwise that choice would vanish silently.
  `((a & b) ++ z)/((c & y)/@/e & d)` is refused;
  `(a & b)/(x/@ ++ w/@)/((c & y)/@/e & d)` runs, since each choice
  puts a join of the chain between the groups.
- A `+` is read as what the command makes of it: as `|` in a query,
  where it is refused exactly when the path with `|` is, and as `++` in
  `mk`. Nothing falls back from the one to the other. The error shows
  the union as the command reads it, and says so where the union it
  names was written with `+`; where that `+` stands inside an
  `%override({plus = relational}, …)`, it says that the override makes
  it `|` and that the way out is `plus = shared` in that override.

```ge
graph: 0 -a-> 1, 0 -b-> 2, 1 -c-> 4, 1 -y-> 4, 2 -c-> 5, 2 -y-> 5, 4 -e-> 9, 5 -e-> 9, 1 -d-> 9, 2 -d-> 9, 0 -z-> 3, 3 -c-> 6, 3 -y-> 6, 6 -e-> 9, 3 -d-> 9, 0 -t-> 1, 0 -t-> 2
$ ls ((a & b) | z)/((c & y)/@/e & d)
This path would copy a meeting point, which has no clear meaning, so the
    command was not run. In `(c & y)/@/e` several walks meet at the `@` or
    leave it, and `(a & b)` would copy it, once for each of its conjuncts: it
    stands next to the group `((c & y)/@/e & d)` with no join of their chain
    between the two. That is how the path reads for a route that takes the
    side `(a & b)` of `(a & b | z)`: a path is refused when one way to take
    the sides of its | unions is. If the walks should come together between
    the two groups, put a join between them (/@). Otherwise write the copies
    out, one conjunct for each pair. (CopiedMeetingPoint)
$ ls ((a & b) ++ z)/((c & y)/@/e & d)
This path would copy a meeting point, which has no clear meaning, so the
    command was not run. In `(c & y)/@/e` several walks meet at the `@` or
    leave it, and `(a & b)` would copy it, once for each of its conjuncts: it
    stands next to the group `((c & y)/@/e & d)` with no join of their chain
    between the two. That is the path with `(a & b)` chosen in `(a & b ++ z)`:
    a path is refused when one choice of its ++ unions is. If the walks should
    come together between the two groups, put a join between them (/@).
    Otherwise write the copies out, one conjunct for each pair.
    (CopiedMeetingPoint)
$ ls ((a & b) + z)/((c & y)/@/e & d)
This path would copy a meeting point, which has no clear meaning, so the
    command was not run. In `(c & y)/@/e` several walks meet at the `@` or
    leave it, and `(a & b)` would copy it, once for each of its conjuncts: it
    stands next to the group `((c & y)/@/e & d)` with no join of their chain
    between the two. That is how the path reads for a route that takes the
    side `(a & b)` of `(a & b | z)` (written with +, which is | in this
    command): a path is refused when one way to take the sides of its | unions
    is. If the walks should come together between the two groups, put a join
    between them (/@). Otherwise write the copies out, one conjunct for each
    pair. Writing ++ for that +, or %override({plus = shared}, …), does not
    help here, since the path is refused with that side alone.
    (CopiedMeetingPoint)
$ ls (a & b)/(x/@ | w/@)/((c & y)/@/e & d)
This path would copy a meeting point, which has no clear meaning, so the
    command was not run. In `(c & y)/@/e` several walks meet at the `@` or
    leave it, and `(a & b)` would copy it, once for each of its conjuncts: it
    stands next to the group `((c & y)/@/e & d)` with no join of their chain
    between the two. Routes that take different sides of `(x/@ | w/@)` pass it
    as one step, so it keeps the two groups apart for none of them; write ++
    there to choose one side for the whole path. If the walks should come
    together between the two groups, put a join between them (/@). Otherwise
    write the copies out, one conjunct for each pair. (CopiedMeetingPoint)
$ ls (a & b)/(x/@ ++ w/@)/((c & y)/@/e & d)
no targets
$ ls (a & b)/(x/@ + w/@)/((c & y)/@/e & d)
This path would copy a meeting point, which has no clear meaning, so the
    command was not run. In `(c & y)/@/e` several walks meet at the `@` or
    leave it, and `(a & b)` would copy it, once for each of its conjuncts: it
    stands next to the group `((c & y)/@/e & d)` with no join of their chain
    between the two. Routes that take different sides of `(x/@ | w/@)`
    (written with +, which is | in this command) pass it as one step, so it
    keeps the two groups apart for none of them; write ++ there to choose one
    side for the whole path. If the walks should come together between the two
    groups, put a join between them (/@). Otherwise write the copies out, one
    conjunct for each pair. For that +, write ++ instead, or wrap the path in
    %override({plus = shared}, …). (CopiedMeetingPoint)
$ ls (a & b)/%targets(t)/((c & y)/@/e & d)
This path would copy a meeting point, which has no clear meaning, so the
    command was not run. In `(c & y)/@/e` several walks meet at the `@` or
    leave it, and `(a & b)` would copy it, once for each of its conjuncts: it
    stands next to the group `((c & y)/@/e & d)` with no join of their chain
    between the two. Routes that take different sides of `(@1 | @2)` (written
    with +, which is | in this command) pass it as one step, so it keeps the
    two groups apart for none of them; write ++ there to choose one side for
    the whole path. If the walks should come together between the two groups,
    put a join between them (/@). Otherwise write the copies out, one conjunct
    for each pair. For that +, write ++ instead, or wrap the path in
    %override({plus = shared}, …). (CopiedMeetingPoint)
```

```ge
graph: 0 -a-> 1, 0 -b-> 2, 1 -x-> 3, 2 -x-> 3, 3 -c-> 4, 3 -y-> 4, 4 -e-> 9, 3 -d-> 9
$ ls (a & b)/x/@/((c & y)/@/e & d)
a/x/c/e @9
$ ls (a & b)/(x/@ | x/@)/((c & y)/@/e & d)
This path would copy a meeting point, which has no clear meaning, so the
    command was not run. In `(c & y)/@/e` several walks meet at the `@` or
    leave it, and `(a & b)` would copy it, once for each of its conjuncts: it
    stands next to the group `((c & y)/@/e & d)` with no join of their chain
    between the two. Routes that take different sides of `(x/@ | x/@)` pass it
    as one step, so it keeps the two groups apart for none of them; write ++
    there to choose one side for the whole path. If the walks should come
    together between the two groups, put a join between them (/@). Otherwise
    write the copies out, one conjunct for each pair. (CopiedMeetingPoint)
$ ls (a & b)/(x/@ ++ x/@)/((c & y)/@/e & d)
a/x/c/e @9
```

Run, on the graph below: `z/((c & y)/@/e & d)` and
`(a | z)/((c & y)/@/e & d)`, where no group stands next to the group
with the meeting point; `(x/@ | z)/((c & y)/@/e & d)`, likewise; and
`((a & b)/@ | z)/((c & y)/@/e & d)`, where the side's own join stands
between its `&` and the group, so the side is one walk from its join on
and means what `x/@` means there:

```ge
graph: 0 -a-> 1, 0 -b-> 2, 1 -c-> 4, 1 -y-> 4, 2 -c-> 5, 2 -y-> 5, 4 -e-> 9, 5 -e-> 9, 1 -d-> 9, 2 -d-> 9, 0 -z-> 3, 3 -c-> 6, 3 -y-> 6, 6 -e-> 9, 3 -d-> 9
$ ls z/((c & y)/@/e & d)
z/c/e @9
$ ls (a | z)/((c & y)/@/e & d)
a/c/e @9
z/c/e @9
$ ls ((a & b)/@ | z)/((c & y)/@/e & d)
z/c/e @9
$ ls (x/@ | z)/((c & y)/@/e & d)
z/c/e @9
```

Also run: `a/(@1 | @2)/(x & y)`, where each copy picks its node (2.9),
`(x/@ | zz)/(c & d)`, and `(p & q)/(x/@ | s)/(m/@/e & d)`, where
`m/@/e` is one walk. Since a `|` is read as a step between the groups,
`(a & b)/%targets(t)/((c & y)/@/e & d)` is refused when `t` has two
targets or more and runs when it has one (section 5).

So "adding a side never removes a result" (4.4) has an exception:
adding a side can make the path refused, as `z/((c & y)/@/e & d)` runs
and `((a & b) | z)/((c & y)/@/e & d)` does not.

### 3.2 A join that does nothing (warning)

An `@` in a copied conjunct that is one walk is run and does nothing:
each copy has its own walk through it. `(x/@ & b)/(c & d)` is
`x/c & x/d & b/c & b/d`, with two `x`, where `x/@/(c & d)` has one.
Since an `@` that begins or ends such a conjunct looks like the pin it
is in `x/@/(c & d)` or `(a & b)/@/c`, the command warns, on stderr,
before it runs (`JoinDoesNothing`).

The warning says that the `@` can be left out, so it is given only
where that is certain whatever is written around the conjunct: better
no warning than one that could be false. It is given exactly when all
of these hold:

1. The path is run, not refused (3.1).
2. The `@` is the first or the last segment of a conjunct of a group,
   and that conjunct is a *plain walk*: at its own level it has nothing
   but steps and `@`, and at least one step. So there is no `@N`, no
   `!{…}`, no union and no fork in the conjunct. `@/x`, `x/@`, `@/x/@`
   and `x/@/w/@` are plain walks; in the last, only the `@` at the end
   is warned about.
3. A product copies the conjunct: its group has another group, or a
   side of a union that is a group where it faces this one (3.1), next
   to it in the chain, with only steps and unions between the two and
   no join of their chain, in some way of taking the unions between
   them. `(x/@ & b)/((c & d) | z)` among the examples below has such a
   side.
   (A group in a conjunct that has no join of its own counts as
   standing where that conjunct's group stands, so the `@` of
   `(b & c)/(a/(@/x & b) & w)` is warned about.)
4. No `|` union anywhere in the path has a join (`@`, `@N` or `!{…}`)
   in a side: written in the side's own chain, or in a side of a union,
   `|` or `++`, that is written in that chain, however deep. So no
   `(m/@ | n)`, no `(@1 | @2)` and no `((m/@ ++ k) | n)`. These are no
   obstacle: a join inside a conjunct of a group in the side
   (`((m & n/@) | k)`); a `++` union with a join in a side, when it
   stands in no side of a `|` (`(m/@ ++ n/@)`); and a `|` whose other
   sides are `%never`, which is no union (`(m/@ | %never)` is `m/@`,
   4.3).

Condition 4 has a consequence worth knowing: one `|` union with a
joined side, anywhere in the path and however unrelated to the `@`,
silences every such warning of the path. A `+` that the command reads
as `|` counts, and so does a `%targets(…)` that finds two nodes or more
in a query, since it is a `|` union of `@N`. The reason is that where
walks take different sides of such a union the order of the walks goes
by the joins they pass, and it has not been shown that an `@` which
joins nothing never changes that order; the first walk is what the edge
commands act on.

The warning names the conjunct and the group that copies it. It also
says what to write for one shared node where that is simple: no union
is read between the two groups, the conjunct's group and the group
that copies it are segments of one chain, and the `@` faces that group
(it ends its conjunct and the group comes after, or begins it and the
group comes before). Otherwise it only says that an `@` pins in its
own chain: when a union stands between, when the `@` is on the far
side (`(@/x & b)/(c & d)`), and when the conjunct lies in a group
nested in a conjunct (`(b & c)/(a/(@/x & b) & w)`). What it says to
write is the part of the chain from the one group to the other, not the
whole path: for `(x/@ & b)/(c & d)/q` it is still
`x/@/(c & d) & b/(c & d)`, to be followed by `/q` (section 5).

```ge
graph: 0 -x-> 1, 0 -x-> 2, 1 -c-> 9, 2 -d-> 9, 0 -b-> 3, 3 -c-> 9, 3 -d-> 9, 3 -z-> 9, 0 -t-> 1, 0 -t-> 2
$ ls (x/@ & b)/(c & d)
x/c @9
Warning: the @ that ends `x/@` does nothing here: `(c & d)` copies `x/@`
    whole, once for each of its conjuncts, so the @ joins nothing and pins
    nothing. If one node was meant, write `x/@/(c & d) & b/(c & d)`; otherwise
    leave the @ out. (JoinDoesNothing)
$ ls x/c & x/d & b/c & b/d
x/c @9
$ ls x/@/(c & d) & b/(c & d)
no targets
$ ls (b & x)/(@/c & d)
b/c @9
Warning: the @ that begins `@/c` does nothing here: `(b & x)` copies `@/c`
    whole, once for each of its conjuncts, so the @ joins nothing and pins
    nothing. If one node was meant, write `(b & x)/@/c & (b & x)/d`; otherwise
    leave the @ out. (JoinDoesNothing)
$ ls (x/@ & b)/((c & d) | z)
x/c @9
Warning: the @ that ends `x/@` does nothing here: `(c & d)` copies `x/@`
    whole, once for each of its conjuncts, so the @ joins nothing and pins
    nothing. (That is with the side `(c & d)` of `(c & d | z)`.) An @ pins
    only in the chain it is written in, as in x/@/(c & d): leave this one out,
    or write out what it should join. (JoinDoesNothing)
$ ls (@/x & b)/(c & d)
x/c @9
Warning: the @ that begins `@/x` does nothing here: `(c & d)` copies `@/x`
    whole, once for each of its conjuncts, so the @ joins nothing and pins
    nothing. An @ pins only in the chain it is written in, as in x/@/(c & d):
    leave this one out, or write out what it should join. (JoinDoesNothing)
$ ls (x/@ & b)/(c & d)/@/(m/@ ++ n)
no targets
Warning: the @ that ends `x/@` does nothing here: `(c & d)` copies `x/@`
    whole, once for each of its conjuncts, so the @ joins nothing and pins
    nothing. If one node was meant, write `x/@/(c & d) & b/(c & d)`; otherwise
    leave the @ out. (JoinDoesNothing)
$ ls (x/@ & b)/(c & d)/@/(m/@ | n)
no targets
$ ls (x/@ & b)/(c & d)/@/((m/@ ++ n) | k)
no targets
$ ls (x/@ & b)/(c & d) & %targets(t)/c
x/c @9
```

The last three have the same `@` that does nothing, and no warning,
because of the `|` with a joined side elsewhere in the path (in the
second of them the join is in a `++` inside the side, and in the third
the `%targets(t)` is `@1 | @2`).

The warning is given by every command, and not while completing with
TAB. A command line prints each warning text once: the same path
written twice, or run at many nodes by `at`, warns once. Two different
paths warn twice unless their warnings read the same, which they do
when the two groups are the same and the paths differ only in what is
written around them (the third example below).

```ge
graph: 0 -x-> 1, 0 -x-> 2, 1 -c-> 9, 2 -d-> 9, 0 -b-> 3, 3 -c-> 9, 3 -d-> 9
$ ls (x/@ & b)/(c & d); ls (x/@ & b)/(c & d)
x/c @9
x/c @9
Warning: the @ that ends `x/@` does nothing here: `(c & d)` copies `x/@`
    whole, once for each of its conjuncts, so the @ joins nothing and pins
    nothing. If one node was meant, write `x/@/(c & d) & b/(c & d)`; otherwise
    leave the @ out. (JoinDoesNothing)
$ ls (x/@ & b)/(c & d); ls (b & x/@)/(c & d)
x/c @9
b/c @9
Warning: the @ that ends `x/@` does nothing here: `(c & d)` copies `x/@`
    whole, once for each of its conjuncts, so the @ joins nothing and pins
    nothing. If one node was meant, write `x/@/(c & d) & b/(c & d)`; otherwise
    leave the @ out. (JoinDoesNothing)
Warning: the @ that ends `x/@` does nothing here: `(c & d)` copies `x/@`
    whole, once for each of its conjuncts, so the @ joins nothing and pins
    nothing. If one node was meant, write `b/(c & d) & x/@/(c & d)`; otherwise
    leave the @ out. (JoinDoesNothing)
$ ls (x/@ & b)/(c & d); ls (x/@ & b)/(c & d)/q
x/c @9
no targets
Warning: the @ that ends `x/@` does nothing here: `(c & d)` copies `x/@`
    whole, once for each of its conjuncts, so the @ joins nothing and pins
    nothing. If one node was meant, write `x/@/(c & d) & b/(c & d)`; otherwise
    leave the @ out. (JoinDoesNothing)
```

Many an `@` that does nothing gets no warning. An `@` strictly inside
the conjunct (`(x/@/w & b)/(c & d)`, 2.5); a `!{…}`, which is a
filter; an `@` whose conjunct is not a plain walk because it has a
union (`((x | y)/@ & z)/(c & d)`, `((x/@ | zz) & b)/(c & d)`) or an
`@N` (`(x/@1/@ & b)/(c & d)`); a conjunct that is only `@`
(`(a & @)/(@7 & d)`, 2.6: there the `@` cannot be left out, since it is
what makes `@7` a test); an `@` that begins or ends a conjunct lying
inside a copied conjunct past a join of its own
(`(p & q)/(m/@1/(x/@ & z) & y)`); and every `@` of a path that
condition 4 silences. Warning about every `@` that does nothing is not
attempted (section 5).

```ge
graph: 0 -x-> 1, 0 -x-> 2, 1 -c-> 9, 2 -d-> 9, 0 -b-> 3, 3 -c-> 9, 3 -d-> 9
$ ls ((x/@ | zz) & b)/(c & d)
x/c @9
$ ls (x/@1/@ & b)/(c & d)
no targets
$ ls (p & q)/(m/@1/(x/@ & z) & y)
no targets
```

### 3.3 Parse errors

A command line that does not parse runs nothing. Besides the usual
"unexpected" errors, the path parser refuses, by name:

```ge
graph: 0 -a-> 1
$ ls a + b | c
<interactive>:1:10:
  |
1 | ls a + b | c
  |          ^
The union operators + and | are mixed without parentheses; add parentheses
    around one of them, e.g. (a + b) | c, near the | at line 1, columns 10-10.
    (MixedUnionOperators)
$ ls %override({plus = both}, a)
<interactive>:1:15:
  |
1 | ls %override({plus = both}, a)
  |               ^
Unknown value both for plus; it accepts shared or relational at line 1,
    columns 15-25. (InvalidOverrideSetting)
$ ls %override({max-repairs = 0}, a)
<interactive>:1:15:
  |
1 | ls %override({max-repairs = 0}, a)
  |               ^
Unknown value 0 for max-repairs; it accepts a positive number or unlimited at
    line 1, columns 15-29. (InvalidOverrideSetting)
$ ls %override({minus = shared}, a)
<interactive>:1:15:
  |
1 | ls %override({minus = shared}, a)
  |               ^
Unknown setting minus in %override; the settings are plus and max-repairs at
    line 1, columns 15-28. (InvalidOverrideSetting)
$ ls %override({plus = shared, plus = relational}, a)
<interactive>:1:30:
  |
1 | ls %override({plus = shared, plus = relational}, a)
  |                              ^
The setting plus is given twice at line 1, columns 30-46.
    (InvalidOverrideSetting)
$ ls a/%{foo}
<interactive>:1:12:
  |
1 | ls a/%{foo}
  |            ^
The splice directive %{foo} only works in the path quasiquoter, not the CLI;
    remove the one at line 1, columns 6-11. (SpliceInCommand)
$ ls @asdf
<interactive>:1:4:
  |
1 | ls @asdf
  |    ^
Incomplete NID '@asdf' at line 1, columns 4-8: a NID needs all 12 base62
    characters. (IncompleteFullNID)
```

### 3.4 No limit on what a path comes to

Nothing refuses a path for being large. A `|` union of `k` sides that a
fork copies into `n` walks has up to `k^n` alternatives, groups written
one after the other multiply their walks, and a command compiles and
matches whatever its path comes to, for as long as that takes. Most
such paths run; some that are short to write do not finish, and a
command that runs too long is stopped with Ctrl-C
([commands.md](commands.md#51-nothing-limits-what-a-path-compiles-to)).

```ge
graph: 0
$ ls (a | b | c)/(d & e & f & g & h & i & j)
no targets
```

That path has `3^7 = 2187` alternatives. The one limit there is belongs
to `mk`, which stops when it would have to walk the path in more than
5000 ways (`TooManyRepairs`,
[commands.md](commands.md#52-walks-max-repairs)).
[path-cost-limits.md](path-cost-limits.md) records what was tried as a
limit on the cost of a path, and why there is none.

## 4. Laws

Each law is stated with the condition under which it holds, and the
smallest counterexample outside that condition, run. "Finds the same"
means the same targets from every node of every graph; the laws are
about queries, and say nothing about which edge a command acts on or
what `mk` creates unless they say so. Where a law speaks of paths that
are refused (3.1), it is about what the compiler makes of them; no
command runs them.

Throughout, a path *has a join of its own chain* when an `@`, `@N` or
`!{…}` is written in that chain itself, or in the top chain of a side
of a union written there, which is a join of the chain for the walks
that take that side (2.9); a join inside a conjunct of a group in it
does not count. A stretch (2.5) ends only at a join written in the
chain itself: a join in a side of a union ends it for the walks that
take that side and not for the others. A union is *copied* as 2.9 says
("What copies a union").

Three conditions below are not proved: when `p | p` finds what `p`
finds (4.2), the product identity across a `|` union (4.6), and when a
union means the same written `|` or `++` (4.9). What supports each is
that no counterexample is known inside it, and that searches among
random small paths on small random graphs found none. Each says so
where it stands.

### 4.1 Regrouping

`/` regroups: `(p/q)/r` and `p/(q/r)` are the chain `p/q/r`, whatever
the parentheses, with no condition. `&` regroups the same way (2.3).

```ge
graph: 0 -a-> 1, 1 -b-> 2, 2 -c-> 9
$ ls (a/b)/c
a/b/c @9
$ ls a/(b/c)
a/b/c @9
```

### 4.2 Side order never matters; a side written twice is two sides

`p | q` is `q | p`, and `p | (q | r)` is `(p | q) | r`, in every
context and for every command: the path compiles to the same thing
whatever the order and grouping of the sides, so the same targets are
found, the same edge is acted on, and `mk` does the same. The same
holds for `++`.

`p ++ p` is `p`, whatever `p` is: the side is chosen once for the whole
path, and either choice is `p`. (Only a warning tells the two apart:
where `p` has an `@` that is warned about, 3.2, the warning of
`p ++ p` adds "The same holds for one more @ in this path", counting
the `@` of each side.)

`p | p` is not `p` in general, since every written side is a side of
its own (2.9). `p | p` finds everything `p` finds (4.4) and may find
more: `(a/@ | a/@)/(d & e)` finds `9` below, through the alternative
`a/@/d & a/@/e` in which the two copies take a side each, and
`a/@/(d & e)` does not. How the two compare depends on whether the
union is copied (2.9) and on what `p` is:

- *Nothing copies the union.* It is gone through once, and `p | p`
  finds what `p` finds, whatever `p` is: `q/(a/@ | a/@)/c` finds what
  `q/a/@/c` finds, and `(p & q)/@/(a/@ | a/@)/@/(d & e)` what
  `(p & q)/@/a/@/(d & e)` finds. (Not proved: no counterexample is known, and searching found none. For
  these two examples the two paths also compile to the same thing, so
  every command does the same with both.) Mind what copies a union. A
  join in a side of another union does not end the stretch: in
  `(p & q)/(m/@ | n)/(a/@ | a/@)/z` the walks of `p` and of `q` that
  take `n` come to the second union apart, so it is copied, and the
  path finds more than `(p & q)/(m/@ | n)/a/@/z` does. And a union in
  a conjunct that a product copies is copied with it:
  `(((x ++ y) | (x ++ y)) & z)/(d & e)` finds more than
  `((x ++ y) & z)/(d & e)`.
- *The union is copied, and `p` has no join of its own chain and no
  `++`.* No case is known in which the two find different targets, and
  trying found none; that is not proved. Often they also compile to
  the same thing (`(a | a)/(d & e)` is `a/(d & e)`). One exception to
  that is known, in which `p | p` has one alternative more: `p` forks,
  and a group in the stretch has a conjunct in which walks meet at an
  `@N` and go on as one walk, as in
  `((a & b) | (a & b))/((c & y)/@1/e & d)`. Whether there is another
  such exception is not known.
- *The union is copied, and `p` has a join of its own chain or a
  `++`.* The two may differ: `(a/@ | a/@)/(d & e)` against
  `a/@/(d & e)`; and `((x ++ y) | (x ++ y))/(d & e)` against
  `(x ++ y)/(d & e)`, since a `++` written twice is two unions, each
  chosen once, and one may be `x` while the other is `y`.

Sides that are one fixed node are that node whichever is taken:
`(@1 | @1)/(d & b)` is `@1/(d & b)`.

```ge
graph: 0 -a-> 1, 1 -d-> 9, 0 -a-> 2, 2 -e-> 9, 0 -b-> 3, 3 -d-> 9, 3 -e-> 9
$ ls (a/@ | a/@)/(d & e)
a/d @9
$ ls a/@/(d & e)
no targets
$ ls (a/@ | b)/(d & e)
a/d @9
b/d @9
$ ls (b | a/@)/(d & e)
a/d @9
b/d @9
$ ls (a | a)/(d & e)
a/d @9
$ ls a/(d & e)
a/d @9
```

```ge
graph: 0 -q-> 5, 5 -a-> 1, 5 -a-> 2, 1 -c-> 9, 2 -c-> 9
$ ls q/(a/@ | a/@)/c
q/a/c @9
q/a/c @9
$ ls q/a/@/c
q/a/c @9
q/a/c @9
```

```ge
graph: 0 -x-> 1, 1 -d-> 9, 0 -y-> 2, 2 -e-> 9
$ ls (x ++ y)/(d & e)
no targets
$ ls ((x ++ y) | (x ++ y))/(d & e)
x/d @9
$ ls ((x ++ y) ++ (x ++ y))/(d & e)
no targets
```

```ge
graph: 0 -a-> 2, 0 -b-> 3, 2 -c-> 1, 2 -y-> 1, 3 -c-> 1, 3 -y-> 1, 1 -e-> 5, 2 -d-> 5, 3 -d-> 5
$ ls (a & b)/((c & y)/@1/e & d)
a/c/e @5
$ ls ((a & b) | (a & b))/((c & y)/@1/e & d)
a/c/e @5
```

```ge
graph: 0 -p-> 1, 0 -q-> 2, 1 -n-> 3, 2 -n-> 4, 3 -a-> 5, 4 -a-> 6, 5 -z-> 9, 6 -z-> 9
$ ls (p & q)/(m/@ | n)/(a/@ | a/@)/z
p/n/a/z @9
$ ls (p & q)/(m/@ | n)/a/@/z
no targets
```

```ge
graph: 0 -x-> 1, 1 -d-> 9, 0 -y-> 2, 2 -e-> 9, 0 -z-> 3, 3 -d-> 9, 3 -e-> 9
$ ls (((x ++ y) | (x ++ y)) & z)/(d & e)
x/d @9
$ ls ((x ++ y) & z)/(d & e)
no targets
```

### 4.3 The empty side

`p | %never` is `p`, `p ++ %never` is `p`, with no condition. A
`%targets(…)` that finds nothing is `%never`, so a union with it is the
other sides. (As a step rather than a side, `%never` makes its walk
fail, 2.9.)

```ge
graph: 0 -a-> 1, 1 -c-> 9, 0 -b-> 2, 2 -d-> 9
$ ls (a | b)/(c & d)
a/c @9
$ ls (a | b | %never)/(c & d)
a/c @9
$ ls (a | %never)/(c & d)
no targets
$ ls a/(c & d)
no targets
```

### 4.4 Adding a side never removes a result

For a path `C[p | q]` with a union somewhere in it, adding a side `r`
to that union loses no target: everything `C[p | q]` finds,
`C[p | q | r]` finds. The same holds for `++`. There is one exception:
the path with the added side can be refused, because it would copy a
meeting point (3.1). Then it finds nothing at all, with an error. The
added side may be one that is there already (4.2).

```ge
graph: 0 -a-> 1, 1 -c-> 9, 0 -b-> 2, 2 -d-> 9
$ ls (a | b)/(c & d)
a/c @9
$ ls (a | b | z)/(c & d)
a/c @9
```

```ge
graph: 0 -z-> 3, 3 -c-> 6, 3 -y-> 6, 6 -e-> 9, 3 -d-> 9, 0 -a-> 1, 0 -b-> 2
$ ls z/((c & y)/@/e & d)
z/c/e @9
$ ls ((a & b) | z)/((c & y)/@/e & d)
This path would copy a meeting point, which has no clear meaning, so the
    command was not run. In `(c & y)/@/e` several walks meet at the `@` or
    leave it, and `(a & b)` would copy it, once for each of its conjuncts: it
    stands next to the group `((c & y)/@/e & d)` with no join of their chain
    between the two. That is how the path reads for a route that takes the
    side `(a & b)` of `(a & b | z)`: a path is refused when one way to take
    the sides of its | unions is. If the walks should come together between
    the two groups, put a join between them (/@). Otherwise write the copies
    out, one conjunct for each pair. (CopiedMeetingPoint)
```

### 4.5 `*` is the `|` of all labels

In a query, a `*` step finds what the `|` union of every label in the
graph finds, in every context, because each copy of a `*` chooses its
edge on its own as each copy of a `|` chooses its side. It is not the
`++` of the labels. The law is one of queries: `mk` cannot create a `*`
edge ([commands.md](commands.md#41-what-a-step-does)).

```ge
graph: 0 -a-> 1, 1 -c-> 9, 0 -b-> 2, 2 -d-> 9
$ ls */(c & d)
a/c @9
$ ls (a | b)/(c & d)
a/c @9
$ ls (a ++ b)/(c & d)
no targets
```

### 4.6 The product identity

Two groups with only steps between them are their product:
`(A & B)/(C & D)` is `A/C & A/D & B/C & B/D`, and `(A & B)/s/(C & D)`
is `A/s/C & A/s/D & B/s/C & B/s/D`, anywhere in a path, for conjuncts
with joins and `|` unions in them. It is stated after the normal form
of 2.4, which matters in three ways:

- A conjunct that forks without a join of its own is multiplied out
  first, and `&` is flattened. `(x/@/w & b)/(c/(e & f) & d)` is
  `(x/@/w & b)/(c/e & c/f & d)`, six walks, with an `x` of its own for
  each of `c/e`, `c/f` and `d`. It is not
  `x/@/w/c/(e & f) & x/@/w/d & b/c/(e & f) & b/d`, in which `e` and `f`
  leave from one `x` target (2.5).
- Every `++` is chosen before, so the identity does not hold across a
  `++`: `(a ++ b)/(c & d)` chooses once, where
  `(a ++ b)/c & (a ++ b)/d` has two unions that choose apart.
- For a conjunct with a meeting point the left side is refused, and
  the identity says what the right side, which the error tells to
  write, means.

```ge
graph: 0 -a-> 1, 1 -c-> 9, 0 -a-> 3, 3 -d-> 9, 0 -b-> 2, 2 -c-> 9, 2 -d-> 9
$ ls (a & b)/(c & d)
a/c @9
$ ls a/c & a/d & b/c & b/d
a/c @9
```

```ge
graph: 0 -x-> 1, 1 -w-> 3, 3 -c-> 5, 5 -e-> 9, 3 -d-> 9, 0 -x-> 2, 2 -w-> 4, 4 -c-> 6, 6 -f-> 9, 0 -b-> 7, 7 -c-> 8, 8 -e-> 9, 8 -f-> 9, 7 -d-> 9
$ ls (x/@/w & b)/(c/(e & f) & d)
x/w/c/e @9
$ ls x/@/w/c/e & x/@/w/c/f & x/@/w/d & b/c/e & b/c/f & b/d
x/w/c/e @9
$ ls x/@/w/c/(e & f) & x/@/w/d & b/c/(e & f) & b/d
no targets
```

```ge
graph: 0 -a-> 1, 1 -c-> 9, 0 -b-> 2, 2 -d-> 9
$ ls (a ++ b)/(c & d)
no targets
$ ls (a ++ b)/c & (a ++ b)/d
a/c @9
```

**Across a `|` union.** With a `|` union between the two groups the
identity holds when no side of that union has a join of its own chain
or an `&`: `(A & B)/(x | y)/(C & D)` is
`A/(x | y)/C & A/(x | y)/D & B/(x | y)/C & B/(x | y)/D`, four unions
that choose apart, as the four copies of the one union do. (Not
proved: no counterexample is known, and searching found none.) It
fails in two ways.

With a join in a side: the copies of the one union that take the
joined side share its join (2.9), and four unions written apart share
nothing. In the first block below, on the left the walks through `a`
and through `b` both take `x/@` and must meet at one `x` target, and
on the right each of the four has an `x` target of its own. Neither
path is refused or warned about.

With an `&` in a side: a side that forks is a group of the stretch for
the walks that take it, and the normal form multiplies it out with the
groups around it. In the second block,
`(x/@ & b)/((c & d) | z)/(e & f)` copies `x/@` for `c` and for `d`,
which may then take an `x` target each (`1` and `2`, on the way to
`9`); written out as `x/@/((c & d) | z)/e & …`, the `x/@` stands
before the fork in its own chain and pins one `x` target for both, and
only `8` is found.

```ge
graph: 0 -a-> 1, 0 -b-> 2, 1 -x-> 3, 2 -x-> 4, 3 -c-> 9, 3 -d-> 9, 4 -c-> 9, 4 -d-> 9
$ ls (a & b)/(x/@ | y)/(c & d)
no targets
$ ls a/(x/@ | y)/c & a/(x/@ | y)/d & b/(x/@ | y)/c & b/(x/@ | y)/d
a/x/c @9
$ ls (a & b)/(x | y)/(c & d)
a/x/c @9
$ ls a/(x | y)/c & a/(x | y)/d & b/(x | y)/c & b/(x | y)/d
a/x/c @9
```

```ge
graph: 0 -x-> 1, 0 -x-> 2, 0 -b-> 3, 1 -c-> 4, 2 -d-> 4, 3 -c-> 4, 3 -d-> 4, 4 -e-> 9, 4 -f-> 9, 0 -x-> 5, 5 -c-> 6, 5 -d-> 6, 3 -c-> 6, 3 -d-> 6, 6 -e-> 8, 6 -f-> 8
$ ls (x/@ & b)/((c & d) | z)/(e & f)
x/c/e @9
x/c/e @8
Warning: the @ that ends `x/@` does nothing here: `(c & d)` copies `x/@`
    whole, once for each of its conjuncts, so the @ joins nothing and pins
    nothing. (That is with the side `(c & d)` of `(c & d | z)`.) An @ pins
    only in the chain it is written in, as in x/@/(c & d): leave this one out,
    or write out what it should join. (JoinDoesNothing)
$ ls x/@/((c & d) | z)/e & x/@/((c & d) | z)/f & b/((c & d) | z)/e & b/((c & d) | z)/f
x/c/e @8
```

### 4.7 Left distribution: holds without joins and `++`

`(A & B)/C = A/C & B/C` holds when `A` and `B` have no join of their own
chain, `C` has no join of the chain the fork is written in, and `C` has
no `++`: it is then the product identity (4.6). It fails in three ways,
each a case of a join or a shared choice that is one thing on the left
and two on the right:

- `A` has a join and `C` forks: on the left the join is copied with `A`
  and does nothing, on the right it pins. The same when the join is in
  the top chain of a union side of `A`.
- `C` has a join of its chain, at its start or later: on the left `A`
  and `B` meet at it, on the right each copy of the join is on one
  walk. The same when the join is in the top chain of a union side of
  `C`.
- `C` has a `++`: on the left it is chosen once, on the right each copy
  chooses on its own.

```ge
graph: 0 -x-> 1, 0 -x-> 2, 1 -c-> 9, 2 -d-> 9, 0 -b-> 3, 3 -c-> 9, 3 -d-> 9
$ ls (x/@ & b)/(c & d)
x/c @9
Warning: the @ that ends `x/@` does nothing here: `(c & d)` copies `x/@`
    whole, once for each of its conjuncts, so the @ joins nothing and pins
    nothing. If one node was meant, write `x/@/(c & d) & b/(c & d)`; otherwise
    leave the @ out. (JoinDoesNothing)
$ ls x/@/(c & d) & b/(c & d)
no targets
$ ls ((x/@ | zz) & b)/(c & d)
x/c @9
$ ls (x/@ | zz)/(c & d) & b/(c & d)
no targets
```

```ge
graph: 0 -a-> 1, 0 -b-> 2, 1 -c-> 9, 2 -c-> 9
$ ls (a & b)/@/c
no targets
$ ls a/@/c & b/@/c
a/c @9
$ ls (a & b)/c/@/d
no targets
$ ls a/c/@/d & b/c/@/d
no targets
```

```ge
graph: 0 -a-> 1, 0 -b-> 2, 1 -c-> 8, 8 -e-> 9, 2 -c-> 7, 7 -e-> 9
$ ls (a & b)/(c/@/e | d)
no targets
$ ls a/(c/@/e | d) & b/(c/@/e | d)
a/c/e @9
```

```ge
graph: 0 -a-> 1, 0 -b-> 2, 1 -c-> 9, 2 -d-> 9
$ ls (a & b)/(c ++ d)
no targets
$ ls a/(c ++ d) & b/(c ++ d)
a/c @9
```

### 4.8 Right distribution: holds for a prefix without joins

`R/(P & Q) = R/P & R/Q` holds when `R` has no join of its own chain
anywhere and no `++`, and, if `R` has an `&` anywhere in it (also in a
side of a union), `P` and `Q` have no join of their own chain either:
`R` is then copied into both walks (2.4). It fails in four ways:

- `R` ends in `@`, `@N` or `!{…}`: `x/@/(c & d)` pins one `x` for
  both, and `x/@/c & x/@/d` has two `x`, each with a join that does
  nothing.
- `R` has a join earlier in its chain: after `x/@` everything leaves
  from one `x` target (2.5), so `x/@/w/(c & d)` has one `x` and
  `x/@/w/c & x/@/w/d` two. The same when the join is in the top chain
  of a union side of `R`: `(x/@ | zz)/(c & d)` against
  `(x/@ | zz)/c & (x/@ | zz)/d`.
- `R` has a `++`: `(a ++ b)/(c & d)` chooses once (4.6).
- `R` has an `&` and `P` has a join of its own chain: on the left the
  product rule copies `P` whole, with its join, for each conjunct of
  `R`; on the right all the walks of `R` arrive at the one join of `P`.
  `(p & q)/(x/@/w & y)` against `(p & q)/x/@/w & (p & q)/y`, and
  `(a & b)/(@/c & d)` against `(a & b)/@/c & (a & b)/d` (where the
  warning says the copied `@` does nothing). It is the same when the
  `&` of `R` is in a side of a union: `((a & b) | z)/(x/@/w & y)`
  against `((a & b) | z)/x/@/w & ((a & b) | z)/y`.

```ge
graph: 0 -a-> 1, 1 -b-> 9, 0 -a-> 2, 2 -c-> 9
$ ls a/(b & c)
a/b @9
$ ls a/b & a/c
a/b @9
$ ls a/@/(b & c)
no targets
$ ls a/@/b & a/@/c
a/b @9
```

```ge
graph: 0 -x-> 1, 1 -w-> 2, 2 -c-> 9, 0 -x-> 3, 3 -w-> 4, 4 -d-> 9
$ ls x/@/w/(c & d)
no targets
$ ls x/@/w/c & x/@/w/d
x/w/c @9
$ ls x/w/(c & d)
x/w/c @9
$ ls x/w/c & x/w/d
x/w/c @9
```

```ge
graph: 0 -x-> 1, 1 -c-> 9, 0 -x-> 2, 2 -d-> 9
$ ls (x/@ | zz)/(c & d)
no targets
$ ls (x/@ | zz)/c & (x/@ | zz)/d
x/c @9
```

```ge
graph: 0 -p-> 1, 0 -q-> 2, 1 -x-> 3, 3 -w-> 9, 2 -x-> 4, 4 -w-> 9, 1 -y-> 9, 2 -y-> 9
$ ls (p & q)/(x/@/w & y)
p/x/w @9
$ ls (p & q)/x/@/w & (p & q)/y
no targets
$ ls (p & q)/(x/w & y)
p/x/w @9
$ ls (p & q)/x/w & (p & q)/y
p/x/w @9
```

```ge
graph: 0 -a-> 1, 1 -c-> 9, 0 -b-> 2, 2 -c-> 9, 1 -d-> 9, 2 -d-> 9
$ ls (a & b)/(@/c & d)
a/c @9
Warning: the @ that begins `@/c` does nothing here: `(a & b)` copies `@/c`
    whole, once for each of its conjuncts, so the @ joins nothing and pins
    nothing. If one node was meant, write `(a & b)/@/c & (a & b)/d`; otherwise
    leave the @ out. (JoinDoesNothing)
$ ls (a & b)/@/c & (a & b)/d
no targets
```

```ge
graph: 0 -a-> 1, 0 -b-> 2, 1 -x-> 3, 2 -x-> 4, 3 -w-> 9, 4 -w-> 9, 1 -y-> 9, 2 -y-> 9
$ ls ((a & b) | z)/(x/@/w & y)
a/x/w @9
$ ls ((a & b) | z)/x/@/w & ((a & b) | z)/y
no targets
```

### 4.9 Unions and sequencing

A `++` union distributes over `/` both ways, with no condition:
`p/(q ++ r)` is `p/q ++ p/r`, and `(p ++ q)/r` is `p/r ++ q/r`, since
either way one side is chosen for the whole path.

```ge
graph: 0 -a-> 1, 0 -b-> 2, 1 -c-> 9, 2 -c-> 9
$ ls (a & b)/(c ++ d)
a/c @9
$ ls (a & b)/c ++ (a & b)/d
a/c @9
```

```ge
graph: 0 -a-> 1, 1 -c-> 9, 1 -d-> 9
$ ls (a ++ b)/(c & d)
a/c @9
$ ls a/(c & d) ++ b/(c & d)
a/c @9
```

A `|` union distributes over `/` only when nothing copies it.
`p/(q | r) = p/q | p/r` fails when `p` has several walk ends (each
copy of the union chooses on its own on the left, once for all on the
right), and `(p | q)/r = p/r | q/r` fails when `r` forks.

A union that nothing copies (2.9: no fork in its stretch, and no other
fork in the stretch of a group or union it is written inside) finds the
same
whether it is written `|`, `++` or `+`. (Not proved: no counterexample
is known, and searching found none.)
A union in a conjunct that a product copies is copied, though no fork
stands in its own chain: `((x | y) & z)/(c & d)` finds `9` in the
second block below, through `x/c & y/d`, and with `++` it does not.

```ge
graph: 0 -a-> 1, 0 -b-> 2, 1 -c-> 9, 2 -d-> 9
$ ls (a & b)/(c | d)
a/c @9
$ ls (a & b)/c | (a & b)/d
no targets
$ ls (a | b)/(c & d)
a/c @9
$ ls a/(c & d) | b/(c & d)
no targets
$ ls (a | b)/c
a/c @9
$ ls (a ++ b)/c
a/c @9
$ ls (a + b)/c
a/c @9
```

```ge
graph: 0 -x-> 1, 0 -y-> 2, 1 -c-> 9, 2 -d-> 9, 0 -z-> 3, 3 -c-> 9, 3 -d-> 9
$ ls ((x | y) & z)/(c & d)
x/c @9
$ ls ((x ++ y) & z)/(c & d)
no targets
```

`a | b` finds what `regex:"^(a|b)$"` finds, in every context (4.5):

```ge
graph: 0 -a-> 1, 0 -b-> 2, 1 -c-> 9, 2 -d-> 9
$ ls (a | b)/(c & d)
a/c @9
$ ls regex:"^(a|b)$"/(c & d)
a/c @9
```

### 4.10 A conjunct written twice

Writing a conjunct twice changes nothing when both paths are *sealed*:
with `@/` in front and `/@` behind, so that nothing outside can copy
into the path's joins and its joins hold nothing outside it together.
For any relative `q` (2.10), `@/(q & q)/@` and `@/q/@` find the same
as parts of any query, wherever both are run (one of them may be
refused where the other is not, below; and this says nothing about
`mk`). A whole query is sealed already (2.10), so `ls (q & q)` finds
what `ls q` finds for every `q`.

Without the seal the two can differ when `q` has a join of its own
chain (an `@` or `!{…}` there, or in the top chain of a side of a
union there) and a group stands next to it on either side:

- A group before it. `P/q` makes the walks of `P` meet at the join of
  `q`, and `P/(q & q)` does not, since each copy of `q` is on one walk.
  As whole queries, `ls P/(q & q)` finds what `ls P/q` finds when `q`
  has no such join, or when `P` ends in a join (so that all its walks
  are at one node when `q` begins); inside a longer path only the
  sealed form is safe, since what follows can fork from `q`'s join.
- A group after it. `(q & q)/S` copies `q` whole for each conjunct of
  `S`, and `q/S` lets all of `S` leave from `q`'s join.

```ge
graph: 0 -a-> 1, 1 -c-> 9, 0 -b-> 2, 2 -c-> 9
$ ls (a & b)/@/c
no targets
$ ls (a & b)/(@/c & @/c)
a/c @9
Warning: the @ that begins `@/c` does nothing here: `(a & b)` copies `@/c`
    whole, once for each of its conjuncts, so the @ joins nothing and pins
    nothing. If one node was meant, write `(a & b)/@/c & (a & b)/@/c`;
    otherwise leave the @ out. The same holds for one more @ in this path.
    (JoinDoesNothing)
$ ls (a & b)/@/@/c/@
no targets
$ ls (a & b)/@/(@/c & @/c)/@
no targets
```

```ge
graph: 0 -x-> 5, 5 -a-> 1, 1 -b-> 9, 5 -a-> 2, 2 -c-> 9
$ ls x/@/(a/@ & a/@)
x/a @1
x/a @2
$ ls x/@/a/@
x/a @1
x/a @2
$ ls x/@/(a/@ & a/@)/(b & c)
x/a/b @9
Warning: the @ that ends `a/@` does nothing here: `(b & c)` copies `a/@`
    whole, once for each of its conjuncts, so the @ joins nothing and pins
    nothing. If one node was meant, write `a/@/(b & c) & a/@/(b & c)`;
    otherwise leave the @ out. The same holds for one more @ in this path.
    (JoinDoesNothing)
$ ls x/@/a/@/(b & c)
no targets
$ ls @/x/@/(a/@ & a/@)/@/(b & c)
no targets
$ ls @/x/@/a/@/@/(b & c)
no targets
```

```ge
graph: 0 -a-> 1, 1 -b-> 9, 0 -a-> 2, 2 -c-> 9
$ ls (a/@ & a/@)/(b & c)
a/b @9
Warning: the @ that ends `a/@` does nothing here: `(b & c)` copies `a/@`
    whole, once for each of its conjuncts, so the @ joins nothing and pins
    nothing. If one node was meant, write `a/@/(b & c) & a/@/(b & c)`;
    otherwise leave the @ out. The same holds for one more @ in this path.
    (JoinDoesNothing)
$ ls a/@/(b & c)
no targets
$ ls @/(a/@ & a/@)/@/(b & c)
no targets
$ ls @/a/@/@/(b & c)
no targets
$ ls a/@/(b & c) & a/@/(b & c)
no targets
```

Where one of the two paths is refused and the other run there is
nothing to compare: in a conjunct that another group copies,
`@/(q & q)/@` has a meeting point and `@/q/@`, for a `q` that is one
walk, has not.

### 4.11 `mk` makes the path hold

After `mk p` succeeds, `p` holds from the node it ran at: `mk p` run
again at that node changes nothing, and a query of `p` finds a node
when the walks of `p` end at one node. This holds for every path `mk`
accepts: a path with `%never` in a walk cannot be constructed, and a
path that writes a node `@N` that does not exist is an error, since
`mk` creates no node with a written id
([commands.md](commands.md#4-mk)). What `mk` creates does not depend on
the order in which the conjuncts of a group or the sides of a union
are written.

```ge
graph: 0
$ mk a/b; ls a/b; mk a/b
created 2 new nodes and 2 new edges
a/b @Pqf7kGOdKJIr
created 0 new nodes and 0 new edges
+ 0 -a-> O8OhMSVtjevt
+ O8OhMSVtjevt -b-> Pqf7kGOdKJIr
$ mk x/@/(c & d); mk x/@/(c & d)
created 3 new nodes and 3 new edges
created 0 new nodes and 0 new edges
+ 0 -x-> O8OhMSVtjevt
+ O8OhMSVtjevt -c-> Pqf7kGOdKJIr
+ O8OhMSVtjevt -d-> DQ1KCJgauHiV
```

```ge
graph: 0, 2
$ mk x/~x/a & a/@2
created 1 new node and 2 new edges
+ 0 -a-> 2
+ 0 -x-> O8OhMSVtjevt
$ mk a/@2 & x/~x/a
created 1 new node and 2 new edges
+ 0 -a-> 2
+ 0 -x-> O8OhMSVtjevt
$ mk a/@7
This path needs @7, which does not exist, and a node with a written id is
    never created: mk gives every new node a new id. Nothing was changed.
    (MissingNodes)
```

### 4.12 Reversal

For a relative path `P` (2.10) with no `@N` anywhere in it, nor
anything that becomes one (`#t`, a `%targets(…)`, `%history(n)`,
`%last`, a URL): node `t` is
among the targets of `ls P` run at `c` exactly when `c` is among the
targets of `ls ~P` run at `t`. It is a statement about whole queries,
which are sealed at both ends, with `@/` in front and `/@` behind
(2.10): every walk starts at the current node, as if `@/` were written
first, and the query ends in `/@`; reversing the path swaps the two.

```ge
graph: 0 -p-> 1, 0 -q-> 2, 1 -x-> 3, 3 -w-> 9, 2 -x-> 4, 4 -w-> 9, 1 -y-> 9, 2 -y-> 9
$ ls (p & q)/(x/@/w & y)
p/x/w @9
$ at @9 ls (~w/@/~x & ~y)/(~p & ~q)
w/x/p @0
```

It does not hold with an `@N`, which is a root one way and a test the
other (2.2):

```ge
graph: 0 -a-> 1
$ ls @1
@1
$ at @1 ls ~@1
@1
```

And it is not a statement about pieces of a path with a leading `@`
left out: the reverse of `(a & b)/@`, a fork that then joins, is
`@/(~a & ~b)`, a join that then forks, and it is the converse only
because that `@` is kept. `((a & b)/@ & c)/q` finds nothing below (`a`
and `b` end apart), and so does its reverse from `9`; with the `@`
dropped, the reverse finds `0`:

```ge
graph: 0 -a-> 1, 0 -b-> 3, 0 -c-> 2, 1 -q-> 9, 2 -q-> 9, 3 -q-> 9
$ ls ((a & b)/@ & c)/q
no targets
$ at @9 ls ~q/(@/(~a & ~b) & ~c)
no targets
$ at @9 ls ~q/(~a & ~b & ~c)
q/a @0
```

A path is refused (3.1) exactly when its reverse is, so the law loses
no path on one side only.

## 5. Known rough edges, and what may change

Each item says what the build does, and where the designer has not
decided, that.

- **A side written twice.** Every written side is a side of its own
  (2.9), which has consequences that may surprise: writing a side again
  can turn a path that runs into one that is refused (3.1); `mk` with a
  `|` lists more ways of creating the path for it; and `p | p` can compile to more than
  `p` does even where the two find the same (4.2).
- **The refusal sometimes refuses a path in which no meeting point
  would be copied.** Each of these is something the designer may relax;
  none of them lets a path run that should be refused.
  - A leading `@` of a forking conjunct whose group starts its chain,
    as in `(@/(a & b) & c)/(d & e)`: every copy of that `@` is the node
    the chain starts at.
  - An `@` right next to an `@N` that is reached through a union side,
    as in `(p & q)/((m & n)/(x/@1 | y/@2)/@ & d)`: every copy of the
    `@` is node `1` or node `2`. (With a step between, as in
    `(p & q)/((m & n)/(x/@1/w | y/@2/v)/@ & d)`, the walks through `m`
    and `n` may take a side each and then meet at the `@`, so that path
    is rightly refused; the check does not tell the two apart.)
  - A side that can never be taken, because its node tests contradict
    something outside it, still counts as a side:
    `!{@1}/(@1/(a & b) | z)/((c & y)/@/e & d)` is refused for the side
    `@1/(a & b)`, which the `!{@1}` before the union rules out. (Tests
    that contradict each other where they are written, as in
    `@1/@2/(a & b)`, do make the side no side.)
  - A `|` union of exactly two sides, both with a join, between a
    conjunct that ends with an `@` and a group with a conjunct whose
    walks an `@N` holds together:
    `(p/@ & q)/(x/@ | w/@)/((c & y)/@1/e & f)` is refused, though in
    nothing it compiles to is `p/@` copied with a fork at one copy.
    With a third joined side, `(x/@ | w/@ | v/@)`, the copy is real,
    so the refusal cannot simply be dropped for such unions.

  ```ge
  graph: 0 -a-> 1, 0 -b-> 2, 1 -c-> 4, 1 -y-> 4, 2 -c-> 5, 2 -y-> 5, 4 -e-> 9, 5 -e-> 9, 1 -d-> 9, 2 -d-> 9, 0 -z-> 3, 3 -c-> 6, 3 -y-> 6, 6 -e-> 9, 3 -d-> 9
  $ ls !{@1}/(@1/(a & b) | z)/((c & y)/@/e & d)
  This path would copy a meeting point, which has no clear meaning, so the
      command was not run. In `(c & y)/@/e` several walks meet at the `@` or
      leave it, and `(a & b)` would copy it, once for each of its conjuncts:
      it stands next to the group `((c & y)/@/e & d)` with no join of their
      chain between the two. That is how the path reads for a route that takes
      the side `@1/(a & b)` of `(@1/(a & b) | z)`: a path is refused when one
      way to take the sides of its | unions is. If the walks should come
      together between the two groups, put a join between them (/@). Otherwise
      write the copies out, one conjunct for each pair. (CopiedMeetingPoint)
  $ ls !{@1}/z/((c & y)/@/e & d)
  z/c/e @9
  $ ls (@1/@2/(a & b) | z)/((c & y)/@/e & d)
  z/c/e @9
  $ ls (p & q)/((m & n)/(x/@1 | y/@2)/@ & d)
  This path would copy a meeting point, which has no clear meaning, so the
      command was not run. In `(m & n)/(x/@1 | y/@2)/@` several walks meet at
      the `@` or leave it, and `(p & q)` would copy it, once for each of its
      conjuncts: it stands next to the group `((m & n)/(x/@1 | y/@2)/@ & d)`
      with no join of their chain between the two. If the walks should come
      together between the two groups, put a join between them: `(p & q)/@/((m
      & n)/(x/@1 | y/@2)/@ & d)`. Otherwise write the copies out, one conjunct
      for each pair. (CopiedMeetingPoint)
  $ ls (p/@ & q)/(x/@ | w/@)/((c & y)/@1/e & f)
  This path would copy a meeting point, which has no clear meaning, so the
      command was not run. `((c & y)/@1/e & f)` would copy `p/@`, once for
      each of its conjuncts: it stands next to the group `(p/@ & q)` with no
      join of their chain between the two. In the copy for `(c & y)/@1/e`,
      which holds several walks together, those walks all leave from the `@`
      of `p/@`, and in the other copies they do not. If the walks should come
      together between the two groups, put a join between them: `(p/@ &
      q)/@/(x/@ | w/@)/((c & y)/@1/e & f)`. Otherwise write the copies out,
      one conjunct for each pair. (CopiedMeetingPoint)
  ```

- **What the refusal and the warning say to write is a part of the
  path.** Both spell the stretch from the one group to the other and
  leave out what is written around the two, without saying so. The
  join that the refusal offers for `z/(a & b)/((c & y)/@/e & d)/q` is
  `(a & b)/@/((c & y)/@/e & d)`, and what the warning says to write
  for `(x/@ & b)/(c & d)/q` is `x/@/(c & d) & b/(c & d)`: typed as
  they stand, each is another path than the one meant. And since a
  command line prints each warning text once, two different paths with
  the same two groups warn once (3.2).

  ```ge
  graph: 0 -x-> 1, 0 -x-> 2, 1 -c-> 9, 2 -d-> 9, 0 -b-> 3, 3 -c-> 9, 3 -d-> 9, 9 -q-> 8
  $ ls (x/@ & b)/(c & d)/q
  x/c/q @8
  Warning: the @ that ends `x/@` does nothing here: `(c & d)` copies `x/@`
      whole, once for each of its conjuncts, so the @ joins nothing and pins
      nothing. If one node was meant, write `x/@/(c & d) & b/(c & d)`;
      otherwise leave the @ out. (JoinDoesNothing)
  $ ls z/(a & b)/((c & y)/@/e & d)/q
  This path would copy a meeting point, which has no clear meaning, so the
      command was not run. In `(c & y)/@/e` several walks meet at the `@` or
      leave it, and `(a & b)` would copy it, once for each of its conjuncts:
      it stands next to the group `((c & y)/@/e & d)` with no join of their
      chain between the two. If the walks should come together between the two
      groups, put a join between them: `(a & b)/@/((c & y)/@/e & d)`.
      Otherwise write the copies out, one conjunct for each pair: `a/(c &
      y)/@/e & a/d & b/(c & y)/@/e & b/d`. (CopiedMeetingPoint)
  ```

- **Few of the `@` that do nothing are warned about.** The warning is
  given only where it is certain that the `@` can be left out (3.2). In
  particular one `|` union with a joined side anywhere in the path, or
  a `%targets(…)` with two targets or more, silences it for the whole
  path, so whether a path warns can depend on the graph. Warning about
  every join that does nothing is a separate piece of work.
- **`@N` joins in forking conjuncts are never refused**
  (`(p & q)/(x/@3/(w & y) & y)` runs). With a `|` beside such a
  conjunct, as in `((a & b) | z)/((c & y)/@1/e & d)`, the path runs and
  has an alternative in which the `e` after `@1` is one step for the
  copies made for `a` and for `b`. Whether such a path can find
  something that the copies written apart do not find is not known.
- **`%targets(…)` is read as a step between groups** when it has two
  or more targets, so whether
  `(a & b)/%targets(t)/((c & y)/@/e & d)` is refused depends on how
  many nodes `t` reaches when the command runs:

  ```ge
  graph: 0 -a-> 1, 0 -b-> 2, 1 -c-> 4, 1 -y-> 4, 2 -c-> 5, 2 -y-> 5, 4 -e-> 9, 5 -e-> 9, 1 -d-> 9, 2 -d-> 9, 0 -t-> 1, 0 -t-> 2, 0 -z-> 3
  $ ls (a & b)/%targets(z)/((c & y)/@/e & d)
  no targets
  $ ls (a & b)/%targets(t)/((c & y)/@/e & d)
  This path would copy a meeting point, which has no clear meaning, so the
      command was not run. In `(c & y)/@/e` several walks meet at the `@` or
      leave it, and `(a & b)` would copy it, once for each of its conjuncts:
      it stands next to the group `((c & y)/@/e & d)` with no join of their
      chain between the two. Routes that take different sides of `(@1 | @2)`
      (written with +, which is | in this command) pass it as one step, so it
      keeps the two groups apart for none of them; write ++ there to choose
      one side for the whole path. If the walks should come together between
      the two groups, put a join between them (/@). Otherwise write the copies
      out, one conjunct for each pair. For that +, write ++ instead, or wrap
      the path in %override({plus = shared}, …). (CopiedMeetingPoint)
  ```

- **A `cd` inside an `at` block** changes the history but not the node
  the following commands of the block run at
  ([commands.md](commands.md#24-at)); not decided whether intended.
- **Nothing bounds what a path costs** (3.4). A path that compiles to
  very much, or whose steps match very many edges, runs until it is
  done or stopped with Ctrl-C;
  [path-cost-limits.md](path-cost-limits.md) has what is known.
- The parser's messages for a regex that does not compile and for an
  `@` followed by more than twelve characters are unhelpful (1.1).
- The rough edges of the commands are in
  [commands.md](commands.md#8-known-rough-edges).
