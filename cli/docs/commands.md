# Commands and paths

This document says what each command does with the path it is given:
which commands take paths, how each reads a `+` union, what each does
with no match, one match and many, which edge the editing commands act
on, how `at` runs a command elsewhere, what `mk` creates and how it
chooses among several ways to do so, the settings a path can carry,
what happens when a command fails, and what commands do next to a
damaged graph. The language itself (operators, joins, unions, refusals,
laws) is in [path-language.md](path-language.md); the compiler and the
engine are in [path-route-ir.md](path-route-ir.md).

Examples follow the conventions of
[path-language.md](path-language.md): node `0` is the node the command
runs at, nodes with small NIDs are shown as `@2`, created nodes show
their fresh NID in full, output is shown without colours and without
the timestamp column of `ls`, and lines beginning with `+` or `-` list
the edges the command created or deleted. **Every `$` line runs on a
fresh copy of the block's graph**; where an example depends on an
earlier command, the commands are on one `$` line with `;` between
them. A `$` line is typed at the prompt of the REPL (`ge GRAPH`); one
written `$ ge -e '…'` is the command line given to `ge GRAPH -e '…'`
instead, which runs it and exits, and `[exit code 1]` after its output
is the exit code when it is not 0. In a few places `↵` separates two
lines typed one after the other in the same REPL session.

A few words of [path-language.md](path-language.md) are used
throughout. A path multiplies out to a list of *walks*, each a sequence
of steps from a start node: `(a & b)/c` is the two walks `a/c` and
`b/c`. `p & q` is a *fork*, and `p` and `q` are its *conjuncts*; the
*first walk* is the one through the first conjunct of every fork. A
*join* is an `@`, `@N` or `!{…}` written after a step: the walks that
come to it must be at one node there. A step, join or union that is
written once can stand in several walks, and in each it is a *copy*:
the `c` above has two. A union (`|`,
`++` or `+`) offers *sides*, and a walk through it takes one. Each way
for the copies of the `|` unions of a path to take their sides is an
*alternative* of the path, and a query finds what any alternative
finds. A *meeting point* is an `@` or `!{…}` at which several walks
meet or from which several leave; a path in which a product would copy
one is *refused*
([path-language.md](path-language.md#31-a-product-may-not-copy-a-meeting-point)).

## 1. Which commands take paths

Every path is parsed before anything on the command line runs, so a
parse error anywhere runs nothing. When a command runs, each of its
paths is expanded (its directives `%targets(…)`, `%last`,
`%history(n)`, URLs and `%override(…)`, relative to the node and the
history at that moment; a URL is imported then, 2.7), its `+` unions
are read as the table says, and the path is checked
([path-language.md](path-language.md#3-refused-and-warned-paths))
before the command reads or edits the graph for it. Earlier commands of
a `;` line, and earlier targets of an `at`, have run by then and are
not undone (section 6).

| command | path arguments | `+` is | what the path is for |
|---|---|---|---|
| `ls [p]`, `preview [p]` | `p`, default `*` | `\|` | lists the targets (2.1) |
| `cd p` | `p` | `\|` | moves to the one target (2.2) |
| `mk p`, `mk --allow-orphans p` | `p` | `++` | creates what is missing for the path to hold (4) |
| `rm p` | `p` | `\|` | deletes the last edge of the first walk of each match (3) |
| `rmnf p` | `p` | `\|` | deletes the target nodes (2.3) |
| `mv p q`, `cp p q` | `p` an edge path, `q` one node | `\|` | moves or copies the selected edge under `q` (3) |
| `rn [--all-parents] p q`, `cl`/`clone`/`alias [--all-parents] p q` | `p` an edge path, `q` a destination | `\|` | renames or clones the selected edge (3) |
| `tag p q`, `t p q` | `p` constructed, `q` one node | `++` in `p`, `\|` in `q` | links every end of `p`'s first match to `q`'s target with an empty label (4.6) |
| `at p cmd` | `p`; `cmd` has paths of its own | `\|` | runs `cmd` at each target (2.4) |
| `mg [--keep] p1 [--keep] p2 …` | each one node | `\|` | merges the nodes (2.5) |
| `%targets(p)` in any path | `p`, a query of its own | `\|`, whatever surrounds it, unless an `%override({plus = …}, …)` is written inside `p` | the nodes `p` reaches, as a union (2.6) |

`dd l` (`dedup`) and `text l …` (`mkt`) take a label, not a path, and
`desc …` (`describe`) takes free text. The other commands (`nid`, `d`,
`back n`, `vi`, `si` (`show-image`), `fsck`, `fix` and
`fix --dry-run`, `wget URL` (`:import-url`, `:iurl`), `:i dir`
(`:import`), `:d` (`:debug`)) take no path.

Commands can be sequenced with `;` and grouped with `{ … }`, with one
exception: `text`, `desc` and `:i` take the rest of the line, as the
text or the directory, so a `;` or a `}` after them is part of it.
`desc hello; nid` describes the node as "hello; nid" and runs no
`nid`, and none of the three can stand inside braces or before a `;`:

```ge
graph: 0 -a-> 1
$ nid; desc hello
@0
+ 0 -description-> O8OhMSVtjevt
$ desc hello; nid
+ 0 -description-> O8OhMSVtjevt
$ at a {desc foo}
<interactive>:1:16:
  |
1 | at a {desc foo}
  |                ^
unexpected end of input
expecting "++", '&', '+', '/', '|', or '}'
```

TAB completion reads the path before the cursor too. It offers nothing
for a path with a directive in it, nothing for a path that is refused,
and nothing when the candidates are not ready in about
half a second, which includes a node file that cannot be read yet
(another program is writing it, or it is not downloaded). The prompt is
usable again after that half second, and Ctrl-C during it gives a new
prompt at once. Completion prints no warning.

## 2. Queries

A query appends `/@` to its path, so that every walk of the path ends
at one node, and finds the nodes such a walk can end at
([path-language.md](path-language.md#210-what-a-query-adds)). A query
never changes the graph by itself; `rm`, `rmnf`, `mv`, `cp`, `rn`, `cl`
and `mg` are queries that then edit what they found. What each command
does when its path reaches no node, one, or several:

| command | no match | one | many |
|---|---|---|---|
| `ls`, `preview` | prints `no targets` | one line per way the first walk went (2.1) | one line per way |
| `cd` | error, "resolved to: []" | moves there | error naming the nodes |
| `rmnf` | nothing, silently | deletes the node | deletes every node |
| `at` | nothing, silently | runs `cmd` there | runs `cmd` at each in turn |
| `mg` | error for that argument | that node is merged | error for that argument |
| `tag p q` (`q`) | error | links to it | error naming the nodes |
| `mv p q`, `cp p q` (`q`) | error, "resolved to: []" (checked before `p`) | the edges go there | error naming the nodes |
| `rm` | changes nothing, with a notice (3) | deletes the selected edge | deletes the selected edge of every match |
| `mv`, `cp` (`p`) | changes nothing, with a notice (3) | moves or copies the edge | every match's edge |
| `rn`, `cl` | error, "found 0" | renames or clones the edge | error, "found N" (counted as distinct first walks, section 3) |
| `mk` | the path holds nowhere: creates it | the path holds: nothing | likewise |

`ls` names the nodes without a node file that it comes to while it
resolves its path, before its result ("nonexistent nodes: @7"). That
is not every such node that is written: `ls (@7/@8 | a)` names
neither, since a side whose tests contradict each other is no side.
The other queries say nothing about such a node: a path through it
matches nothing, with what that means for the command (`at @7 nid`
runs nothing, `cd @7` fails as any `cd` without a target does). `tag`
constructs its first path (4.6), and `tag @7 a` names the node in its
refusal to create nodes that are not connected to the graph.

```ge
graph: 0 -a-> 1, 1 -b-> 2, 0 -c-> 3, 0 -c-> 4
$ ls zz
no targets
$ ls @7
nonexistent nodes: @7
no targets
$ ls (@7/@8 | a)
a @1
$ cd zz
cd needs a path that resolves to a single node
but it resolved to: []
$ cd @7
cd needs a path that resolves to a single node
but it resolved to: []
$ rmnf zz
$ at zz nid
$ at @7 nid
$ rm zz
the path of rm ("zz") matched nothing; nothing was changed.
$ mv zz a
the first argument of mv ("zz") matched nothing; nothing was changed.
$ mv zz qq
the last argument of mv needs a path that resolves to a single node
but it resolved to: []
$ rn zz z
rn needs one selected source route, but found 0.
$ rn c z
rn needs one selected source route, but found 2.
$ tag #t zz
the last argument of tag needs a path that resolves to a single node
but it resolved to: []
$ tag #t c
the last argument of tag needs a path that resolves to a single node
but it resolved to: [@3,@4]
$ mg zz a
argument 1 of mg ("zz") needs a path that resolves to a single node
but it resolved to: []
```

### 2.1 `ls` and `preview`

`ls p` prints one line for each distinct way the *first walk* of the
path was walked: the labels of that walk and the node it ended at,
followed by the node's tags and timestamp. The first walk is the first
conjunct's, at every level (the first route; section 3 says why it
matters). Without a path, `ls` lists every edge out of the current
node.

```ge
graph: 0 -a-> 1, 1 -b-> 2, 0 -c-> 3, 0 -c-> 4, 2 -d-> 5
$ ls
a @1
c @3
c @4
$ ls a/b
a/b @2
$ ls c
c @3
c @4
$ ls a/b/d & c
no targets
$ ls ~a
no targets
$ at @2 ls ~b/~a
b/a @0
```

Two lines can show the same labels and target when the walk went
through different nodes on the way; and a walk that is not the first
is not shown, however it went. With a union, the first walk of every
alternative is listed.

```ge
graph: 0 -a-> 1, 0 -a-> 2, 1 -c-> 9, 2 -c-> 9, 0 -b-> 9
$ ls a/c
a/c @9
a/c @9
$ ls a/c & b
a/c @9
a/c @9
$ ls b & a/c
b @9
```

### 2.2 `cd`

`cd p` needs exactly one target and moves there; otherwise it is an
error naming the targets (none, or several), and the location does not
change. `nid` prints the current node.

```ge
graph: 0 -a-> 1, 1 -b-> 2, 0 -c-> 3, 0 -c-> 4
$ cd a; nid
@1
$ cd c
cd needs a path that resolves to a single node
but it resolved to: [@3,@4]
$ cd a/b; ls ~b
b @1
```

### 2.3 `rmnf`

`rmnf p` deletes every target node, with all its edges; with no target
it does nothing and prints nothing.

```ge
graph: 0 -a-> 1, 1 -b-> 2, 0 -c-> 3, 0 -c-> 4
$ rmnf a
- 0 -a-> 1
- 1 -b-> 2
$ rmnf c
- 0 -c-> 3
- 0 -c-> 4
```

A system node (the origin, `tags`, …) is never deleted. If one is among
the targets the command fails and deletes nothing, not the other
targets either:

```ge
graph: 0 -a-> 1
$ rmnf @
rmnf cannot delete a system node (the origin and the special nodes such as
    tags must stay in the graph)
the path of rmnf (@) resolves to @0
$ rmnf (a | @)
rmnf cannot delete a system node (the origin and the special nodes such as
    tags must stay in the graph)
the path of rmnf ("a" | @) resolves to @0
```

### 2.4 `at`

`at p cmd` expands `p` once, then runs `cmd` at each target in turn
(none: nothing runs, silently). For every command of `cmd` the current
node is the target, so their paths are relative to it and their
directives are expanded there (`at * mk z/%targets(@)` adds a `z` loop
on every child). `at` adds no entry to the history: inside `cmd`,
`%history(0)` is the node `at` was run from and `%last` the node
before that, as if the `at` were not there. A `cd` inside `cmd`
changes the history (`%history(0)` then names the node it moved to)
but not the node the following commands of `cmd` run at: in
`at a {cd b; nid}` the `nid` prints the `at` target, and
`at a {cd b; mk q}` creates `q` under it. Whether that is intended has
not been decided (section 8). Afterwards `at` restores the history, so
the location is what it was before. `at` stops at the first target
where `cmd` fails (what it did at the targets before stays, and a note
says at which, section 6);
an `rm`, `mv` or `cp` whose path matches nothing at a target is no
failure, and its notice names the target (3). A warning about a path
([path-language.md](path-language.md#32-a-join-that-does-nothing-warning))
is given once, not once per target. `at p c1; c2` is `at p c1` and
then `c2`, once, at the original node; write `at p {c1; c2}` to run
both at each target.

```ge
graph: 0 -a-> 1, 1 -b-> 2, 0 -c-> 3, 0 -c-> 4
$ at c nid
@3
@4
$ at * ls
b @2
no targets
no targets
$ at (a + c) nid
@1
@3
@4
$ at (a + c) {mk z/%targets(@)}
created 0 new nodes and 1 new edge
created 0 new nodes and 1 new edge
created 0 new nodes and 1 new edge
+ 1 -z-> 1
+ 3 -z-> 3
+ 4 -z-> 4
$ at a ls; nid
b @2
@0
$ cd a; at b ls %last
@0
$ cd a; at b ls %history(0)
@1
$ at a {cd b; nid}
@1
$ at a {cd b; ls %history(0)}
@2
$ at a {cd b; mk q}
created 1 new node and 1 new edge
+ 1 -q-> O8OhMSVtjevt
$ at a {cd b}; nid
@0
```

### 2.5 `mg`

`mg p1 p2 …` merges nodes, one per argument; it takes two arguments or
more (one is a parse error), each of which must reach exactly one node.
The nodes' edges are moved onto the one that survives (edges between
merged nodes become loops), and the location is then the survivor.
Which node survives:

- A system node (the origin, `tags`, …) is never merged away: if one
  of the nodes is one, it survives whatever the order of the arguments;
  two system nodes, or `--keep` on another node, is an error that
  changes nothing.
- Otherwise `--keep` before one argument (at most one) marks the
  survivor, which keeps its NID and its data; the other nodes' data is
  discarded with a warning.
- Otherwise the first argument's node survives. At most one of the
  nodes may then have data, and the survivor takes it, whichever node
  it came from; with data on several, nothing changes and the error
  suggests a `--keep`.

```ge
graph: 0 -a-> 1, 1 -b-> 2, 0 -c-> 3, 0 -c-> 4
$ mg --keep c/@3 a; nid
@3
+ 0 -a-> 3
+ 3 -b-> 2
- 0 -a-> 1
- 1 -b-> 2
$ mg a c
argument 2 of mg ("c") needs a path that resolves to a single node
but it resolved to: [@3,@4]
$ mg a a
mg needs at least two distinct nodes to merge
but its arguments all resolved to @1
$ mg a
<interactive>:1:5:
  |
1 | mg a
  |     ^
unexpected end of input
expecting "!{", "%history(", "%last", "%never", "%override(", "%targets(",
    "%{", "++", "--keep", "http://", "https://", '#', '&', '(', '*', '+', '/',
    '@', '|', '~', <transition>, @<nid>, alphanumeric character, or
    regex:"<regex>"
$ mg a @0; nid
@0
+ 0 -a-> 0
+ 0 -b-> 2
- 0 -a-> 1
- 1 -b-> 2
$ mg --keep a @0
mg cannot merge a system node away (the origin and the special nodes such as
    tags must keep their NIDs)
--keep would merge away the system node @0 (@0); keep it instead
```

### 2.6 `%targets(p)`

`%targets(p)` runs the query `p` at the current node and becomes the
`+` union of the nodes it finds (`@N1 + @N2 + …`), read as the command
reads `+`: in a query each copy of it may pick a different node, in
`mk` one node is picked for the whole path. `p` is a query of its own:
a `+` inside it is `|` whatever command the `%targets` is in and
whatever `%override` is written around it; only an `%override` inside
`p` changes that
([path-language.md](path-language.md#29-unions)). When `p` finds
nothing it is `%never`, and if the command then fails its error says
so.

```ge
graph: 0 -p-> 1, 0 -p-> 2, 1 -x-> 9, 2 -y-> 9
$ ls %targets(p)/(x & y)
x @9
$ mk x/%targets(p)
This path can be created in 2 ways; nothing was changed. To choose one, write
    @N after a step for the node it should go to, or !{@N} for nodes it should
    not, or keep one side of a +.
  1. @0 -"x"-> @1
    creates: @0 -"x"-> @1
    use: mk x/@1
  2. @0 -"x"-> @2
    creates: @0 -"x"-> @2
    use: mk x/@2
$ mk x/%targets(p/@1)
created 0 new nodes and 1 new edge
+ 0 -x-> 1
$ mk a/%targets(zz)/b
This path cannot be constructed.
Note: %targets("zz") at line 1, columns 6-17 matched no nodes, so it expanded
    to %never.
```

### 2.7 History and URLs

`%history(n)` is the node `n` steps back in the history and `%last` is
`%history(1)`; `%history(0)` is the current node, a negative `n` goes
forward (after `back`), and an `n` beyond either end gives the node at
that end. In one command line they are expanded when their command
runs, so in `cd a; cd %last` the `%last` is the location before
`cd a`.

```ge
graph: 0 -a-> 1, 0 -b-> 2
$ cd a; cd %last; nid
@0
$ cd a; cd ~a; cd %history(2); nid
@0
$ ls %history(0)
@0
$ cd a; cd %history(7); nid
@0
$ cd a; back 1; cd %history(-1); nid
@1
```

A URL (`https://…`, `http://…`) in a path imports the resource and
becomes the node made for it. The import happens while the path is
expanded, before the path is checked and before the command runs, so it
stays even if the path is then refused or the command fails. Each
distinct URL is fetched at most once per command line, however often it
is written or expanded (repeated across `;`, at each target of an `at`,
or after a `wget` of it), and later uses take the node of the first
import while that node exists (`mk x/URL ; rmnf x ; mk y/URL` fetches
it again). A URL extends to the next space, so one followed by `;` or
`)` needs a space before it: `mk x/URL ; ls`.

## 3. The edge commands: `rm`, `mv`, `cp`, `rn`, `cl`

These commands act on an edge found by a query. The edge is the *last
step of the first walk*: the path is resolved as a query, and in each
way it matched, the first walk (the first conjunct's, at every level,
also called route 0) names an edge by its last step. `rm` deletes that
edge in every match; `mv` and `cp` move or copy it in every match;
`rn` and `cl` need exactly one *distinct first walk*: several matches
that agree on the first walk are one, and no match is an error.

The order of the conjuncts decides which edge that is:

```ge
graph: 0 -a-> 1, 0 -b-> 1, 1 -c-> 2
$ ls a & b
a @1
$ rm a & b
- 0 -a-> 1
$ rm b & a
- 0 -b-> 1
$ rm (a & b)/c
- 1 -c-> 2
$ rm (a | b)
- 0 -a-> 1
- 0 -b-> 1
```

```ge
graph: 0 -a-> 9, 0 -b-> 1, 0 -b-> 2, 1 -c-> 9, 2 -c-> 9
$ ls b/c & a
b/c @9
b/c @9
$ rn (a & b/c) z
+ 0 -z-> 9
- 0 -a-> 9
$ rn (b/c & a) z
rn needs one selected source route, but found 2.
$ rm (b/c & a)
- 1 -c-> 9
- 2 -c-> 9
```

A reversed step names the edge it walked (`rm ~a` at the target of an
`a` edge deletes that edge). A path whose first walk has no step
(`rm (@ & a)`) is an error: put the conjunct with the edge first.

```ge
graph: 0 -a-> 0, 0 -b-> 1
$ rm (@ & a)
rm selected a zero-edge route. Put the edge branch first; nothing was changed.
$ rm (a & @)
- 0 -a-> 0
$ rm ~a
- 0 -a-> 0
$ rn (@ & a) z
rn selected a zero-edge route. Put the branch containing the edge first;
    nothing was changed.
```

When the path matches nothing, `rn` and `cl` fail with "found 0". `rm`,
and `mv` and `cp` for their first argument, change nothing and print a
notice on stderr that names the path as it was written. The notice is
no failure: the rest of a `;` line runs, `ge -e` exits with 0, and
`at p cmd` goes on to its other targets, where the path may match (the
notice then names the target). A path of which some side of a union
matches is a match, and prints nothing.

```ge
graph: 0 -a-> 1, 1 -b-> 2, 0 -c-> 3, 0 -c-> 4
$ rm zz
the path of rm ("zz") matched nothing; nothing was changed.
$ cp zz a
the first argument of cp ("zz") matched nothing; nothing was changed.
$ rm zz; ls a
a @1
the path of rm ("zz") matched nothing; nothing was changed.
$ rm (a + zz)
- 0 -a-> 1
$ at * rm b
the path of rm ("b") matched nothing at @3; nothing was changed.
the path of rm ("b") matched nothing at @4; nothing was changed.
- 1 -b-> 2
$ ge -e 'rm zz'
the path of rm ("zz") matched nothing; nothing was changed.
$ ge -e 'rn zz y'
rn needs one selected source route, but found 0.
[exit code 1]
```

### 3.1 `rm`

`rm p` deletes the selected edge of every match. (`rmnf` deletes the
target nodes instead, 2.3.)

```ge
graph: 0 -a-> 1, 1 -b-> 2, 0 -c-> 3, 0 -c-> 4
$ rm a/b
- 1 -b-> 2
$ rm c
- 0 -c-> 3
- 0 -c-> 4
$ rm a & c
the path of rm ("a" & "c") matched nothing; nothing was changed.
```

The last matched nothing: `a` and `c` end at different nodes, so the
query had no target and nothing was deleted. The notice shows the path
with its labels quoted.

### 3.2 `mv` and `cp`

`mv p q` moves the selected edge of every match so that it leaves from
`q`'s target instead (for a reversed step, so that it arrives there);
`cp p q` adds such an edge and keeps the original. `q` must reach
exactly one node, and is checked first: a bad `q` is an error even
when `p` matches nothing.

```ge
graph: 0 -a-> 1, 1 -b-> 2, 0 -c-> 3, 0 -c-> 4
$ mv a/b c/@3
+ 3 -b-> 2
- 1 -b-> 2
$ cp a/b c/@3
+ 3 -b-> 2
$ mv c @1
+ 1 -c-> 3
+ 1 -c-> 4
- 0 -c-> 3
- 0 -c-> 4
$ mv a/b c
the last argument of mv needs a path that resolves to a single node
but it resolved to: [@3,@4]
$ mv zz c
the last argument of mv needs a path that resolves to a single node
but it resolved to: [@3,@4]
```

### 3.3 `rn` and `cl`

`rn p q` renames the one selected edge: `q` is a *destination*, a path
whose last step is a literal label and whose prefix (everything before
that label) must reach exactly one node; the edge is recreated under
that node with the new label, and the original is deleted (`cl` keeps
it). The new edge hangs under the prefix node, not where the old one
was: with a bare label the prefix is the current node, so `rn a z`
renames `a` in place but `rn a/b z` moves the edge up to the current
node. A reversed source step is the edge it walked; renamed to a
forward label it is turned round (`at a rn ~a z` makes an edge from the
`a` target back), and to a reversed label (`~z`) it keeps its
direction.

```ge
graph: 0 -a-> 1, 1 -b-> 2, 0 -c-> 3, 0 -c-> 4
$ rn a z
+ 0 -z-> 1
- 0 -a-> 1
$ cl a z
+ 0 -z-> 1
$ rn a/b z
+ 0 -z-> 2
- 1 -b-> 2
$ at a rn ~a z
+ 1 -z-> 0
- 0 -a-> 1
$ at a rn ~a ~z
+ 0 -z-> 1
- 0 -a-> 1
$ cl a/b c/@3/z
+ 3 -z-> 2
```

The destination is refused when the last step of its first walk is
not a literal label (`*` or a regex) or it has no step at all (`@`);
when its prefix reaches no node, or several (`SeveralParents`;
`--all-parents` then creates the edge under each); and when it matches
through several alternatives of a union that would make different
edges (`(y | z)`, `(y + z)`, or a `%targets(…)` with several nodes),
which `--all-parents` does not help with. A `*` in the prefix is one
alternative with several parents and does work with `--all-parents`.
Alternatives that come to the same edge are no error: `rn a (b/z | c/z)`
works where `b` and `c` lead to one node. A node test written after
the last label is no step: it tests the node at which the destination
ends, which has to be the node at which the selected source walk
ends. So `rn a z/@1` renames as `rn a z` does where `a` leads to `1`,
and `rn a z/@7` matches no parent. The end of the walk is not always
the node the new edge leads to: for a reversed last label it is the
node the new edge leaves from (`rn a ~z/@1` makes `1 -z-> 0`), and for
a reversed source step it is the source of the edge renamed
(`at @1 rn ~a z/@0`).

```ge
graph: 0 -a-> 1, 1 -b-> 2, 0 -c-> 3, 0 -c-> 4
$ rn a *
The destination must end in a literal label.
$ rn a @
The destination has no final label.
$ rn a zz/y
rn destination has no matching parent or final label; nothing was changed.
$ rn a c/z
The rn destination's path before its final label resolves to 2 parent nodes
    (@3, @4); nothing was changed. Pin one with /@N before the final label, or
    use rn --all-parents to create the edge under each. (SeveralParents)
$ rn a c/@3/z
+ 3 -z-> 1
- 0 -a-> 1
$ rn --all-parents a c/z
+ 3 -z-> 1
+ 4 -z-> 1
- 0 -a-> 1
$ rn a (y + z)
The destination matches through several alternatives; choose one. Nothing was
    changed.
$ rn a %targets(c)/z
The destination matches through several alternatives; choose one. Nothing was
    changed.
$ rn --all-parents a %targets(c)/z
The destination matches through several alternatives; choose one. Nothing was
    changed.
```

```ge
graph: 0 -a-> 1, 0 -c-> 2, 0 -d-> 3
$ rn --all-parents a (c | d)/z
The destination matches through several alternatives; choose one. Nothing was
    changed.
$ rn --all-parents a */z
+ 1 -z-> 1
+ 2 -z-> 1
+ 3 -z-> 1
- 0 -a-> 1
```

```ge
graph: 0 -a-> 1, 0 -b-> 2, 0 -c-> 2
$ rn a (b/z | c/z)
+ 2 -z-> 1
- 0 -a-> 1
$ rn a (b/z | b/y)
The destination matches through several alternatives; choose one. Nothing was
    changed.
$ rn a z/@1
+ 0 -z-> 1
- 0 -a-> 1
$ rn a z/@7
rn destination has no matching parent or final label; nothing was changed.
$ rn a ~z/@1
+ 1 -z-> 0
- 0 -a-> 1
$ at @1 rn ~a z/@0
+ 1 -z-> 0
- 0 -a-> 1
```

## 4. `mk`

`mk p` makes the path hold from the current node: it walks `p` from
left to right, follows the edges that are there and creates the nodes
and edges that are missing. It then says how many it created; if the
path already holds, that is `created 0 new nodes and 0 new edges`.
`mk` does not append `/@`: the walks of a fork need not end at one
node (`mk a & b` makes an `a` edge and a `b` edge). A `+` in `mk` is
`++`: one side is chosen for the whole path. A path with `%never` in a
walk cannot hold and is an error.

Every node `mk` creates is a new node with a new id. It never creates
a node whose id is written in the path: `mk a/@7` when there is no node
7 is an error (4.5).

```ge
graph: 0
$ mk a/b/c
created 3 new nodes and 3 new edges
+ 0 -a-> O8OhMSVtjevt
+ O8OhMSVtjevt -b-> Pqf7kGOdKJIr
+ Pqf7kGOdKJIr -c-> DQ1KCJgauHiV
$ mk a & b
created 2 new nodes and 2 new edges
+ 0 -a-> O8OhMSVtjevt
+ 0 -b-> Pqf7kGOdKJIr
$ mk a/b & a/c
created 3 new nodes and 3 new edges
+ 0 -a-> O8OhMSVtjevt
+ O8OhMSVtjevt -b-> Pqf7kGOdKJIr
+ O8OhMSVtjevt -c-> DQ1KCJgauHiV
$ mk (a & b)/@
created 1 new node and 2 new edges
+ 0 -a-> O8OhMSVtjevt
+ 0 -b-> O8OhMSVtjevt
$ mk x/@/(c & d)
created 3 new nodes and 3 new edges
+ 0 -x-> O8OhMSVtjevt
+ O8OhMSVtjevt -c-> Pqf7kGOdKJIr
+ O8OhMSVtjevt -d-> DQ1KCJgauHiV
$ mk ~a
created 1 new node and 1 new edge
+ O8OhMSVtjevt -a-> 0
$ mk a/%never
This path cannot be constructed.
```

```ge
graph: 0 -a-> 1, 1 -b-> 2
$ mk a/b
created 0 new nodes and 0 new edges
$ mk a/b/c
created 1 new node and 1 new edge
+ 2 -c-> O8OhMSVtjevt
$ mk a/(b & c)
created 1 new node and 1 new edge
+ 1 -c-> O8OhMSVtjevt
$ mk a/@2
created 0 new nodes and 1 new edge
+ 0 -a-> 2
$ mk x/@1/b
created 0 new nodes and 1 new edge
+ 0 -x-> 1
$ mk a/!{@1}
created 1 new node and 1 new edge
+ 0 -a-> O8OhMSVtjevt
```

### 4.1 What a step does

`mk` walks every walk of the path from its start, one step at a time.
A step's **distance** is how many steps it is from the start of its
walk: in `x/~x/a` the `a` is at distance 3.

1. Standing on a node, a step looks for an edge with its label and
   direction at that node: an edge of the graph, or one that this
   command creates (rule 6 says which of those). If there is one, it
   follows it. If there is none, it creates a new node and the edge to
   it.
2. A step followed by `@N` goes to node `N`: along an edge if there is
   one, and along an edge it creates otherwise. Node `N` has to exist
   (4.5).
3. The steps into a join (`/@`) go together: to a node that one of
   them can follow an edge to, the others creating their edges to it;
   or, if none can follow an edge, to one new node.
4. Two steps with the same label from the same node make one new node
   (`a/b & a/c` creates one `a` edge).
5. Nothing is decided from the far end. With `1 -b-> 2`, `mk a/b/@2`
   does not look for a node that already has a `b` edge to 2: it
   creates `0 -a-> new -b-> 2`. Write `a/@1/b/@2` for node 1.
   `mk a/b/@2 & c/b/@2` creates two nodes; write `(a & c)/@/b/@2` for
   one.
6. The walks of a path are walked at the same pace. An edge that this
   command creates counts for a step like an edge of the graph, if the
   step that creates it is no further from the start of its walk than
   the step itself. `mk a & a/@2` creates only `0 -a-> 2`: both steps
   are at distance 1. `mk a/~a` creates only `0 -a-> new` (the `~a`
   walks back along it). `mk x/~x/a & a/@2` creates `0 -x-> new` and
   `0 -a-> 2`: the `a` at distance 3 follows the edge created at
   distance 1. But `mk a & b/~b/a/@2` creates `0 -a-> new`,
   `0 -b-> new` and `0 -a-> 2`: the first `a` is at distance 1 and
   does not see an edge that is created at distance 3. The steps into
   a join count as far as the furthest of them.
7. An edge never counts for a step if it is only there because of
   where that step went: `mk a/~a/a/@2` creates both `0 -a-> new` and
   `0 -a-> 2`.
8. When steps at the same distance could each follow an edge that the
   other creates, so that each would have to wait for the other, none
   of them gets the others' edges: each does what it would do if the
   others' edges were not there. (4.2 has an example.)
9. A `*` or regex step follows any edge whose label matches and that
   counts by rules 6 to 8, existing or created (`mk * & a` creates only
   `a`). When it has to create an edge it has no label to create it
   with, and the command fails (`UnsupportedLabel`); it does not fall
   back to another way of creating the path that needs no label.

```ge
graph: 0, 2
$ mk a & a/@2
created 0 new nodes and 1 new edge
+ 0 -a-> 2
$ mk a/b & a/@2/c
created 2 new nodes and 3 new edges
+ 0 -a-> 2
+ 2 -b-> O8OhMSVtjevt
+ 2 -c-> Pqf7kGOdKJIr
$ mk a/b/@2 & c/b/@2
created 2 new nodes and 4 new edges
+ 0 -a-> O8OhMSVtjevt
+ 0 -c-> Pqf7kGOdKJIr
+ O8OhMSVtjevt -b-> 2
+ Pqf7kGOdKJIr -b-> 2
$ mk (a & c)/@/b/@2
created 1 new node and 3 new edges
+ 0 -a-> O8OhMSVtjevt
+ 0 -c-> O8OhMSVtjevt
+ O8OhMSVtjevt -b-> 2
$ mk x/~x/a & a/@2
created 1 new node and 2 new edges
+ 0 -a-> 2
+ 0 -x-> O8OhMSVtjevt
$ mk a & b/~b/a/@2
created 2 new nodes and 3 new edges
+ 0 -a-> 2
+ 0 -a-> O8OhMSVtjevt
+ 0 -b-> Pqf7kGOdKJIr
$ mk a/~a/a/@2
created 1 new node and 2 new edges
+ 0 -a-> 2
+ 0 -a-> O8OhMSVtjevt
$ mk (x/y & a)/@ & a/@2
created 1 new node and 3 new edges
+ 0 -a-> 2
+ 0 -x-> O8OhMSVtjevt
+ O8OhMSVtjevt -y-> 2
```

```ge
graph: 1 -b-> 2, 0
$ mk a/b/@2
created 1 new node and 2 new edges
+ 0 -a-> O8OhMSVtjevt
+ O8OhMSVtjevt -b-> 2
$ mk a/@1/b/@2
created 0 new nodes and 1 new edge
+ 0 -a-> 1
```

```ge
graph: 0
$ mk a/~a
created 1 new node and 1 new edge
+ 0 -a-> O8OhMSVtjevt
$ mk a/a
created 2 new nodes and 2 new edges
+ 0 -a-> O8OhMSVtjevt
+ O8OhMSVtjevt -a-> Pqf7kGOdKJIr
$ mk * & a
created 1 new node and 1 new edge
+ 0 -a-> O8OhMSVtjevt
$ mk *
Cannot create the * edge without a label: give it a literal label. Nothing was
    changed. (UnsupportedLabel)
  @0 -*-> new1
    creates: @0 -*-> new1
$ mk * + a
Cannot create the * edge without a label (possibility 2 below), and even with
    one this path can be created in 2 ways. mk does not choose one that needs
    no label; nothing was changed. Give the edge a literal label, or write @N
    after a step for the node it should go to, or !{@N} for nodes it should
    not, or keep one side of a +. (UnsupportedLabel)
  1. @0 -"a"-> new1
    creates: @0 -"a"-> new1
  2. @0 -*-> new1
    creates: @0 -*-> new1
$ mk a + a/*
created 1 new node and 1 new edge
+ 0 -a-> O8OhMSVtjevt
```

```ge
graph: 0, 2
$ mk */@2 & a/@2
created 0 new nodes and 1 new edge
+ 0 -a-> 2
$ mk */@2/~b/@0
Cannot create the * edge without a label: give it a literal label. Nothing was
    changed. (UnsupportedLabel)
  @0 -*-> @2 <-"b"- @0
    creates: @0 -"b"-> @2, @0 -*-> @2
```

In the last command the `*` does not go along the `b` edge that the
path creates one step further on, and has no label of its own.

### 4.2 When a step has several edges to follow

If a step has edges to several nodes that it could follow, `mk` does
not choose: it creates nothing, lists what it could create, and asks.
It makes no difference whether the edges are existing ones, ones this
command creates, or both. To choose, write `@N` after the step for the
node it should go to, or `!{@N}` for nodes it should not go to.

```ge
graph: 0 -a-> 1, 0 -a-> 2
$ mk a/b
This path can be created in 2 ways; nothing was changed. To choose one, write
    @N after a step for the node it should go to, or !{@N} for nodes it should
    not, or keep one side of a +.
  1. @0 -"a"-> @1 -"b"-> new1
    creates: @1 -"b"-> new1
    use: mk a/@1/b
  2. @0 -"a"-> @2 -"b"-> new1
    creates: @2 -"b"-> new1
    use: mk a/@2/b
$ mk a/@1/b
created 1 new node and 1 new edge
+ 1 -b-> O8OhMSVtjevt
$ mk a/!{@1}/b
created 1 new node and 1 new edge
+ 2 -b-> O8OhMSVtjevt
$ mk a/!{@1, @2}/b
created 2 new nodes and 2 new edges
+ 0 -a-> O8OhMSVtjevt
+ O8OhMSVtjevt -b-> Pqf7kGOdKJIr
```

```ge
graph: 0 -a-> 1
$ mk a/!{@1}/b & a/c
These steps could each follow an edge that another of them creates, so mk has
    no order to take them in: `a/!{@1}` and `a`. Write the node a step should
    go to (/@N after it), or create them with separate commands. Nothing was
    changed. (CircularSuppliers)
```

```ge
graph: 0 -a-> 1, 0 -c-> 5
$ mk (a & c)/@
This path can be created in 2 ways; nothing was changed. To choose one, write
    @N after a step for the node it should go to, or !{@N} for nodes it should
    not, or keep one side of a +.
  1. @0 -"a"-> @1 & @0 -"c"-> @1
    creates: @0 -"c"-> @1
    use: mk (a/@1 & c/@1)/@
  2. @0 -"a"-> @5 & @0 -"c"-> @5
    creates: @0 -"a"-> @5
    use: mk (a/@5 & c/@5)/@
```

What is listed:

1. If the path already holds in some way, nothing is done.
2. A way of creating the path that creates everything another way
   creates and more (up to the naming of new nodes) is not listed, and
   ways that create the same are listed once.
3. They are ordered by the number of nodes and edges created, then by
   a fixed order of their text.

One way left is what `mk` creates. Several are the error above. None
means the path cannot hold (`mk a/%targets(zz)` when `zz` reaches
nothing, 2.6).

```ge
graph: 0 -a-> 1, 0 -a-> 2, 1 -b-> 3
$ mk a/b
created 0 new nodes and 0 new edges
$ mk a/b & a/c
This path can be created in 2 ways; nothing was changed. To choose one, write
    @N after a step for the node it should go to, or !{@N} for nodes it should
    not, or keep one side of a +.
  1. @0 -"a"-> @1 -"b"-> @3 & @0 -"a"-> @1 -"c"-> new1
    creates: @1 -"c"-> new1
    use: mk a/@1/b & a/@1/c
  2. @0 -"a"-> @1 -"b"-> @3 & @0 -"a"-> @2 -"c"-> new1
    creates: @2 -"c"-> new1
    use: mk a/@1/b & a/@2/c
```

Steps that could follow each other's edges (rule 8 of 4.1) are listed
in every way that holds together. Below, the `c` of the first conjunct
may not go to node 2, and the join `(a & c)/@` can go to node 1 along
the `a` edge or to node 2 along the `c` edge. In the first way the
join goes to node 1 and creates `0 -c-> 1`, which the first `c` then
follows. In the third and fourth the first `c` creates its own node
first, and the join goes to node 2 or to that new node. In the second
each does what it would do without the other's edge: the first `c`
creates a node, and the join goes to node 1.

```ge
graph: 0, 1, 2
$ mk c/!{@2}/a & (a & c)/@ & a/@1 & c/@2
These steps could each follow an edge that another of them creates, so mk has
    no order to take them in: `c/!{@2}` and the node where `a` and `c` meet.
    If a step means the same edge as a step into a node where steps meet,
    leave it out and write what follows it after that node, as in (a & c)/@/b
    for a/b & (a & c)/@; otherwise write the node a step should go to (/@N
    after it), or create them with separate commands. Nothing was changed.
    (CircularSuppliers)
```

### 4.3 The list and its `use:` lines

An ambiguous `mk` lists at most five ways and says how many more there
are. Each is shown as the walks of the path with every position at a
node (existing nodes as `@N`, new nodes as `new1`, `new2`, …), then
`creates:` with the edges, then, where there is one, `use: mk …` with
a path that creates exactly that, to be typed at the prompt. The path
is the written path with `@N` after each step that went to an existing
node where it had several to go to, and `!{@N, …}` naming the others
after each step that went to the new one. When the command ran at
another node than the prompt's (inside `at`) the line is
`use: at @N mk …`, and for `tag` it is a `tag` command. A line is only
shown when running it creates exactly what it stands beside.

```ge
graph: 0 -a-> 1, 1 -b-> 2, 1 -b-> 3
$ at a mk b/c
This path can be created in 2 ways; nothing was changed. To choose one, write
    @N after a step for the node it should go to, or !{@N} for nodes it should
    not, or keep one side of a +.
  1. @1 -"b"-> @2 -"c"-> new1
    creates: @2 -"c"-> new1
    use: at @1 mk b/@2/c
  2. @1 -"b"-> @3 -"c"-> new1
    creates: @3 -"c"-> new1
    use: at @1 mk b/@3/c
$ at a mk b/@2/c
created 1 new node and 1 new edge
+ 2 -c-> O8OhMSVtjevt
```

There is no `use:` line when the step could also have gone to another
new node, which has no name to write (`mk a & b & */c` below: `c` under
the new `a` node or the new `b` node; write `a/c` or `b/c` instead);
when the copies of a step went different ways (`mk a/(b & c)` with two
`a` nodes: what is written after the one `a` applies to both copies);
when steps waited for each other (the second way of the last example
of 4.2); or when the way's side of a `|` union has no spelling as a
path without unions.

```ge
graph: 0
$ mk a & b & */c
This path can be created in 2 ways; nothing was changed. To choose one, write
    @N after a step for the node it should go to, or !{@N} for nodes it should
    not, or keep one side of a +.
  1. @0 -"a"-> new1 & @0 -"b"-> new2 & @0 -"a"-> new1 -"c"-> new3
    creates: @0 -"a"-> new1, @0 -"b"-> new2, new1 -"c"-> new3
    (no command was found that creates just this)
  2. @0 -"a"-> new1 & @0 -"b"-> new2 & @0 -"b"-> new2 -"c"-> new3
    creates: @0 -"a"-> new1, @0 -"b"-> new2, new2 -"c"-> new3
    (no command was found that creates just this)
```

```ge
graph: 0 -a-> 1, 0 -a-> 2
$ mk a/(b & c)
This path can be created in 4 ways; nothing was changed. To choose one, write
    @N after a step for the node it should go to, or !{@N} for nodes it should
    not, or keep one side of a +.
  1. @0 -"a"-> @1 -"b"-> new1 & @0 -"a"-> @1 -"c"-> new2
    creates: @1 -"b"-> new1, @1 -"c"-> new2
    use: mk a/@1/(b & c)
  2. @0 -"a"-> @1 -"b"-> new1 & @0 -"a"-> @2 -"c"-> new2
    creates: @1 -"b"-> new1, @2 -"c"-> new2
    (no command was found that creates just this)
  3. @0 -"a"-> @2 -"b"-> new1 & @0 -"a"-> @1 -"c"-> new2
    creates: @1 -"c"-> new2, @2 -"b"-> new1
    (no command was found that creates just this)
  4. @0 -"a"-> @2 -"b"-> new1 & @0 -"a"-> @2 -"c"-> new2
    creates: @2 -"b"-> new1, @2 -"c"-> new2
    use: mk a/@2/(b & c)
```

Suggested paths are written with `|` and `++` only (never `+`), so
they mean the same in every command.

### 4.4 Unions in `mk`

A `+` is `++`: no way of creating `mk (a + b)/(c & d)` mixes the sides
between the copies, so on an empty graph there is one way per side,
and `mk` asks which; on a graph where a side has several edges to
follow, each is a way of its own (`mk (a/b ++ z)` with two `a` edges
has three). A `|` written in `mk` chooses per copy, and offers the
mixed ways too:

```ge
graph: 0
$ mk (a + b)/(c & d)
This path can be created in 2 ways; nothing was changed. To choose one, write
    @N after a step for the node it should go to, or !{@N} for nodes it should
    not, or keep one side of a +.
  1. @0 -"a"-> new1 -"c"-> new2 & @0 -"a"-> new1 -"d"-> new3
    creates: @0 -"a"-> new1, new1 -"c"-> new2, new1 -"d"-> new3
    use: mk a/(c & d)
  2. @0 -"b"-> new1 -"c"-> new2 & @0 -"b"-> new1 -"d"-> new3
    creates: @0 -"b"-> new1, new1 -"c"-> new2, new1 -"d"-> new3
    use: mk b/(c & d)
$ mk (a | b)/(c & d)
This path can be created in 4 ways; nothing was changed. To choose one, write
    @N after a step for the node it should go to, or !{@N} for nodes it should
    not, or keep one side of a +.
  1. @0 -"a"-> new1 -"c"-> new2 & @0 -"a"-> new1 -"d"-> new3
    creates: @0 -"a"-> new1, new1 -"c"-> new2, new1 -"d"-> new3
    use: mk a/(c & d)
  2. @0 -"b"-> new1 -"c"-> new2 & @0 -"b"-> new1 -"d"-> new3
    creates: @0 -"b"-> new1, new1 -"c"-> new2, new1 -"d"-> new3
    use: mk b/(c & d)
  3. @0 -"a"-> new1 -"c"-> new2 & @0 -"b"-> new3 -"d"-> new4
    creates: @0 -"a"-> new1, @0 -"b"-> new3, new1 -"c"-> new2, new3 -"d"->
    new4
    use: mk a/c & b/d
  4. @0 -"b"-> new1 -"c"-> new2 & @0 -"a"-> new3 -"d"-> new4
    creates: @0 -"a"-> new3, @0 -"b"-> new1, new1 -"c"-> new2, new3 -"d"->
    new4
    use: mk b/c & a/d
```

```ge
graph: 0 -a-> 1, 0 -a-> 2
$ mk (a/b ++ z)
This path can be created in 3 ways; nothing was changed. To choose one, write
    @N after a step for the node it should go to, or !{@N} for nodes it should
    not, or keep one side of a +.
  1. @0 -"a"-> @1 -"b"-> new1
    creates: @1 -"b"-> new1
    use: mk a/@1/b
  2. @0 -"a"-> @2 -"b"-> new1
    creates: @2 -"b"-> new1
    use: mk a/@2/b
  3. @0 -"z"-> new1
    creates: @0 -"z"-> new1
    use: mk z
```

### 4.5 Nodes written by their id

`mk` gives every node it creates a new id. A node that the path writes
as `@N` has to exist already: a way of creating the path that would
need a node `@N` that does not exist is no way. If that leaves none,
`mk` fails, names the nodes and changes nothing; `ge -e` exits with 1.
A side of a union that needs such a node is left out, and the other
sides are created as if it were not written. A node inside `!{…}` need
not exist: it only says where a step may not go.

```ge
graph: 0
$ ge -e 'mk a/@7'
This path needs @7, which does not exist, and a node with a written id is
    never created: mk gives every new node a new id. Nothing was changed.
    (MissingNodes)
[exit code 1]
$ mk a/@7/b/@8
This path needs @7, @8, which do not exist, and a node with a written id is
    never created: mk gives every new node a new id. Nothing was changed.
    (MissingNodes)
$ mk @7/x
This path needs @7, which does not exist, and a node with a written id is
    never created: mk gives every new node a new id. Nothing was changed.
    (MissingNodes)
$ mk a/@7 + b
created 1 new node and 1 new edge
Note: mk left out `a/@7`, which needs @7: that node does not exist, and a node
    with a written id is never created.
+ 0 -b-> O8OhMSVtjevt
$ mk (a/@7 | b)
created 1 new node and 1 new edge
Note: mk left out `a/@7`, which needs @7: that node does not exist, and a node
    with a written id is never created.
+ 0 -b-> O8OhMSVtjevt
$ mk a/@7 + b/@8
This path needs @7, @8, which do not exist, and a node with a written id is
    never created: mk gives every new node a new id. Nothing was changed.
    (MissingNodes)
$ mk a/!{@7}
created 1 new node and 1 new edge
+ 0 -a-> O8OhMSVtjevt
```

`mk --allow-orphans p` does what `mk p` does. The option allows new
nodes that no edge connects to an existing node, and no path creates
such nodes: every walk starts at a node that exists.

```ge
graph: 0
$ mk --allow-orphans @7/x
<interactive>:1:20:
  |
1 | mk --allow-orphans @7/x
  |                    ^
mk has no --allow-orphans any more: every node that mk creates is connected to
    the graph, so write mk <path>
```

### 4.6 `tag`

`tag p q` (or `t p q`) constructs `p` like `mk` (with `+` as `++`, and
with the same error for a written node that does not exist), then
links the end of every walk of one match of `p` to the one target of
`q` with an edge labelled `""`. Which match: the alternatives of `p`
that match are ordered by their first match, and matches by the nodes
and labels along their walks; `tag` takes the first match of the first
alternative. That order depends on the graph, not on how a union is
written: `tag (b | a) t` below links the `a` end. Other matches are
not linked. `q` must reach exactly one node (none or several is an
error, section 2). `#t` is `@<tags>/t`, so `tag #todo a/b` tags the
target of `a/b` with `todo`, creating the tag node if needed. An
ambiguous `p` is listed with `use: tag … @N` lines (4.3). `tag` does
not say how much it created. The target and the nodes `p` created get
the same timestamp.

```ge
graph: 0 -a-> 1, 1 -b-> 2
$ tag #todo a/b
+ O8OhMSVtjevt -""-> 2
+ pbYxBO6fzBQV -todo-> O8OhMSVtjevt
$ tag #todo *
+ O8OhMSVtjevt -""-> 1
+ pbYxBO6fzBQV -todo-> O8OhMSVtjevt
```

```ge
graph: 0 -a-> 1, 0 -b-> 2, 0 -t-> 9, 0 -a-> 3
$ tag (a & b) t
+ 1 -""-> 9
+ 2 -""-> 9
$ tag (b | a) t
+ 1 -""-> 9
$ tag a/x t
This path can be created in 2 ways; nothing was changed. To choose one, write
    @N after a step for the node it should go to, or !{@N} for nodes it should
    not, or keep one side of a +.
  1. @0 -"a"-> @1 -"x"-> new1
    creates: @1 -"x"-> new1
    use: tag a/@1/x @9
  2. @0 -"a"-> @3 -"x"-> new1
    creates: @3 -"x"-> new1
    use: tag a/@3/x @9
$ tag a/@7 t
This path needs @7, which does not exist, and a node with a written id is
    never created: tag gives every new node a new id. Nothing was changed.
    (MissingNodes)
```

### 4.7 One command after another

`mk p` and then `mk p/q` usually leaves what `mk p/q` alone leaves, but
not always. It does when `q` begins with a step, both commands decide
without asking, and the path writes no label twice and has no `*` or
regex. Outside that, these break it:

```ge
graph: 0, 2
$ mk a/@2/(a & b)
created 2 new nodes and 3 new edges
+ 0 -a-> 2
+ 2 -a-> O8OhMSVtjevt
+ 2 -b-> Pqf7kGOdKJIr
$ mk a ; mk a/@2/(a & b)
created 1 new node and 1 new edge
created 2 new nodes and 3 new edges
+ 0 -a-> 2
+ 0 -a-> O8OhMSVtjevt
+ 2 -a-> w4VxcgSm0Js6
+ 2 -b-> w4P7BSd8jU2v
```

`mk a` cannot know that the `a` was to lead to node 2 (the cut is just
before `@2`), so its node is left over.

```ge
graph: 0 -a-> 0, 3
$ mk a/a/@3/a
created 1 new node and 2 new edges
+ 0 -a-> 3
+ 3 -a-> O8OhMSVtjevt
$ mk a/a/@3 ; mk a/a/@3/a
created 0 new nodes and 1 new edge
This path can be created in 2 ways; nothing was changed. To choose one, write
    @N after a step for the node it should go to, or !{@N} for nodes it should
    not, or keep one side of a +.
  1. @0 -"a"-> @3 -"a"-> @3 -"a"-> @3
    creates: @3 -"a"-> @3
    use: mk a/@3/a/@3/a
  2. @0 -"a"-> @0 -"a"-> @3 -"a"-> new1
    creates: @3 -"a"-> new1
    use: mk a/@0/a/@3/a
Note: the command before this one in the line had already run; what it did
    stays.
+ 0 -a-> 3
```

The first command adds `0 -a-> 3` beside the `0 -a-> 0` that its own
first step follows; after it the first `a` has two edges.

```ge
graph: 0, 4
$ mk (b & c/~c/b/@4)/b
created 4 new nodes and 5 new edges
+ 0 -b-> 4
+ 0 -b-> O8OhMSVtjevt
+ 0 -c-> DQ1KCJgauHiV
+ 4 -b-> sYumkceKpoAh
+ O8OhMSVtjevt -b-> Pqf7kGOdKJIr
$ mk b & c/~c/b/@4 ; mk (b & c/~c/b/@4)/b
created 2 new nodes and 3 new edges
created 1 new node and 1 new edge
+ 0 -b-> 4
+ 0 -b-> O8OhMSVtjevt
+ 0 -c-> Pqf7kGOdKJIr
+ 4 -b-> w4P7BSd8jU2v
```

The first `b` is at distance 1 and does not see the `0 -b-> 4` created
at distance 3, so it creates a node of its own. The second command
finds both `b` edges in the graph; going along `0 -b-> 4` creates
less, and nothing is put under the first command's node.

```ge
graph: 0 -c-> 0
$ mk (*/a & */c)/b
This path has more than one * or regex step in one alternative: `*` and `*`.
    mk builds a path with at most one. Write the labels you mean; if steps
    from one node are meant to be one step, write it once, with a join after
    it for what continues from it, as in */@/(a & b) for */a & */b. Nothing
    was changed. (SeveralWildcards)
$ mk */a & */c ; mk (*/a & */c)/b
This path has more than one * or regex step in one alternative: `*` and `*`.
    mk builds a path with at most one. Write the labels you mean; if steps
    from one node are meant to be one step, write it once, with a join after
    it for what continues from it, as in */@/(a & b) for */a & */b. Nothing
    was changed. (SeveralWildcards)
```

The first command creates `0 -a-> new`, and after it each `*` has two
edges to follow, the loop and the new edge.

## 5. Settings and the one limit

`%override({key = value, …}, p)` changes a setting for the part `p` of
a path. There are two settings: `plus` (5.3), which says how `+` is
read, and `max-repairs` (5.2), the limit on the walks `mk` makes. That
limit is the only one there is: nothing limits what a path compiles to
(5.1).

### 5.1 Nothing limits what a path compiles to

No command refuses a path for what it compiles to. (`mk` alone has a
limit, on the walks it makes, 5.2.) Each copy of a `|` union
chooses on its own, so a `k`-way `|` that a fork copies into `n` walks
has up to `k^n` alternatives, and groups written one after the other
multiply their walks; a command compiles its path however much that
comes to, and matches it, for as long as that takes. The only paths a
command does not run are those that would copy a meeting point
([path-language.md](path-language.md#31-a-product-may-not-copy-a-meeting-point)),
which is found by reading the path.

```ge
graph: 0
$ ls (a | b | c)/(d & e & f & g)
no targets
$ ls (a | b | c)/(d & e & f & g & h & i & j)
no targets
$ ls (a ++ b)/(c & d & e & f & g & h & i & j & k & l)
no targets
```

The first has 81 alternatives and the second 2187.

Some paths that are short to write do not finish:
`ls (x | y)/(a & b & c)/(a & b & c)/(a & b & c)/(a & b & c)/(a & b & c)/(a & b & c)/(a & b & c)`
has one union that 2187 walks choose on their own. What to do about a
command that runs long:

- **In the REPL, Ctrl-C stops the command** and gives the prompt back,
  every time, whether the command is working out its path or reading
  node files. It prints `^C` and nothing else, and the next command
  works. A command works its paths out before it writes, so one that is
  stopped while doing that has changed nothing; the commands before it
  on a `;` line have run and stay.
- **`ge -e` ends at an interrupt**, by the signal, as an interrupted
  program does (a shell shows exit status 130), and prints nothing of
  its own.
- Either way, damage that the line had read past by then is still
  warned of (section 7).
- **TAB completion gives up by itself** after about half a second
  (section 1), so a prefix that would take long never holds the prompt.

In `mk`, a `+` is `++`, so many alternatives arise only for a `|`
written there; `mk` then lists as many ways of creating the path
(`mk (a | b | c)/(d & e & f & g & h & i & j)` on an empty graph has
2187), or stops at its own limit (5.2).

[path-cost-limits.md](path-cost-limits.md) records what was tried as a
limit on alternatives, why a count of alternatives does not predict how
long a command takes, where the time of a path command goes, and what a
check would look like if one is wanted.

### 5.2 Walks: `max-repairs`

A step with edges to several nodes multiplies the ways `mk` has to
walk the path: `n` edges at each of `k` steps are `n^k` walks, and
every one is walked before any is dropped (4.2). `mk` counts the walks
it makes and stops with an error, nothing changed, as soon as there are
more than the limit: 5000 (`defaultMaxRepairs`) unless
`%override({max-repairs = N}, p)` (or `unlimited`) says otherwise. The
largest limit among the parts of the path applies to all of it, since
a walk is of the whole path; an override on a part never lowers the
limit. A path that already holds is found to hold whatever the limit.
Writing `@N` after a step leaves it one node to go to and divides the
count. Steps that have one edge to follow, or none, do not multiply
anything: ten existing conjuncts and a new one are one walk.

The count is of walks made, not of ways listed. Ways that create more
than another are walked and then dropped. And when `mk` finds that a
step was taken before an edge that counts for it was created (4.1,
rule 6: this happens between steps at the same distance, and with a
join one of whose steps is nearer the start than another), it walks
the path again with that step waiting, and those walks count too. In
the example the second `a` is taken first and goes to node 1; then the
first `a` creates a node that the second could have gone to as well.
That is one walk, and two more when the path is walked again.

```ge
graph: 0 -a-> 1, 0 -a-> 2, 0 -a-> 3, 0 -a-> 4, 0 -a-> 5, 0 -a-> 6
$ mk --max-repairs 5 a/b
Working out what this path creates takes too long: its steps have several
    edges to follow, and every combination of the edges followed has to be
    worked out, over all alternatives of the path; there are more combinations
    than the limit of 5. This does not mean that the path cannot be created.
    Nothing was changed. Write @N after a step for the node it should go to,
    or !{@N} for nodes it should not, so that fewer edges can be followed, or
    raise the limit by giving mk the option --max-repairs N before the path,
    with N a larger number or unlimited. (TooManyRepairs)
```

```ge
graph: 0 -a-> 1
$ mk --max-repairs 2 a/!{@1}/b & a/c
These steps could each follow an edge that another of them creates, so mk has
    no order to take them in: `a/!{@1}` and `a`. Write the node a step should
    go to (/@N after it), or create them with separate commands. Nothing was
    changed. (CircularSuppliers)
$ mk --max-repairs 3 a/!{@1}/b & a/c
These steps could each follow an edge that another of them creates, so mk has
    no order to take them in: `a/!{@1}` and `a`. Write the node a step should
    go to (/@N after it), or create them with separate commands. Nothing was
    changed. (CircularSuppliers)
```

The setting combines with `plus` (`{plus = shared, max-repairs = 5000}`).
Queries accept the setting and ignore it. The `use:` lines of an
ambiguous `mk` are confirmed within the default limit, not the
override's.

### 5.3 `%override({plus = …}, p)`

`plus = shared` reads every `+` in `p` as `++`, `plus = relational` as
`|`, in any command; the innermost override wins, `|` and `++` written
in `p` are unaffected, and a `+` inside a `%targets(…)` in `p` is not
reached (2.6). Settings combine:
`%override({plus = shared, max-repairs = 50000}, p)`. An unknown
key or value, or a key given twice, is a parse error
(`InvalidOverrideSetting`,
[path-language.md](path-language.md#33-parse-errors)).

## 6. When a command fails

A command *fails* when it reports an error: a path that does not parse,
a refused path, a `cd` without exactly one target, an ambiguous `mk`,
an `rn` that selects no edge, an edit next to damage (section 7), and
so on. A warning (`JoinDoesNothing`, damage that was only read) and the
notice of an `rm`, `mv` or `cp` that matched nothing (3) are not
failures, nor is an `ls` that finds no targets.

- **In the REPL** the error is printed and the next line is read.
- **`ge GRAPH -e 'line'`** runs the one line and exits: with 0 when
  every command ran, with 1 when one failed. A line that does not parse
  is reported with the usage text, and exits with 1.
- **On a `;` line** the commands run from left to right, and the line
  stops at the first that fails: the commands after it are not run, and
  what the commands before it did stays. The error is then followed by
  a note saying so. The same holds inside the braces of an `at` block,
  and across the targets of an `at`, which stops at the first target
  where `cmd` fails: what `cmd` did at the earlier targets stays, and
  `at` adds a note of its own that names those targets.
- **`fsck` and `fix` are the exception.** They fail when they find or
  leave damage, which is for the exit code, and do not stop the line:
  `ge -e 'fsck; fix'` lists the damage, repairs it, and exits with 1
  because `fsck` found some ([damaged-graphs.md](damaged-graphs.md#fix)).
- **`ge --dry-run`** (with `-e` or as a REPL) prints the changes
  instead of writing them, as `would write: …` and
  `would delete at path: …` lines that name the node files (and a
  `delete data for …` line for the data of a node that would be
  deleted), and leaves the node files as they were. (One command
  does write in a dry run, which is a bug: `vi`, section 8.) Since
  nothing is written, a later command of the line sees the graph as it
  was and not as the earlier ones would have
  left it, so a line in which one command builds on what another wrote
  can end differently in a dry run
  ([damaged-graphs.md](damaged-graphs.md#in-a-dry-run)).

```ge
graph: 0 -a-> 1, 0 -c-> 3, 0 -c-> 4
$ cd zz ↵ nid ↵ rn zz y ↵ ls a
@0
a @1
cd needs a path that resolves to a single node
but it resolved to: []
rn needs one selected source route, but found 0.
$ ge -e 'cd zz'
cd needs a path that resolves to a single node
but it resolved to: []
[exit code 1]
$ ge -e 'ls zz'
no targets
$ ge -e 'ls a b'
option -e: <interactive>:1:6:
  |
1 | ls a b
  |      ^
unexpected 'b'
expecting "++", '&', '+', '/', '|', or end of input
Usage: ge <graph directory> [-e|--expression <command>] [--new] [--dry-run]
  Manipulate information graphs.
[exit code 1]
$ ge -e 'mk q; cd zz; mk r'
created 1 new node and 1 new edge
cd needs a path that resolves to a single node
but it resolved to: []
Note: the command before this one in the line had already run; what it did
    stays.
[exit code 1]
+ 0 -q-> O8OhMSVtjevt
$ mk q; cd zz; mk r
created 1 new node and 1 new edge
cd needs a path that resolves to a single node
but it resolved to: []
Note: the command before this one in the line had already run; what it did
    stays.
+ 0 -q-> O8OhMSVtjevt
$ ge -e 'at c {mk q; cd zz}'
created 1 new node and 1 new edge
cd needs a path that resolves to a single node
but it resolved to: []
Note: the command before this one in the line had already run; what it did
    stays.
[exit code 1]
+ 3 -q-> O8OhMSVtjevt
$ ge -e 'mk q; mk r; cd zz'
created 1 new node and 1 new edge
created 1 new node and 1 new edge
cd needs a path that resolves to a single node
but it resolved to: []
Note: the 2 commands before this one in the line had already run; what they
    did stays.
[exit code 1]
+ 0 -q-> O8OhMSVtjevt
+ 0 -r-> w4VxcgSm0Js6
$ ge --dry-run -e 'ls a; rn zz y'
a @1
rn needs one selected source route, but found 0.
Note: the command before this one in the line had already run; what it did
    stays.
[exit code 1]
$ ge -e 'at (a + c) {nid; cd ~a}'
@1
@3
cd needs a path that resolves to a single node
but it resolved to: []
Note: the command before this one in the line had already run; what it did
    stays.
Note: at had already run the command at @1; what it did there stays.
[exit code 1]
$ ge --dry-run -e 'rm a'
would write: @0{out=[@3 via "c", @4 via "c"]} -> "GRAPH/0.json"
would write: @1{out=[]} -> "GRAPH/1.json"
```

In the first example the two errors come after the output because
errors are printed on stderr, which these examples show last. In the
fifth and sixth, `q` was created and `r` was not. In the seventh, `at`
stopped at its first target, `3`, where `q` stays, and did nothing at
`4`. In the tenth, `at` failed at its second target, `3`, after the
block had run at `1`: the first note is the block's (its `nid` had
run), and the second is `at`'s. The last is a dry run of a command
that would write two node files, and wrote none.

## 7. Damaged graphs

A graph is a directory of node files, and every edge is recorded in the
file of each of its two nodes. Files that contradict each other, are
missing or cannot be read are *damage*.
[damaged-graphs.md](damaged-graphs.md) says what counts as damage and
what each command does about it. In short:

- A command that **changes** something is refused before it writes
  anything if the node files it read or would write are damaged, with
  `this change was not written: the graph is damaged where it applies
  (damaged-graph)` and a line for each finding. That is a failure
  (section 6). What a command read in any of its steps counts for
  every change it then makes, so an `mg`, an `at` or a `%targets(…)`
  that read past damage refuses the change it leads to. The refusal is
  of that change only, and what the command wrote before it stays, as
  on a `;` line: in `at @1 {rmnf @5; at @7 mk z}`, node `5` is deleted
  by the time `mk z` is refused for damage between a file that the
  `rmnf` read and one that the `mk` reads
  ([damaged-graphs.md](damaged-graphs.md#in-a-dry-run)).
- A command that only **reads** goes on and warns when the command line
  is done, also when the line fails or is interrupted: `warning: the
  graph is damaged where this command read it (damaged-graph)`.
- `fsck` reads every node file and lists all damage; `fix` repairs what
  it can and says what it cannot.

So a path can find fewer nodes next to damage than the graph was meant
to have, and says so; and edits there wait for `fix`.

## 8. Known rough edges

Each item says what the build does and what is pending or undecided.

- A `cd` inside an `at` block changes the history but not the node the
  following commands of the block run at (2.4). Not decided whether
  intended.
- The note after a failed command of a `;` line says that the command
  before it "had already run; what it did stays" under `--dry-run` too,
  where that command wrote nothing.
- A block in braces repeats the note: `ge -e 'mk q; {mk r; cd zz}'`
  prints "Note: the command before this one in the line had already
  run; what it did stays." twice, once for the block and once for the
  line.
- An ambiguous `mk` can list a way of creating the path without a
  `use:` line because its alternative has no spelling as a path (4.3),
  besides the reasons that no path can express.
- When steps could follow each other's edges, `mk` lists every way
  that holds together, among them one in which each step ignores the
  other's edge and one more node is created than in another way
  listed (4.2).
- `mk --allow-orphans` does nothing that `mk` does not (4.5).
- Nothing bounds the cost of compiling or matching a large path (5.1),
  and `max-repairs` bounds a count of walks, not time (5.2).
- The rough edges of the language itself are in
  [path-language.md](path-language.md#5-known-rough-edges-and-what-may-change).

### Known bugs

These are bugs. They are known, and they are not being fixed now.

- **`ls` through a node with many children is slow, and gets slower
  with the square of their number.** At a node with `N` children under
  `n`, each with a `c` edge to one and the same node, `ls n/c` took
  2.5 s of processor time for 300 children, 26 s for 1,000 and 221 s
  for 3,000 (4.6 s, 45 s and 284 s of wall time). Measured on
  2026-10-11 with `ge GRAPH -e 'ls n/c'`, each once, on the build this
  document is checked in with (unoptimized), on a machine that was
  doing other work. Ctrl-C stops it (5.1).
- **`ge --dry-run -e vi` writes.** A dry run of `vi` starts the editor,
  and what the editor saves is written to the data file of the node:
  with an editor that writes one line, `ge --dry-run GRAPH -e vi` left
  a `000000000000.data` in the graph.
- **Node ids that differ only in letter case are one file** on a volume
  that does not tell upper from lower case in file names, which is the
  usual kind on a Mac. There, in a graph with a node `aBcDeFgHiJkL`,
  `mk b/@AbCdEfGhIjKl` is refused as an edit next to damage
  ("AbCdEfGhIjKl.json says it is the node @aBcDeFgHiJkL"); and a graph
  that has two such nodes, brought from a volume that tells them apart,
  reads as damaged (not run: it takes such a volume).
- **Several `ge` processes that write to one graph at the same time
  damage it.** There is no lock on the graph, and every one of them
  adds records to node files that all of them write, the node of the
  current day for one, so the one that writes last drops what the
  other recorded. Three loops of forty
  `ge GRAPH -e 'mk w/n/m ; rm w/n/m'` at once, each with a `w` of its
  own, left 18 edges recorded at one of their two nodes only (run on
  2026-10-11; `fsck` lists them). After several programs wrote to a
  graph at the same time, run `fsck`
  ([damaged-graphs.md](damaged-graphs.md#several-programs-writing-at-once)).
