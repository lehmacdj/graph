# Damaged graphs

A graph is a directory of node files. `<id>.json` holds one node: its id
and its edges, the outgoing ones and the incoming ones. Every edge is
therefore recorded twice, among the outgoing edges of its source and among
the incoming edges of its sink. `ge` writes both records, but a write that
was interrupted, a sync that has delivered only some of the files, two
programs that write at the same time, or an edit by hand can leave files
that contradict each other or that can't be used. That is damage. `ge`
detects it where it reads, refuses to edit next to it, and has two commands
for it: `fsck` lists it and `fix` repairs it.

The graph is what the node files say. A file that is not a node file, such
as `<id>.data` for a node that has no `<id>.json`, is no part of the graph:
it is not damage, `fsck` doesn't report it, and `fix` leaves it where it is.

## What counts as damage

| damage | example |
|---|---|
| An edge that only one of its two nodes records | `0.json` has `b` to `@2` among its outgoing edges, `2.json` doesn't have it among its incoming edges (or the other way round) |
| An edge to or from a node that has no file | `0.json` has `b` to `@2`, and there is no `2.json` |
| A file that says it is another node than it is named after | `2.json` holds the id `@3` |
| A file that can't be used as a node | `2.json` is empty, cut off, not JSON, unreadable for its permissions, a directory, or a symbolic link to a file that isn't there |
| No file for the origin | there is no `000000000000.json` |
| A copy that `fix` kept of a node file, without that node file | there is a file `2.json.unparsable` and no `2.json`: it is left over from a deleted node (see `fix`) |

Every message about damage names the edge and the files, for instance

```
the edge @000000000000 -"b"-> @000000000002 is among the outgoing edges in 000000000000.json but there is no 000000000002.json
```

A file that could not be read at all is described in words, followed once
by what the system said, which has the whole path:

```
000000000005.json cannot be read as a node: there is no permission to read it (/graph/000000000005.json: withBinaryFile: permission denied (Permission denied))
```

## Editing and reading next to damage

Nothing but `fsck` reads a file in order to look for damage. A command
compares the node files that it reads anyway, in all of its steps, and the
ones that it is about to write, with each other:

- A command that **changes** something is refused, before it writes
  anything, if those files are damaged: `this change was not written: the
  graph is damaged where it applies (damaged-graph)`, then every finding,
  then a pointer to `fsck` and `fix`. Deleting or writing the data of a node
  is a change like any other, and so is `vi`, which is refused before the
  editor starts. A dry run (`--dry-run`) is refused in the same way.
- A command that only **reads** goes on, and the command line warns once
  for each finding when it is done, however it ends (also if a later
  command of the line fails, an editor exits with an error, or the line is
  interrupted): `warning: the graph is damaged where this command read it
  (damaged-graph)`. If the line is refused for a finding, only the refusal
  names it.
- A file that can't be used as a node is read as a node **without edges**.
  The node is there, since its file is, and other nodes may record edges
  with it, but nothing is known of what its own file recorded: `ls` lists
  it where other nodes lead to it, `cd` and `at` go there, `ls` there lists
  nothing, and nothing is said about the edges that other nodes record with
  it. The command warns of the file like of any other damage, and a change
  that read the file, or that would write it, is refused. This is so for
  every command and for every such file, whatever is under the name
  `<id>.json`: a directory of that name and a symbolic link to a file that
  isn't there are not missing nodes either, and nothing is written over
  the one or through the other. (A symbolic link to a node file is used as
  that file.) `mk --allow-orphans @2` next to such a `2.json` makes nothing,
  since the node is there, and warns.
- `cd` fails if its destination has no node file, and `at` runs nothing if
  one of its targets has none (`at did nothing: a target has no node file
  (damaged-graph)`, with the edge that led there).

So damage next to a node that many commands pass through (the origin, a
hub) blocks edits there until it is repaired, and `fix` is the way to drop
an edge that leads to a node without a file (`rm` is an edit next to
damage): `fix` brings the node back, empty, and `rmnf` then removes it.

`rmnf` doesn't delete a system node (the origin, tags, …), which `ge` needs
to open the graph, just as `mg` never merges one away: `rmnf cannot delete a
system node (the origin and the special nodes such as tags must stay in the
graph)`.

### One command is one decision

Many commands work in several steps: `mg` resolves each of its arguments,
`at` its targets and a `%targets(…)` its path, and only then something is
changed, somewhere else. All the steps of a command are one decision: once
one of them has read past damage of any kind, every change that the command
makes after it is refused before anything is written, with the refusal
above, which names the finding (and no warning for it as well). With

```
0 -b-> 2   2 -c-> 3   0 -h-> 5   0 -i-> 6
```

and a `2.json` that can't be parsed, or none, the path `b/c + h` resolves
past the damage to the one node `@5`, where it is two nodes when `2.json`
is as it should be. So these write nothing and fail, also in a dry run:

```
mg (b/c + h) i
mg i (b/c + h)
mg %targets(b/c + h) i
at (b/c + h) rmnf @
rmnf %targets(h + b/c)
```

What a command runs is part of it: the command that `at` runs at each of
its targets, also if that is several commands in braces. Each command of a
`;` line is a decision of its own, and what failed in one of them is not
written while what the commands before it did stays. In `ge -e`, a refused
or failed command makes the exit code 1. A command that only reads warns
and carries on, as before.

The steps of a command also read for each other: the node files that one
step read are compared with the ones that the later steps read, as the
files that one step read are compared with each other. No file is read for
this. With `0 -b-> 2` recorded in `0.json` only, resolving `b` reads
`0.json`, which has the record, and what then runs at `@2` reads `2.json`,
which doesn't. So these are refused like `rmnf b`, which reads both files
in one step, and write nothing, also in a dry run:

```
at b rmnf @
rmnf %targets(b)
at b mk x
at b vi
```

and `at b ls` lists the edges of `@2` and warns of the edge `b`, once. What
is kept of a file for this is what the command last read or wrote of it, so
a command is never refused for what its own earlier steps changed. Nothing
is kept from one command to the next: `cd b` warns of the edge and moves,
and `rmnf @` on the next line, or after a `;`, then deletes `@2`, since
that command doesn't read `0.json`.

`fix` inside a command starts that command's decision afresh, since it
changes the node files that the steps before it read: the command forgets
those files and the damage that it read past, and what comes after `fix` is
judged by what it reads from then on. With the same lone record,
`at b {ls @0/b ; fix ; mk x}` lists `b`, adds the record to `2.json` and
makes `x` at `@2`; it exits with 0 and warns of the edge as `ls` read it, as
`ls @0/b ; fix` does. Damage that `fix` did not repair is caught again when
a later step reads its files.

**`cd` is not covered**, which the designer knows of. `cd` is a command of
its own, and a read: `cd (b/c + h)` warns and moves to `@5` (it fails for
resolving to two nodes when `2.json` can be read), and a later command
edits where it stands. `ge -e 'cd (b/c + h) ; rmnf @'` deletes `@5`, exits
with 0 and warns of `2.json` when the line is done; in the REPL, `rmnf @`
on the line after that `cd` deletes `@5` without a message.

### In a dry run

`ge --dry-run` writes nothing, and looks for damage as the real run does:
among the node files that the command reads in all of its steps. The files
that a step would have written count as read by the command, as they are,
like the ones that the real run wrote. With

```
0 -a-> 1   0 -e-> 5   5 -m-> 6   6 -k-> 7
```

and `k` recorded in `6.json` only, `at @1 {rmnf @5 ; at @7 mk z}` is refused
at `mk z` in a dry run as in the real run, with exit code 1: deleting `@5`
reads `6.json`, to take `m` out of it, and `mk z` reads `7.json`. (The real
run has deleted `@5` by then, and says so; the dry run has said what it
would delete and write.) `at @1 {rmnf @5 ; ls @7}` warns of `k` in both.

A dry run doesn't make the writes of a command's earlier steps, though, so a
later step of the same command, or a later command of the line, sees the
graph as it was and not as those steps would have left it. A dry run can
therefore end differently from the real run where one step builds on what
another one wrote. In particular `fix` only lists its repairs in a dry run,
so an edit after it in the same command still meets the damage: with
`0 -b-> 2` recorded in `0.json` only, `at b {fix ; mk x}` repairs the edge
and makes `x` at `@2`, and `ge --dry-run -e 'at b {fix ; mk x}'` lists the
repair and then refuses `mk x` for that edge, with exit code 1.

### What this does not catch

Detection has no extra reads, so damage in a file that a command doesn't
read goes unnoticed until `fsck`: with `0 -b-> 2` recorded in `0.json`
only, `at @2 mk x` and `at @2 vi` go through, since no step of them reads
`0.json` (`at b mk x` and `at b vi` read it to resolve `b`, and are
refused). And a command can make **existing** damage worse, or change its
form, through a file that it never read. Two classes of this are accepted,
since no undamaged graph gets damaged this way:

1. A node is deleted while a file that the command didn't read holds a lone
   record with it. The record then leads to a node without a file.

   | graph | command | left behind |
   |---|---|---|
   | only `1.json` records `1 -b-> 2`; `0 -a-> 2` | `rmnf a` | `1.json` keeps `b` to `@2`, and `2.json` is gone |
   | only `1.json` records `2 -b-> 1`; `0 -a-> 2` | `rmnf a` | `1.json` keeps `b` from `@2` |
   | only `1.json` records `1 -b-> 2`; `0 -a-> 2`, `0 -c-> 3` | `mg c a` | `1.json` keeps `b` to `@2`; the merged node doesn't get `b` |
   | only `0.json` records `0 -b-> 2` | `at @2 rmnf @2` | `0.json` keeps `b` to `@2` |

2. A node file is created at an id that a file which the command didn't
   read records an edge with. The node that was lost then looks as if it
   were there: `fsck` reports a one-sided edge where it would have reported
   a missing file, and `fix` adds the record instead of saying that the
   node's other edges and its data are lost.

   | graph | command | written |
   |---|---|---|
   | `2 -d-> 1` recorded in `1.json`, no `2.json` | `mk x/@2` | `2.json` with the new records only |
   | only `1.json` records `1 -d-> 2`, no `2.json` | `tag x/@2 k` | `2.json` with the new records only |
   | the same | `mk --allow-orphans @2` | `2.json` without edges |
   | no file for the tags node, which a node `5` records `foo` from, and the system node doesn't record `tags` | any command (opening the graph creates the tags node) | the tags file with the `tags` edge only, announced as a created system node |

Likewise `mk b/x` adds a second `b` edge when only the file of the node
that the first one leads to records it: nothing leads there from where `mk`
starts.

### While a graph is being synced

A sync delivers files one at a time, so for a while an edge can be recorded
in the file that has arrived and not in the one that hasn't. During that
time edits next to such an edge are refused, and they go through again once
the other file is there. `fix` must not be run then: it adds the missing
record, which is right for an edge that is being added and wrong for one
whose **deletion** has half arrived (it comes back). Wait for the sync to
finish and run `fsck` first.

### Several programs writing at once

A change is written file by file, and `ge` has no lock on the graph as a
whole. One `ge` alone doesn't damage a graph, but two that write to the same
graph at the same time can: both read a node file, each adds its own record
to what it read, and the one that writes last drops the record of the other,
whose edge is then recorded at its other node only. It needs no shared
target for this, since every `mk` adds an edge from the node of the current
day (under `import-dates`) to the node it makes. Three REPLs that each ran
`mk w/n/m ; rm w/n/m` 250 times on one graph left between one and a few
hundred such one-sided edges, and three loops of sixty `ge -e` commands
each, which start a new `ge` for every command, left dozens: it takes
nothing that a REPL carries from one command to the next. A change that
finds a node file other than it was when the change read it is refused
(`The following nodes were changed before trying to write the diff`), which
catches some of it, not all. Two programs that write one node file at the
same time can also leave a file that can't be parsed: one run of three
REPLs left the node of the current day cut off (`… cannot be read as a
node: … endOfInput`) and no one-sided edge. After several programs wrote to
a graph at the same time, run `fsck`.

A command of several steps compares what an earlier step read with what a
later one reads, and reads no file again for it. So an edge that another
program adds or removes between two steps of a command, at a node that the
first of them had read, looks like a one-sided edge to that command: it is
refused (or warns, if it only reads) although the files agree by then.
Nothing is written, and the command goes through when it is run again.

## `fsck`

`fsck` reads every node file once, as it is, and lists all the damage of
the kinds above; nothing is written. It needs no open graph: it runs when
the origin or a system node is missing or unreadable. `ge -e fsck` exits
with 1 if it found damage and with 0 if not. Files named like copies that a
sync left (`<id> 2.json`) are ignored.

Which files there are is what the directory lists: an id that differs from
the name of a file only in letter case has no file, also where the volume
would open that file for it, and a symbolic link to a file that isn't there
is a file that can't be read.

### While the graph is being changed

Reading every file takes a while, and a file that another `ge` or a sync
writes meanwhile can look like damage: a node that was created after the
directory was listed looks like a missing file, and an edge of which one
file was read before it was added and the other one after looks like an edge
that one node records. So before `fsck` reports, it reads the files of its
findings again, and what it reports is what its **latest** reading of the
files shows:

- a finding that is gone is dropped;
- damage that has changed its form meanwhile (the file that lacked a record
  was deleted, a file that didn't parse was cut off elsewhere) is reported in
  its new form;
- `no damage found` is only said if the latest reading of every file shows
  none;
- if files changed between the readings, the first line says so: `node files
  changed while fsck was reading them; what follows is what its latest
  reading of them shows`.

`fsck` reads the files of its findings a second time right away. If that
finds the same damage, it is reported. While a reading finds other damage
than the one before it, something may be half way through writing, and
`fsck` reads the files again after a pause, up to five times. This makes it
up to a second and a half slower on a graph that is being written, and no
slower on one that holds still.

That files changed is only ever told from two readings of the node files
that differ: in what one of them held, or in which of them the directory
lists. No clock is asked, and no other file counts. So a node file or a
directory that is dated in the future, or a file that is no node file and
comes and goes in the directory (a file of something else, the data of a
node, also of a damaged one), neither makes `fsck` wait nor say that files
changed.

What this leaves: a node file that was created after `fsck` listed the
directory is not read, so damage in it is not reported; and a change that
is half written both times that `fsck` reads its files (the second time
comes right after the first, unless the readings differ) is reported as
damage, without the first line.
A report made while the first line says that files changed is a report
about a graph in motion: run `fsck` again when nothing writes to the graph.

## `fix`

`fix` scans like `fsck` (once, without reading files again), lists what it
will do, and then does it. `fix --dry-run` only lists. It deletes nothing
but a file that is named as `fix` names the copies it keeps
(`<id>.json.unparsable`, `<id>.json.unparsable-2`, `-3`, …) while there is
no `<id>.json`, in a directory that looks like a graph (see below): it takes
such a file for the copy that an earlier `fix` kept of the file of a node
that was deleted since. So don't give a file of your own such a name in a
graph. Nothing else is taken for a kept copy, neither by `fix` nor by
`fsck` nor when a node is deleted: not a directory or a symbolic link of
such a name, and no file with another number than `fix` gives (`-0`, `-1`,
`-02`) or with anything after the name (`.txt`, `-old`). Those are not
reported and never removed.

| damage | repair |
|---|---|
| an edge that one node records | the other record is **added**, whichever one is missing (the edge can be removed afterwards; a record that was dropped could not be brought back) |
| an edge with a node that has no file | the node file is **rebuilt** from every record that other nodes have of the node; the edges that only the lost file recorded are lost, and so is the node's data unless its data file is still there |
| a file whose content is not a node | a copy of it is kept as `<id>.json.unparsable` (`-2`, `-3`, … if something has that name: a file, a directory, a symbolic link, or a file whose name differs only in letter case where the volume takes the two for one name), and the node file is then rebuilt in the same way, empty if no node records an edge with it |
| a copy that `fix` kept, without the node file | **removed**: it is left over from a node that was deleted. No node is ever made from a kept copy |
| a file with another id, whose edges the other nodes record under the file's name | the id in the file is corrected |
| no file for the origin | rebuilt like any other node |

What `fix` does not repair, it lists under `not repaired:` with the reason:
a file with another id whose edges other nodes record under **that** id may
be a copy of that node's file or a misnamed file. Neither it, nor the node
it says it is, nor any edge with them is touched: rename the file or correct
the id by hand.

`fix` changes nothing at all

- in a directory that doesn't look like a graph: at least one file must be a
  node file that says the id it is named after, whatever else the directory
  holds. (Any file named with twelve letters and digits and `.json` is named
  like a node file, and anyone can name a file `<id>.json.unparsable`, so a
  graph in which no node file can be read any more is refused like a
  directory of something else: `fix did nothing: no file in … is a node file
  that says the id it is named after, so it doesn't look like a graph`.)
  `fsck` calls nothing left over from a deleted node there either: it says
  that the origin has no file, and what it finds in the files that are named
  like node files.
- if a node file is there but **can't be read at all** (its permissions, a
  directory of that name, a symbolic link to a file that isn't there). Such
  a file may be a good node file that holds any id and any edge, so no
  repair is known to be right without it: it may be the very node that
  `fix` would rebuild, under another name. `fix did nothing: it can't read
  every node file, …` names each such file once, with the reason and what
  to do about it (make it readable; move the directory away; remove the
  link).
- if a file that it would replace, or that a repair goes by, is no longer as
  it was scanned (`fix wrote nothing: … changed while fix was looking at the
  graph; run fix again`);
- if a file that it would write can't be written;
- if a node file that it would create opens as another file although the
  directory doesn't list it, as on a volume that takes names that differ
  only in letter case for one name: writing it would replace that file.
  `ge` can't repair this on such a volume, and says so: `fix` can't rebuild
  the node, and `rm` and `rmnf` of the edges that lead to it are refused,
  like every edit next to damage. What is left is to run `fix` on a copy of
  the graph on a volume that tells such names apart, or to take those edges
  out of the node files by hand.

If a write fails all the same, `fix` stops and says which file and why. No
node is ever without its file meanwhile: `fix` first writes the copy of a
file that is not a node, and replaces the file only then. If the copy can't
be written, the file is as it was (`<id>.json is as it was`); if the new
file can't be written, what the old one held is in the copy.

Deleting a node (`rmnf`, `mg`) deletes the copies that `fix` kept of its
file as well: `<id>.json.unparsable`, `<id>.json.unparsable-2`, and so on,
which are tried in this order until one is not there or is no file (a
directory, a symbolic link), and no file whose name only starts like these
(`<id>.json.unparsable.txt`). A dry run lists
them. The directory is not listed for this, which would make deleting a
node as slow as the graph is big, so a copy under a later name than a
missing one is not found. A copy that is not found, or that can't be
removed, is left over: the command's change stands (`warning: the node was
deleted, but <id>.json.unparsable, which fix had kept of its file, could
not be removed: …`), `fsck` reports the copy (`<id>.json.unparsable is left
over from a deleted node: …`), and `fix` removes it. Take what you need
from an `.unparsable` file before you delete its node.

`fix` reads the files again that its repairs go by before it writes, not
the whole graph. A record that another program adds after the scan is not
in the repair: with `1 -x-> M` and `3 -y-> M` recorded and no `M.json`, a
`fix` during which `5.json` gains `z` to `M` rebuilds `M.json` without `z`
from `5`, and exits with 0. The next `fix` adds it. After a `fix` during
which other programs wrote to the graph, run `fsck`.

`ge -e fix` exits with 1 if damage is left or nothing was done, and with 0
otherwise. Like `fsck` it needs no open graph, and neither stops a `;`
line when it finds or leaves damage: `ge -e 'fsck ; fix'` lists the damage,
repairs it, and exits with 1 because `fsck` found some; `ge -e 'fix ; ls'`
repairs a graph that can't be opened and then lists it.

In a dry run of `ge` (`ge --dry-run -e fix`), `fix` lists its repairs, makes
its checks, and says what it would do: `fix would write nothing: …`. Nothing
is repaired then, so what comes after `fix` still meets the damage (see "In
a dry run" above).

## Opening the graph

Every other command needs the origin and the system nodes (tags, import
dates, …). The REPL opens the graph before the first prompt, and `ge -e`
before the first command that needs it:

- If the origin has no file, the command fails with the reason and `the
  graph could not be opened (cannot-open-graph): only fsck and fix can run
  until this is repaired`; the REPL starts anyway and goes on normally after
  `fix`.
- A system node or an edge to one that is missing is added, also by a
  command that only reads, and also by a REPL in which no command is run.
  That is said: `warning: created the system nodes that had no file: …`,
  `warning: added the system edges that no node recorded: …`. A graph that
  has nodes and none of the system nodes (made by an old `ge`) gets all of
  them this way, with that warning; only a new graph, which has no node file
  but perhaps the origin's, gets them silently. In a dry run nothing is
  added and the warnings say `would create …` and `would add …`.
- If the graph is damaged where they go, they are not added and the graph is
  not opened (`a system node or an edge to one is missing and was not
  added: …`).
- An origin or a system node whose file can't be used as a node (also one
  that is a directory, or a symbolic link to a file that isn't there) is a
  node without edges like any other such file: the graph opens, with the
  warning, as long as nothing has to be added.

Opening the graph is no part of a command's decision (see above): the
damage that it reads past is warned of, and the commands change what they
would have changed in a REPL, which opens the graph before any command.
