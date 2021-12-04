[![CI](https://github.com/lehmacdj/graph/actions/workflows/ci.yml/badge.svg)](https://github.com/lehmacdj/graph/actions/workflows/ci.yml)

# graph
This is a library/command line application that manipulates graphs.
It implements a variety of unix-like commands in order to provide the
ability to sort/store files.

The main executable is `ge` you can invoke it with `stack exec ge`.

The main option this provides is entering a REPL where you can use various
commands to manipulate the state of the graph. For now, there is no
documentation built into the REPL, but there is a decent description of what
most of the commands do available in the `Lang.Command`.

View its documentation for additional functionality using `stack exec ge -- --help`.

# Planning
This is the list of things that are small and already broken up into small
enough tasks conceptually to be worth trying to work on. I keep them sorted in
hybrid precedence/dependency order as well as I can.
- switch to `Data.Labels.Generic` for lenses for field accessors
- switch to god module pattern for tests
- include filetypes in graph
  - maybe use UTIs ala Apple if they seem public enough to use in a third party
    tool because it would be nice to have something a little bit more advanced
    than file extensions that can also interop with mime-types etc. eventually
- label edges with nodes instead of string labels
- allow slicing a Graph out of the filesystem both with associated data and
  with minimal metadata only (as two separate functions)
  - some metadata may be needed to follow edges for example
  - api will probably be similar to System.Filesystem.Tree
  - will require generalizing nodes to take a type parameter specifying the
    type of their associated data
- add some initialization logic to the graph
  - this is related to
- make ergonomic changes I need to allow me to take notes more easily using this
  tool. Likely canidates are:
  - implement support for extracting wiki style links from markdown files and
    converting them to wiki style links
  - implement some kind of support for dynamic queries
- migrate from integer NIDs to UUIDs/GUIDs/something similar; there is
  `Graph.Types.BigNID` which is my current candidate for the type to use for
  this purpose.
  - The origin will probably just stay as 0 (or a fully expanded out version
    thereof, because I think it will need to be unique)
  - As a side requirement for this item, we need to mostly obsolesce NIDs from
    user facing interaction in order to make this a viable change, because
    typing giant identifier strings isn't really possible.
  - In order to acomplish this, we need at minimum a way to refer to the origin
    node from anywhere in the graph. Then it will be possible to store aliases
    to locations as necesary off of the origin.

# big features/future wishes:

## Implement a better completion engine
Generalize completion utilizing some of the ideas from this article:
https://www.oilshell.org/blog/2020/01/history-and-completion.html
The gist is to instrument the parser to directly create completion information,
and then utilize that to generate completion.

## Modification/Access Times on Edges
Once our edges are labeled with nodes, we will be able to affix more data to
them. It would be useful to add modification/access times to edges.

## Access Control
Self explanatory. Should be affixed to edges (which are of course themselves
nodes) or possibly just all nodes. Design is very up in the air for this thing.

## Distributed editing/synchronization
There are a lot of open questions here:
- Graph merging/conflict resolution
- Graph history/git like functionality

## Garbage collection
It will be useful to have an algorithm for sorting through a graph and deciding
what data can be thrown out and which data must be saved. Especially for machine
generated data that might take up a lot of space, it will be important to
compact it occasionally.

## teach external tools to use the graph like a filesystem
Requires implementing a user space file system

## more fully featured programming language
Write some more general commands for:
- sequencing,
- parallel execution,
- nondeterministic execution,
- choice

Ideally these can take the form of the same operators we already have
