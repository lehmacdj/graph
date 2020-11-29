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

# Roadmap
These are the next things I am planning on working on in mostly precedence
order:
- label edges with nodes instead of string labels
- include filetypes in graph
- refactor error handling
- make ergonomic changes I need to allow me to take notes more easily using this
  tool. Likely canidates are:
  - implement support for extracting wiki style links from markdown files and
    converting them to wiki style links
  - implement some kind of support for dynamic queries
- add some initialization logic to the graph and make a concept of root location
  that is sensible
- migrate from integer NIDs to UUIDs/GUIDs/something similar; there is
  `Graph.Types.BigNID` which is my current candidate for the type to use for
  this purpose. As a side requirement for this item, we need to mostly obsolesce
  NIDs from user facing interaction in order to make this a viable change,
  because typing giant identifier strings isn't really possible.

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
