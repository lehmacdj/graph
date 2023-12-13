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
