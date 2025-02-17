[![CI](https://github.com/lehmacdj/graph/actions/workflows/ci.yml/badge.svg)](https://github.com/lehmacdj/graph/actions/workflows/ci.yml)

# graph
This project is an attempt to construct a more expressive datastructure than a filesystem tree for organizing data. It allows users to organize arbitrary binary data in a graph structure with labeled links to other data. The basic format is extremely simple, for each node in the graph with nid `n` there are two files `n`.json and `n`.data (`n`.data may not exist if there is no data associated with a node, which can occur if the node is metadata only acting kind of like a directory).

# Subprojects

## cli
The cli is the original implementation of the graph. It is implemented as a Haskell library, and comes with a few executables which allow editing graphs using a command line interface.

Historical:
- I used to support an `Extensibility` module with an `exec` command that allowed running Haskell scripts.
  - It ran very slowly, and was a maintenance burden whenever rearranging/rearchitecting things so I removed it
  - The last version of it in the git history is https://github.com/lehmacdj/graph/blob/2c837dbc6c867c71b8d236d61cf38690ef424af3/cli/src/Utils/Extensibility.hs#L1-L9

## Nodal
This will be an implementation of an iOS graph that interops with the same graph format and displays it and has limited support for editing it. It is still under development.
