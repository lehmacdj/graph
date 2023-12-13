[![CI](https://github.com/lehmacdj/graph/actions/workflows/ci.yml/badge.svg)](https://github.com/lehmacdj/graph/actions/workflows/ci.yml)

# graph
This project is an attempt to construct a more expressive datastructure than a filesystem tree for organizing data. It allows users to organize arbitrary binary data in a graph structure with labeled links to other data. The basic format is extremely simple, for each node in the graph with nid `n` there are two files `n`.json and `n`.data (`n`.data may not exist if there is no data associated with a node, which can occur if the node is metadata only acting kind of like a directory).

# Subprojects

## cli
Core is the original/cli implementation of the graph. It is implemented as a Haskell library, and comes with a few executables which allow editing graphs using a command line interface.

## Nodal
This will be an implementation of an iOS graph that interops with the same graph format and displays it and has limited support for editing it. It is still under development.
