# graph
This project is an attempt to construct a more expressive datastructure than a filesystem for organizing data.
It allows users to organize arbitrary binary data in a graph structure with labeled links to other data.

# Subprojects

## core
Core is the main implementation of the graph. 
It is implemented as a Haskell library, and comes with a few executables which allow editing graphs using a command line interface.

## view
This will be an implementation of an iOS graph that interops with the same graph format and displays it. 
It is still under development.
