# Graph Project Documentation

This repository contains multiple implementations of the graph data structure for organizing data in a more expressive way than traditional filesystem trees.

## Repository Structure

### cli/
The original Haskell CLI implementation of the graph. This is a Haskell library with executables that allow editing graphs using a command line interface.

See [cli/CLAUDE.md](cli/CLAUDE.md) for CLI-specific documentation.

### Nodal/
An iOS implementation of the graph that interoperates with the same graph format. Provides visualization and limited editing support. Still under development.

### browser-extension/
A browser extension for graph-related functionality.

### examples/
Example graphs and usage patterns.

### scripts/
Utility scripts for working with the repository.

## Setting Up Haskell Development Environment

For instructions on setting up Stack and running tests in web/container environments, see [cli/docs/claude-code-web.md](cli/docs/claude-code-web.md).
