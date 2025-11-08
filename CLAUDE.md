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

The CLI project uses Stack as its build tool. To set up your development environment:

### Installing Stack

We recommend using ghcup to install Stack:

1. Install ghcup (the Haskell toolchain installer):
   ```bash
   curl --proto '=https' --tlsv1.2 -sSf https://get-ghcup.haskell.org | BOOTSTRAP_HASKELL_NONINTERACTIVE=1 BOOTSTRAP_HASKELL_MINIMAL=1 sh
   ```

2. Source the ghcup environment:
   ```bash
   source ~/.ghcup/env
   ```

3. Install Stack using ghcup:
   ```bash
   ghcup install stack
   ```

4. Add the ghcup environment to your shell configuration (e.g., `~/.bashrc` or `~/.zshrc`):
   ```bash
   echo 'source ~/.ghcup/env' >> ~/.bashrc  # or ~/.zshrc
   ```

### Running Tests

From the `cli/` directory, run:

```bash
STACK_ROOT=~/.stack stack test
```

**Note:** The `STACK_ROOT=~/.stack` prefix is required in web/container environments to avoid permission issues with the default Stack root directory.

On first run, Stack will:
- Download and install GHC (Glasgow Haskell Compiler)
- Download the Hackage package index
- Build all project dependencies
- Build and run the tests

This initial setup can take several minutes. Subsequent runs will be much faster.

### Building the CLI

To build the CLI executable:

```bash
cd cli
STACK_ROOT=~/.stack stack build
```

To run the CLI:

```bash
STACK_ROOT=~/.stack stack exec graph -- <arguments>
```
