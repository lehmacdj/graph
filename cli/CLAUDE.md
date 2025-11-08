# CLI Project Documentation

This directory contains the Haskell CLI implementation for the graph project.

## Setting Up Haskell Development Environment

### Installing Stack

The project uses Stack as its build tool. To install Stack, we recommend using ghcup:

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
stack test
```

Note: On first run, Stack will:
- Download and install GHC (Glasgow Haskell Compiler)
- Download the Hackage package index
- Build all project dependencies
- Build and run the tests

This initial setup can take several minutes. Subsequent runs will be much faster.

#### Permission Issues

If you encounter permission errors like "Preventing creation of Stack root", you may need to set a custom `STACK_ROOT`:

```bash
STACK_ROOT=~/.stack stack test
```

## Key Documentation

- **[MyPrelude Usage](docs/MyPrelude.md)** - Custom prelude built on ClassyPrelude with additional utilities. Read this first to understand import rules, collections, effects, and common patterns.

- **[Testing Best Practices](docs/Testing.md)** - Guidelines for writing tests, including in-file tests, test utilities, golden tests, and avoiding cyclic imports.

## Quick Reference

### Imports
Always `import MyPrelude` in modules (never `Prelude` or `ClassyPrelude` directly).

### Collections
Prefer regular `Set`/`Map` over ordered `OSet`/`OMap` unless order is semantically important.

### Testing
- Prefer in-file tests placed directly alongside implementation
- Use `Utils.Testing.External` in MyPrelude modules to avoid cyclic imports
- Use `Utils.Testing` in application code
- Create helper functions for readable tests (e.g., `parsesTo`, `formatsTo`)

### Effects
MyPrelude re-exports Polysemy with utilities for early returns, error handling, and state management.
