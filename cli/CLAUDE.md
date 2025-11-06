# CLI Project Documentation

This directory contains the Haskell CLI implementation for the graph project.

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
