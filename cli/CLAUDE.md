# CLI Project Documentation

This directory contains the Haskell CLI implementation for the graph project.

For setup instructions (installing Stack, running tests), see [docs/claude-code-web.md](docs/claude-code-web.md).

## Key Documentation

- **[MyPrelude Usage](docs/MyPrelude.md)** - Custom prelude built on ClassyPrelude with additional utilities. Read this first to understand import rules, collections, effects, and common patterns.

- **[The path language](docs/path-language.md)** - The surface language:
  syntax, what each operator means, the product rule and joins, unions,
  refused and warned paths, and the laws. **[Commands and
  paths](docs/commands.md)** - What each command does with a path,
  including `mk`, the settings, and what happens when a command fails.

- **[Route patterns](docs/path-route-ir.md)** - For changing the compiler:
  how paths compile to route patterns, how resolution, construction
  (`mk`: the shapes it refuses and the one walk it makes) and edge
  selection work, and which test pins which law.
  **[Limits on what a path costs](docs/path-cost-limits.md)** - Why nothing
  limits what a query compiles to, what was tried, and what is still
  bounded (`mk`'s size estimate and its limit on walks).

- **[Damaged graphs](docs/damaged-graphs.md)** - What counts as damage to a
  graph's node files, how commands refuse to edit and warn when reading next
  to it, what `fsck` reports and what `fix` repairs.

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
MyPrelude re-exports effectful with utilities for early returns and error
handling. The `Input` effect lives in `Effect.Input` (not re-exported from
MyPrelude); dynamically dispatched `State`/`Reader` come from
`Effectful.State.Dynamic`/`Effectful.Reader.Dynamic`.
