# CLI Project Documentation

This directory contains the Haskell CLI implementation for the graph project.

Use the `language-design` skill when designing or changing the semantics of
the Command or Path languages.

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

- **[WebAssembly](docs/wasm.md)** - Reference for the wasm module
  (path parsing, syntax feedback, completion) for the browser extension:
  its JavaScript and Haskell APIs, how to build and test it, and what code
  it compiles must not reach. Its history and decisions are in
  [docs/archive/wasm-plan.md](docs/archive/wasm-plan.md).

- **[Testing Best Practices](docs/Testing.md)** - Guidelines for writing tests, including in-file tests, test utilities, golden tests, and avoiding cyclic imports.

