# CLI Project Documentation

This directory contains the Haskell CLI implementation for the graph project.

For setup instructions (installing Stack, running tests), see [docs/claude-code-web.md](docs/claude-code-web.md).

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

## Code Quality

### HLint

HLint is a linting tool that suggests improvements to Haskell code. The project is configured with a `.hlint.yaml` file at the repository root, and a GitHub Actions workflow enforces that all code passes HLint without warnings.

**Installing HLint:**
```bash
# Using Stack (recommended for this project)
stack install hlint

# Or using cabal
cabal install hlint
```

**Running HLint locally:**
```bash
# Check all Haskell files in the cli directory
hlint cli/

# Check a specific file
hlint cli/src/Graph/Command.hs

# Apply suggestions automatically (use with caution)
hlint cli/ --refactor --refactor-options="-i"
```

**Common workflow:**
1. Run `hlint cli/` before committing your changes
2. Review the suggestions and apply fixes manually or using `--refactor`
3. The CI will fail if any HLint warnings remain

**Note:** The project uses custom HLint configuration in `.hlint.yaml` which includes special handling for TypeApplications and RankNTypes extensions.
