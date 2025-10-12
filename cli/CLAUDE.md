# MyPrelude Usage

This project uses a custom prelude (`MyPrelude`) built on top of ClassyPrelude with additional utilities.

## Import Rules
- **Always** `import MyPrelude` in modules (never import `Prelude` or `ClassyPrelude` directly)
- The project uses `NoImplicitPrelude` in package.yml
- Only import `ClassyPrelude` within MyPrelude modules themselves

## Module Organization

MyPrelude is composed of several submodules:

- **Collections**: Lens operators, ordered/unordered maps and sets, collection utilities
- **EarlyReturn**: Polysemy effect for early returns in computations
- **Effect**: Polysemy utilities and effect handling
- **Function**: Function composition utilities, natural transformations (`~>`)
- **FunctorClasses**: Re-exports of `Eq1`, `Ord1`, `Show1`
- **IO**: Utilities for stderr output (`eputStr`, `eprint`) and error handling
- **JSON**: Generic JSON encoding/decoding helpers
- **MaybeEither**: Extensive utilities for `Maybe`/`Either` handling, `Validation` type
- **MonadApplicative**: Monadic/applicative combinators
- **Regex**: Type-safe regex with `CheckedRegex` and `[re|...|]` quasiquoter
- **RawStrings**: `[rq|...|]` quasiquoter for raw string literals

## Collections

- **Prefer regular collections** (`Set`, `Map`) over ordered ones (`OSet`, `OMap`)
- **Use ordered collections** when:
  - They're already in use in the codebase
  - Order is semantically important
- Available utilities:
  - everything from `mono-traversable` unprefixed
    - this is the default way to perform operations like conversions between collections, folds on collections, etc.
    - e.g. `mapToList`, `setFromList`, etc.
    - these are all polymorphic and work on arbitrary collections
  - `toSetOf`: Lens-based set construction
  - `mapSet`, `mapOSet`: Type-safe mapping over sets
  - `whenNonNull`: Execute action when collection is non-empty
  - `singletonNN*`: Create non-null singletons
  - `assertSingleton`, `assertMaxOne`: Runtime assertions for collection size
  - `diagonal`, `diagonals`: Fair interleaving for potentially infinite lists
  - `cartesianProduct`, `choices`: Cartesian product operations

## Lens Usage

MyPrelude exports a curated subset of lens operators. When you need additional lens operators:
- **Add them incrementally** to `MyPrelude/Collections.hs` exports
- **Avoid bulk exports** to prevent namespace collisions
- Commonly used operators: `^.`, `^?`, `^..`, `.~`, `%~`, `&`, `over`, `view`, `preview`, `set`

## Regex

- **Use `[re|pattern|]` quasiquoter** for compile-time regex literals
- **Use `compileRegex`** when processing user input or runtime patterns
- `CheckedRegex` ensures patterns are validated at construction
- Use `regexing'` for lens-based regex traversals

## Raw Strings

Use `[rq|...|]` quasiquoter when:
- String contains many escape sequences (e.g., regex patterns, Windows paths)
- String spans multiple lines (e.g., HTML, SQL, XML)

Example:
```haskell
windowsPath = [rq|C:\Windows\System32\user32.dll|]
multiline = [rq|<HTML>
<HEAD>
<TITLE>Title</TITLE>
</HEAD>
</HTML>|]
```

## Maybe/Either Utilities

Rich set of helpers for `Maybe` and `Either`:
- `withJust`, `whenJust`: Execute action when `Just`
- `onNothing`, `whenNothing`: Handle `Nothing` cases
- `unwrapDefaulting`: Unwrap with default value
- `fromJustEx`, `unwrapEx`: Unwrap with error message
- `justIfTrue`: Convert `Bool` to `Maybe`
- `Validation`: Applicative validation that accumulates errors (use when you need to collect multiple errors)

**Note**: Preferred error-handling patterns are TBD - update this section as conventions emerge.

## Effects (Polysemy)

- MyPrelude re-exports core Polysemy modules
- `EarlyReturn` effect for early returns:
  - `returnEarly`: Exit computation with a result
  - `withEarlyReturn`: Run computation with early return capability
  - `unwrap`, `unwrapM`: Unwrap `Maybe` or return early with `Nothing`
- Effect utilities:
  - `handleError`: Handle errors without error in result type
  - `errorToLeft`, `errorToNothing`: Convert errors to values
  - `embedStateful`: Embed stateful functions into `State` effect

**Note**: Specific effect patterns are TBD - update this section as conventions emerge.

## Debugging
`trace...` functions are the best way to check assumptions about what happens at runtime:
- for tracing in monadic contexts: `traceIO`, `traceM`, `traceShowM`
- for tracing in non-monadic contexts: `trace`, `traceShowId`, `traceWith`, `traceShowWith`, `traceShow`, `traceId`
  - note these trace functions need to be forced to be printed:
    - for example, `let x = trace "the answer to life" 42 in ...` won't print until the value x is used, and will never print anything if `x` is never used
    - thus prefer adding traces to arguments of functions you know are being evaluated
- these create eventlog markers and aren't visible in stdout/stderr: `traceMarkerIO`, `traceMarkerM`, `traceMarker`
- **OK to use temporarily** during development
- **NEVER commit** changes with traces
- Traces all have an attached warning to remind you they are temporary only

## Additional Exports

MyPrelude also exports:
- **Comonads**: `Control.Comonad`
- **DeepSeq**: `Control.DeepSeq` for strict evaluation
- **Generic programming**: `Generic`, `Rep`, `Generically`, `Generically1`
- **Type-level programming**: `Type`, `Constraint`, `Coercible`, `coerce`
- **Void**: `Void` and `absurd`
- **CallStack**: `HasCallStack` for better error messages
- **Lift**: Template Haskell lifting

## Utility Functions

- `unconsumed`: Convert any contravariant functor to `f Void`
- `nonNull`: Iso between `NonNull mono` and `mono`
- `minOn`, `maxOn`: Compare by projection
