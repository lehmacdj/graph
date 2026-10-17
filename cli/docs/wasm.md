# Graph core to WebAssembly

Design and progress tracking for compiling the path language, completion
and graph APIs to WebAssembly, so the browser extension can call them.

Started 2026-10-17. Status: refactoring the native build (items 1 and 2
done); nothing compiled for WebAssembly yet.

## Goal and scope

Compile the `cli` library's path-language parsers, completion engine and
higher-level graph APIs to WebAssembly, and call them from the browser
extension.

- **In scope:** `Models.Path.*`, `Models.PathPattern.*`, the completion
  core (`completionTransitions`, `Models.PathPattern.Suggest`), and graph
  reading and editing through the effect interfaces (`Graph.GraphEditing`,
  `Graph.GraphMetadataEditing`, `Graph.PathPattern.Resolve` / `Execution`).
- **Out of scope:** the REPL (`haskeline`), HTTP fetching, image display,
  running an external editor, macOS file coordination, and the on-disk
  storage interpreters. These stay native-only.
- **Constraint:** the native `ge` build and its tests keep working at every
  step. Each refactor should be worthwhile on its own, even if the
  WebAssembly build never ships.

## Target architecture: hybrid

```
 browser extension
 ┌───────────────────────────────────────────────┐
 │  JS UI ──► graph.wasm                         │
 │            ├─ parsing, syntax feedback (pure) │
 │            ├─ completion core                 │
 │            └─ graph effects ──► JS shim ──────┼──┐
 └───────────────────────────────────────────────┘  │ native messaging
                                                    ▼
                                   native host (ge, on-disk graph)
```

- Parsing and syntax feedback run entirely in WebAssembly: instant, no
  graph data needed.
- Graph reads and writes go through the same effect interfaces as native
  `ge` (`GraphMetadataEditing` and friends). The wasm build interprets them
  by calling JavaScript, which talks to a native messaging host.
- GHC's wasm backend can import async JavaScript functions; the calling
  Haskell thread blocks until the promise resolves, so interpreters keep
  the same shape as the native ones.
- The planned SQLite + operation log storage could later replace the native
  host with an in-browser store (SQLite on OPFS) without changing the
  effect interfaces.

## Current blockers

The target modules are mostly pure, but their imports reach storage,
`haskeline`, HTTP and PCRE. Four entry points accounted for every chain
when traced on 2026-10-17; the first two are fixed (items 1 and 2).
Nothing has been compiled for WebAssembly yet.

| Entry point | What it drags in | Example chain |
| --- | --- | --- |
| ~~`Utils.Testing` (co-located tests)~~ fixed: item 1 | storage layer, `Error.UserError` (HTTP, `haskeline`), macOS file coordination | `Models.Path.Parse` → `Utils.Testing` → `DAL.Serialization` → `DAL.DirectoryFormat` → `DAL.RawGraph` |
| `MyPrelude` re-exports | ~~`haskeline` via `MyPrelude.Effect` (only to detect its Ctrl-C exception)~~ fixed: item 2; PCRE via `MyPrelude.Regex` | `Models.Path.Parse` → `MyPrelude` → `MyPrelude.Effect` |
| Effects defined beside their file-backed interpreter | `DAL.FileSystemOperations.*`, `DAL.Interpreters` | `Graph.PathPattern.Resolve` → `Graph.GraphMetadataEditing` → `MetadataWriteDiff` → `DAL.Interpreters` |
| Completion tied to the REPL | `haskeline` completion type, `forkIO` / `threadDelay`, full app interpreters | `Executable.GraphEditor.Completion` (89 local modules) |

Local modules reached from each entry point (2026-10-17, after items 1
and 2; before them in brackets):

- `Models.PathPattern.Match`: 29 (29); reaches no `haskeline` or `DAL.*`,
  only PCRE through `MyPrelude.Regex`.
- `Models.Path.Parse`: 35 (44); the same: only PCRE.
- `Graph.GraphEditing`: 51 (50) and `Graph.PathPattern.Execution`: 66
  (65); both still reach the storage interpreters (item 4) and, through
  `Error.Warn`, `Error.UserError` and `DAL.Interpreters`, `haskeline`. The
  one more module is `Error.Interrupt`.
- 24 non-Spec modules under `src/` import `Utils.Testing`; it now reaches
  29 local modules and none of storage, `haskeline` or file coordination.

## Decision: co-located tests

**Proposed:** keep tests co-located and split `Utils.Testing` by weight,
with no conditional compilation. Add CPP only if the test libraries fail to
build for WebAssembly.

The storage chain came from two of `Utils.Testing`'s exports:
`withTempGraph` and `withEmptyTempGraph` (which use `initializeGraph`,
`directory-tree` and `copyFile`). `representedByJson`, once listed here,
only needs aeson and stays in `Utils.Testing`. Outside
`*Spec.hs` files, only `Executable.GraphEditor` and
`Executable.GraphEditor.Completion` use them, and both stay native-only.
Moving those helpers to a new `Utils.Testing.TempGraph` cuts the chain from
every target module. There is precedent: `Utils.Testing.External` already
splits off light helpers so `MyPrelude` modules avoid cyclic imports.

| | Split `Utils.Testing` only | CPP-gated tests | Move tests out |
| --- | --- | --- | --- |
| Tests stay next to code | yes | yes | no |
| Edits | `Utils.Testing` + 2 modules + Spec files using the helpers | `#if` around imports, test bodies and export-list entries in ~24 modules | every module with tests |
| Test libraries in the wasm build | tasty, hspec, QuickCheck, tasty-golden must compile for wasm | no | no |
| ormolu | unaffected | limited CPP support; gated regions may go unformatted | unaffected |
| Generated `Spec.hs` (tasty-discover, in `src/`) | unchanged | must be excluded from the wasm build | moves with the tests |
| Risk of rot | none | the gated-off configuration only builds for wasm; CI must build both | none |

Splitting first is a small refactor with no lasting cost, and it may be all
that is needed. If tasty-golden or another test dependency won't compile
for wasm32-wasi, one flag (`-DWASM`, from a cabal flag) gates the test
code. Golden tests read files and wouldn't run under WebAssembly anyway.

To verify: whether the wasm linker drops unused test code from the shipped
binary. If it doesn't, binary size is a reason to gate tests even when
everything compiles.

## Work items

Ordered so each step lands independently and keeps native `ge` green.

1. [x] Split `Utils.Testing`: move `withTempGraph` and
   `withEmptyTempGraph` to `Utils.Testing.TempGraph`.
2. [x] Move the `haskeline` interrupt check out of `MyPrelude.Effect`: it,
   `isInterrupt`, `EndedBySignal` and `fromException`/`fromExceptionVia`
   are now in `Error.Interrupt`, outside `MyPrelude`.
   Only four native-side modules use them. A predicate supplied by the
   REPL would have to reach `fromExceptionVia` in every interpreter that
   catches exceptions, since haskeline throws Ctrl-C at the thread
   wherever it is; the wasm interpreters will need their own catching
   helper or a haskeline-free variant if they use these.
3. [ ] Decide on regex for wasm: cross-compile PCRE with wasi-sdk, or put a
   pure-Haskell engine behind a flag. Document any behaviour differences.
4. [ ] Separate effect definitions from interpreters: `GraphMetadataEditing`
   and siblings into their own modules; `DAL.FileSystemOperations.*`
   imports them, not the reverse.
5. [ ] Give completion a pure core: a function returning plain candidates;
   `Executable.GraphEditor.Completion` adapts it to `haskeline`.
6. [ ] Re-run the import trace; target modules should reach none of
   `haskeline`, `network`/`wreq`, `typed-process`, `System.Posix`, `DAL.*`.
7. [ ] Toolchain: upgrade to GHC 9.12+ (see below).
8. [ ] Add a wasm build (cabal project with ghc-wasm-meta); build the pure
   modules first.
9. [ ] JS-backed interpreters for the graph effects + native messaging
   host.
10. [ ] Extension integration: load the wasm module, expose parse /
    complete / query to the UI.

## Toolchain

- GHC 9.12 or newer is needed for Template Haskell under the wasm backend
  (to confirm). The project is on LTS 22.44 (GHC 9.6) and uses TH in about
  18 places (`effectful-th`, lens, `ghc-hs-meta`).
- ghc-wasm-meta builds with `wasm32-wasi-cabal`, not stack, so the wasm
  build needs its own cabal project and freeze file.
- `effectful-plugin` is a compiler plugin; unknown whether plugins work
  when cross-compiling. Fallback: drop it for wasm and add type
  annotations where inference needs it.
- The wasm runtime is single-threaded: `forkIO` and async exceptions work,
  `pooledForConcurrentlyN` runs without parallelism.
- `System.Directory.Clone` (`clonefile`) is macOS-only; already moot if
  it stays out of the wasm module set.

## Open questions

- Does the extension need to work without a native `ge` installed? If so,
  the in-browser store needs to come sooner.
  - no: the native messenger can be assumed
- Which API does the extension call: the command language, or a narrower
  typed API (resolve path, list children, suggest)?
- Do the test libraries compile for wasm32-wasi?

## Progress log

| Date | Change |
| --- | --- |
| 2026-10-17 | Traced blockers; chose hybrid architecture; proposed splitting `Utils.Testing` over CPP-gated tests. |
| 2026-10-17 | Items 1 and 2: `Utils.Testing.TempGraph` split off; interrupt predicates moved to `Error.Interrupt`. `Models.Path.Parse` and `Models.PathPattern.Match` now reach neither `haskeline` nor `DAL.*`; PCRE is what remains. |
