# Graph core to WebAssembly

Design and progress tracking for compiling the path language, completion
and graph APIs to WebAssembly, so the browser extension can call them.

Started 2026-10-17. Status: refactoring the native build (items 1, 2, 4
and 5 done); nothing compiled for WebAssembly yet.

## Goal and scope

Compile the `cli` library's path-language parsers, completion engine and
higher-level graph APIs to WebAssembly, and call them from the browser
extension.

- **In scope:** `Models.Path.*`, `Models.PathPattern.*`, the completion
  core (`Models.Completion`; with a graph, `completionTransitions`), the
  syntax feedback (`Utils.Parsing.Problems`), and graph
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
- The same effect interfaces are the seam for the storage rewrite: the
  node files are to be replaced by a database (a triple store with an
  operation log), used through `Graph.GraphEditing`,
  `Graph.GraphMetadataEditing` and their siblings, at least at first. So
  the effects and the logic on them must not depend on `DAL.*`, the file
  system or the node-file format (item 4).

## Current blockers

The target modules are mostly pure, but their imports reach storage,
`haskeline`, HTTP and PCRE. Four entry points accounted for every chain
when traced on 2026-10-17; three are fixed (items 1, 2 and 4), the fourth
for what needs no graph (item 5), and PCRE (item 3) is what the graph logic
still reaches. Nothing has been compiled for WebAssembly yet.

| Entry point | What it drags in | Example chain |
| --- | --- | --- |
| ~~`Utils.Testing` (co-located tests)~~ fixed: item 1 | storage layer, `Error.UserError` (HTTP, `haskeline`), macOS file coordination | `Models.Path.Parse` → `Utils.Testing` → `DAL.Serialization` → `DAL.DirectoryFormat` → `DAL.RawGraph` |
| `MyPrelude` re-exports | ~~`haskeline` via `MyPrelude.Effect` (only to detect its Ctrl-C exception)~~ fixed: item 2; PCRE via `MyPrelude.Regex` | `Models.Path.Parse` → `MyPrelude` → `MyPrelude.Effect` |
| ~~Effects defined beside their file-backed interpreter~~ fixed: item 4 | `DAL.FileSystemOperations.*`, `DAL.Interpreters` | `Graph.PathPattern.Resolve` → `Graph.GraphMetadataEditing` → `MetadataWriteDiff` → `DAL.Interpreters` |
| ~~`Error.UserError` carrying native types~~ fixed: item 4 | HTTP (`http-conduit`, `network`), macOS file coordination, `haskeline` via `Error.Interrupt` | `Graph.PathPattern.Execution` → `Error.UserError` → `Network.HTTP.Conduit` |
| `Effect.IOWrapper.*` effects beside their IO interpreters (not on the path of the graph logic) | HTTP, `typed-process`, `System.Posix`, `directory-tree` | `Graph.Command` → `Effect.IOWrapper.Web` → `Network.Wreq`; `Graph.Import.ByteString` → `Effect.IOWrapper.Web` |
| ~~Completion tied to the REPL~~ fixed for what needs no graph: item 5 | `haskeline` completion type, `forkIO` / `threadDelay`, full app interpreters | `Executable.GraphEditor.Completion` (89 local modules before; the adapter now) |

Local modules reached from each entry point, counting itself (2026-10-17,
after items 1, 2 and 4; after items 1 and 2 in brackets):

- `Models.PathPattern.Match`: 29 (29); reaches no `haskeline` or `DAL.*`,
  only PCRE through `MyPrelude.Regex`.
- `Models.Path.Parse`: 35 (35); the same: only PCRE.
- `Graph.GraphEditing`: 31 (51), `Graph.GraphMetadataEditing`: 29 (47),
  `Graph.PathPattern.Resolve`: 36 (53) and `Graph.PathPattern.Execution`:
  54 (66). None reaches `DAL.*`, `System.Directory`, `System.FilePath`,
  file coordination, `haskeline`, `network`/`http-conduit`/`wreq`,
  `typed-process` or `System.Posix`; what is left of the native libraries
  is PCRE (item 3). They do reach the test libraries through
  `Utils.Testing` (see "Decision: co-located tests") and `random`.
- 24 non-Spec modules under `src/` import `Utils.Testing`; it reaches
  29 local modules and none of storage, `haskeline` or file coordination.
- `Models.Completion`: 40 and `Utils.Parsing.Problems`: 36 (41 together;
  item 5); the same: only PCRE.

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
4. [x] Separate effect definitions from interpreters. Also a prerequisite
   for the storage rewrite (a triple store replacing the node files),
   which is to interpret the same effects. Now:
   - Effects, with only their in-memory interpreters:
     `Graph.GraphMetadataEditing`, `Graph.GraphDataEditing`,
     `Graph.GraphEditing` (and `Graph.Dualizeable`); the error and warning
     types they use, `Error.UserError` and `Error.Warn`.
   - Node-file interpreters, which import the effects:
     `DAL.Interpreters.GraphMetadataEditing` (the transactional cache,
     damage checks), `DAL.Interpreters.GraphDataEditing`,
     `DAL.Interpreters.GraphEditing`, over the storage effects in
     `DAL.FileSystemOperations.*` (moved unchanged; their specs with them).
   - `Error.UserError` no longer imports HTTP, file coordination or
     `haskeline`: `NSErrorException` moved to `Error.FileCoordination` (a
     plain record, re-exported by `System.FileCoordination`); `WebError`
     keeps the HTTP exception as a `SomeException` (shown the same); the
     catching of IO exceptions (`embedCatchingErrors` and friends) moved to
     `Error.Catching`; `notingWhatInterruptLeaves` moved from `Error.Warn`
     to `Error.Interrupt`. Messages are unchanged.
   - Not split: the `Effect.IOWrapper.*` effects (web, editor, file
     system, image display, file types) still sit beside their IO
     interpreters. The graph logic doesn't reach them; `Graph.Command` and
     `Graph.Import.*` do, so they matter if the extension calls the command
     language.
5. [x] Give completion and syntax feedback a pure core, for what needs no
   graph (rescoped 2026-10-17): `Models.Completion` and
   `Utils.Parsing.Problems` (see [The typed API](#the-typed-api)).
   `Executable.GraphEditor.Completion` is now an adapter: it gives the core
   lookups that read the graph on disk and turns its completions into
   haskeline's. REPL behaviour is unchanged.
6. [ ] Re-run the import trace (done for the graph effects and the path
   logic after item 4, see above); target modules should reach none of
   `haskeline`, `network`/`wreq`, `typed-process`, `System.Posix`, `DAL.*`.
7. [ ] Toolchain: upgrade to GHC 9.12+ (see below).
8. [ ] Add a wasm build (cabal project with ghc-wasm-meta); build the pure
   modules first.
9. [ ] JS-backed interpreters for the graph effects + native messaging
   host.
10. [ ] Extension integration: load the wasm module, expose parse /
    complete / query to the UI.

## The typed API

What the first proof of concept in the extension calls (item 5). All of
it is pure and reaches none of `haskeline`, `DAL.*`, HTTP, processes,
`System.Posix` or the macOS bindings; PCRE only through `MyPrelude.Regex`
(item 3).

Parsing and syntax feedback:

- `Models.Path.Parse.parsePath :: Text -> Either ParseErrorBundle'
  ParsedPath`: a whole path, directives kept, as commands take it. The path
  quasiquoter and `Models.PathPattern.Suggest.parseSurfacePath` use it.
- `Utils.Parsing.Problems.syntaxProblems :: ParseErrorBundle' -> NonEmpty
  SyntaxProblem`, with `SyntaxProblem {start, end :: Int, message ::
  Text}`: where each problem is (offsets in code points, end exclusive;
  custom errors use their source range) and the parser's message for it,
  as `errorBundlePretty` words it. It works for any of the parsers, the
  command parser included.

Completion (`Models.Completion`):

- `complete :: Monad m => CompletionLookups m -> Text -> m Completions`:
  given the text before the cursor, `Completions {replaced :: Int,
  candidates :: [Candidate]}`, where each `Candidate {text :: Text,
  finished :: Bool}` replaces the last `replaced` characters.
- `CompletionLookups m {nodesWithPrefix :: Text -> m [NID], labelsAfter ::
  Path -> m [Text]}` is what completion needs from a graph; `withoutGraph`
  has none of it, and `completeWithoutGraph = runIdentity . complete
  withoutGraph`.
- Without a graph it offers the `/` after a term that is not a label (`(a
  & b)`, `@`), and nothing for a path with a directive, one that does not
  parse or one that no command runs (`checkCopies`). It also says which
  word a graph would complete (`replaced`).

A graph-aware layer plugs in by giving `CompletionLookups`: the REPL's
(`graphLookups` in `Executable.GraphEditor.Completion`) globs the node
files for ids and runs `Graph.PathPattern.Execution.completionTransitions`
under the app interpreters for labels. A wasm build would give lookups
that ask the native messaging host, or run `completionTransitions` over
JS-backed `PathReading` interpreters, now that item 4 has separated the
effects from the storage. The order of what is tried (an id after `@`, then the
path) stays in the core either way.

Found while doing it, kept as they were:

- An id is completed after the `@`, which stays, to a candidate with its
  own `@`: the REPL completes `@0` to `@@000000000001`.
- The `/` candidate is finished, so haskeline adds a space after it when it
  is the only one: `(a & b)` completes to `(a & b)/ `.

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
  - a narrow typed API, but made of generic helpers that the command
    language (the REPL, its completion) uses too, not a second
    implementation of parts of it. The first proof of concept does path
    parsing, syntax feedback and the completion that needs no graph;
    completion from graph data waits for the native messaging host.
- Do the test libraries compile for wasm32-wasi?

## Progress log

| Date | Change |
| --- | --- |
| 2026-10-17 | Traced blockers; chose hybrid architecture; proposed splitting `Utils.Testing` over CPP-gated tests. |
| 2026-10-17 | Items 1 and 2: `Utils.Testing.TempGraph` split off; interrupt predicates moved to `Error.Interrupt`. `Models.Path.Parse` and `Models.PathPattern.Match` now reach neither `haskeline` nor `DAL.*`; PCRE is what remains. |
| 2026-10-17 | Item 4: node-file interpreters moved to `DAL.Interpreters.*`; `Error.UserError` and `Error.Warn` freed of HTTP, file coordination and `haskeline`. `Graph.PathPattern.Execution` reaches 54 modules (66), `Graph.GraphEditing` 31 (51), none of storage or the native libraries but PCRE. |
| 2026-10-17 | Item 5, rescoped to what needs no graph: `parsePath`, `Utils.Parsing.Problems` (syntax feedback as data) and `Models.Completion` (completion over `CompletionLookups`); the REPL's completion is an adapter over it. The core reaches 41 local modules, only PCRE of the blockers. |
