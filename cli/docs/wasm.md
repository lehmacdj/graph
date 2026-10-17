# Graph core to WebAssembly

Design and progress tracking for compiling the path language, completion
and graph APIs to WebAssembly, so the browser extension can call them.

Started 2026-10-17. Status: items 1–5, 7 and 8 done: parsing, syntax
feedback and completion without a graph build for WebAssembly and are
callable from JavaScript ([The wasm build](#the-wasm-build)).

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
still reaches. What needs no graph builds for WebAssembly (item 8).

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
- `Models.Completion`: 42 and `Utils.Parsing.Problems`: 38 (43 together,
  with the command parser for the names of commands and for the errors of
  a command line; item 5); the same: only PCRE.

## Decision: co-located tests

**Decided:** keep tests co-located and split `Utils.Testing` by weight,
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

Verified 2026-10-17: the test libraries (tasty, hspec, QuickCheck,
tasty-golden, tasty-hunit) build and run under wasm, and the linker drops
unused test code entirely (a reactor built from a module with co-located
tests is the same size as one without them). No CPP is needed. Only the
generated `src/Spec.hs` must stay out of the wasm build: tasty-discover runs
as a native preprocessor.

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
3. [x] Regex for wasm, decided 2026-10-17: cross-compile PCRE 8.45 for
   wasm32-wasi and keep `pcre-light` / `lens-regex-pcre` and
   `MyPrelude.Regex` unchanged. Behaviour is identical to native (13
   checks, quasiquoters included). The build needs both `libpcre.a` and a
   `libpcre.so` (see Toolchain), set up in item 8.
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
   haskeline's. The refactor itself left the REPL's behaviour unchanged;
   the follow-ups after it changed it on purpose: an id completes with one
   `@`, no space follows the `/` candidate, TAB where a command begins (an
   empty line, a line's first word, after `;`, `{` or `at` and its path)
   offers the names of commands instead of labels, a `%` that starts a
   term offers the names of directives, and `:q`/`:quit` quit with spaces
   around them (completing `:quit` leaves one after it).
6. [ ] Re-run the import trace (done for the graph effects and the path
   logic after item 4, see above); target modules should reach none of
   `haskeline`, `network`/`wreq`, `typed-process`, `System.Posix`, `DAL.*`.
7. [x] Toolchain: upgrade to LTS 24.63 (GHC 9.10.3, the same compiler as
   the wasm 9.10 flavour) and drop `effectful-plugin` (see Toolchain).
   - Without the plugin, effect types it inferred are written out: 168
     type applications in 21 modules (116 of them `@Text` on the
     `GraphMetadataEditing'` operations in
     `DAL.Interpreters.GraphEditingSpec`), and `AllowAmbiguousTypes` in
     the 5 modules whose definitions mention a type only in a constraint
     (`Graph.GraphMetadataEditing`, `Graph.GraphDataEditing`,
     `Graph.Dualizeable`, `Error.Interrupt`, `MyPrelude.EarlyReturn`).
   - `conduit-extra`, `html-conduit` and `xml-conduit` were removed: nothing
     used them.
   - The upgrade: the generic error helpers (`mapError`, `untry`,
     `withEarlyReturn`, `fromExceptionVia`) throw with effectful 2.6's
     `throwError_`, which needs no `Show` and behaves as `throwError` did;
     `Utils.Testing` hides QuickCheck 2.15's new `Witness`, `witness` and
     `witnesses`, which clashed with `Models.PathPattern.Match`'s; the
     uncaught-exception message shows the exception without the backtrace
     GHC 9.10 adds to `displayException`. aeson 2.2 words its parse errors
     differently, and an unparsable node file is reported in its words
     (`Unexpected "not json", expecting record key literal or }` where it
     said `Error in $: object key > 34: Failed reading: satisfy`); the
     golden files say so. `extra-deps`: `vary` and `megaparsec` are in the
     snapshot now; `ixset-typed` is not, so it is pinned (0.5.1.1); the
     haskeline and prettyprinter forks and `inline-c-objc` stay.
8. [x] Add a wasm build (cabal project with ghc-wasm-meta); build the pure
   modules first. Done for parsing, syntax feedback and completion without
   a graph: see [The wasm build](#the-wasm-build).
9. [ ] JS-backed interpreters for the graph effects + native messaging
   host.
10. [ ] Extension integration: load the wasm module, expose parse /
    complete / query to the UI.

## The typed API

What the first proof of concept in the extension calls (item 5). All of
it is pure and reaches none of `haskeline`, `DAL.*`, HTTP, processes,
`System.Posix` or the macOS bindings; PCRE only through `MyPrelude.Regex`
(item 3).

Offsets (`Completions.replaced`, `SyntaxProblem.start` and `end`) count
code points, as `Text` does. The wasm export layer (item 8) converts them
to UTF-16 code units for JavaScript, whose strings are indexed by those
(decided 2026-10-17).

Parsing and syntax feedback:

- `Models.Path.Parse.parsePath :: Text -> Either ParseErrorBundle'
  ParsedPath`: a whole path, directives kept, as commands take it. The path
  quasiquoter and `Models.PathPattern.Suggest.parseSurfacePath` use it.
- `Utils.Parsing.Problems.syntaxProblems :: ParseErrorBundle' -> NonEmpty
  SyntaxProblem`, with `SyntaxProblem {start, end :: Int, message ::
  Text}`: where each problem is (offsets in code points, end exclusive;
  custom errors use their source range) and the parser's message for it,
  as `errorBundlePretty` words it. It takes the errors of any parser that
  returns a `ParseErrorBundle'`: `parsePath`, or
  `Models.Command.Parse.parseCommandLine :: Text -> Either
  ParseErrorBundle' Command` for a whole command line (what
  `parseCommand` parses with, the REPL's and `ge -e`'s parser).

Completion (`Models.Completion`):

- `complete :: Monad m => CompletionLookups m -> Text -> m Completions`:
  given the text before the cursor, `Completions {replaced :: Int,
  candidates :: [Candidate]}`, where each `Candidate {text :: Text,
  finished :: Bool}` replaces the last `replaced` characters (a caller may
  add a space after the only candidate that is finished).
  `completeCommandLine` takes the names of the commands as well, for a
  command line with more of them (the REPL adds `:q` and `:quit`).
- `CompletionLookups m {nodesWithPrefix :: Text -> m [NID], labelsAfter ::
  Path -> m [Text]}` is what completion needs from a graph; `withoutGraph`
  has none of it, and `completeWithoutGraph = runIdentity . complete
  withoutGraph`.
- Without a graph it offers:
  - the names of commands where a command begins: at the start, after a
    `;` or `{`, or after `at` and its path (`Models.Command.Parse.
    startsCommand`, which reads the text with the command parser).
    Finished. The names are `commandNames`, from `commandSyntaxes`, the
    table of each command's names and the parser of what follows them,
    which `pCommandTerm` is made of.
  - the names after a `%` that starts a term (`%last`, `%never`,
    `%history(`, `%targets(`, `%override(`): where a label typed instead
    would be a label of its own, as path completion reads the text. So
    none in a label, a quote, a regex, a URL or a path with a directive
    (a directive's arguments included). The names are `Models.Path.Parse.
    percentNames`, from `namedDirectives`, the table that `pDirective`
    parses them with, and `%never`. A directive that takes arguments comes
    with its `(`. Not finished: a `/` or the arguments follow. `%{`
    (splices) is not offered: commands refuse it.
  - the `/` after a term that is not a label (`(a & b)`, `@`), not
    finished;
  - nothing for a path with a directive, one that does not parse or one
    that no command runs (`checkCopies`). It also says which word a graph
    would complete (`replaced`).
- An id is completed with its `@`, which replaces the typed one: `@0`
  completes to `@000000000001`.

A graph-aware layer plugs in by giving `CompletionLookups`: the REPL's
(`graphLookups` in `Executable.GraphEditor.Completion`) globs the node
files for ids and runs `Graph.PathPattern.Execution.completionTransitions`
under the app interpreters for labels. A wasm build would give lookups
that ask the native messaging host, or run `completionTransitions` over
JS-backed `PathReading` interpreters, now that item 4 has separated the
effects from the storage. The order of what is tried (an id after `@`, then the
path) stays in the core either way.

The two bugs found while doing item 5 are fixed: the doubled `@` of a
completed id, and the space that haskeline added after the `/` candidate.

Open: a label that has to be quoted in a path (one with characters that
an unquoted label can't have, such as a space or `!`) is offered as it
is, unquoted, so taking it gives a path that doesn't parse or means
something else; and nothing is completed inside an open quote (`ls "he`).
Neither did the REPL's completion before item 5: its quoting helpers
were never called (removed).

## The wasm build

`cli/wasm/` builds a WebAssembly reactor of `parsePath`,
`syntaxProblems` and `completeWithoutGraph`, with an export layer for
JavaScript (item 8). From a clean checkout, with ghc-wasm-meta's 9.10
flavour installed (`~/.ghc-wasm`, or `wasm32-wasi-ghc` on `PATH`):

```
cli/wasm/build.sh        # [--no-test]; [--freeze] after changing the LTS
```

It writes `cli/wasm/dist/web/`: `graph.wasm`, `ghc_wasm_jsffi.js` (the
glue GHC's post-linker generates) and `graph.mjs` (the loader), then runs
the node tests (`cli/wasm/test/graph.test.mjs`). `dist/` and
`dist-newstyle/` are ignored by git.

Layout:

- `graph-wasm.cabal`: one executable, a reactor (`-no-hs-main
  -optl-mexec-model=reactor`, exports named with `-optl-Wl,--export=`),
  whose sources are `wasm/src` (only `Main`, the JSFFI exports) and the
  native library's `../src`, so no source is copied. Its generated block
  is written by `sync-cabal.py`, which `build.sh` runs: the extensions,
  `ghc-options` and `cpp-options` from `package.yaml`, and the modules that
  `wasm/src` imports, transitively (read from the import lines; an import
  no source directory has is a package's). A new import in the closure
  updates the list on the next build (commit the change); `--check` only
  reports. `build-depends` is by hand: a package missing there fails the
  build by name.
- Versions: `cabal.project.freeze`, generated by `build.sh --freeze` from
  Stackage's `cabal.config` for `stack.yaml`'s resolver, the Hackage
  extra-deps of `stack.yaml` (`ixset-typed`) and an `index-state`.
  `build.sh` refuses to build when the freeze names another resolver than
  `stack.yaml`. `allow-newer: all`, as `stack.yaml` has. The git forks in
  `stack.yaml` (haskeline, prettyprinter-ansi-terminal) are not used by
  the wasm build.
- PCRE: `build.sh` downloads PCRE 8.45 from SourceForge, checks its
  SHA-256, builds the library alone (`pcretest` needs `setrlimit`, which
  WASI lacks) into `dist/pcre/prefix`, links `libpcre.so` from it (see
  Toolchain), and writes `dist/pcre.project`, which `cabal.project`
  imports, pointing `pcre-light` at it.
- `Wasm.Api` (in `src/`, the native library): the export layer's logic,
  as JSON text, so the native tests cover it; `wasm/src/Main.hs` only
  converts strings and catches exceptions.

The export API, as `graph.mjs` gives it:

```js
import { load } from "./graph.mjs";
const graph = await load({ wasm, wasi }); // bytes, Module or Response;
                                          // a WASI with args: ["graph"]
graph.parsePath("a/)");
// {ok: false, problems: [{start: 2, end: 3,
//   message: "unexpected ')'\nexpecting ..."}]}; {ok: true} if it parses
graph.complete("ls %la");
// {replaced: 3, candidates: [{text: "%last", finished: false}]}
```

- Offsets are in UTF-16 code units (decided 2026-10-17): the core counts
  code points, and `Wasm.Api` converts every `start`, `end` and
  `replaced` it returns, so `text.slice(start, end)` is a problem's
  stretch and `before.slice(0, before.length - replaced) + candidate.text`
  takes a candidate. An emoji is not a label character (a label with one
  is quoted); a letter outside the Basic Multilingual Plane is.
- The wasm exports are synchronous (`"parsePath sync"`), returning JSON
  text that `graph.mjs` parses. A synchronous export cannot pass on a
  Haskell exception, so `Main` returns `{"error": message}` and
  `graph.mjs` throws it as an `Error`.
- `wasi` is node's `node:wasi` in the tests; a browser needs a WASI shim
  such as `@bjorn3/browser_wasi_shim` (item 10).

Size (2026-10-17): `graph.wasm` is 7.4 MB as linked, 3.7 MB after
`wasm-opt -Oz`, 0.92 MB gzipped (`gzip -9`). Build time: 4 minutes from
a clean checkout and an empty cabal store (130 packages, PCRE and the
Hackage index included; an 18-core M5 Max); about 5 seconds after
editing a module.

## Toolchain

Tested 2026-10-17 with ghc-wasm-meta (commit `8079eb96`, installed by its
`setup.sh` into `~/.ghc-wasm`, no nix), by building, not from docs. The test
projects and logs are in `cli/tmp/wasm-feasibility/` (not committed).

- **GHC 9.10.3 is enough.** Template Haskell works under the wasm backend
  on the 9.10, 9.12 and 9.14 flavours (9.10's is a backport in the
  haskell-wasm fork; upstream has it from 9.12). The project's own TH works
  on 9.10: `makeEffect`, `[re|…|]` (PCRE at compile time) and the
  `[path|…|]` splices in `Models.PathPattern.Suggest`. On aarch64-darwin
  only 9.10, 9.12 and 9.14 have bindists; 9.6/9.8 stopped updating in
  2025-02.
- **Native target: LTS 24.63 (GHC 9.10.3)**, in use since item 7, so
  native and wasm builds share package versions; importing Stackage's
  `cabal.config` as constraints works in the wasm cabal project. No LTS
  ships 9.12 yet. LTS 24's effectful 2.6 makes `throwError` need `Show e`
  (4 errors in `MyPrelude.Effect` / `MyPrelude.EarlyReturn`);
  `effectful-th` needs 1.0.0.4 or newer (LTS 24.63 has it).
- **No compiler plugins.** Every wasm GHC fails with `Plugins require
  -fno-external-interpreter`, and the cross compiler always uses the
  external interpreter. `effectful-plugin` is dropped everywhere (item 7)
  so native and wasm type-check the same code. Without it, 3 of 63 trial
  modules needed type applications (`MyPrelude.EarlyReturn`,
  `Graph.FreshNID`, `MyPrelude.Effect.fromException`); the whole project
  needed 168, most of them in one spec (see item 7).
- **Dependencies:** everything the wasm modules need builds. Fail:
  `terminal-size` (no `struct winsize`; used by `Graph.Command`),
  `network` (no `getaddrinfo`) and what needs it (`http-conduit`, `wreq`):
  all native-only. `conduit-extra`, `html-conduit` and `xml-conduit` are
  unused by the project. `effectful-plugin` builds but can't be loaded.
- **PCRE:** cross-compile 8.45 with
  `configure --host=wasm32-wasi --disable-shared --enable-static
  --disable-cpp --enable-utf --enable-unicode-properties CFLAGS="-O2
  -mcpu=lime1 -fPIC"`, then link a `libpcre.so` from the `.a` with
  `wasm32-wasi-clang -shared -fPIC -Wl,--experimental-pic
  -Wl,--whole-archive`. Build only `libpcre.la`: `pcretest` doesn't
  compile for WASI (`setrlimit`). Point `extra-include-dirs` / `extra-lib-dirs` for
  `pcre-light` at it in `cabal.project`. Without the `.so`, the
  compile-time loader fails (`loadArchive: unsupported on wasm`) and **GHC
  hangs instead of exiting**. Build the `.so` before `pcre-light`.
  Alternatives checked: `regex-pcre-builtin` (same engine, bundled C, but
  `MyPrelude.Regex` rewritten), `pcre2` (fails: JIT, `mmap`),
  `regex-tdfa` (no lookaround, backreferences, `(*UTF8)` or `(?m)`;
  characters not bytes; multiline by default).
- **Runtime:** single-threaded, one capability. `forkIO`, `timeout`,
  `killThread` and `pooledForConcurrentlyN` work, concurrently but not in
  parallel. `-threaded` / `-with-rtsopts=-N` must not reach wasm
  executables (link failure).
- **Async JavaScript imports work on 9.10:** an awaited import blocks only
  the calling Haskell thread, `killThread` interrupts a thread waiting on a
  promise, and exported Haskell functions return promises. With node's
  WASI, `args: []` makes the runtime exit during initialisation (an opaque
  `Symbol(kExitCode)`); pass `args: ["prog"]`.
- **Build cost:** the full dependency set takes about 20 minutes the first
  time; compile-time splices run in node.
- ghc-wasm-meta builds with `wasm32-wasi-cabal`, not stack, so the wasm
  build needs its own cabal project and freeze file.

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
  - yes, and they run (see "Decision: co-located tests").

## Progress log

| Date | Change |
| --- | --- |
| 2026-10-17 | Traced blockers; chose hybrid architecture; proposed splitting `Utils.Testing` over CPP-gated tests. |
| 2026-10-17 | Items 1 and 2: `Utils.Testing.TempGraph` split off; interrupt predicates moved to `Error.Interrupt`. `Models.Path.Parse` and `Models.PathPattern.Match` now reach neither `haskeline` nor `DAL.*`; PCRE is what remains. |
| 2026-10-17 | Item 4: node-file interpreters moved to `DAL.Interpreters.*`; `Error.UserError` and `Error.Warn` freed of HTTP, file coordination and `haskeline`. `Graph.PathPattern.Execution` reaches 54 modules (66), `Graph.GraphEditing` 31 (51), none of storage or the native libraries but PCRE. |
| 2026-10-17 | Item 5, rescoped to what needs no graph: `parsePath`, `Utils.Parsing.Problems` (syntax feedback as data) and `Models.Completion` (completion over `CompletionLookups`); the REPL's completion is an adapter over it. The core reaches 41 local modules, only PCRE of the blockers. |
| 2026-10-17 | Toolchain tested: GHC 9.10.3 is enough (TH, async JS imports); no compiler plugins; test libraries build and their unused code is dropped; PCRE cross-compiles. Decided: PCRE (item 3), LTS 24.63 and no `effectful-plugin` (item 7). |
| 2026-10-17 | Item 7: `effectful-plugin` dropped (168 type applications, 5 modules allow ambiguous types); unused conduit packages removed; LTS 24.63 (GHC 9.10.3). Parse errors of node files are now aeson 2.2's wording. |
| 2026-10-17 | Completion follow-ups: an id completes with one `@`; no space after the `/`; directive names and command names completed without a graph, from the tables their parsers are made of (`namedDirectives`, `commandSyntaxes`); the REPL's `:q`/`:quit` too. The core reaches 43 local modules (`Models.Command`, `Models.Command.Parse` added), still only PCRE of the blockers. |
| 2026-10-17 | Review fixes: a `%` in a quote, regex or URL is offered no names; `syntaxProblems` takes both ends of a custom error from its range (`mk %{foo}` is (3,9), was (9,9)); `parseCommandLine` gives the command parser's errors as a bundle; the REPL completion's unused helpers and `completePath` removed; label quoting on completion noted as open; offsets documented as code points, UTF-16 at the wasm export. |
| 2026-10-17 | Completion after a space: a whole path followed by a space (`mv a `, `mv a b `, a tab too) now starts the next path, which completes from the current node; it was read as the label being typed, so TAB took `mv x ` to `mv x xyz` (a bug older than the core: the REPL's `completePathHandling` read it the same way). Space after a `/` stays in the path (`mv a/ ` completes after `a`), as commands read it. |
| 2026-10-17 | Item 8: `cli/wasm/` builds a reactor of path parsing, syntax feedback and completion without a graph, exported to JavaScript as JSON (`Wasm.Api`, `wasm/src/Main.hs`, `graph.mjs`), offsets in UTF-16 code units; modules and extensions generated from the imports and `package.yaml`, versions frozen from LTS 24.63, PCRE built by `build.sh`. 3.7 MB after `wasm-opt`, 0.92 MB gzipped. |
