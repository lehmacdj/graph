# WebAssembly

`cli/wasm/` builds part of the `cli` library into a WebAssembly module
that JavaScript calls: path parsing, syntax feedback and completion without
a graph. It is for the browser extension. This doc is for building the
module, changing code it compiles, and integrating it into the extension.
Why things are as they are (the blocker traces, the alternatives tried, the
history) is in [archive/wasm-plan.md](archive/wasm-plan.md).

## What is in the module

The module exports two functions: `parsePath` (does a path parse, and
where are its problems) and `complete` (completion of the text before the
cursor, without a graph). They are built from `Models.Path.Parse`,
`Utils.Parsing.Problems` and `Models.Completion`, and the modules those
import: about 45 local modules, listed in `wasm/graph-wasm.cabal`.

Native-only, deliberately: the REPL (`haskeline`), the node-file storage
(`DAL.*`), HTTP, running processes (the external editor), image display,
macOS file coordination, and `terminal-size`. Graph reading and editing
(`Graph.GraphEditing` and friends, `Graph.PathPattern.Execution`) reach
none of these and could be built for wasm, but nothing exports them yet:
they need interpreters that reach a graph (see [Future work](#future-work)).

## The JavaScript API

`build.sh` writes `wasm/dist/web/`: `graph.wasm`, `ghc_wasm_jsffi.js` (the
glue GHC's post-linker generates) and `graph.mjs`, the loader. Ship all
three together.

```js
import { load } from "./graph.mjs";
const graph = await load({ wasm, wasi });
graph.parsePath("a/)");
// {ok: false, problems: [{start: 2, end: 3,
//   message: "unexpected ')'\nexpecting ..."}]}
graph.parsePath("a/b"); // {ok: true}
graph.complete("ls %la");
// {replaced: 3, candidates: [{text: "%last", finished: false}]}
```

- `wasm` is the bytes of `graph.wasm`, a compiled `WebAssembly.Module`,
  or a `Response` from `fetch` (compiled while it streams).
- `wasi` is a WASI preview 1 implementation with `wasiImport` and
  `initialize(instance)`: `node:wasi` in the tests, a shim such as
  `@bjorn3/browser_wasi_shim` in a browser. Give it a non-empty `args`
  (the tests use `["graph"]`): with none, the runtime exits while it
  initialises, and node reports only `Symbol(kExitCode)`.
- `parsePath(text)` gives `{ok: true}`, or `{ok: false, problems}` where
  each problem is `{start, end, message}`: `text.slice(start, end)` is
  what it is about (empty for a place, such as the end of the text), and
  `message` is the parser's, as `errorBundlePretty` words it.
- `complete(before)` takes the text of the command line before the cursor
  and gives `{replaced, candidates: [{text, finished}]}`. Each candidate
  replaces the last `replaced` units of `before`:
  `before.slice(0, before.length - replaced) + candidate.text`. A caller
  may add a space after the only candidate that is `finished`.
- Offsets and lengths count UTF-16 code units, as JavaScript strings do.
  The Haskell core counts code points; `Wasm.Api` converts.
- The exports are synchronous: they return JSON text, which `graph.mjs`
  parses. A synchronous export cannot pass on a Haskell exception, so
  `Main` returns `{"error": message}` and `graph.mjs` throws it as an
  `Error` (`graph.wasm parsePath: …`).

## The Haskell API underneath

All pure, and all in the native library, so the native tests cover it.

Parsing and syntax feedback:

- `Models.Path.Parse.parsePath :: Text -> Either ParseErrorBundle'
  ParsedPath`: a whole path, directives kept, as commands take it.
- `Models.Command.Parse.parseCommandLine :: Text -> Either
  ParseErrorBundle' Command`: a whole command line, as the REPL and
  `ge -e` parse it.
- `Utils.Parsing.Problems.syntaxProblems :: ParseErrorBundle' -> NonEmpty
  SyntaxProblem`, `SyntaxProblem {start, end :: Int, message :: Text}`:
  the problems of either parser's errors, offsets in code points, `end`
  exclusive; a custom error uses its source range.

Completion (`Models.Completion`):

- `complete :: Monad m => CompletionLookups m -> Text -> m Completions`,
  with `Completions {replaced :: Int, candidates :: [Candidate]}` and
  `Candidate {text :: Text, finished :: Bool}`; `replaced` in code points.
  `completeCommandLine` takes the names of the commands too (the REPL adds
  `:q` and `:quit`).
- `CompletionLookups m {nodesWithPrefix :: Text -> m [NID], labelsAfter
  :: Path -> m [Text]}` is all completion asks of a graph. `withoutGraph`
  has nothing, and `completeWithoutGraph = runIdentity . complete
  withoutGraph`. What to try in which order (a command name, an id after
  `@`, a `%` name, then the path) stays in the core whatever the lookups.
- `Wasm.Api`: `pathProblems` and `completions` are the above with UTF-16
  offsets; `completionsWith` takes lookups, for a graph-aware layer;
  `parsePathJson` and `completeJson` are what `Main` exports.

A graph-aware layer plugs in by giving `CompletionLookups`. The REPL's are
`graphLookups` in `Executable.GraphEditor.Completion`: it globs the node
files for ids and runs `Graph.PathPattern.Execution.completionTransitions`
(over `PathReading`, i.e. `GraphMetadataReading`) for labels. The effects
(`Graph.GraphMetadataEditing`, `Graph.GraphDataEditing`,
`Graph.GraphEditing`) are defined apart from their storage interpreters
(`DAL.Interpreters.*`) and come with in-memory ones only, so a wasm build
can interpret them by calling JavaScript and run the same graph logic.

## Building and testing

Install ghc-wasm-meta's 9.10 flavour (`FLAVOUR=9.10 setup.sh`, into
`~/.ghc-wasm` or `$GHC_WASM_DIR`, or put `wasm32-wasi-ghc` on `PATH`). You
also need python3, curl and node. Then:

```
cli/wasm/build.sh            # build into wasm/dist/web/, run node tests
cli/wasm/build.sh --no-test  # skip the node tests
cli/wasm/build.sh --freeze   # regenerate the freeze file first
```

- It refuses a `wasm32-wasi-ghc` that is not the native build's GHC
  (9.10.3) and a freeze file made from another resolver than
  `stack.yaml`'s.
- It runs `sync-cabal.py`, builds with `wasm32-wasi-cabal`, post-links the
  JS glue, runs `wasm-opt -Oz`, copies `js/graph.mjs`, prints the sizes and
  runs `test/graph.test.mjs` under node (`GRAPH_WASM_DIR` points the tests
  at another build).
- `dist/` and `dist-newstyle/` are ignored by git. `wasm32-wasi-cabal`
  keeps its own cabal store and index inside the ghc-wasm-meta install.
- As a rough guide (2026-10-17, an 18-core M5 Max): 4 minutes from a clean
  checkout and an empty store, about 5 seconds after editing a module.
  `graph.wasm` is 7.4 MB as linked, 3.7 MB after `wasm-opt`, 0.9 MB
  gzipped.

Test the logic natively: `Wasm.Api` and the modules under it have
co-located tests that `stack test` runs, including UTF-16 offsets for
text outside the Basic Multilingual Plane. `test/graph.test.mjs` only
checks the built module end to end.

### The cabal file, the module list and the freeze file

ghc-wasm-meta builds with cabal, not stack, so `cli/wasm/` is its own
cabal project, `graph-wasm`: one executable built as a reactor
(`-no-hs-main -optl-mexec-model=reactor`). Its sources are `wasm/src`
(only `Main`, the JS FFI exports) and the native `../src`; nothing is
copied.

- The block between `BEGIN` and `END` in `graph-wasm.cabal` is generated
  by `sync-cabal.py` (`build.sh` runs it): `default-extensions`,
  `ghc-options` and `cpp-options` from `package.yaml`'s top-level lists,
  and `other-modules`, the local modules `wasm/src` imports, transitively,
  read from the import lines. A new import in the closure changes the list
  on the next build; commit the change. `sync-cabal.py --check` only
  reports.
- `build-depends` is kept by hand. A missing package fails the build by
  name.
- A new export needs a `foreign export javascript` in `Main`, its name in
  the `-optl-Wl,--export=` option, and a wrapper in `graph.mjs`.
- `cabal.project.freeze` pins the versions to the Stackage snapshot that
  `stack.yaml` uses. After changing the resolver, run `build.sh --freeze`:
  it downloads the snapshot's `cabal.config`, adds `stack.yaml`'s Hackage
  extra-deps (`ixset-typed`), and freezes through `freeze.project`.
  `stack.yaml`'s git forks (haskeline, prettyprinter-ansi-terminal) are
  not used by the wasm build. Like `stack.yaml`, `cabal.project` has
  `allow-newer: all`.

### PCRE

`MyPrelude.Regex` uses `pcre-light` and `lens-regex-pcre`, so the module
links PCRE 8.45, cross-compiled; it behaves as natively. `build.sh`
downloads it, checks its SHA-256 and builds it once into `dist/pcre/`:

- `configure --host=wasm32-wasi --disable-shared --enable-static
  --disable-cpp --enable-utf --enable-unicode-properties`, with
  `CFLAGS="-O2 -mcpu=lime1 -fPIC"`; only `libpcre.la`, since the test
  programs need `setrlimit`, which WASI lacks.
- Then a `libpcre.so` linked from the `.a` (`wasm32-wasi-clang -shared
  -fPIC -Wl,--experimental-pic -Wl,--whole-archive`).
- `dist/pcre.project`, which `cabal.project` imports, points `pcre-light`
  at them.

The `.so` is needed because GHC loads PCRE to run the `[re|…|]` splices at
compile time. Without it the loader fails (`loadArchive: unsupported on
wasm`) and **GHC hangs instead of exiting**. If a build hangs compiling a
module with a regex quasiquoter, check for the `.so`.

## Keeping the closure wasm-clean

Everything `Main` imports, transitively, must build for wasm32-wasi. A
module in the closure must not reach:

- `haskeline` (the REPL; `Error.Interrupt` holds its Ctrl-C handling,
  outside `MyPrelude`), `DAL.*` or the file system as storage,
  `network`/`http-conduit`/`wreq`, `typed-process`, `System.Posix`, the
  macOS bindings (file coordination, `inline-c-objc`), or `terminal-size`.
  `network` and `terminal-size` don't compile for WASI; the others make no
  sense in a browser.
- Effects defined beside an IO interpreter. Define an effect with its
  in-memory interpreter only and put the IO one in another module, as
  `Graph.*` and `DAL.Interpreters.*` are split. The `Effect.IOWrapper.*`
  effects are not split yet; `Graph.Command` and `Graph.Import.*` reach
  them, so the command language as a whole is not wasm-clean.
- `Utils.Testing.TempGraph` (temporary graphs on disk). Co-located tests
  may stay: the test libraries build for wasm and the linker drops unused
  test code, so `Utils.Testing` is fine. The generated `src/Spec.hs`
  (tasty-discover, a native preprocessor) must stay out; nothing imports
  it, so `sync-cabal.py` never lists it.

The toolchain sets these rules too:

- **No compiler plugins.** The wasm GHC always uses the external
  interpreter, which can't load plugins (`Plugins require
  -fno-external-interpreter`). `effectful-plugin` was dropped from the
  native build too, so both type-check the same code; effect types it
  inferred are written out with type applications. A later GHC (10.x)
  may lift this.
- **Single-threaded runtime.** One capability; `forkIO`, `timeout` and
  `killThread` work, concurrently but not in parallel. `-threaded` and
  `-with-rtsopts=-N` fail to link: keep them in the executables'
  `ghc-options` in `package.yaml`, never the top-level list that
  `sync-cabal.py` copies.
- **Same GHC as native.** The native build is on LTS 24.63, GHC 9.10.3,
  the compiler of ghc-wasm-meta's 9.10 flavour, so both use the same
  package versions. Template Haskell works under it (`makeEffect`,
  `[re|…|]`, the `[path|…|]` splices). Move both together: a new LTS needs
  a ghc-wasm-meta flavour of its GHC, then `build.sh --freeze`.

## Completion, as an integrator sees it

The core and its tests (`spec_complete` in `Models.Completion`,
`Models.LegacyPartialPath`) have the details. Without a graph, `complete`
offers:

- the names of commands where a command begins: an empty line, the first
  word, after `;` or `{`, after `at` and its path (`startsCommand`).
  Finished.
- the names after a `%` that starts a term (`percentNames`): `%last`,
  `%never`, and `%history(`, `%targets(`, `%override(` with their `(`. Not
  finished. None in a label, a quote, a regex or a URL.
- `/` after a term that is not a label (`@`, a closed group, a closed
  quote).
- nothing else: the labels and ids are the graph's. It still sets
  `replaced` to the word a graph would complete.

With lookups, ids complete with their `@` (`@0` to `@000000000001`), and
labels continue the path before the cursor. A label is offered as a path
writes it (`renderLabel`): a plain word as it is, anything else quoted
(`quoteLabel`), and all candidates quoted when one is and a prefix is
typed, so they share it. In an open quote the label is completed and
closed (`ls "he` to `"hello world"`, `replaced` covering the quote). In an
open group the last side continues the path that leads to the group
(`ls a/(x | b` completes labels after `a`). A path with a directive, one
that does not parse, or one no command runs gets nothing.

## Future work

- **Graph access from the extension** (item 9 in the archive). The
  intended design is hybrid: parsing, syntax feedback and the completion
  core run in wasm; graph reads and writes go through the same effects as
  native `ge`, interpreted in wasm by calling JavaScript, which asks a
  native messaging host that has the graph on disk. The host could be
  `ge`, or the planned sync background service could double as it. GHC's
  wasm backend imports async JavaScript functions: an awaited import
  blocks only the calling Haskell thread, so these interpreters keep the
  native ones' shape. An export that awaits must be asynchronous (it
  returns a promise), unlike today's `sync` exports. Completion with a
  graph is `completionsWith` over lookups that ask the host, or
  `completionTransitions` over a JS-backed `GraphMetadataReading`. Later,
  the planned SQLite and operation log storage could become an in-browser
  store (SQLite on OPFS) behind the same effects, with no host.
- **Extension integration** (item 10): load the module with a browser WASI
  shim and expose parse, complete and, later, queries to the UI.
- Completion of a path after `;` or inside `{…}` offers nothing: the path
  reader (`pLastLegacyPartialPath`) reads one command. Command names are
  offered there.
- An emoji is not a label character (labels are `isAlphaNum` plus
  `-._'`), so a label with one is quoted. By design for now.
