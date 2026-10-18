// Loads graph.wasm and gives its exports as JavaScript functions.
//
// build.sh copies this file next to graph.wasm and ghc_wasm_jsffi.js (the
// glue GHC's post-linker generates) in dist/web/. Nothing here is
// specific to node.
import makeJsffiImports from "./ghc_wasm_jsffi.js";

// Instantiates the module and initialises the Haskell runtime.
//
// - wasm: graph.wasm's bytes, a compiled WebAssembly.Module, or a
//   Response (fetch(...)) for streaming compilation.
// - wasi: a WASI preview 1 implementation with `wasiImport` and
//   `initialize(instance)`, such as node:wasi's WASI or
//   @bjorn3/browser_wasi_shim's. Give it args: ["graph"]; with no
//   arguments the runtime exits while it initialises.
//
// Offsets and lengths in the results count UTF-16 code units, as
// JavaScript strings do.
export async function load({ wasm, wasi }) {
  const exports = {};
  const imports = {
    ghc_wasm_jsffi: makeJsffiImports(exports),
    wasi_snapshot_preview1: wasi.wasiImport,
  };
  const instance = await instantiate(wasm, imports);
  Object.assign(exports, instance.exports);
  wasi.initialize(instance);

  const call = (name, arg) => {
    const result = JSON.parse(instance.exports[name](arg));
    if ("error" in result) {
      throw new Error(`graph.wasm ${name}: ${result.error}`);
    }
    return result;
  };
  return {
    // The syntax problems of a path: {ok: true}, or {ok: false, problems:
    // [{start, end, message}]}, where text.slice(start, end) is the
    // stretch of the text that a problem is about (empty for a problem at
    // a place, such as the end of the text).
    parsePath: (text) => call("parsePath", text),
    // The completions, without a graph, of the text before the cursor:
    // {replaced, candidates: [{text, finished, listed}]}. Each
    // candidate's text replaces before.slice(before.length - replaced); a
    // caller may add a space after the only candidate that is finished,
    // and shows listed for it in a list of the candidates.
    complete: (before) => call("complete", before),
  };
}

async function instantiate(wasm, imports) {
  if (wasm instanceof WebAssembly.Module) {
    return WebAssembly.instantiate(wasm, imports);
  }
  if (typeof Response !== "undefined" && wasm instanceof Response) {
    return (await WebAssembly.instantiateStreaming(wasm, imports)).instance;
  }
  return (await WebAssembly.instantiate(wasm, imports)).instance;
}
