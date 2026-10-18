// End-to-end checks of the wasm build: node --test test/graph.test.mjs
// (build.sh runs it). GRAPH_WASM_DIR overrides where the build is
// (default ../dist/web).
import assert from "node:assert/strict";
import fs from "node:fs/promises";
import path from "node:path";
import { before, test } from "node:test";
import { fileURLToPath } from "node:url";
import { WASI } from "node:wasi";

const here = path.dirname(fileURLToPath(import.meta.url));
const dir = path.resolve(
  process.env.GRAPH_WASM_DIR ?? path.join(here, "..", "dist", "web"),
);

// Two UTF-16 code units each. An emoji is not a letter, so a label with
// it is quoted; the letter (MATHEMATICAL SCRIPT CAPITAL A) needs no quotes.
const emoji = "\u{1F600}";
const quoted = `"${emoji}"`;
const letter = "\u{1D49C}";
let graph;

before(async () => {
  const { load } = await import(path.join(dir, "graph.mjs"));
  const wasi = new WASI({
    version: "preview1",
    args: ["graph"],
    env: {},
    returnOnExit: true,
  });
  graph = await load({
    wasm: await fs.readFile(path.join(dir, "graph.wasm")),
    wasi,
  });
});

const stretches = (text) =>
  graph.parsePath(text).problems.map((p) => text.slice(p.start, p.end));

test("a path that parses is ok", () => {
  assert.deepEqual(graph.parsePath(`a/b & %last`), { ok: true });
  assert.deepEqual(graph.parsePath(`${letter}/${quoted}`), { ok: true });
});

test("a syntax error is placed in the text", () => {
  const result = graph.parsePath("a/)");
  assert.equal(result.ok, false);
  assert.equal(result.problems.length, 1);
  const [p] = result.problems;
  assert.deepEqual([p.start, p.end], [2, 3]);
  assert.match(p.message, /^unexpected '\)'\nexpecting /);
});

test("offsets after an emoji count UTF-16 code units", () => {
  assert.deepEqual(stretches(`${quoted}/)`), [")"]);
  assert.deepEqual(stretches(`${quoted}/@abc/${letter}/@de`), [
    "@abc",
    "@de",
  ]);
  const [p] = graph.parsePath(`${letter}/${quoted}/`).problems;
  assert.deepEqual([p.start, p.end], [8, 8]);
});

test("an unquoted emoji is a problem, all of it", () => {
  assert.deepEqual(stretches(`a/${emoji}`), [emoji]);
});

test("command names complete", () => {
  const result = graph.complete("l");
  assert.equal(result.replaced, 1);
  assert.ok(result.candidates.some((c) => c.text === "ls" && c.finished));
  assert.ok(result.candidates.every((c) => c.text.startsWith("l")));
});

test("directive names complete after an emoji", () => {
  const before = `ls ${quoted} & %la`;
  const result = graph.complete(before);
  assert.equal(before.slice(before.length - result.replaced), "%la");
  assert.deepEqual(result.candidates, [
    { text: "%last", finished: false, listed: "%last" },
  ]);
});

test("the replaced label counts UTF-16 code units", () => {
  const before = `ls a/b${letter}`;
  const result = graph.complete(before);
  assert.equal(result.replaced, 3);
  assert.equal(before.slice(before.length - result.replaced), `b${letter}`);
  assert.deepEqual(result.candidates, []);
});

test("an open quote is replaced in UTF-16 code units", () => {
  const before = `ls a/"${emoji}b`;
  const result = graph.complete(before);
  assert.equal(result.replaced, 4);
  assert.equal(before.slice(before.length - result.replaced), `"${emoji}b`);
  assert.deepEqual(result.candidates, []);
});

test("the / after a term completes", () => {
  assert.deepEqual(graph.complete("ls @"), {
    replaced: 0,
    candidates: [{ text: "/", finished: false, listed: "/" }],
  });
});
