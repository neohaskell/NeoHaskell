import assert from "node:assert/strict";
import test from "node:test";
import fs from "node:fs";
import path from "node:path";
import { fileURLToPath } from "node:url";

const currentFile = fileURLToPath(import.meta.url);
const currentDir = path.dirname(currentFile);
const root = path.resolve(currentDir, "..");

function readJson(filePath) {
  const fullPath = path.join(root, filePath);
  const content = fs.readFileSync(fullPath, "utf8");
  return JSON.parse(content);
}

test("language configuration keeps < and > out of brackets", () => {
  const config = readJson("language-configuration.json");
  const hasAngleInBrackets = config.brackets.some(
    ([open, close]) => open === "<" || close === ">"
  );

  assert.equal(hasAngleInBrackets, false);
});

test("language configuration auto-closes generic angle brackets", () => {
  const config = readJson("language-configuration.json");
  const hasAnglePair = config.autoClosingPairs.some(
    (pair) => pair.open === "<" && pair.close === ">"
  );

  assert.equal(hasAnglePair, true);
  assert.equal(config.autoCloseBefore, ";:.,=}])>` \n\t");
});

test("language configuration has dedicated doc-comment auto-close", () => {
  const config = readJson("language-configuration.json");
  const hasDocPair = config.autoClosingPairs.some(
    (pair) => pair.open === "/**" && pair.close === "*/"
  );
  const hasBlockPair = config.autoClosingPairs.some(
    (pair) => pair.open === "/*" && pair.close === "*/"
  );

  assert.equal(hasDocPair, true);
  assert.equal(hasBlockPair, true);
});

test("package contributions register NeoHaskell language and .nh extension", () => {
  const pkg = readJson("package.json");
  const language = pkg.contributes.languages.find(
    (entry) => entry.id === "neohaskell"
  );

  assert.ok(language);
  assert.ok(language.extensions.includes(".nh"));
  assert.equal(pkg.engines.vscode, "^1.75.0");
});

test("word pattern supports apostrophes", () => {
  const config = readJson("language-configuration.json");
  const re = new RegExp(config.wordPattern);

  // Positive: valid identifiers
  assert.ok(re.test("foo"));
  assert.ok(re.test("foo_bar"));
  assert.ok(re.test("x'"));
  assert.ok(re.test("foo'bar"));
  assert.ok(re.test("_hidden"));

  // Negative: should not match as full identifier
  assert.equal(re.test("123"), false);
  assert.equal(re.test("-"), false);
});
