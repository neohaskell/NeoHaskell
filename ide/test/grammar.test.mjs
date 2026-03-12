import test from "node:test";
import assert from "node:assert/strict";
import { readFileSync } from "node:fs";
import path from "node:path";
import { fileURLToPath } from "node:url";

const __filename = fileURLToPath(import.meta.url);
const __dirname = path.dirname(__filename);

const grammarPath = path.join(__dirname, "..", "syntaxes", "neohaskell.tmLanguage.json");
const packagePath = path.join(__dirname, "..", "package.json");

const grammar = JSON.parse(readFileSync(grammarPath, "utf8"));
const manifest = JSON.parse(readFileSync(packagePath, "utf8"));

test("grammar is contributed from package.json", () => {
  const [grammarContribution] = manifest.contributes.grammars;
  assert.equal(grammarContribution.language, "neohaskell");
  assert.equal(grammarContribution.scopeName, "source.neohaskell");
  assert.equal(grammarContribution.path, "./syntaxes/neohaskell.tmLanguage.json");
});

test("let! declaration rule is defined before plain let", () => {
  const declarationPatterns = grammar.repository.keywords_declaration.patterns;
  assert.equal(declarationPatterns[0].match, "\\blet!(?=[\\s(])");
  assert.match(declarationPatterns[1].match, /let/);
});

test("doc comments are matched before block comments", () => {
  const commentIncludes = grammar.repository.comments.patterns.map((pattern) => pattern.include);
  assert.equal(commentIncludes[0], "#comments_doc");
  assert.equal(commentIncludes[1], "#comments_block");
});

test("block comments support nesting via recursive include", () => {
  const nestedInclude = grammar.repository.comments_block.patterns[0].patterns[0].include;
  assert.equal(nestedInclude, "#comments_block");
});

test("tuple prefix and interpolation patterns are present", () => {
  assert.equal(grammar.repository.tuple_prefix.patterns[0].match, "#\\(");
  const stringPattern = grammar.repository.strings.patterns[0];
  assert.equal(stringPattern.patterns[1].begin, "\\$\\{");
  assert.equal(stringPattern.patterns[1].end, "\\}");
});

test("operators include required multi-character tokens", () => {
  const operatorPattern = grammar.repository.operators.patterns[0].match;
  assert.equal(operatorPattern.includes("\\|>"), true);
  assert.equal(operatorPattern.includes("->"), true);
  assert.equal(operatorPattern.includes("=>"), true);
  assert.equal(operatorPattern.includes("\\+\\+"), true);
  assert.equal(operatorPattern.includes("=="), true);
  assert.equal(operatorPattern.includes("/="), true);
  assert.equal(operatorPattern.includes("<="), true);
  assert.equal(operatorPattern.includes(">="), true);
});
