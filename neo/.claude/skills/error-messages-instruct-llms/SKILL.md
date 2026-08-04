---
name: error-messages-instruct-llms
description: Use when about to write or modify any user-facing error in the NeoCLI repo at `/Users/nick/repos/neo` — a `NeoError` variant, a `miette::miette!`/`bail!` call, a subprocess-wrap result, a `format!` into stderr, a `panic!` reachable from user input, an `expect(...)` on a `Result` returned to the user, or wrapping an error with `?`. Also use when reviewing an existing error path that surfaced as opaque (e.g. `Subprocess execution failed: cabal build all` with no actionable detail). Enforces the hard invariant that every user-facing error must be parsable by the dumbest small LLM (gemini-flash-2.5, haiku, gpt-3.5-nano level) and tell it exactly how to fix the underlying issue without re-reading any documentation. Skips when the error is purely internal/debug — `tracing::debug!`, `#[derive(Debug)]` on private types — that an end user will never see.
kind: leaf
executor: script
---

# Errors as LLM-actionable repair instructions

A hard invariant in this repo: **every error a human or LLM sees must be readable and repairable by the smallest dumbest model** (gemini-flash-2.5, claude-haiku-4.5, gpt-3.5-nano level). If a tiny model cannot derive the fix from the error alone — without docs, without web search, without reading the source — the error is broken.

Why this matters: agentic loops, retries, and self-healing pipelines run on tiny models. If `neo build` returns `Subprocess execution failed: cabal build all` to an autonomous agent, that agent has no idea what to do. If it returns a clear "the package `X` in `neo.json` doesn't exist in the registry, change it to `hackage:X` or remove it", the agent fixes the file and moves on.

Concrete failure mode this skill prevents (real, happened in this session on 2026-06-10):

```
Error: Subprocess execution failed: cabal build all
    Diagnostic severity: error
diagnostic help: Check the output above for more details.
diagnostic code: neo::subprocess
```

The user (and any LLM) is told nothing. The actual cause was 90 lines of nix+haskell.nix+cabal error scrolled past, ending in "unknown package: definitely-wrong-package-should-come-from-neopackages". A tiny model has zero chance of fixing this. A senior engineer barely does.

## The four parts every user-facing error MUST include

Write errors so the reader can reconstruct the WHO/WHAT/WHERE/HOW without leaving the message:

1. **What operation failed** — verb + noun ("parsing `neo.json` dependency value", "fetching NeoPackages registry", "building cabal project"). Not just "Subprocess error" or "I/O error".
2. **What the bad input was** — quote the offending string, file path, line number, env var name. If the input is empty, say `""` — never blank.
3. **What the expected shape is** — the grammar, the schema, the valid values. One sentence, one example.
4. **What to do to fix it** — a concrete, copy-pasteable repair recipe. Not "check the documentation"; not "verify your config". A specific edit, command, or replacement.

Order matters: lead with #1 and #2 (what + where), then #3 (expected), then #4 (fix). Tiny LLMs latch onto the front of the message.

### Template

```
<Operation> failed: <one-line cause involving the bad input>.
Expected: <grammar / valid shape / one example>.
Fix: <concrete action — edit X to Y, run Z, replace A with B>.
```

## Bad → good (real examples from this codebase)

### Subprocess wrapping — `src/errors.rs` `SubprocessError`

```
// BAD (current shape — to be improved)
Subprocess execution failed: cabal build all
    diagnostic help: Check the output above for more details.

// GOOD
cabal build failed while resolving build-depends of `test-project`.
Cause: package `definitely-wrong-package-should-come-from-neopackages` is referenced in `neo.json` but not found on Hackage and not declared in `cabal.project` as a git or file source.
Expected: every name in `neo.json` `dependencies` must be either (a) bare = present in https://github.com/NeoHaskell/neopackages, (b) prefixed `hackage:<name>` for Hackage, or (c) prefixed `git:<url>` / `github:<owner>/<repo>` / `file:<path>` for an explicit source.
Fix: edit `neo.json` and replace the dep with `"hackage:definitely-wrong-package-should-come-from-neopackages": "..."` if it's on Hackage, or remove it. Then run `neo build` again.
```

The subprocess wrap should not just bubble the child's stderr — it should **interpret** the failure when the pattern is known (cabal "unknown package", nix "attribute missing", git "ref not found") and produce a fix-recipe.

### Stale generated artifacts — direnv re-evaluating a broken `.cabal`

```
// BAD — silent: direnv just dumps haskell.nix's stack trace
error: Cannot build '/nix/store/.../neohaskell-starter-plan-to-nix-pkgs.drv'.

// GOOD — neo should detect that `<name>.cabal` exists but `neo.json` mtime is newer,
// or that `<name>.cabal` references packages not in the current resolved set, and emit:
Stale generated artifacts detected: `test-project.cabal` was produced by a previous reconcile run and no longer matches `neo.json`.
Fix: run `rm test-project.cabal cabal.project flake.nix && neo build` to regenerate from the current `neo.json`. Direnv will pick up the new flake automatically.
```

### Parse error — `src/errors.rs` `InvalidConfig`

```
// BAD
Failed to parse `neo.json` at line 10, column 5: unexpected comma

// GOOD
Failed to parse `neo.json` at line 10, column 5: unexpected comma after `"author"`.
Expected: JSON object syntax — comma between key:value pairs, no trailing comma before `}`.
Fix: open `neo.json` at line 10, remove the trailing comma after `"author": "..."`, save, re-run.
```

### Dep grammar — what my new code does today (good baseline, can sharpen)

```
// CURRENT (acceptable, can be sharpened)
Invalid dependency `foo` = ``: package `foo` not found in the NeoPackages registry. Hint: use `hackage:<name>` for a Hackage package, or `git:<url>` for a git source.
diagnostic help: Dependency values use npm-style semver (...). Use prefix hackage:, git:, github:, or file: for explicit sources.

// IMPROVED — keeps the value field populated and gives one ready-to-paste replacement:
Resolving `neo.json` dependency `foo` (value `^1.2.0`): package `foo` is not listed in the NeoPackages registry at https://github.com/NeoHaskell/neopackages.
Available registry packages: [aeson, text, uuid, ...].
Expected: a name that exists in the registry, OR an explicit-source prefix.
Fix: choose one of the following replacements in `neo.json`:
  "hackage:foo": "^1.2.0"                          // if `foo` is on Hackage
  "foo": "git:github.com/<owner>/foo.git#<ref>"    // if it lives on GitHub
  "foo": "github:<owner>/foo#<ref>"                // shorthand for the above
  "foo": "file:../path/to/foo"                     // if it's a local sibling
```

## Patterns for the common sources

### `NeoError` variants in `src/errors.rs`

- `#[error("...")]` is the **headline** — operation + bad input.
- `#[diagnostic(help("..."))]` is the **fix recipe** — concrete, single-paste-able. Never "check the docs" or "verify your config".
- Add `url(...)` when there's a canonical doc page.
- If the variant wraps a foreign error (`reqwest`, `serde_json`), the `Display` must still carry the operation + input — do not just delegate to the foreign error's message.

### Subprocess failures (`subprocess/*.rs`, `SubprocessError`)

The current `SubprocessError { command, output }` is the worst offender. Replace the `Display` with:

```
<command_friendly_name> failed: <interpreted reason | last meaningful line of child stderr>.
Expected: <what the subprocess was supposed to do>.
Fix: <recipe>.
```

If the child's stderr matches a known pattern, interpret it. A small library of `match`-on-stderr-substring → fix-recipe mappings beats dumping raw output.

Known patterns to interpret (add as encountered):

| Tool | Stderr substring | Interpreted cause | Fix recipe |
|---|---|---|---|
| cabal | `unknown package: X` | dep `X` not on Hackage / not in `cabal.project` | "Add `X` to neo.json as `hackage:X` or `git:...`, or remove it." |
| cabal | `Could not resolve dependencies` | version constraints conflict | "Loosen the `^`/`~` ranges in neo.json so they overlap." |
| nix | `attribute 'X' missing` | flake.nix references a removed attr | "Run `neo build` after `rm flake.nix` to regenerate." |
| git | `couldn't find remote ref X` | git: dep points at a non-existent branch/tag | "Edit neo.json: change `#X` to a real branch or tag." |
| reqwest | `connection refused` / `dns error` | offline or DNS down | "Check network or set NEO_SKIP_NETWORK=1 for offline mode." |

### Parse / validation errors (config, dep_spec, semver)

Always echo the **byte-exact bad input** quoted. Always say what the next character should have been or what shape the whole field should have taken. Always give one concrete example of valid input.

### `unwrap`, `expect`, `panic!` on user-reachable paths

Don't. Every `unwrap` on a `Result` whose `Err` can come from user input is a violation. Convert to `?` with an error variant that follows this skill's contract.

The only acceptable `unwrap` is on a value the type system guarantees can't fail (e.g. building a `BufReader` from an owned `&str`).

## Checklist before submitting any error-producing code

Read the message aloud as if you were the smallest LLM:

- [ ] Does the first sentence tell me **what operation failed**?
- [ ] Does it quote the **exact bad input** (file, line, value, env var)?
- [ ] Is the **expected shape** stated in one sentence with one example?
- [ ] Is the **fix** a copy-pasteable edit/command, not "check the docs"?
- [ ] If a subprocess child wrote useful stderr lines, are the **important lines surfaced** (not buried in the middle of 80 lines of unrelated output)?
- [ ] If the failure has a known pattern, has it been **interpreted** into a recipe (not just dumped)?
- [ ] Could I, as a tiny LLM, write the fix WITHOUT searching docs?

If any box is unchecked: rewrite.

## How to test that errors are LLM-readable

When adding or changing an error path:

1. Trigger it (write a failing fixture, an invalid input, etc.).
2. Capture the **exact** terminal output an end user would see.
3. Paste the captured output (and nothing else) into a fresh tiny-model context (e.g. via the API, or in your head simulating a 1B-param model).
4. Ask: "what file do I edit and what do I change?" If the answer isn't obvious from the error alone, the error fails.

For the test suite, prefer `predicate::str::contains` on the specific fix-recipe phrasing so a regression that drops the recipe fails fast.

## Refusals

- The user asks to add an error message that just says "operation failed, see logs" → refuse. Either interpret the failure or surface the specific offending input.
- The user asks to swallow a foreign error's message with `.into_diagnostic()?` without context → refuse. Wrap with `.wrap_err_with(|| format!("while doing X to `{}`", input))` or convert to a `NeoError` variant.
- The user asks to remove the `help(...)` clause of a `NeoError` variant to "make it less noisy" → refuse. The `help` IS the fix recipe.

## Why this is a HARD invariant

This repo's binary is run by humans AND by autonomous agents (Ralph loops, AI assistants, future MCP integrations). Half of `neo`'s value is that when something goes wrong, both audiences can fix it without context-switching. An opaque error is a bug — the same severity as a wrong result.

This skill was added on 2026-06-10 after the user spent multiple turns debugging a `Subprocess execution failed: cabal build all` that hid both a stale-artifact issue AND a bare-dep registry-miss inside an 80-line cabal trace. The rule from that incident: **if a small LLM cannot write the fix from the error message alone, the error message is the bug.**
