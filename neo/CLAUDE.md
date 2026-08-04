# NeoCLI agent guide

See `AGENTS.md` for the full Ralph-loop workflow (`STATE.md` → `NEXT_STEP.md` → `IMPLEMENTATION_PLAN.md`). This file documents harness-specific testing details.

## Working environment

**All toolchain commands must run inside `nix develop`.** The host system does not (and is not expected to) provide `cargo`, `rustc`, `cabal`, `ghc`, `hurl`, `iconv`, `node`, `npm`, etc. — they are pinned by `flake.nix` and live in the nix store.

## Building the full stack

The bundled IDE has two halves: the Vite/React frontend at `assets/ide/` (built into `assets/ide/dist/`) and the Rust binary that embeds `dist/` via `rust-embed`. Both halves are built in one command:

```sh
nix develop --command ./scripts/build.sh                  # debug
nix develop --command ./scripts/build.sh --release        # release
```

The script runs `npm install` only on first use (when `assets/ide/node_modules/` is absent), then `npm run build` (`tsc -b && vite build`), then `cargo build` (forwarding any flags you pass). Resulting binary is `target/{debug,release}/neo`.

For Rust-only iterations: `cargo build` reads the already-built `assets/ide/dist/`. For frontend-only iterations: `cd assets/ide && npm run build` (the debug binary's `rust-embed` reads `dist/` from disk on each request, so a Rust rebuild is not needed). The script is the catch-all when you don't want to think about which half changed.

Wrap every invocation with `nix develop --command`:

```sh
nix develop --command cargo build
nix develop --command cargo test --test integration_tests -- --test-threads=1
nix develop --command cabal repl
nix develop --command hurl tests/smoke.hurl
```

Telltale sign that this rule was skipped: `ld: library not found for -liconv` from a bare `cargo build`. The fix is always "wrap in `nix develop --command`", never "install iconv".

The `nix build` (release artifact) and `nix flake check` (eval) commands are themselves nix CLI — those run *outside* the dev shell. Everything else runs inside.

## Test layers

| Layer | Command | Network | Speed | Notes |
|---|---|---|---|---|
| Unit + integration | `cargo test` | Real | minutes (Haskell builds inside) | Default suite. `tests/integration_tests.rs` uses `assert_cmd::Command::cargo_bin("neo")` — runs the cargo-built debug binary against real `nix`, real `git`, and real GitHub. Assertions are strict: no escape hatches for missing prereqs. Prereqs (`nix`, `git`, network) are required — fix the environment, never weaken the assertion. |
| End-to-end (shell-level) | `cargo test --test e2e -- --ignored --test-threads=1` | Real | minutes (Haskell builds inside) | `tests/e2e.rs` exercises the full user-facing flow against the **nix-built** `result/bin/neo` (the release artifact) in per-scenario sandbox dirs under `target/e2e-sandbox/`. Goal: prove the binary an end user would install behaves correctly end-to-end. |
| Neo-on-Neo smoke | `./ralph.sh` | Real | minutes | Bash loop driven by the Ralph agent; not wired to `cargo test`. |

**Integration vs e2e:** both hit real `nix` and real network. The split is the binary under test — integration runs `cargo`-built debug (the dev loop), e2e runs the `nix`-built release artifact (what an end user installs). When a behavior is worth testing, prefer adding it to e2e so it covers the shipped binary; mirror in integration only when you also want it to gate `cargo test`.

## Running the e2e suite

```sh
nix build                                                           # produces result/bin/neo
cargo test --test e2e -- --ignored --test-threads=1 --nocapture
```

Prerequisites (all present inside `nix develop`):

- `result/bin/neo` must exist; the helper panics with a clear hint if missing.
- `nix`, `git`, `pgrep`, `timeout` must be on `PATH`.
- Real network is required — the suite intentionally does not set `NEO_SKIP_NETWORK`. It calls real `git ls-remote https://github.com/NeoHaskell/neohaskell` and downloads the real starter tarball.
- Single-threaded: `--test-threads=1` is mandatory (real network rate limits + shared nix-store lock during builds).

### Env knobs

- `NEO_E2E_KEEP=1` — preserve sandbox directories after each test (success or failure). Default behavior preserves sandboxes only on failure.

### When happy-path scenarios go red

The `build` / `run` / `test` happy-path scenarios in `tests/e2e.rs` (groups D, F, G) — and their counterparts in `tests/integration_tests.rs` — require the generated NeoHaskell project to actually compile. The two recurring sources of breakage are:

1. **Starter template drift against upstream `neohaskell` API.** `neo new` tarballs `github.com/NeoHaskell/neo-starter@main` and then `neo build` updates `flake.lock` to the latest `neohaskell` `main`. When upstream renames or removes a module the starter imports (recent example: `Service.Query.Auth` → `Service.AccessControl`, `QueryAuthError` → `AccessError`), the generated project fails GHC compile. Fix in `neo-starter` and push to `main`.
2. **A transitive Haskell dep refusing to build under plain cabal.** Historical example: `jose` needing native crypto paths that only `haskell.nix`'s `hix.project` supplies — fixed in commit `87dde77` by templating the right `flake.nix`.

Either way, do not mask or `#[ignore]` these scenarios — they are the intended signal that the starter ↔ upstream contract is broken.

## When changing CLI behavior

If you change any subcommand surface, error message, output prefix (`[info]` / `[ok]` / `[error]` / `[fail]`), or the generated project layout, look for the affected assertions in both `tests/integration_tests.rs` and `tests/e2e.rs` and update them in the same change.

## Errors as repair instructions (HARD invariant)

Every user-facing error in this repo — `NeoError` variants, miette diagnostics, subprocess wraps, panics reachable from user input — must be readable and **actionable by the smallest dumb LLM** (gemini-flash-2.5, haiku, gpt-3.5-nano level). Opaque errors are bugs of equal severity to wrong results.

Every error message must include, in this order:

1. **What operation failed** — verb + noun ("parsing `neo.json` dependency value", not "Subprocess error").
2. **The bad input quoted** — file path + line, the exact string, the env var name. Empty values appear as `""`, never blank.
3. **The expected shape** — grammar, schema, one valid example.
4. **A concrete fix recipe** — copy-pasteable edit or command. Not "check the docs". Not "verify your config".

Subprocess wraps must interpret known stderr patterns (cabal `unknown package: X` → "add `X` to neo.json as `hackage:X` or `git:...`", nix `attribute missing` → regenerate, git `couldn't find ref` → fix the `#ref`). Do not dump raw child stderr without interpretation.

The full contract, examples, and bad→good rewrites live in `.claude/skills/error-messages-instruct-llms/SKILL.md`. Read it before writing or modifying any error-producing code.

## Writing implementation plans

When proposing a non-trivial change in plan mode, the **Tests** and **Verification** sections are not summaries — they are specifications. Treat them as a contract with the reviewer, not a TODO list. They must enumerate:

1. **Happy paths** — at least one test per user-facing surface (CLI flag, JSON field, output line, generated artifact). Name each test in `snake_case` so it can be grep'd in the suite later. Prefer a table: one row per test, with input + assertion.
2. **Edge cases** — exhaustively, organised by the component under test. Cover at minimum:
   - empty / missing / whitespace-only inputs
   - case sensitivity, Unicode
   - boundary values (0, 1, max, negative)
   - conflicting inputs (same key two ways, ambiguous prefixes)
   - malformed inputs (bad JSON, bad semver, bad URL)
   - network failures and degraded modes (`NEO_SKIP_NETWORK`, 404s, timeouts)
   - idempotence (run twice → identical output) and determinism (input reordering → identical output)
3. **Test-layer mapping** — for each scenario, name the layer that catches it (unit / integration / e2e per the table in §"Test layers"). Each scenario lives at the lowest layer that observes the behaviour.
4. **Verification steps** — beyond running tests, list the manual checks (artifact inspection, `nix flake check`, `cabal check`, behaviour under `--watch`).
5. **Regression list** — explicit names of existing tests that must continue to pass, even when the change "doesn't touch them".

A plan is complete only when every code change in it can be tied to at least one test in §1–3 and one verification step in §4.

## Capturing durable rules (MUST → auto-memory)

Whenever you encounter or write a statement of the form *"in the future this MUST do XXX"*, *"never YYY"*, *"every ZZZ must WWW"* — anywhere: code comment, miette `help()` block, design doc, plan file, chat — lift it to auto-memory at `/Users/nick/.claude/projects/-Users-nick-repos-neo/memory/` immediately. Don't leave it as a code comment.

Code comments and `TODO` markers do not survive refactors. Auto-memory does. A "MUST" claim is by definition a durable invariant; its only safe home is the memory system. Pick the type that fits (`project` for "the codebase works this way", `feedback` for "the user wants me to work this way", `reference` for external systems) and link it from `MEMORY.md`.

Examples worth lifting from this repo's history:
- "Adding a new IDE method should be a one-file change; if you touch `src/ide/rpc.rs` or `src/ide/registry.rs`, the foundation is wrong" → `project_ide_jsonrpc_architecture.md`.
- "Future process / command methods MUST take a closed enum, never a free string" → same.
- "Plans must spec Tests + Verification exhaustively" → already a feedback memory.

## End-of-session location map

At the end of any session that adds or restructures modules (new top-level dir under `src/`, new test layer, new asset directory, new schema), save a memory recording where the new things live: top-level dir, entry points, test fixtures, wiring points.

The standard auto-memory default ("don't save derivable file paths") is **explicitly overridden** in this repo. Each session pays for itself by leaving the next session a map. Otherwise every cold-start session repeats the exploration the prior session already did.

Skip the location map for trivial single-file edits. Save it for additions/restructurings that future-you would benefit from a map of.

## Files

- `tests/e2e.rs` — scenarios (each `#[test] #[ignore]`)
- `tests/common/mod.rs` — `Sandbox`, `neo_bin()`, `cmd` wrappers, isolated `HOME` / git identity, prepended `PATH` so the installed pre-commit hook can resolve `neo`
- `tests/integration_tests.rs` — real-network, real-nix CLI tests via `cargo_bin` (assertions are strict — no escape hatches for missing prereqs)
- `ralph.sh` — Ralph-driven smoke loop
