---
name: running-neo-tests
description: Use when about to run any test layer for the NeoCLI repo at `/Users/nick/repos/neo` — phrases like "run the tests", "cargo test", "run integration tests", "run e2e", "verify cargo test passes", or before declaring a code change done. Wraps the right command for the layer being targeted (unit, integration, e2e, smoke), always inside `nix develop --command` because the toolchain is flake-pinned, and respects the strict-assertion contract (integration assertions assume `nix`+`git`+network are present — fix the env, never weaken the assertion). Skips when the user has explicitly entered `nix develop` already or when the target is the published binary in `result/bin/neo`.
kind: leaf
executor: script
---

# Running NeoCLI tests

NeoCLI has three test layers, each with a specific invocation. **All cargo commands must be wrapped in `nix develop --command`** — see the `wrapping-toolchain-in-nix-develop` skill for why; this skill applies that rule to neo's specific commands.

## Layers (from `CLAUDE.md`)

| Layer | What it tests | Command | Time |
|---|---|---|---|
| Unit | Library functions, parsers, TUI widgets, prereqs guards | `nix develop --command cargo test --bins` | seconds |
| Integration | Full CLI surface via `cargo_bin("neo")` against real nix + real network | `nix develop --command cargo test --test integration_tests -- --test-threads=1` | minutes |
| E2E | Full end-user simulation against the `nix`-built `result/bin/neo` | `nix build && nix develop --command cargo test --test e2e -- --ignored --test-threads=1` | minutes |
| Neo-on-Neo smoke | Ralph loop driving the binary on a generated project | `./ralph.sh` | minutes |

## Strict-assertion contract

Integration and e2e tests assume the environment provides what an end user would have: `nix`, `git`, network, and (for e2e) a freshly `nix build`ed `result/bin/neo`. If a test fails because the env is missing one of those, **fix the environment, do not weaken the assertion**. There are no `if success { … } else accept missing-nix { … }` escape hatches anymore — that pattern was deleted on 2026-06-10.

## Plan

1. Identify the layer being requested → user wording maps to one of unit/integration/e2e/smoke. If unclear, default to unit + integration (the `cargo test` default suite).
2. If the layer is e2e, ensure `result/bin/neo` is fresh → run `nix build` first.
3. Wrap the cargo invocation in `nix develop --command`.
4. For integration and e2e: always pass `--test-threads=1` (real network rate-limits and the shared nix-store lock during Haskell builds forbid parallelism).
5. Let the wrapped exit code propagate; if it fails, surface the failing test name and last ~40 lines of stderr — do not retry, do not edit the assertion.

## Commands by intent

- "run the tests" / "cargo test" → `nix develop --command cargo test --bins` (fast) plus offer to run the slow integration suite separately.
- "run unit tests" → `nix develop --command cargo test --bins`
- "run integration tests" → `nix develop --command cargo test --test integration_tests -- --test-threads=1`
- "run e2e" → `nix build && nix develop --command cargo test --test e2e -- --ignored --test-threads=1 --nocapture`
- "run a single test" → `nix develop --command cargo test --test integration_tests <test_name> -- --test-threads=1`

## Common failure fingerprints

- `ld: library not found for -liconv` → cargo was run outside `nix develop`. Re-run wrapped.
- `error: no library targets found in package 'neo'` → `cargo test --lib` was used. NeoCLI is a binary crate; use `--bins` for unit tests.
- `Nix is required but not found` in a test stderr → the assertion fired correctly. The env is broken, not the test. Confirm `nix` is on PATH (`which nix`) and re-run.
- An integration `build`/`run`/`test` scenario failing GHC compile → likely upstream `neohaskell` drift; see `CLAUDE.md` § "When happy-path scenarios go red" before touching anything.
- A wrapped command hung for >5 minutes inside `nix develop --command cargo test --test integration_tests` → that's normal. Haskell builds inside. Do not kill unless approaching the test's own timeout.

## Refusals

- The user asked to run cargo against `result/bin/neo` directly — that's the e2e binary, not cargo's target. Point them at `cargo test --test e2e`.
- The user asked to set `NEO_SKIP_NETWORK=1` for an integration test — refuse. Integration is real-network on purpose now; if a test needs stubbing, it belongs in unit tests under `src/`.

## Why

The wrapping rule and the strict-assertion contract come from the same root: NeoCLI's job is to produce a binary that orchestrates `nix`, `git`, and GitHub for end users. Tests that hide missing prereqs lie about whether the binary works for the user; tests that run with a host-installed toolchain lie about whether the binary works as shipped from `nix build`. Both lies were eliminated on 2026-06-10 — this skill exists to keep them out.
