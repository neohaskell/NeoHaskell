---
name: neo-cli-testing
description: Run and reason about the Neo CLI test layers under `neo/**` - unit, integration, e2e, and the generated-project / Neo-on-Neo smoke verification. Use before declaring any `neo/**` change done, or on "run the neo tests / cargo test / integration / e2e". Wraps the right command per layer inside `nix develop`, states network/Nix prerequisites, and enforces the strict-assertion contract (fix the environment, never weaken the assertion).
---

# Neo CLI testing

Four layers, each with an invocation from the monorepo root. Every Cargo command selects `neo/flake.nix` explicitly; host toolchains are unsupported. `nix build` is a Nix CLI command and runs outside the dev shell.

| Layer | Binary under test | Command | Network / Nix | Time |
|---|---|---|---|---|
| Unit | in-crate functions, parsers, TUI widgets, prereqs guards | `nix develop ./neo -c cargo test --manifest-path neo/Cargo.toml --locked --bins` | none | seconds after compilation; cold compile can take minutes |
| Integration | Cargo-built debug `neo` via `cargo_bin("neo")` | `nix develop ./neo -c cargo test --manifest-path neo/Cargo.toml --locked --test integration_tests -- --test-threads=1` | real Nix + real network | minutes |
| E2E | the Nix-built `neo/result/bin/neo` that a user installs | `nix build ./neo -o neo/result && nix develop ./neo -c cargo test --manifest-path neo/Cargo.toml --locked --test e2e -- --ignored --test-threads=1 --nocapture` | real Nix + real network | minutes |
| Neo-on-Neo smoke | the binary driven over a generated project | run `neo new`, then `neo build` + `neo test` inside the project | real Nix + real network | minutes |

Notes:
- `neo` is a **binary** crate - unit tests are `--bins`, never `--lib`
  (`cargo test --lib` errors with "no library targets found").
- Integration and e2e **must** pass `--test-threads=1` - real network rate limits
  and the shared nix-store build lock forbid parallelism.
- E2E requires a fresh `neo/result/bin/neo`; run `nix build ./neo -o neo/result` first (the helper panics
  with a clear hint if it's missing).
- `NEO_E2E_KEEP=1` preserves e2e sandbox dirs (default: kept only on failure).

## Which layer to add a test to

Each scenario lives at the **lowest layer that observes the behavior**. Pure logic
(parsers, dep grammar, error formatting, output-mode branching) -> unit under
`src/**`. Full-CLI behavior against real nix/git/GitHub -> integration. Behavior of
the *shipped* binary end-to-end -> e2e. When a behavior is worth testing, prefer
adding it to e2e (it covers the installed binary) and mirror into integration only
when you also want it to gate the fast `cargo test`.

## Strict-assertion contract (do not weaken)

Integration and e2e assume the environment an end user has: `nix`, `git`, network,
and (for e2e) a `nix build`ed `result/bin/neo`. There are **no** `if success {…}
else accept-missing-nix {…}` escape hatches - that pattern was deleted on
2026-06-10. If a test fails because the env lacks a prereq, **fix the environment,
never soften the assertion**. Refuse `NEO_SKIP_NETWORK=1` for integration/e2e - they
are real-network on purpose; logic that needs stubbing belongs in unit tests.

## Generated-project / Neo-on-Neo verification (cross-component gate)

The `build` / `run` / `test` happy-path scenarios (integration + e2e) require the
generated NeoHaskell project to actually compile. The two recurring breakages:

1. **Starter-to-upstream drift.** `neo new` tarballs `neo-starter@main`; `neo build`
   locks the latest `neohaskell` `main`. When upstream renames/removes a module the
   starter imports, the generated project fails GHC compile. **Fix `neo-starter`
   upstream and push** - do not touch the neo test.
2. **A transitive Haskell dep refusing to build under plain cabal** (historically
   `jose` needing native crypto paths from `haskell.nix`) - fix the templated
   `flake.nix`.

Either way these red bars are the intended signal that the starter-to-upstream
contract broke. **Never `#[ignore]` or mask them.**

## Failure fingerprints

- `ld: library not found for -liconv` -> ran outside `nix develop`. Re-wrap.
- `no library targets found in package 'neo'` -> used `--lib`; use `--bins`.
- `Nix is required but not found` in a test's stderr -> the assertion fired
  correctly; the env is broken, not the test. Confirm `which nix`, re-run.
- An integration/e2e `build`/`run`/`test` scenario failing GHC compile -> likely
  upstream drift (case 1 above); investigate the starter contract, touch nothing
  in `neo/`.
- A wrapped `cargo test --test integration_tests` "hanging" >5 min -> normal;
  Haskell builds inside. Don't kill it unless it nears the test's own timeout.

## Completion criteria for a `neo/**` change

Done = the layers that observe the change are green (unit always; integration when
the CLI surface/behavior changed; e2e when the shipped-binary behavior changed),
`nix develop ./neo -c cargo clippy --manifest-path neo/Cargo.toml` exits zero (report baseline warnings; do not fix unrelated lint while completing another task), and - if the generated project
layout or starter contract was touched - a Neo-on-Neo smoke run compiles. State
which layers you ran and their results; do not claim a layer you did not run.
