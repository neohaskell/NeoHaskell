# Neo CLI — agent guide (`neo/**`)

`neo/` is the imported **Rust** Neo CLI: a `clap` + `tokio` + `ratatui` + `miette`
binary that orchestrates `nix`, `git` and GitHub to scaffold, build, run and test
NeoHaskell projects, plus a bundled browser IDE (`neo ide`, Vite/React frontend at
`assets/ide/`). It is a normal Cargo crate that happens to live in the NeoHaskell
monorepo.

## Scope override — this is Rust, not NeoHaskell dialect

The repo-root `AGENTS.md` governs the Haskell trees (`core/`, `testbed/`,
`integrations/`). Under `neo/**` those rules **do not apply**:

- The Haskell **dialect table** (`|>`, `Task`, no-`$`, `case`-of-Bool, Core
  wrappers, …) and the dialect edit-hook are Haskell-only. Write idiomatic Rust
  that matches the surrounding code.
- The **spec-gated change pipeline** (`docs/changes/`, `./dev spec-check`,
  `./dev pipeline`, tier lints) governs Haskell changes. `neo/**` work is verified
  by its own Rust test layers, not that pipeline.
- The root **HARD RULE / codemap localization** (`./dev api`, `codemap/`) indexes
  the Haskell API. Localize `neo/**` with the `neo-cli-localizer` skill instead.

**Still binding (monorepo governance):** branch off `main`, never edit `main`
directly; ADRs live in `docs/decisions/NNNN-slug.md`; every change ships with tests
(happy + error + boundary, bug fixes get a regression test); never modify an
existing test expectation to turn a red bar green without confirming the behavior
change was intended; and the **cross-component correctness gate** below.

## Skills — one source of truth (repo-root `neo-cli-*`)

Route `neo/**` work through these discoverable skills; they hold the detail this
file only summarizes:

- **`neo-cli-localizer`** — find the command/subsystem/module before editing.
- **`neo-cli-implementer`** — Rust conventions, the error-as-LLM-repair contract,
  output prefixes, and interactive-vs-CI (`--ci`) behavior.
- **`neo-cli-testing`** — the unit/integration/e2e/smoke layers and the
  strict-assertion contract.
- **`neo-cli-ide`** — the Vite frontend, embedded `dist/` sync, and screen critique.

## Working environment

All toolchain commands run inside `nix develop` (host has no `cargo`/`node`/`hurl`
— they are flake-pinned). Bare `cargo build` failing with `ld: library not found
for -liconv` means the wrapper was skipped; the fix is always to wrap, never to
install iconv. `nix build` / `nix flake check` are nix-CLI and run *outside* the
dev shell.

```sh
nix develop --command ./scripts/build.sh            # full stack (frontend + binary)
nix develop --command ./scripts/build.sh --release  # release
nix develop --command cargo build                   # Rust-only (reuses built dist/)
```

## Test layers (detail in `neo-cli-testing`)

| Layer | Command | Binary under test |
|---|---|---|
| Unit | `nix develop --command cargo test --bins` | in-crate (`neo` is a binary crate — never `--lib`) |
| Integration | `nix develop --command cargo test --test integration_tests -- --test-threads=1` | `cargo`-built debug `neo` (real nix + network) |
| E2E | `nix build && nix develop --command cargo test --test e2e -- --ignored --test-threads=1` | nix-built `result/bin/neo` (real nix + network) |
| Neo-on-Neo smoke | `neo new` a project, then `neo build` + `neo test` inside it | the built binary on a generated project |

`--test-threads=1` is mandatory for integration/e2e. The strict-assertion contract
holds: if a test fails because `nix`/`git`/network is missing, **fix the
environment, never weaken the assertion** (no missing-prereq escape hatches).

## Cross-component correctness gate

Neo generates NeoHaskell projects. The `build`/`run`/`test` happy paths require the
generated project to actually compile, which depends on the **starter ↔ upstream
contract**: `neo new` tarballs `github.com/NeoHaskell/neo-starter@main`, then `neo
build` locks the latest `neohaskell` `main`. When upstream renames/removes a module
the starter imports, generated projects fail GHC compile. That red bar is the
intended signal — **fix `neo-starter` upstream; never mask or `#[ignore]` it.**

## Errors are LLM-actionable repair instructions (HARD invariant)

Every user-facing error (`NeoError` variants, miette diagnostics, subprocess wraps,
panics reachable from user input) must be fixable by the smallest dumb model from
the message alone: state (1) what operation failed, (2) the bad input quoted, (3)
the expected shape, (4) a concrete fix recipe. Subprocess wraps interpret known
stderr signatures (`src/interpret/patterns/*.rs`) rather than dumping raw output.
Full contract, examples, and bad→good rewrites: the `neo-cli-implementer` skill.

## When changing CLI behavior

If you change a subcommand surface, flag, error message, output prefix
(`[info]`/`[ok]`/`[warn]`/`[error]`/`[fail]`), JSON field, or the generated project
layout, update the affected assertions in **both** `tests/integration_tests.rs` and
`tests/e2e.rs` in the same change.

## Governance self-check

`./dev neo-skills-check` (run by `./dev doctor` and CI) validates this layer:
the `neo-cli-*` skills exist, `neo/**` routing is intact, there are no references to
retired standalone state/plan files, and there is one source of truth. Keep it green.
