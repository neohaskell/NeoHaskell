# Neo CLI - agent guide (`neo/**`)

`neo/` is the imported **Rust** Neo CLI: a `clap` + `tokio` + `ratatui` + `miette`
binary that orchestrates `nix`, `git` and GitHub to scaffold, build, run and test
NeoHaskell projects, plus a bundled browser IDE (`neo ide`, Vite/React frontend at
`assets/ide/`). It is a normal Cargo crate that happens to live in the NeoHaskell
monorepo.

## Scope override - this is Rust, not NeoHaskell dialect

The repo-root `AGENTS.md` governs the Haskell trees (`core/`, `testbed/`,
`integrations/`). Under `neo/**` those rules do **not** apply:

- The Haskell **dialect table** (`|>`, `Task`, no-`$`, `case`-of-Bool, Core
  wrappers, and so on) and the dialect edit-hook are Haskell-only. Write idiomatic
  Rust that matches the surrounding code.
- The **spec-gated change pipeline** (`docs/changes/`, `./dev spec-check`,
  `./dev pipeline`, tier lints) governs Haskell changes. `neo/**` work is verified
  by its own Rust test layers, not that pipeline.
- The root **HARD RULE / codemap localization** (`./dev api`, `codemap/`) indexes
  the Haskell API. Localize `neo/**` with the `neo-cli-localizer` skill instead.

**Still binding (monorepo governance):** this project uses official GitHub stacked
PRs (`gh stack`), so branch from the owning lower stack layer (not from `main`) and
never edit `main` directly. ADRs live in `docs/decisions/NNNN-slug.md`. Every change
ships with tests (happy + error + boundary, bug fixes get a regression test). Never
modify an existing test expectation to turn a red bar green without confirming the
behavior change was intended. And the **cross-component correctness gate** below
holds.

## Skills - one source of truth (repo-root `neo-cli-*`)

Route `neo/**` work through these discoverable skills; they hold the detail this
file only summarizes:

- **`neo-cli-localizer`**: find the command/subsystem/module before editing.
- **`neo-cli-implementer`**: Rust conventions, the error-as-LLM-repair contract,
  output prefixes, and interactive-vs-CI (`--ci`) behavior.
- **`neo-cli-testing`**: the unit/integration/e2e/smoke layers and the
  strict-assertion contract.
- **`neo-cli-ide`**: the Vite frontend, embedded `dist/` sync, and screen critique.

## Working environment

Run every toolchain command through the dev shell defined by `neo/flake.nix` (the
only authoritative toolchain for this crate). Host-installed toolchains are
unsupported: do not assume a host `cargo`/`node`/`hurl` exists, and do not let one
shadow the pinned tools. Because `neo/flake.nix` is a sibling flake in this
monorepo, `cd` into `neo/` first so `nix develop` resolves the neo flake and not the
Haskell root flake:

```sh
cd neo                                               # from the monorepo root, once per shell
nix develop --command ./scripts/build.sh             # full stack (frontend + binary)
nix develop --command ./scripts/build.sh --release   # release
nix develop --command cargo build                    # Rust-only (reuses built dist/)
```

From the monorepo root without `cd`, use the explicit flake ref and manifest path,
for example `nix develop ./neo -c cargo build --manifest-path neo/Cargo.toml`. A
bare `nix develop --command cargo ...` at the monorepo root resolves the Haskell
root flake, not neo's, and can pick up the wrong profile tools. The link error
`ld: library not found for -liconv` means the neo dev shell was not active; the fix
is to enter it (`cd neo` or `nix develop ./neo -c`), never to install iconv.
`nix build ./neo` and `nix flake check ./neo` are nix-CLI and run outside the dev
shell.

## Test layers (detail in `neo-cli-testing`)

| Layer | Command (from the monorepo root) | Binary under test |
|---|---|---|
| Unit | `cd neo && nix develop --command cargo test --bins` | in-crate (`neo` is a binary crate, never `--lib`) |
| Integration | `cd neo && nix develop --command cargo test --test integration_tests -- --test-threads=1` | `cargo`-built debug `neo` (real nix + network) |
| E2E | `cd neo && nix build && nix develop --command cargo test --test e2e -- --ignored --test-threads=1` | nix-built `result/bin/neo` (real nix + network) |
| Neo-on-Neo smoke | `neo new` a project, then `neo build` + `neo test` inside it | the built binary on a generated project |

Timing is cold-vs-warm: test *execution* is seconds, but the *first* compile of the
Rust crate (and the Haskell that integration/e2e build inside) can take minutes on a
cold cache; warm re-runs are fast. `--test-threads=1` is mandatory for
integration/e2e. The strict-assertion contract holds: if a test fails because
`nix`/`git`/network is missing, fix the environment, never weaken the assertion (no
missing-prereq escape hatches).

## Cross-component correctness gate

Neo generates NeoHaskell projects. The `build`/`run`/`test` happy paths require the
generated project to actually compile, which depends on the **starter to upstream
contract**: `neo new` tarballs `github.com/NeoHaskell/neo-starter@main`, then `neo
build` locks the latest `neohaskell` `main`. When upstream renames/removes a module
the starter imports, generated projects fail GHC compile. That red bar is the
intended signal: fix `neo-starter` upstream; never mask or `#[ignore]` it. A change
that must move the CLI and its NeoHaskell counterpart together is an atomic
cross-component change: use `neo-cli-localizer` for the Rust side and the NeoHaskell
localizer for the matching lower/adjacent stack layer.

## Errors are LLM-actionable repair instructions (HARD invariant)

Every user-facing error (`NeoError` variants, miette diagnostics, subprocess wraps,
panics reachable from user input) must be fixable by the smallest dumb model from
the message alone: state (1) what operation failed, (2) the bad input quoted, (3)
the expected shape, (4) a concrete fix recipe. Subprocess wraps interpret known
stderr signatures (`src/interpret/patterns/*.rs`) rather than dumping raw output.
Full contract, examples, and bad-to-good rewrites: the `neo-cli-implementer` skill.

## When changing CLI behavior

If you change a subcommand surface, flag, error message, output prefix
(`[info]`/`[ok]`/`[warn]`/`[error]`/`[fail]`), JSON field, or the generated project
layout, update the affected assertions in **both** `tests/integration_tests.rs` and
`tests/e2e.rs` in the same change.

## Governance self-check

`./dev neo-skills-check` (run by `./dev doctor` and CI) validates this layer: the
`neo-cli-*` skills exist, `neo/**` routing is intact, there are no references to
retired standalone state/plan files, and there is one source of truth. Keep it green.
