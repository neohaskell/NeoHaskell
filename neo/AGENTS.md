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

## Canonical monorepo package output

The Neo CLI is a first-class monorepo output. From the repo root:

```sh
nix build .#neo        # build bin/neo; its checkPhase runs the in-crate unit tests
nix run   .#neo -- --version
```

The derivation is defined once in `nix/neo-package.nix` (called by the root
`flake.nix`). It stays an independent crate: it reads its version from
`neo/Cargo.toml`, vendors deps from the pinned `neo/Cargo.lock`, and does NOT
join a root Cargo workspace, so the Neo CLI release train is not coupled to the
NeoHaskell library version. `nix build ./neo` (the crate-local flake) still works
for a quick dev build without the packaged test check.

The binary embeds `assets/ide/dist/` (rust-embed). That built bundle is committed
and the package consumes it, so `./dev neo-dist-check` proves the committed bundle
is a faithful from-lockfile rebuild (also run in the `neo-ci.yml` component gate).
The Neo CLI is gated by `.github/workflows/neo-ci.yml` (Rust fmt/clippy baseline,
the full binary unit-test suite, IDE install/test/build, and the Nix package build
plus app smoke on Linux and macOS), which runs on arbitrary stacked-PR bases, not
by the Haskell Test gate.

## Release compatibility contract (neo <-> NeoHaskell)

NeoHaskell (the framework) and neo (the CLI) keep **independent SemVer** and
release trains (`neo-v*` / `installer-v*` are decoupled from the library tags).
Because of that independence, **every neo release must publish an explicit
compatibility contract** stating which NeoHaskell source revision that neo
version is compatible with.

- **Single, executable source of truth — never a hand-maintained table.**
  `./dev neo-release compat` (subcommand of `scripts/neo-release`) DERIVES the
  contract from the embedded starter's authoritative pins in `neo/starter/`:
  `flake.nix` `neohaskellCommit`, `flake.lock`'s neohaskell `rev`, and every
  `cabal.project` `tag:`. It **fails closed on any drift** among those pins and
  emits `neo-compatibility.json` (`schema: neo-compat/v1`, `neo_version`,
  `neohaskell.{repo,ref,source_revision}`).
- **Released artifact.** `neo-release.yml`'s publish job generates
  `neo-compatibility.json`, includes it in `SHA256SUMS`, and ships it with the
  binaries. `scripts/workflow-check` (`check_neo_release`) freezes that.
- **Gated, not just documented.** The generated-project consumer contract
  (`./dev neo-consumer-contract`, phase 2b) enforces two things: (A) the contract
  GENERATES and is FAITHFUL — its `source_revision` equals the committed starter
  `flake.lock` rev (re-read independently); and (B) the pins neo actually EMITS
  into a generated project are internally COHERENT — the flake input `?rev=`, the
  flake `neohaskellCommit`, and every generated `cabal.project` `tag:` are the
  same revision. (`neo new` offline renders a deterministic placeholder rev; the
  real fetched revision is proven against the checkout in phase 3.) The gate
  never hardcodes a revision, so nothing drifts.
- **Generated projects PIN this revision** (not the moving `main` ref). `neo new`
  writes `neo.json` `neo-version` = the revision the embedded starter is locked to
  (`starter_neohaskell_rev()`), and `fetch_neo_sha` returns a 40-hex version
  as-is. So a fresh project builds the exact, immutable closure the contract
  declares — reproducible and cacheable.
- **Cache priming:** the trusted `cache-populate` job (neo-ci.yml) also runs
  `./dev cache-prime`, which builds the DEFAULT released generated-project closure
  (no checkout override) and pushes it to the public Cachix, so the clean-machine
  onboarding SLO substitutes it within 600 s. This is separate from the
  exact-checkout `neo-consumer-contract` closure (both are primed).
- **To bump the compatible NeoHaskell revision:** update all three starter pins
  together (see the comment in `neo/starter/flake.nix`); the drift self-test
  (`scripts/neo-release --self-test`, run by `./dev doctor` + CI) refuses a
  partial bump. If a source byte changes, it must land BEFORE cutting the tag.

## Test layers (detail in `neo-cli-testing`)

| Layer | Command (from the monorepo root) | Binary under test |
|---|---|---|
| Unit | `cd neo && nix develop --command cargo test --bins` | in-crate (`neo` is a binary crate, never `--lib`) |
| Integration | `cd neo && nix develop --command cargo test --test integration_tests -- --test-threads=1` | `cargo`-built debug `neo` (real nix + network) |
| E2E | `cd neo && nix build && nix develop --command cargo test --test e2e -- --ignored --test-threads=1` | nix-built `result/bin/neo` (real nix + network) |
| Neo-on-Neo smoke | `neo new` a project, then `neo build` + `neo test` inside it | the built binary on a generated project |
| Consumer contract | `./dev neo-consumer-contract` (from the repo root) | the Nix-packaged `.#neo` on a generated project, against THIS checkout |

The generated-project consumer contract is run with `./dev neo-consumer-contract`.
It proves the packaged CLI and generated project against the checkout under test.
Treat `scripts/neo-consumer-contract` and `.github/workflows/neo-ci.yml` as the
executable sources of truth for its phases, routing, platform, and gate wiring; use
`--self-test` when changing its orchestration helpers. Do not weaken the contract
to report-only.

Timing is cold-vs-warm: test *execution* is seconds, but the *first* compile of the
Rust crate (and the Haskell that integration/e2e build inside) can take minutes on a
cold cache; warm re-runs are fast. `--test-threads=1` is mandatory for
integration/e2e. The strict-assertion contract holds: if a test fails because
`nix`/`git`/network is missing, fix the environment, never weaken the assertion (no
missing-prereq escape hatches).

## Internalized starter (source of truth: `neo/starter/`)

`neo new` scaffolds from the **internalized starter** at `neo/starter/`, embedded
into the binary at compile time (rust-embed, `src/network.rs`). There is no runtime
download: generation is offline and pinned to the exact monorepo revision the binary
was built from. Fix generation/starter bugs in `neo/starter/`, never in an external
repository. Provenance and the intentional-exclusion policy live in
`neo/starter/IMPORT.md`; `./dev neo-skills-check` enforces both (no leaked VCS
metadata/secrets/build artifacts, manifest present, load-bearing surfaces exist).

## Cross-component correctness gate

Neo generates NeoHaskell projects. The `build`/`run`/`test` happy paths require the
generated project to actually compile, which depends on the **starter to upstream
contract**: `neo new` scaffolds from the embedded `neo/starter/`, then `neo build`
locks the latest `neohaskell` `main`. When upstream renames/removes a module the
starter imports, generated projects fail GHC compile. That red bar is the intended
signal: fix `neo/starter/` in this monorepo; never mask or `#[ignore]` it. Because
the starter is now in-repo, moving the starter and its NeoHaskell counterpart
together is a single-repo atomic change across stack layers: use `neo-cli-localizer`
for the Rust side and the NeoHaskell localizer for the matching lower/adjacent
stack layer.

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
