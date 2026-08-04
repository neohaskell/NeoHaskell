---
name: neo-cli-localizer
description: Route a request about the Rust Neo CLI under `neo/**` to the exact command, subsystem and module before editing. Use at PLAN time for any `neo/**` change — locate by command surface (new/build/run/test/lock/ide/inspect/validate/skills), by subsystem (reconcile, subprocess, interpret, ide, tui), or by error/output contract. This is a Rust crate, not NeoHaskell dialect code; do not apply the Haskell HARD RULE, dialect table, or spec-gated pipeline here.
---

# Neo CLI localizer

`neo/` is the imported Rust Neo CLI — a `clap`/`tokio`/`ratatui` binary that
orchestrates `nix`, `git` and GitHub to scaffold, build, run and test NeoHaskell
projects, plus a bundled browser IDE (`neo ide`). Localization here is lookup by
**command** and **subsystem**, not tree-walking. Read the target module, then use
`neo-cli-implementer` to change it and `neo-cli-testing` to verify it.

## By command surface (`src/cli.rs` → `src/commands/*`)

The `clap` command enum lives in `src/cli.rs`; each variant dispatches through
`src/app.rs` to a module under `src/commands/`.

| Command | Entry module | What it does |
|---|---|---|
| `neo new [name] [--library]` | `commands/new.rs` | Interactive interview → scaffold from the `neo-starter` tarball |
| `neo build [--watch] [--skip-lock-check]` | `commands/build.rs` | Reconcile config → `nix`/`cabal` build; `--watch` = GHCi hot-reload |
| `neo run [--watch]` | `commands/run.rs` | Build then execute the app |
| `neo test [--watch]` | `commands/test.rs` | Cabal unit tests then Hurl integration tests |
| `neo lock <file>` | `commands/lock.rs`, `src/lock.rs` | Lock event-sourced domain files (`.locked-files`, pre-commit hook) |
| `neo ide [--host --port]` | `commands/ide.rs`, `src/ide/**` | Serve the embedded Vite IDE + JSON-RPC/WebSocket backend |
| `neo inspect [sub]` | `commands/inspect.rs`, `src/inspect/**` | Emit the project's domain layout as JSON |
| `neo validate [path] [--json]` | `commands/validate.rs`, `src/ide/validate.rs` | Validate `event-model.json` (exit codes 0/1/2/3/4) |
| `neo skills <sub>` | `commands/skills.rs`, `src/skills.rs` | Install the shared skill library into AI tools |

Global flags (`src/cli.rs`): `--verbose`, `--ci` (both `global = true`).

## By subsystem

- **Config reconcile** — `src/reconcile/**`: `neo.json` → generated `flake.nix`,
  `cabal.project`, `<name>.cabal`, module list. `dep_spec.rs` = the dependency
  grammar (`hackage:` / `git:` / `github:` / `file:` prefixes); `resolve.rs` =
  registry resolution; `flake.rs` / `cabal.rs` / `cabal_project.rs` = emitters.
- **Subprocess wrappers** — `src/subprocess/**`: `nix.rs`, `ghci.rs`, `hurl.rs`.
  All child-process orchestration; every failure is routed through interpretation.
- **Error interpretation** — `src/interpret/patterns/*.rs`: one file per known
  stderr signature (cabal unknown-package, nix attribute-missing, git bad-ref,
  hurl connection-refused, …). A new interpretable failure = a new pattern file
  registered in `interpret/patterns/mod.rs`. Unrecognized failures append to the
  local backlog via `src/errlog.rs`.
- **Errors + diagnostics** — `src/errors.rs`: the `NeoError` enum (miette
  `Diagnostic`). Output contract lives here; see `neo-cli-implementer`.
- **Output / TUI** — `src/output.rs` (`OutputMode`), `src/tui/**` (banner,
  spinner, progress, prompts, multiselect, watch view), `src/theme.rs`.
- **IDE** — `src/ide/**` (Rust backend: `rpc.rs`, `registry.rs`, `server.rs`,
  `methods/*.rs`, `heal/*`, `sync.rs`, `validate.rs`) and `assets/ide/**` (the
  Vite/React frontend). See `neo-cli-ide`.
- **Prereqs / env** — `src/prereqs.rs` (nix/git presence guards), `src/network.rs`
  (`NEO_SKIP_NETWORK`), `src/config.rs`, `src/git.rs`.

## Cross-component correctness (monorepo governance)

Neo generates NeoHaskell projects. The happy-path `build`/`run`/`test` scenarios
depend on the **starter ↔ upstream contract**: `neo new` tarballs
`github.com/NeoHaskell/neo-starter@main`, then `neo build` locks the latest
`neohaskell` `main`. When upstream renames/removes a module the starter imports,
generated projects fail to compile. That is a real signal — fix `neo-starter`
upstream; never mask or `#[ignore]` the failing scenario. Details in
`neo/AGENTS.md` and `neo-cli-testing`.

## Rule

Read the one module the table points at. If the command/subsystem tables and
`neo/AGENTS.md` still don't locate it, then (and only then) grep `neo/src/`.
Never open the Haskell `core/`, `testbed/`, or `integrations/` trees for a
`neo/**` task.
