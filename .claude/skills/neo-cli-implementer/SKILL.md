---
name: neo-cli-implementer
description: Write or modify Rust code in the Neo CLI under `neo/**` - crate conventions, the error-as-LLM-repair contract, output prefixes, and interactive-vs-CI (`--ci`) behavior. Use when editing any `neo/**` `.rs` file, adding a subcommand or JSON field, wrapping a subprocess, or writing any user-facing error. This is Rust, not NeoHaskell - the Haskell dialect table and edit-hook do not apply under `neo/**`.
---

# Neo CLI implementer

`neo/` is a Rust crate (`edition = "2024"`, `clap` + `tokio` + `ratatui` +
`miette`). Match the surrounding code. The dialect rules that govern `core/`,
`testbed/`, `integrations/` (`|>`, `Task`, no-`$`, Core wrappers, …) are **Haskell
rules and do not apply here** - do not import them into `.rs` files. Localize
first with `neo-cli-localizer`; verify with `neo-cli-testing`.

## Toolchain

Run commands from the monorepo root using the `neo/flake.nix` toolchain explicitly. Host-installed `cargo`, `rustc`, Node or npm are unsupported and must not shadow the pinned tools:

```sh
nix develop ./neo -c cargo build --manifest-path neo/Cargo.toml
nix develop ./neo -c cargo clippy --manifest-path neo/Cargo.toml
nix develop ./neo -c cargo fmt --manifest-path neo/Cargo.toml
neo/scripts/build.sh
```

`neo/scripts/build.sh` enters the Neo dev shell itself and builds the frontend before the Rust binary. The telltale of using the wrong toolchain is `ld: library not found for -liconv`; enter `nix develop ./neo`, never install iconv. `nix build ./neo` and `nix flake check ./neo` are Nix CLI commands and run outside the dev shell.

## Interactive vs CI (`OutputMode`)

`src/main.rs` computes `is_ci = cli.ci || std::env::var("CI").is_ok()` and picks
`OutputMode::Ci` vs `OutputMode::Interactive` (`src/output.rs`). Any code that
animates, prompts, or draws TUI must branch on the mode:

- **Interactive** - `ratatui` TUI, spinners, progress bars, interview prompts
  (`src/tui/**`). Reachable only when a real terminal is attached.
- **Ci** - no prompts, no animation, no color; plain line output. `neo new` in CI
  requires the project name as an arg (no interview). Every command must be fully
  driveable headlessly under `--ci`.

New user-facing progress lines use the existing prefixes: `[info]`, `[ok]`,
`[warn]`, `[error]`, `[fail]`. Grep `src/commands/*` for the current phrasing and
stay consistent - the test suites assert on these prefixes and messages (see
"When changing CLI behavior" below).

## Errors are LLM-actionable repair instructions (HARD invariant)

Every user-facing error - `NeoError` variants (`src/errors.rs`), `miette::miette!`
/ `bail!`, subprocess wraps, `format!` into stderr, any `panic!`/`expect` reachable
from user input - **must be repairable by the smallest dumb model** (haiku,
gemini-flash, gpt-3.5-nano level) from the message *alone*, without docs or source.
Agentic loops and self-healing pipelines run on tiny models; an opaque error is a
bug of the same severity as a wrong result.

Every such error states, in this order:

1. **What operation failed** - verb + noun ("parsing `neo.json` dependency value",
   not "Subprocess error").
2. **The bad input, quoted** - file + line, the exact string, the env var name.
   An empty value prints as `""`, never blank.
3. **The expected shape** - grammar / schema / one valid example, one sentence.
4. **A concrete fix recipe** - a copy-pasteable edit or command. Never "check the
   docs" or "verify your config".

Template:

```
<Operation> failed: <one-line cause naming the bad input>.
Expected: <grammar / valid shape / one example>.
Fix: <edit X to Y | run Z | replace A with B>.
```

Bad -> good:

```
// BAD
Subprocess execution failed: cabal build all
    diagnostic help: Check the output above for more details.

// GOOD
cabal build failed while resolving build-depends of `test-project`.
Cause: package `foo` is in `neo.json` but not on Hackage and not declared as a git/file source.
Expected: every name in `neo.json` `dependencies` is bare (in the NeoPackages registry),
  or prefixed `hackage:` / `git:` / `github:` / `file:`.
Fix: edit `neo.json` - set `"hackage:foo": "^1.2.0"` if it's on Hackage, or remove it. Re-run `neo build`.
```

### Applying it in the two hot spots

- **`NeoError` variants** - `#[error("…")]` is the headline (operation + bad
  input); `#[diagnostic(help("…"))]` is the fix recipe (concrete, single-paste);
  add `url(…)` for a canonical page. If a variant wraps a foreign error, its
  `Display` must still carry the operation + input, not just delegate.
- **Subprocess wraps** (`src/subprocess/**` -> `src/interpret/patterns/*.rs`) -
  do **not** dump raw child stderr. When the stderr matches a known signature,
  interpret it into a recipe. Adding a newly-seen interpretable failure = a new
  `interpret/patterns/<name>.rs` registered in `patterns/mod.rs`; unrecognized
  failures still get logged verbatim to the local backlog (`src/errlog.rs`) so
  they can become issues. Known signatures already covered include cabal
  unknown-package / could-not-resolve, nix attribute-missing, git bad-ref, and
  hurl connection-refused.

Do not `unwrap`/`expect`/`panic!` on any user-reachable `Result`. Convert to `?`
with an error variant that meets the contract. The only acceptable `unwrap` is on
a value the type system guarantees cannot fail.

Checklist before submitting any error-producing code - read the message aloud as
the smallest model: does the first sentence name the operation? Is the exact bad
input quoted? Is the expected shape one sentence + one example? Is the fix
copy-pasteable? If any answer is no, rewrite. Prefer a
`predicate::str::contains("<the fix phrase>")` assertion so a regression that drops
the recipe fails fast.

## When changing CLI behavior - update both suites in the same change

If you change a subcommand surface, a flag, an error message, an output prefix, a
JSON field, or the generated project layout, the assertions in **both**
`tests/integration_tests.rs` and `tests/e2e.rs` reference it. Find and update them
in the same change (`neo-cli-testing` covers the layers). Never modify an existing
test expectation to make a red bar green without confirming the behavior change was
intended - that is the monorepo's non-negotiable rule.

## Release compatibility contract (neo <-> NeoHaskell)

neo and NeoHaskell keep independent SemVer, so every neo release publishes
`neo-compatibility.json` mapping the neo version to the compatible NeoHaskell
source revision. It is generated — never hand-written — by `./dev neo-release
compat` (`scripts/neo-release`, the release single-source-of-truth), which reads
the embedded starter's pins (`neo/starter/` `flake.nix` `neohaskellCommit`,
`flake.lock` `rev`, `cabal.project` `tag:`) and **fails closed on drift**.
`neo-release.yml` ships it as a checksummed asset; the consumer contract (phase
2b) gates that a generated project pins the declared revision. If you touch the
starter's NeoHaskell pin, update all three pins together and keep `neo-release
--self-test` green. Full contract: `neo/AGENTS.md`.

## Ship-with-tests

Every change ships with tests (happy path + error + boundary); bug fixes include a
regression test at the lowest layer that observes the bug. See `neo-cli-testing`
for layer selection and the strict-assertion contract.
