# NeoHaskell Installer

The official installer for [NeoHaskell](https://neohaskell.org). Sets up everything you need to start building with Neo.

## Quick Install

```sh
curl -fsSL https://raw.githubusercontent.com/neohaskell/NeoHaskell/main/installer/scripts/bootstrap.sh | sh
```

## Manual Install

Download the latest binary for your platform from [Releases](https://github.com/neohaskell/NeoHaskell/releases):

| Platform | Download |
|----------|----------|
| macOS (Apple Silicon) | `neo-install-aarch64-apple-darwin` |
| macOS (Intel) | `neo-install-x86_64-apple-darwin` |
| Linux (x86_64) | `neo-install-x86_64-unknown-linux-gnu` |
| Linux (aarch64) | `neo-install-aarch64-unknown-linux-gnu` |

```sh
chmod +x neo-install-*
./neo-install-*
```

## Options

| Flag | Description |
|------|-------------|
| `--dry-run` | Show what would be done without doing it |
| `--verbose` | Show detailed output |
| `--force` | Reinstall even if already installed |
| `--help` | Show help information |
| `--version` | Show version |

| Environment variable | Description |
|----------------------|-------------|
| `NEO_VERSION` | Pin a specific Neo CLI release, e.g. `NEO_VERSION=neo-v0.1.0`. Unset resolves the newest `neo-v*` release. Validated to a `neo-v*` tag. |
| `NEO_BIN_DIR` | Absolute directory the native `neo` binary is installed into. Defaults to `$HOME/.local/bin`. Validated as an absolute, control-character-free path (rejected otherwise) and single-quoted when written to your shell profile. |

## How It Works

The installer performs three steps:

1. **Toolchain Setup** — Installs the required build toolchain (Nix) if not
   already present. Generated NeoHaskell projects require Nix, so this stays a
   declarative Nix path (`neo build`/`neo run` use the project's own flake).
2. **Neo CLI** — Downloads the **prebuilt native `neo` binary** for your platform
   from the [NeoHaskell releases](https://github.com/neohaskell/NeoHaskell/releases),
   verifies it against the release's `SHA256SUMS` **before** installing, and
   installs it atomically to a user-writable bin directory. It does **not**
   evaluate or compile the Neo CLI from a flake — normal users get a fast, native
   download with a checksum guarantee.
3. **Verification** — Confirms everything is working correctly.

After installation, run `neo new myproject` to create your first project.

### Release contract (why the tag prefixes)

Three independent release trains publish to the one `neohaskell/NeoHaskell`
monorepo, disambiguated only by tag prefix (GitHub tag triggers ignore path
filters):

- **`installer-v*`** — this installer's own native binaries
  (`installer-neo-install-<target>`), published by
  [`installer-ci.yml`](../.github/workflows/installer-ci.yml). The
  `curl | sh` bootstrap downloads these.
- **`neo-v*`** — the native `neo` CLI binaries (`neo-<target>`), published by
  [`neo-release.yml`](../.github/workflows/neo-release.yml). This installer
  downloads these at step 2.
- The NeoHaskell library tags — unrelated to either binary.

Because assets live **only** under their own tag prefix, neither the bootstrap
nor the installer uses the repository-wide `releases/latest` redirect (it would
resolve some *other* train's newest release and 404). Both resolve the newest
tag of their **own** prefix explicitly. The asset-naming, target list, repository,
and tag prefixes are frozen against drift by the tests in
[`tests/consistency.rs`](tests/consistency.rs) and by `./dev workflow-check`.

## Supported Platforms

| OS | Architecture | Status |
|----|-------------|--------|
| macOS | Apple Silicon (aarch64) | ✅ Supported |
| macOS | Intel (x86_64) | ✅ Supported |
| Linux | x86_64 | ✅ Supported |
| Linux | aarch64 | ✅ Supported |

## Development

### Prerequisites

- Rust toolchain (install via [rustup](https://rustup.rs))

### Build

```sh
cargo build
```

### Test

```sh
cargo test
```

### Lint

```sh
cargo clippy -- -D warnings
cargo fmt --check
```

### Architecture

The installer is built in Rust and uses the [Determinate Nix Installer](https://github.com/DeterminateSystems/nix-installer) under the hood to set up the Nix package manager, then downloads and installs the prebuilt native `neo` binary (verified against `SHA256SUMS`).

Source modules:
- `detect.rs` — Platform and existing installation detection
- `install.rs` — Nix installation + shell PATH setup (delegates Neo to `release`)
- `release.rs` — Native Neo release resolution, download, checksum verification, atomic install
- `verify.rs` — Post-install verification
- `ui.rs` — Terminal output and progress indicators
- `error.rs` — Error types and exit codes

## License

MIT
