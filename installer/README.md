# NeoHaskell Installer

The official installer for [NeoHaskell](https://neohaskell.org). Sets up everything you need to start building with Neo.

## Quick Install

```sh
curl -fsSL https://raw.githubusercontent.com/neohaskell/neo-installer/main/scripts/bootstrap.sh | sh
```

## Manual Install

Download the latest binary for your platform from [Releases](https://github.com/neohaskell/neo-installer/releases):

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

## How It Works

The installer performs three steps:

1. **Toolchain Setup** — Installs the required build toolchain if not already present
2. **Neo CLI** — Installs the Neo command-line interface
3. **Verification** — Confirms everything is working correctly

After installation, run `neo new myproject` to create your first project.

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

The installer is built in Rust and uses the [Determinate Nix Installer](https://github.com/DeterminateSystems/nix-installer) under the hood to set up the Nix package manager, then installs Neo CLI via `nix profile install`.

Source modules:
- `detect.rs` — Platform and existing installation detection
- `install.rs` — Nix and Neo CLI installation logic
- `verify.rs` — Post-install verification
- `ui.rs` — Terminal output and progress indicators
- `error.rs` — Error types and exit codes

## License

MIT
