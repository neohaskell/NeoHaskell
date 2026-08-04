# NeoCLI

The NeoHaskell CLI tool.

## Features

- **Branded Developer Experience**: Interactive TUI with the Neo mascot, spinners, and progress bars.
- **Smart Scaffolding**: `neo new` guides you through project creation with a full interview.
- **Nix-Powered**: Seamless integration with Nix flakes for reproducible builds.
- **Watch Mode**: `neo build --watch`, `neo run --watch`, and `neo test --watch` for instant feedback.
- **Lock System**: Prevent accidental modification of domain files with `neo lock`.
- **Integrated Testing**: Run both unit tests and Hurl integration tests with `neo test`.

## Prerequisites

- **Nix**: Required for building and running projects. [Install Nix](https://nixos.org/download).
- **Git**: Required for project initialization and the locking system.
- **direnv** (Optional): Recommended for automatic editor integration (HLS).

## Installation

```bash
cargo install --path .
```

## Usage

### Create a new project
```bash
neo new
```
*(Runs an interactive interview if no project name is provided)*

### Build the project
```bash
neo build
```

### Run the project
```bash
neo run
```

### Run tests
```bash
neo test
```

### Watch mode
```bash
neo build --watch
```

### Lock domain files
```bash
neo lock MyDomainFile
```

### CI Mode
For headless environments, use the `--ci` flag to disable interactive prompts and animations:
```bash
neo build --ci
```

## Testing

Fast unit + integration suite:
```bash
cargo test
```

Full end-to-end suite against the nix-built binary (requires `nix build` first and real network access):
```bash
cargo test --test e2e -- --ignored --test-threads=1
```

## License

MIT
