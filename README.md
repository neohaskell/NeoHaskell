<div align="center">
  <img src=".assets/img/logo.png" height="256px"/>
  <h1>NeoHaskell</h1>
  <b>
    NeoHaskell is a dialect of Haskell that is focused on newcomer-friendliness and productivity.
  </b>
  <p>
    It is designed to be easy to learn and use, while also being powerful enough to release your app with minimum effort and maximum confidence.
  </p>
  <a href="https://github.com/neohaskell/NeoHaskell/graphs/contributors">
    <img src="https://img.shields.io/github/contributors/neohaskell/NeoHaskell?color=ee8449&style=flat-square" alt="Contributors" />
  </a>
</div>

---

# Welcome to the contributor guide

If you want to learn about NeoHaskell itself, checkout
[the NeoHaskell website](https://neohaskell.org).

This guide is intended to streamline the process of
contributing to the NeoHaskell tooling.

The repository will be a mono-repo that contains all the
different parts of NeoHaskell.

## Installing the required tools

(This assumes that you're using MacOS, WSL2 or Linux)

1. Install [Nix](https://nixos.org/download/) with flakes enabled
2. Run `nix develop` to enter the development shell
3. Run `cabal update && cabal build all`

### Binary Cache (Faster Builds)

The project uses [Cachix](https://cachix.org) to cache build artifacts. The `flake.nix` is pre-configured to use our cache, so `nix develop` should automatically prompt you to trust it on first run.

If you want to explicitly enable it (or if you have a restrictive Nix config):

```sh
# Install cachix (if not already installed)
nix-env -iA cachix -f https://cachix.org/api/v1/install

# Add the NeoHaskell cache
cachix use neohaskell
```

This dramatically speeds up the first `nix develop` (from ~30 min to ~2 min).

The recommended IDE for any NeoHaskell project is [Visual Studio Code](https://code.visualstudio.com/).

## Get the code

- Fork this repository
- `git clone <url to your fork>`
- `cd NeoHaskell && code .`

## Install the recommended extensions

When opening the project for the first time, you will be prompted to install the recommended extensions, install them.

## Code Formatting

This project uses the fourmolu formatter for consistent Haskell code styling. When using VS Code with the recommended extensions:

- Code will automatically format on save
- The formatter settings are controlled by the fourmolu.yaml file in the root directory

## Linting

This project uses hlint, it will automatically be run in VSCode by the recommended extension.
To run manually (same paths and config as the CI gate):

```sh
./dev lint
```

> Since 2026-07-07 the hlint config is **dialect-first**: it encodes
> NeoHaskell's style rules (vanilla modules restricted to their Core wrappers,
> banned partials, `$` → `|>` teaching hints) and runs as a CI gate.
> `./dev lint` runs the exact same check locally. No Core wrapper for what you
> need? The escape hatch is documented at the top of `.hlint.yaml`.

## Fast inner loop

The same tools the CI/agent pipeline uses work on demand for humans (they are
the *same scripts* on purpose — if you can't reproduce what the pipeline saw,
you can't debug it). One entrypoint, run `./dev` for the full menu:

```sh
./dev watch                          # resident typecheck watcher (ghcid, -O0)
./dev check                          # instant typecheck status
./dev test "EventStore"              # run only matching specs, no linking (~4-9s)
./dev test "insert" nhcore-test-service   # pick a suite
./dev refresh                        # re-warm the -O0 build after pull/branch switch
./dev exec ghc --version             # anything else, with the pinned toolchain
```

Typecheck feedback lands in under a second once the watcher is running. You
don't need to be inside `nix develop`: every verb enters it on demand (pinned
toolchain from any bare shell). Measured baselines: `telemetry/SCHEMA.md`.

## Running Tests

The core library tests are split into domain-specific suites that run in parallel on CI:

```sh
# Run all tests
cabal test all

# Run specific test suites
cabal test nhcore-test-core         # Core primitives (fast)
cabal test nhcore-test-auth         # Auth & JWT tests
cabal test nhcore-test-service      # Service layer (requires PostgreSQL)
cabal test nhcore-test-integration  # Integration tests
```

Note: `nhcore-test-service` requires a PostgreSQL instance. You can start one with:

```sh
docker run -d --name neohaskell-postgres \
  -e POSTGRES_USER=neohaskell \
  -e POSTGRES_PASSWORD=neohaskell \
  -e POSTGRES_DB=neohaskell \
  -p 5432:5432 \
  postgres:16-alpine
```

## Collaborate on Discord

It's always better to hack with people, so why not join the [Discord server](https://discord.gg/invite/wDj3UYzec8)?

## Contributors

Thanks to everyone who has contributed to NeoHaskell!

<a href="https://github.com/neohaskell/NeoHaskell/graphs/contributors">
  <img src="https://contrib.rocks/image?repo=neohaskell/NeoHaskell" alt="Contributors" />
</a>

The image above is generated automatically from the
[GitHub contributors graph](https://github.com/neohaskell/NeoHaskell/graphs/contributors)
by [contrib.rocks](https://contrib.rocks) — no manual upkeep required.
