<div align="center">
  <img src=".assets/img/logo.png" height="256px"/>
  <h1>NeoHaskell</h1>
  <b>
    NeoHaskell is a dialect of Haskell that is focused on newcomer-friendliness and productivity.
  </b>
  <p>
    It is designed to be easy to learn and use, while also being powerful enough to release your app with minimum effort and maximum confidence.
  </p>
  <a href="#contributors">
    <img src="https://img.shields.io/github/all-contributors/neohaskell/neohaskell?color=ee8449&style=flat-square" alt="All Contributors" />
  </a>
  <a href="https://vscode.dev/redirect?url=vscode://ms-vscode-remote.remote-containers/cloneInVolume?url=https://github.com/neohaskell/neohaskell">
    <img src="https://img.shields.io/static/v1?label=Dev%20Containers&message=Open&color=blue&style=flat-square" alt="Open in Dev Containers" />
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

- Install [Nix](https://nixos.org/download/)
- Run `nix-shell`
- Run `cabal update && cabal build all`

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
To run manually:

```sh
hlint .
```

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

<!-- ALL-CONTRIBUTORS-LIST:START - Do not remove or modify this section -->
<!-- prettier-ignore-start -->
<!-- markdownlint-disable -->
<table>
  <tbody>
    <tr>
      <td align="center" valign="top" width="14.28%"><a href="https://github.com/NickSeagull"><img src="https://avatars.githubusercontent.com/u/7448243?v=4?s=100" width="100px;" alt="Nick Seagull"/><br /><sub><b>Nick Seagull</b></sub></a><br /><a href="#code-NickSeagull" title="Code">💻</a></td>
      <td align="center" valign="top" width="14.28%"><a href="https://github.com/DavideWalder"><img src="https://avatars.githubusercontent.com/u/58290976?v=4?s=100" width="100px;" alt="Davide Walder"/><br /><sub><b>Davide Walder</b></sub></a><br /><a href="#infra-DavideWalder" title="Infrastructure (Hosting, Build-Tools, etc)">🚇</a></td>
      <td align="center" valign="top" width="14.28%"><a href="https://github.com/SiriusStarr"><img src="https://avatars.githubusercontent.com/u/2049163?v=4?s=100" width="100px;" alt="ScribblyBirb"/><br /><sub><b>ScribblyBirb</b></sub></a><br /><a href="#ideas-siriusstarr" title="Ideas, Planning, & Feedback">🤔</a></td>
      <td align="center" valign="top" width="14.28%"><a href="https://github.com/JYCabello"><img src="https://avatars.githubusercontent.com/u/11141026?v=4?s=100" width="100px;" alt="Yeray Cabello"/><br /><sub><b>Yeray Cabello</b></sub></a><br /><a href="#test-JYCabello" title="Tests">⚠️</a> <a href="#ideas-JYCabello" title="Ideas, Planning, & Feedback">🤔</a></td>
      <td align="center" valign="top" width="14.28%"><a href="https://github.com/ussgarci"><img src="https://avatars.githubusercontent.com/u/91670077?v=4?s=100" width="100px;" alt="Steven Garcia"/><br /><sub><b>Steven Garcia</b></sub></a><br /><a href="#code-ussgarci" title="Code">💻</a></td>
      <td align="center" valign="top" width="14.28%"><a href="https://github.com/DeviousStoat"><img src="https://avatars.githubusercontent.com/u/74479846?v=4?s=100" width="100px;" alt="Thomas"/><br /><sub><b>Thomas</b></sub></a><br /><a href="#code-DeviousStoat" title="Code">💻</a></td>
      <td align="center" valign="top" width="14.28%"><a href="https://github.com/verogarp"><img src="https://avatars.githubusercontent.com/u/9210219?v=4?s=100" width="100px;" alt="Verónica García Pulido"/><br /><sub><b>Verónica García Pulido</b></sub></a><br /><a href="#doc-verogarp" title="Documentation">📖</a></td>
    </tr>
  </tbody>
</table>

<!-- markdownlint-restore -->
<!-- prettier-ignore-end -->

<!-- ALL-CONTRIBUTORS-LIST:END -->
