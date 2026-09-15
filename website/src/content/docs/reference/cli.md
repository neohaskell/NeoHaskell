---
title: Neo CLI reference
description: Consult the implemented commands, flags, side effects, and validation exit codes.
sidebar:
  order: 1
---

The Neo CLI helps you create, build, test, and inspect a NeoHaskell project. Use the command that matches your immediate goal; the visual IDE offers another way to understand the same project.

Run project commands from the directory containing `neo.json`. Run `neo --help` or a command's `--help` for the installed binary's own help. This page describes the command surface in this repository; an older installed release may differ.

## Shared flags

| Flag | Effect |
| --- | --- |
| `-v`, `--verbose` | Enable debug-level CLI output |
| `--ci` | Disable interactive prompts, animations, and colours |
| `--help` | Show help |
| `--version` | Show the version |

`--ci` is useful in repeatable scripts. It does not turn a local test into a production deployment.

## Create, build, run, and test

```sh
neo --ci new mug-shop
cd mug-shop
neo build
neo run
neo test
```

| Command | Options | Behaviour |
| --- | --- | --- |
| `neo new [project_name]` | `--library` | Scaffold from the embedded starter; a name is required in CI mode. A library omits the launcher and executable stanza. |
| `neo build` | `--watch`, `--skip-lock-check` | Reconcile configuration and build; watch uses GHCi feedback; skip only bypasses the build's lock check. |
| `neo run` | `--watch` | Reconcile, build, and run; watch rebuilds/restarts on changes. |
| `neo test` | `--watch` | Run Cabal tests, then discovered Hurl integration tests. |

Build, run, and test regenerate managed Nix/Cabal artifacts from `neo.json`. Make dependency/configuration changes at the intended source rather than relying on edits to regenerated output.

When Hurl tests exist, the test command starts the application and waits for an HTTP response on `127.0.0.1:8080`. That wait accepts any HTTP response; it does not wait for the `/ready` projection contract. Keep the starter's test port available and add readiness-aware checks to scenarios that depend on rebuilt queries. A custom application port requires attention to both test targets and the current fixed startup probe.

## Explore the application

```sh
neo ide
neo ide --port 2324
neo inspect
neo inspect commands
neo inspect wiring
```

`neo ide` defaults to `127.0.0.1:2323`. `--host` accepts an IP address literal, not a hostname. For example, `--host 0.0.0.0` exposes the IDE on other IPv4 interfaces; choose that intentionally. Stop the server with Ctrl-C.

`neo inspect` prints JSON. Its views are `domains`, `commands`, `events`, `queries`, `integrations`, and `wiring`. With no view it prints the whole inspected project.

`neo inspect sync` is a **mutation**: it refreshes `event-model.json` from source. Existing field edits can preserve layout; new nodes trigger layout work. Use it when you intend to update the model, not as a read-only report.

See [the visual IDE journey](/getting-started/visual-ide/) for how to read that model.

## Validate a saved model

```sh
neo validate
neo validate ./event-model.json --json
```

Validation is read-only. The optional path defaults to `event-model.json` in the current directory. It checks schema and reference integrity, not whether your business rules are correct.

| Exit code | Meaning |
| --- | --- |
| `0` | Valid model |
| `1` | I/O or tool failure |
| `2` | Model fails validation |
| `3` | Malformed JSON |
| `4` | File absent |

`--json` emits the structured validation outcome without human log prefixes; exit codes retain the same meaning.

## Protect domain files

```sh
neo lock --all
neo lock Cart
neo lock install
neo lock check
```

`neo lock [search]` uses a fuzzy domain-file search. `--all`, or no search, selects all discovered domain files. The manifest lives in `.locked-files`. `install` writes the Git pre-commit hook, overwriting an existing hook at that path; `check` detects modified locked files, including working-tree changes. See [evolution](/operate/evolution/) for the historical compatibility problem behind the feature.

Locking stages the chosen paths and `.locked-files` and creates a Git commit. Check `git status` first: unrelated content already staged can be included in that commit. Preserve any existing pre-commit hook before installing the lock hook.

## Install the separate agent skills

```sh
neo skills setup --tool codex --dry-run
neo skills setup --tool codex
```

This command fetches the shared skill library and installs it for selected tools. Human documentation remains separate from those instructions.

Setup options: repeatable `--tool` (`claude`, `codex`, `kiro`, `cursor`), `--all-tools`, repeatable `--skill`, `--force`, `--dry-run`, `--refresh`, and `--no-primer`. `--dry-run` prints the plan without installing into the project, but fetching the library may populate the local cache; `--force` permits overwriting destinations; `--refresh` re-clones the library; `--no-primer` omits the always-on primer and its instructions-file wiring. Bare `neo skills` runs setup.

The source of truth is [the CLI definition](https://github.com/neohaskell/NeoHaskell/blob/main/neo/src/cli.rs). For a failure, use [troubleshooting](/reference/troubleshooting/).
