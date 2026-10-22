---
name: neohaskell-consumer-verification
description: Verify generated projects, source-owned agent guidance, or the embedded Neo IDE bundle. Use for CLI consumer contracts, starter compatibility, guidance publication, or IDE bundle freshness.
---

# NeoHaskell consumer verification

Use this skill when a change can affect a project created by `neo new`, the
embedded starter, the generated build and runtime path, or the bundled IDE
frontend. The full contract tests a consumer-visible result. A script
self-test checks only its orchestration or comparison logic.

## Generated project contract

Run from the repository root, or set `NEOHASKELL_ROOT` to the checked-out
repository.

```sh
./.agents/skills/neohaskell-consumer-verification/scripts/neo-consumer-contract --self-test
./.agents/skills/neohaskell-consumer-verification/scripts/neo-consumer-contract
```

The self-test exercises bounded child execution, process cleanup, quoting,
failure propagation and readiness polling. It does not build a Neo binary or
prove a generated project works. It uses only local fixture commands and a
loopback readiness server when the environment permits binding one.

The full contract builds the packaged `neo` binary unless `NEO_BIN` points to an
existing executable. It then generates a project with `neo new` in offline mode
from the embedded starter and checks the expected project files and absence of
internal provenance. It verifies that the starter compatibility record agrees
with the starter lock and that generated Flake and Cabal pins agree.

The build, test and runtime phases must all pass for a full result:

- `neo build` compiles the generated app against an isolated Git snapshot of the
  exact checked-out NeoHaskell revision. The contract sets
  `NEO_NEOHASKELL_SOURCE` only for this exact-checkout proof and verifies the
  regenerated Flake and Cabal inputs point to that revision.
- `neo test --ci` completes the generated project's Haskell unit, property and
  integration tests plus its Hurl scenarios.
- `neo run` stays alive in a dedicated process session, answers HTTP `200` from
  `/health` with the expected body, and passes the starter smoke command.
  Teardown terminates the whole process group.

Each long-running child has a wall-clock bound. On failure, the scratch project
and bounded phase logs remain under
`.agents-state/neohaskell-consumer-verification/` for diagnosis. The full
contract requires Nix, Git, curl, Python and a working Haskell build environment.
Offline `neo new` generation does not make the build, test or runtime phases
offline.

The exact-checkout contract and released closure have different purposes.
`neo-consumer-contract` tests the current checkout. The release-owned
`cache-prime` script realizes the default released-source closure without
`NEO_NEOHASKELL_SOURCE`. Keep that distinction when reviewing or changing
workflow wiring.

## Embedded IDE bundle

Run the fixture self-test or the full lockfile rebuild:

```sh
./.agents/skills/neohaskell-consumer-verification/scripts/neo-dist-check --self-test
./.agents/skills/neohaskell-consumer-verification/scripts/neo-dist-check
```

The self-test checks exact file sets and bytes and rejects missing, extra,
changed or symlink entries. It is offline and does not run the frontend build.
The full check uses the pinned Neo development shell, runs `npm ci`, compiles
TypeScript and builds Vite output into temporary owner-scoped storage. It then
compares every relative file path and byte sequence against
`neo/assets/ide/dist`. Extra files are detected even when Git would not report
them because they are untracked. A full pass proves the committed bundle matches
that rebuild; it does not replace frontend behavior tests.

The CI workflow runs both owner self-tests and the applicable full check. The
generated-project contract is a separate blocking gate for source changes that
can affect the starter or compiled application.

## Source-owned agent guidance

Run the corpus gate from the repository root inside the pinned Nix shell:

```sh
nix develop --command .agents/skills/neohaskell-consumer-verification/scripts/agent-docs-check
nix develop --command .agents/skills/neohaskell-consumer-verification/scripts/agent-docs-check --verify-evidence
```

The gate validates `docs/agent/manifest.json` against the complete page set,
local links, and a source file and maintained Hurl scenario for each required
capability. It compiles every runnable Haskell fence with `markdown-unlit`
against the checkout's `nhcore` and `nhintegrations` packages. Use `haskell
ignore` only for fragments that need surrounding application definitions.
Record each such fragment and its owning source in `docs/agent/evidence.json`.

`--structure-only` provides a quick local check. It does not prove the
examples compile. `--verify-evidence` also runs the service and auth specs,
the maintained Hurl testbed, and the generated-project contract. CI and both
release workflows run that check through `scripts/agent-docs-ci` before
publication, including artifact reuse. A pass requires all evidence commands
to finish successfully. Use a fresh PostgreSQL database for a repeated local
evidence run. The testbed retains events across runs and uses in-memory query
projections.
