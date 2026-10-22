---
name: neohaskell-consumer-verification
description: Verify generated projects, starter and testbed freshness, or the embedded IDE bundle. Use when framework exports, Template Haskell markers, generated APIs, starter dependencies, CLI generation or IDE sources change.
---

# NeoHaskell consumer verification

Use this skill when a change can affect a project created by `neo new`, the
embedded starter, the generated build and runtime path, or the bundled IDE
frontend. The full contract tests a consumer-visible result. A script
self-test checks only its orchestration or comparison logic.

## Framework changes include their consumers

For framework export, marker or generated-API changes, inventory affected uses in
`neo/starter/` and `testbed/` before implementation. Migrate those consumers in
the same PR. Update the marker's public documentation when its generated
contract changes. A framework suite passing does not establish starter freshness.

Run the owned freshness check before the generated-project contract:

```sh
python3 .agents/skills/neohaskell-consumer-verification/scripts/check-consumer-freshness.py
```

The check rejects manual defaults already supplied by a type's TH marker.
Keep custom instances, business logic and derivations the marker does not own.
When changing marker output, update the check's ownership model and fixtures
with the implementation. Compilation alone cannot detect redundant defaults
because the markers preserve existing instances.

Verify the generated starter and the affected testbed behavior against the
same PR source. Use [NeoHaskell tests](../neohaskell-test/SKILL.md) for testbed
entrypoints and focused framework suites. Record the tested source revision,
checks run and any unavailable coverage. A skipped required check is incomplete.

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

`neo-consumer-contract` tests the current checkout. PR verification uses that
revision for the CLI, starter and framework. Release preparation freezes the
starter pins to its candidate framework revision before packaging the CLI.
Cache population must use the corresponding tested source and binary.

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

The CI workflow runs both owner self-tests and the applicable full check. Every
PR runs the generated-project contract, including drafts and documentation
changes. The aggregate gate requires its success. Component-specific draft and
path selection does not permit skipping the generated-project contract.
