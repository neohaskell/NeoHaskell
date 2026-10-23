---
name: neohaskell-consumer-verification
description: Verify generated projects, review source-owned agent guidance for consumer-facing PRs, or check the embedded Neo IDE bundle. Use for CLI consumer contracts, starter compatibility, guidance accuracy, or IDE bundle freshness.
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

Before opening a PR that changes a consumer-facing framework contract, generated
CLI behavior, or `docs/agent/`, compare the changed source with the private
capability registry and published topic pages. Read the maintained implementation behind each
affected claim. Update stale pages, examples, scenarios, and evidence records.
For each affected capability, report the updated page or explain why its guide
still matches the source. Review explanatory fragments against their cited
source; compilation cannot prove prose claims.

Follow [Agent guide authoring](../agent-guide-authoring/SKILL.md) for the
published pages. Keep implementation paths and evidence in this skill's private
registry so generated applications receive only consumer guidance.

Run the local corpus check from the repository root inside the pinned Nix shell:

```sh
nix develop --command .agents/skills/neohaskell-consumer-verification/scripts/agent-docs-check
nix develop --command .agents/skills/neohaskell-consumer-verification/scripts/agent-docs-check --verify-evidence
```

The gate validates the published `docs/agent/manifest.json` page inventory,
local links, and the private capability registry at
`references/agent-docs-evidence.json`. Each required capability retains a
source file and maintained Hurl scenario in that registry. The gate rejects
framework repository paths in published guidance. It compiles every runnable
Haskell fence with `markdown-unlit`
against the checkout's `nhcore` and `nhintegrations` packages. Use `haskell
ignore` only for fragments that need surrounding application definitions.
Record each such fragment and its owning source in `references/agent-docs-evidence.json`.

`--structure-only` provides quick feedback but does not compile examples.
Run the default check for each affected PR. For a corpus-wide change or a
change to the capability evidence, run `--verify-evidence`. It also runs the
service and auth specs, the maintained Hurl testbed, and the generated-project
contract. For a narrower runtime change, run the affected maintained scenario
or suite and record the command and result in the PR. Use a fresh PostgreSQL
database for repeated local evidence runs. The testbed retains events across
runs and uses in-memory query projections.

If you change the corpus validator, run its local cases with
`python3 .agents/skills/neohaskell-consumer-verification/tests/agent_docs_check_cases.py`.
