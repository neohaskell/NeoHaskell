---
name: neohaskell-consumer-verification
description: Verify generated projects, starter and testbed freshness, source-owned agent guidance, or the embedded IDE bundle. Use when framework exports, Template Haskell markers, generated APIs, starter dependencies, CLI generation, guidance accuracy or IDE sources change.
---

# NeoHaskell consumer verification

Use this skill when a change can affect a project created by `neo new`, the
embedded starter, the generated build and runtime path, or the bundled IDE
frontend. The full contract tests a consumer-visible result. A script
self-test checks only its orchestration or comparison logic.

## Framework changes include their consumers

Every developed public NeoHaskell feature needs a working example in the
repository testbed. Exercise the public API in testbed application code, assert
its observable behavior with a maintained automated testbed scenario, and give
concrete commands to run the example and its test in testbed documentation.
Add or update these in the feature PR. Unit tests, compiler fixtures,
generated-project checks, and
documentation snippets are useful additional evidence, but none replaces the
testbed example and assertion. A change with no public feature can state that
the testbed requirement does not apply and identify the internal-only scope.
Do not exempt a public feature because its API is compile-time, because it has
no convenient HTTP endpoint, or because its example is deferred. Adapt the
testbed's application and maintained runner to exercise the actual contract.

During review, trace the testbed example from the public API through its
registration and runtime path to the scenario's independent expected result.
Run the affected scenario through [NeoHaskell tests](../neohaskell-test/SKILL.md)
against the PR revision. Report the example, assertion, commands, revision,
and result. A missing or skipped required run is incomplete.

For framework export, marker or generated-API changes, inventory affected uses in
`neo/starter/` and `testbed/` before implementation. Migrate those consumers in
the same PR. Compare every affected example with the resulting public API and
marker output during review. Remove redundant generated defaults, unused
derivations and stale imports. Retain each capability only when an actual
framework constraint or example operation needs it under the
[application example rules](../neohaskell-code-guidelines/SKILL.md#application-examples).
Trace registration and execution constraints as well as the type's own class.
Preserve custom instances and business logic. Update the marker's public
documentation when its generated contract changes.

Keep the starter, testbed and consumer fixtures in the
[NeoHaskell application dialect](../neohaskell-code-guidelines/SKILL.md#application-examples).
Maintain their freshness through this implementation and review work. Use real
builds and runtime tests to verify them. Do not add source-parsing checks solely
to police example freshness. Compilation alone cannot establish idiomatic
examples because the markers preserve existing instances.

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

## Source-owned agent guidance

Before opening a PR that changes a consumer-facing framework contract, generated
CLI behavior, or `docs/agent/`, compare the changed source with the private
capability registry and published topic pages. Read the maintained implementation behind each
affected claim. Update stale pages, examples, scenarios, and review records.
For each affected capability, report the updated page or explain why its guide
still matches the source. Review explanatory fragments against their cited
source; compilation cannot prove prose claims.

Follow [Agent guide authoring](../agent-guide-authoring/SKILL.md) for the
published pages. Keep implementation paths and required checks in this skill's
private policy at `references/agent-docs-policy.json`. Keep review receipts in
the ignored `.agents-state/neohaskell-consumer-verification/agent-docs-reviews.json`
of the current worktree.

Run the local corpus check from the repository root inside the pinned Nix shell:

```sh
nix develop --command .agents/skills/neohaskell-consumer-verification/scripts/agent-docs-check
nix develop --command .agents/skills/neohaskell-consumer-verification/scripts/agent-docs-check --verify-evidence
```

The gate validates the published `docs/agent/manifest.json` page inventory,
local links, and the private capability policy at
`references/agent-docs-policy.json`. Each mapped capability retains a
source file and maintained Hurl scenario in that policy. Add a capability and
page mapping when a new public feature changes the generated application guide.
The gate preserves its baseline capabilities and accepts new mappings with
valid evidence. It does not infer that every public API has a testbed example
from filenames or a passing guide check. Review the feature's actual public
path and scenario under the requirement above. The gate rejects
framework repository paths in published guidance. It compiles every runnable
Haskell fence with `markdown-unlit`
against the checkout's `nhcore` and `nhintegrations` packages. Use `haskell
ignore` only for fragments that need surrounding application definitions.
Declare each such fragment and its owning source in the policy.

A fresh checkout has no review receipts. The default and `--structure-only`
checks fail until the current worktree has reviewed every ignored fragment.
Run `agent-docs-check --prepare-reviews` to create a local draft with the
current revision, worktree path, fragment hashes, and source hashes. Read each
declared source and fragment, then fill its empty `reason` with the concrete
review finding. Run the default check again. Preparing a draft alone never
passes the gate. A new commit, changed fragment, changed source, or another
worktree invalidates the receipt. Regenerate and review after the final commit.

`--structure-only` provides quick feedback but does not compile examples.
Run the default check for each affected PR. For a corpus-wide change or a
change to the capability policy, run `--verify-evidence`. It also runs the
service and auth specs, the maintained Hurl testbed, and the generated-project
contract. For a narrower runtime change, run the affected maintained scenario
or suite and record the command and result in the PR. Use a fresh PostgreSQL
database for repeated local evidence runs. The testbed retains events across
runs and uses in-memory query projections.

If you change the corpus validator, run its local cases with
`python3 .agents/skills/neohaskell-consumer-verification/tests/agent_docs_check_cases.py`.
