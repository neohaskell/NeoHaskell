---
name: neohaskell-test
description: Run NeoHaskell doctest examples, all local Cabal test targets, or the maintained Hurl testbed when a framework change needs runtime verification.
---

# NeoHaskell test runs

Use the owned scripts for broad runtime checks. Launch them from the repository root through `nix develop --command`, as required by [Nix Haskell tools](../nix-haskell-tools/SKILL.md). They keep transcripts under `.agents-state/neohaskell-test/` and report unavailable checks as `SKIP`. Add `--strict` when every discovered target must run. Each child command has a timeout and an 8 MiB output limit.

- Run source examples with `nix develop --command .agents/skills/neohaskell-test/scripts/run-doctest`. The script discovers local packages through `cabal.project` and their source settings through each Cabal file.
- Run every Cabal test-suite component with `nix develop --command .agents/skills/neohaskell-test/scripts/run-full-suite`. Cabal receives the `all` target, and the script checks results against test-suite components declared by the project. It then runs the maintained Hurl testbed when Hurl and PostgreSQL are ready.
- Run only the maintained Hurl testbed with `nix develop --command .agents/skills/neohaskell-test/scripts/run-testbed --strict`. PostgreSQL must already be available at the runner's default local port 5432. A host PostgreSQL service or an isolated Docker PostgreSQL container can supply it. Check that the port is free before starting a container, record its ID, and stop only that container afterward. The owned entrypoint delegates application startup and teardown to `testbed/scripts/run-tests.sh`. It does not install Hurl or start PostgreSQL.
- Run a focused Hspec selection with the [TDD runner](../tdd/SKILL.md) through `nix develop --command .agents/skills/tdd/scripts/test-match <pattern> [suite]`. It rejects empty selections, setup errors, failed assertions, and missing or ambiguous summaries.

These commands exercise the local development build. Use optimized-build, generated-consumer, cross-platform, or CI checks when those contracts matter. The owned scripts run no lint or formatter checks.
The non-strict testbed `SKIP` result is incomplete coverage. Keep a required PostgreSQL check pending until it passes on the host or Linux Actions at the PR revision. Follow the [verification environment decision](../nix-haskell-tools/SKILL.md#choose-the-verification-environment). Docker supplies only PostgreSQL here, not the framework build.

Framework export, TH marker and generated-API changes also require
[NeoHaskell consumer verification](../neohaskell-consumer-verification/SKILL.md).
Review its starter and testbed migration requirements and run the generated-project
contract alongside the affected testbed checks. A passing framework suite does
not replace those consumer checks.

For workflow path selection and draft full-CI opt-in, see [CI verification](references/ci-verification.md).
