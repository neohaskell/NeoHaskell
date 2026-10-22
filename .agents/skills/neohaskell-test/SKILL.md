---
name: neohaskell-test
description: Run NeoHaskell doctest examples, all local Cabal test targets, or the maintained Hurl testbed when a framework change needs runtime verification.
---

# NeoHaskell test runs

Use the owned scripts for broad runtime checks. They enter the shared project toolchain, keep transcripts under `.agents-state/neohaskell-test/`, and report unavailable checks as `SKIP`. Add `--strict` when every discovered target must run. Each child command has a timeout and an 8 MiB output limit. A host fallback is labelled as unpinned.

- Run source examples with `scripts/run-doctest`. The script discovers local packages through `cabal.project` and their source settings through each Cabal file.
- Run every Cabal test-suite component with `scripts/run-full-suite`. Cabal receives the `all` target, and the script checks results against test-suite components declared by the project. It then runs the maintained Hurl testbed when Hurl and PostgreSQL are ready.
- Run only the maintained Hurl testbed with `scripts/run-testbed`. PostgreSQL must already be available. The owned entrypoint delegates application startup and teardown to `testbed/scripts/run-tests.sh`. It does not install Hurl or start PostgreSQL.
- Run a focused Hspec selection with the [TDD runner](../tdd/scripts/test-match). It rejects empty selections, setup errors, failed assertions, and missing or ambiguous summaries.

These commands exercise the local development build. Use optimized-build, generated-consumer, cross-platform, or CI checks when those contracts matter. The owned scripts run no lint or formatter checks.

Framework export, TH marker and generated-API changes also require
[NeoHaskell consumer verification](../neohaskell-consumer-verification/SKILL.md).
Review its starter and testbed migration requirements and run the generated-project
contract alongside the affected testbed checks. A passing framework suite does
not replace those consumer checks.

For workflow path selection and draft full-CI opt-in, see [CI verification](references/ci-verification.md).
