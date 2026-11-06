---
name: neohaskell-test
description: Run NeoHaskell doctest examples, all local Cabal test targets, or the maintained Hurl testbed when a framework change needs runtime verification.
---

# NeoHaskell test runs

Use the owned scripts for broad runtime checks. Launch them from the repository root through `nix develop --command`, as required by [Nix Haskell tools](../nix-haskell-tools/SKILL.md). They keep transcripts under `.agents-state/neohaskell-test/` and report unavailable checks as `SKIP`. Add `--strict` when every discovered target must run. Each child command has a timeout and an 8 MiB output limit.

- Run source examples with `nix develop --command .agents/skills/neohaskell-test/scripts/run-doctest`. The script discovers local packages through `cabal.project` and their source settings through each Cabal file.
- Before a full service-suite run, complete the [PostgreSQL setup](#prepare-postgresql-for-service-suites). Run every Cabal test-suite component with `nix develop --command .agents/skills/neohaskell-test/scripts/run-full-suite`. Cabal receives the `all` target, and the script checks results against test-suite components declared by the project. It then runs the maintained Hurl testbed when Hurl and PostgreSQL are ready.
- Run only the maintained Hurl testbed with `nix develop --command .agents/skills/neohaskell-test/scripts/run-testbed --strict`. PostgreSQL must already be available at the runner's default local port 5432. A host PostgreSQL service or an isolated Docker PostgreSQL container can supply it. Check that the port is free before starting a container, record its ID, and stop only that container afterward. The owned entrypoint delegates application startup and teardown to `testbed/scripts/run-tests.sh`. It does not install Hurl or start PostgreSQL.
- Run a focused Hspec selection with the [TDD runner](../tdd/SKILL.md) through `nix develop --command .agents/skills/tdd/scripts/test-match <pattern> [suite]`. It rejects empty selections, setup errors, failed assertions, and missing or ambiguous summaries.

## Prepare PostgreSQL for service suites

Use an owned disposable PostgreSQL instance at `localhost:5432` for the full
suite and full guide evidence runs. Create the `neohaskell` database and a
`neohaskell` role with password `neohaskell` and permission to create databases
when you provision that instance. Other service specs use that database directly.
`FILE_UPLOAD_TEST_DB` isolates only the file-state fixture.

Before each run, create a separate disposable file-state database and export its
name for the whole command. The fixture resets its tables and rejects an unset
name or `neohaskell`. Run from the repository root in the same shell:

```sh
unset PGOPTIONS
export FILE_UPLOAD_TEST_DB="nh_file_upload_$(python3 -c 'import uuid; print(uuid.uuid4().hex[:12])')"
PGPASSWORD=neohaskell nix develop --command createdb -h localhost -p 5432 -U neohaskell "$FILE_UPLOAD_TEST_DB"
nix develop --command .agents/skills/neohaskell-test/scripts/run-full-suite --strict
```

Keep `PGOPTIONS` unset throughout the run. A global fixture schema override can
break the fresh databases that the testbed creates. For full guide evidence,
keep `FILE_UPLOAD_TEST_DB` exported and run the consumer skill's
`agent-docs-check --verify-evidence` command after this database setup.

For only the focused file-state fixture, a dedicated disposable database on an
existing server is sufficient. Use the same creation command and connection
settings above. Run the focused command in the
[testbed instructions](../../../testbed/README.md#generate-and-download-an-owned-pdf).

These commands exercise the local development build. Use optimized-build, generated-consumer, cross-platform, or CI checks when those contracts matter. The owned scripts run no lint or formatter checks.
The non-strict testbed `SKIP` result is incomplete coverage. Keep a required PostgreSQL check pending until it passes on the host or Linux Actions at the PR revision. Follow the [verification environment decision](../nix-haskell-tools/SKILL.md#choose-the-verification-environment). Docker supplies only PostgreSQL here, not the framework build.

Framework export, TH marker and generated-API changes also require
[NeoHaskell consumer verification](../neohaskell-consumer-verification/SKILL.md).
Review its starter and testbed migration requirements and run the generated-project
contract alongside the affected testbed checks. A passing framework suite does
not replace those consumer checks.

For workflow path selection and draft full-CI opt-in, see [CI verification](references/ci-verification.md).
