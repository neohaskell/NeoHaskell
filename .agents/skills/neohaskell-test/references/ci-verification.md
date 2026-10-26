# CI verification

Every pull request runs the generated-project consumer contract against its own
framework revision, including drafts and documentation changes. Its aggregate
gate rejects failure, cancellation and skipped execution. See
[NeoHaskell consumer verification](../../neohaskell-consumer-verification/SKILL.md)
for starter and testbed freshness and same-PR migration requirements.

Other jobs select work by changed paths. Apply the optional `run-full-ci` label
to a draft pull request to request the selected framework build, doctest,
test-suite, native Rust, IDE and Nix package jobs while keeping it in draft.
The label requests execution only and grants no merge, release, cache-write or
publication authority. Each aggregate gate requires its selected jobs to succeed.

Each subscribed pull-request event runs those selected jobs when the pull request
is ready or when a draft has `run-full-ci`. Removing `run-full-ci` stops them on
later draft events. The generated-project contract continues to run.
Push behavior and cache, secret, publication, checkout-credential,
immutable-tool-reference and security gates remain owned by their workflows.

The Linux `test-integrations` job runs the whole `nhintegrations-test` suite with
detailed output. It uploads `.nhintegrations-test.report` even when tests fail.
`ci-gate` requires the job to succeed when framework tests are selected. The
macOS matrix also runs the whole suite with detailed output.

For local full-suite verification, run
`.agents/skills/neohaskell-test/scripts/run-full-suite --strict`.
The runner invokes `cabal test all`, checks every declared component result, and
runs the maintained Hurl testbed. Missing results or prerequisites fail strict
verification. The retired `./dev test-all` and `criteria-runtime` report consumer
are not part of this flow.

Run the offline regression checks with Python. `checks.yml` also discovers these
files in its owned-tooling job and propagates failures.

```sh
python3 -B .agents/skills/neohaskell-test/tests/test_test_runtime.py
python3 -B .agents/skills/neohaskell-test/tests/test_ci_verification.py
```

These checks exercise runner commands and workflow gates with owned fixtures.
They do not execute native Haskell suites or replace full-suite evidence.
