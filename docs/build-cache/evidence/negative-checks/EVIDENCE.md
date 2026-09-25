# PR899 local negative checks

Date: 2026-09-25
Checkout: `d5a39fb588ae5c8639e536bc7e3554b9a59d6829`
Scratch: `/tmp/nh-luna-negative-899`

The existing bundle at `/tmp/nh-component-local-45bb2f6` is producer revision
`45bb2f64cfcbc8d5d80d236705323f9363451685`, so it was copied unchanged into a
scratch directory and exercised only to confirm the revision guard. No
producer metadata was rewritten.

## Observations

* `./dev nix-components run --directory /tmp/nh-luna-negative-899/stale-cli-2 --suite nhcore-test-service`
  exited `1`: `component artifact belongs to a different checkout`. Log:
  `stale-cli-2.log` (the matching `timings.jsonl` is under `stale-cli-2/`).
* The real manifest root
  `/nix/store/86xv6cmccp57g7yhhhlfqx5kmraj2k1j-neohaskell-ci-components` passed
  `environment` and `run_suite` was called directly with
  `POSTGRES_HOST=198.51.100.1` and `PGCONNECT_TIMEOUT=1`. `pg_isready` returned
  `2` (`198.51.100.1:5432 - no response`); no report was created. The wrapper
  exited `0` after asserting this expected preflight failure. Log:
  `missing-postgres.log`.
* The existing local PostgreSQL was not touched: after the check,
  `lsof -nP -iTCP:5432 -sTCP:LISTEN` still showed PID `28506` on
  `127.0.0.1:5432`, and the runtime `pg_isready` probe returned `0`.
* `PATH=/usr/bin:/bin NHTESTBED_BINARY=/nix/store/2xpyhah7qqn82d20m9s26g7h25vky76l-nhtestbed-exe-nhtestbed-0.10.1/bin/nhtestbed bash testbed/scripts/run-tests.sh`
  exited `1` with `Error: hurl is not installed`. The supplied real binary was
  accepted, the script stopped before starting it, no process remained, and
  port `8080` had no listener. Log: `missing-hurl-2.log`.
* The exact missing-cache transfer command used by `fetch` was run against a
  disposable empty cache:
  `nix copy --from file:///tmp/nh-luna-negative-899/cache-empty-1 --no-check-sigs --option max-jobs 0 --option builders '' /nix/store/86xv6cmccp57g7yhhhlfqx5kmraj2k1j-neohaskell-ci-components`
  exited `1` (`no substituter that can build it`). Log:
  `missing-output-copy.log`.
* `nix path-info --recursive /nix/store/11111111111111111111111111111111-missing-ci-components-output`
  exited `1` (`path ... is not valid`). Log: `missing-output-path-info.log`.
  The known real root's recursive path-info exited `0` with 644 paths
  (`known-output-path-info.log`).
* `python3 -m unittest scripts/tests/test_nix_components.py` exited `0`:
  7 tests, `OK` (`unit-suite.log`).

## Disposable hosted failure probe

After review, add this one new example to `core/test-core/Main.hs` inside the
existing `Hspec.hspec do` block, run the hosted `test-core` job and observe the
required `ci-gate` failure, then remove it. It does not modify any existing
expectation:

```haskell
  Hspec.it "[DISPOSABLE_CI_GATE_FAILURE_PROBE]" do
    Hspec.expectationFailure "DISPOSABLE_CI_GATE_FAILURE_PROBE"
```

No tracked file was edited and the worktree was clean at the end. These checks
do not prove hosted Linux/macOS behavior or a current-revision fetched bundle;
the stale artifact was intentionally blocked by provenance validation.
