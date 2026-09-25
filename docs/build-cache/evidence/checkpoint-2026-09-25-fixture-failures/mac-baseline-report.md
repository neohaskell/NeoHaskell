# Corrected Cabal macOS baseline collection

- Run: `36158410770` (`Test macOS`, `workflow_dispatch`)
- Run URL: https://github.com/neohaskell/NeoHaskell/actions/runs/36158410770
- Measured revision: `5604e6c0642d232f3e0651206e538b4219f29aea`
- Workflow source captured from that revision: `runs/test-macos.yml` (sha256 in `runs/test-macos.yml.sha256`)
- Final result: **FAILURE** (four matrix jobs passed; `nhcore-test-service` failed)
- Valid matched baseline n=1: **NO**. The five-suite total is `3088 examples, 1 failure, 67 pending`, versus the candidate reference `3088 examples, 0 failures, 67 pending`.

## Suite counts

| suite | examples | failures | pending | result |
| --- | ---: | ---: | ---: | --- |
| `nhcore-test-auth` | 189 | 0 | 0 | pass |
| `nhcore-test-core` | 1095 | 0 | 3 | pass |
| `nhcore-test-integration` | 70 | 0 | 4 | pass |
| `nhcore-test-service` | 1128 | 1 | 57 | **fail** |
| `nhintegrations-test` | 606 | 0 | 3 | pass |
| **total** | **3088** | **1** | **67** | **unmatched** |

The exact failing assertion is in raw service log lines 4546-4565:

```text
Service.QueryObjectStore.Postgres.createQueryObjectStore fails with ConnectionFailed if credentials are invalid
Expected failure but got success
```

The immutable source at this revision sets `badConfig = testConfig { password = "wrong_password_xyz" }` in `core/test-service/Service/QueryObjectStore/PostgresSpec.hs:129-137`. The service job did initialize PostgreSQL (`CREATE ROLE`, `CREATE DATABASE`), exported `POSTGRES_AVAILABLE=true`, and reached the test assertion. Its setup log also reports `initdb: warning: enabling "trust" authentication for local connections`. The invalid-password case therefore has a fixture/authentication mismatch: the test expects a rejected password, while the runner accepted the connection. The unreachable-host and nonexistent-database cases did produce the expected `ConnectionFailed` results. This is a fixture/configuration issue, not a build/runner failure and not an expectation change. The actionable matching fix is to provision the service fixture with password-enforcing host authentication (while retaining the existing assertion), then rerun; do not weaken the test.

## Timings

Production matrix job wall intervals, including each job's setup/cache/build/test/post-cleanup:

- Critical span: **719 s**, earliest job start `16:04:43Z`, latest job completion `16:16:42Z`.
- Runner aggregate: **3167 s** (sum of five job intervals).

Step timings are in `timing.json`. Build/test seconds were:

| job | PostgreSQL setup | Cabal build | suite test | job wall |
| --- | ---: | ---: | ---: | ---: |
| auth | 0 | 371 | 52 | 564 |
| core | 0 | 329 | 144 | 619 |
| integration | 0 | 406 | 78 | 636 |
| service | 11 | 366 | 180 | 715 |
| nhintegrations | 0 | 383 | 103 | 633 |

## Cache conditions

The baseline workflow at the measured revision used the original topology:

- Determinate Nix with extra substituters `https://cache.iog.io` and `https://neohaskell.cachix.org` (default `https://cache.nixos.org` remains the Magic Nix Cache upstream).
- `DeterminateSystems/magic-nix-cache-action@v14`, local daemon `127.0.0.1:37515`, and `actions/cache@v6.1.0` over `dist-newstyle` with key `macOS-cabal-f8d4e222566feb5efe3a93dcdefb1ee6012b76dd73e787dd70f49b6f980d0db3`.
- All five jobs reported a hit and restored that Cabal cache key. Determinate installer and Magic Nix Cache bootstrap caches also hit.
- FlakeHub login failed with unauthenticated/netrc errors in each job; Magic Nix Cache continued with native GitHub Actions cache. The service log has 48 HTTP 418 responses from the local Magic Nix Cache endpoint while retrying/throttling; its Nix build still completed successfully. Per-log copy-source counts are in `cache-summary.tsv`.

These are cache observations for the baseline; they do not explain the test assertion failure, which occurred after build completion.

## Criterion-runtime local check

The existing checker can be run directly without mutation:

```text
./dev spec-check --criteria-runtime origin/main --report /tmp/nh-full-ci-evidence-899-36155662081-c0d34a8/artifacts/36155654547-test-service-evidence/files/.nhcore-test-service.report
```

At checker repository SHA `8b586f126f85f617f66a346ce374c06c27e0a190` (`scripts/spec-check` blob `5a6d45ae7fe1acd61541f3d8640e9e5dbfa3474c`), it exited `0` with:

```text
spec-check: OK — 0 integration locator(s) selected exact runtime fixtures
```

The supplied report came from Linux run `36155654547`, revision `c0d34a80f1ae99474231715232f28119a6a297fe`, sha256 `3ceeba2a7e2751d135ff7b92f0a0d39960f1022b14137a837ace25caaac41483`. Relevant checker/spec files are unchanged since `c0d34a8`. This is a local parser result with zero selected integration locators: `docs/changes/016-cached-nix-components.md` declares only the C1 unit locator. It does not verify current hosted registered integration selectors, and should not be reported as hosted criterion-runtime evidence. Full command/output metadata is in `criterion-runtime-metadata.txt` and `criterion-runtime.out`.

## Evidence paths

- `runs/run.json`, `runs/artifacts.json`, `runs/test-macos.yml`, `runs/test-macos.yml.sha256`
- `jobs/*.json` and `logs/*.log` (all five jobs; service failure is `logs/108148590449-nhcore-test-service.log`)
- `failure-summary.txt`
- `suite-counts.tsv`, `timing.json`, `cache-summary.tsv`
- `criterion-runtime.out`, `criterion-runtime-metadata.txt`

All collection was read-only against GitHub and local source/log files; no workflow was dispatched or repository file edited by this collection.
