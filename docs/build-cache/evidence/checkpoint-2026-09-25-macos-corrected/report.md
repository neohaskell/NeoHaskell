# macOS Cabal baseline evidence — run 36160842540

Run URL: https://github.com/neohaskell/NeoHaskell/actions/runs/36160842540  
Workflow: `Test macOS`, `workflow_dispatch`  
Revision: `65b227dde4d81a9c539364cebee3032e552b1de5`  
Run conclusion: **success**

This is the corrected baseline workflow run after the PostgreSQL fixture update. The five matrix jobs all completed successfully and the suite census matches the candidate baseline shape: **3088 examples, 0 failures, 67 pending**.

## Suite census

| suite | examples | failures | pending | job |
| --- | ---: | ---: | ---: | --- |
| `nhcore-test-auth` | 189 | 0 | 0 | 108156722685 |
| `nhcore-test-core` | 1095 | 0 | 3 | 108156722543 |
| `nhcore-test-integration` | 70 | 0 | 4 | 108156722782 |
| `nhcore-test-service` | 1128 | 0 | 57 | 108156722712 |
| `nhintegrations-test` | 606 | 0 | 3 | 108156722771 |
| **total** | **3088** | **0** | **67** | |

The exact parsed values and source log names are in `runs/counts.json`. Each raw job log is under `logs/`.

## PostgreSQL fixture and bad-credentials assertion

The service job ran with `POSTGRES_AVAILABLE: true` (raw service log line 2200). The fixture step's executed command block shows:

- `initdb -D "$RUNNER_TEMP/nh-postgres" -U neohaskell`
- `--auth-local=trust --auth-host=scram-sha-256`
- `--pwfile=<(printf '%s\\n' neohaskell)`
- `pg_ctl ... -o "-h 127.0.0.1 -p 5432 -k $RUNNER_TEMP -c max_connections=200" -w start`
- `createdb -h 127.0.0.1 -U neohaskell neohaskell`

The unchanged test source sets `testConfig.host = "localhost"`, port `5432`, user/password `neohaskell`, then changes only the invalid case password to `wrong_password_xyz` and requires `Err (ConnectionFailed _)`; see `runs/source-proof.txt`. The actual run reports:

```
... fails with ConnectionFailed if credentials are invalid [✔]
1128 examples, 0 failures, 57 pending
Test suite nhcore-test-service: PASS
```

Selected proof with raw line numbers is in `runs/service-proof.txt`; the complete fixture/test output is `logs/108156722712.log`.

Homebrew's install output also mentions its automatically created default cluster and trust auth. The measured test uses the explicit disposable `$RUNNER_TEMP/nh-postgres` cluster and explicit `pg_ctl`/`createdb` commands above; the default-cluster notice is not the test server.

## Timing

Using the GitHub job API `started_at` → `completed_at` values:

- critical workflow span: `2026-09-25T16:27:18Z` → `2026-09-25T16:38:36Z` = **678 s**
- runner aggregate (sum of the five job wall durations) = **2828 s**
- per-job wall durations: auth 518 s, core 603 s, integration 399 s, service 677 s, integrations 631 s

The exact step and job values are in `runs/timings.json`. These are whole-job CI wall timings and include setup, cache actions, build, test, and post steps.

## Cache conditions observed

Workflow source was fetched at the measured revision; SHA-256 is recorded in `runs/test-macos.yml.sha256` (`d93af2a77ea15c4c9942daa4c6df66f55c0fd4b6921e90256e5d78260a36e6f0`). It configures `https://cache.iog.io`, `https://neohaskell.cachix.org`, the Nix default `https://cache.nixos.org`, and `actions/cache` over `dist-newstyle` with key:

```
macOS-cabal-f8d4e222566feb5efe3a93dcdefb1ee6012b76dd73e787dd70f49b6f980d0db3
```

All five jobs reported a hit for each of the Determinate installer cache, Magic Nix Cache runner cache, and this Cabal `actions/cache` key; the Cabal key was restored in all five jobs. Build logs record 3,852 copies from `cache.iog.io`, 3,238 from `cache.nixos.org`, 836 from `neohaskell.cachix.org`, and 509 attempted local Magic Nix Cache proxy copies. They also contain 307 HTTP 418/throttle records from the local `127.0.0.1:37515` proxy (including “GitHub Actions Cache throttled Magic Nix Cache”), with fallback copies from the configured upstream substituters, and five FlakeHub unauthenticated login warnings. The run still built and passed; these contention/auth caveats make this run unsuitable for a claim that every cache layer was unconstrained.

The raw counts and interpretation are in `runs/cache-conditions.json`; a short source table is `runs/cache-conditions.tsv`. These are Cabal baseline cache observations, not evidence that any Nix component output was or was not present.

## Archive contents

- `runs/run.json`, `runs/artifacts.json`, `runs/test-macos.yml`, source/cache/count/timing JSON and proof text (including `runs/branch-proof.txt`)
- `jobs/*.json` (GitHub job metadata)
- `logs/*.log` (complete raw logs for all five jobs)

Collection wrote only this `/tmp` evidence directory. No repository files, shared Nix store, cache, or expectations were modified; no dispatch or push was performed during collection.
