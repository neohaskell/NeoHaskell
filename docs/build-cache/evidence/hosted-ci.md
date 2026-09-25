# PR899 hosted CI evidence (UTC, 2026-09-25)

All revision claims below are only for the listed run heads. PR latest clean HEAD `d5a39fb` was not run by these executions.

## Main production runs

### macOS — run 36141142936 @08ecae9 — success
Run: https://github.com/neohaskell/NeoHaskell/actions/runs/36141142936
Created 13:28:11Z; updated 13:52:47Z. Critical production elapsed (first build start to gate finish): 13:28:20–13:52:46 = 24m26s. Production runner sum (build + five consumers + gate): 2303s = 38m23s.

Jobs:
- build-macos 108091011601, 13:28:20–13:47:02, 1122s: https://github.com/neohaskell/NeoHaskell/actions/runs/36141142936/job/108091011601
- nhintegrations consumer 108097671622, 13:47:12–13:50:41, 209s: https://github.com/neohaskell/NeoHaskell/actions/runs/36141142936/job/108097671622
- nhcore-test-service consumer 108097671697, 13:47:16–13:52:40, 324s: https://github.com/neohaskell/NeoHaskell/actions/runs/36141142936/job/108097671697
- nhcore-test-integration consumer 108097671814, 13:47:14–13:51:30, 256s: https://github.com/neohaskell/NeoHaskell/actions/runs/36141142936/job/108097671814
- nhcore-test-auth consumer 108097671898, 13:47:10–13:49:43, 153s: https://github.com/neohaskell/NeoHaskell/actions/runs/36141142936/job/108097671898
- nhcore-test-core consumer 108097671927, 13:47:11–13:51:07, 236s: https://github.com/neohaskell/NeoHaskell/actions/runs/36141142936/job/108097671927
- ci-gate-macos 108099697514, 13:52:43–13:52:46, 3s: https://github.com/neohaskell/NeoHaskell/actions/runs/36141142936/job/108099697514

### Linux — run 36143016033 @c65700c — success
Run: https://github.com/neohaskell/NeoHaskell/actions/runs/36143016033
Created 13:45:38Z; updated 14:06:55Z. Critical production elapsed (changes start to gate finish): 13:45:42–14:06:54 = 21m12s. Production runner sum excluding six optional baseline measurement jobs: 2098s = 34m58s. Optional measurement sum: 4635s, excluded.

Jobs:
- changes 108097176205, 13:45:42–13:45:48, 6s: https://github.com/neohaskell/NeoHaskell/actions/runs/36143016033/job/108097176205
- build 108097227903, 13:46:27–14:04:37, 1090s: https://github.com/neohaskell/NeoHaskell/actions/runs/36143016033/job/108097227903
- codemap-sync 108097227907, 13:45:51–13:49:52, 241s: https://github.com/neohaskell/NeoHaskell/actions/runs/36143016033/job/108097227907
- doctest 108097228234, 13:45:51–13:49:05, 194s: https://github.com/neohaskell/NeoHaskell/actions/runs/36143016033/job/108097228234
- test-integration 108104075430, 14:04:41–14:06:46, 125s: https://github.com/neohaskell/NeoHaskell/actions/runs/36143016033/job/108104075430
- test-hurl 108104075453, 14:04:41–14:06:46, 125s: https://github.com/neohaskell/NeoHaskell/actions/runs/36143016033/job/108104075453
- test-service 108104075464, 14:04:41–14:06:25, 104s: https://github.com/neohaskell/NeoHaskell/actions/runs/36143016033/job/108104075464
- test-integrations 108104075555, 14:04:42–14:06:12, 90s: https://github.com/neohaskell/NeoHaskell/actions/runs/36143016033/job/108104075555
- test-auth 108104075591, 14:04:41–14:05:40, 59s: https://github.com/neohaskell/NeoHaskell/actions/runs/36143016033/job/108104075591
- test-core 108104076246, 14:04:41–14:05:40, 59s: https://github.com/neohaskell/NeoHaskell/actions/runs/36143016033/job/108104076246
- ci-gate 108104865598, 14:06:49–14:06:54, 5s: https://github.com/neohaskell/NeoHaskell/actions/runs/36143016033/job/108104865598

### Pilot — run 36143308159 @987fe3d — success
Run: https://github.com/neohaskell/NeoHaskell/actions/runs/36143308159
Created 13:48:23Z; updated 14:06:24Z. Critical production elapsed (changes start to gate finish): 13:48:27–13:57:03 = 8m36s. Production runner sum excluding three optional pilot measurement jobs: 1311s = 21m51s. Optional measurement sum: 2610s, excluded.

Jobs:
- changes 108098159111, 13:48:27–13:48:35, 8s: https://github.com/neohaskell/NeoHaskell/actions/runs/36143308159/job/108098159111
- codemap-sync 108098221712, 13:49:08–13:53:42, 274s: https://github.com/neohaskell/NeoHaskell/actions/runs/36143308159/job/108098221712
- doctest 108098221714, 13:49:09–13:52:28, 199s: https://github.com/neohaskell/NeoHaskell/actions/runs/36143308159/job/108098221714
- build 108098221951, 13:49:25–13:55:07, 342s: https://github.com/neohaskell/NeoHaskell/actions/runs/36143308159/job/108098221951
- test-auth 108100585178, 13:55:10–13:56:20, 70s: https://github.com/neohaskell/NeoHaskell/actions/runs/36143308159/job/108100585178
- test-service 108100585284, 13:55:10–13:56:55, 105s: https://github.com/neohaskell/NeoHaskell/actions/runs/36143308159/job/108100585284
- test-hurl 108100585328, 13:55:11–13:56:34, 83s: https://github.com/neohaskell/NeoHaskell/actions/runs/36143308159/job/108100585328
- test-core 108100585338, 13:55:10–13:56:13, 63s: https://github.com/neohaskell/NeoHaskell/actions/runs/36143308159/job/108100585338
- test-integration 108100585355, 13:55:11–13:56:56, 105s: https://github.com/neohaskell/NeoHaskell/actions/runs/36143308159/job/108100585355
- test-integrations 108100585411, 13:55:11–13:56:09, 58s: https://github.com/neohaskell/NeoHaskell/actions/runs/36143308159/job/108100585411
- ci-gate 108101247295, 13:56:59–13:57:03, 4s: https://github.com/neohaskell/NeoHaskell/actions/runs/36143308159/job/108101247295

## Exact test counts (each main run)

| suite | examples | pending | failures | executed |
|---|---:|---:|---:|---:|
| nhcore-test-auth | 189 | 0 | 0 | 189 |
| nhcore-test-core | 1095 | 3 | 0 | 1092 |
| nhcore-test-integration | 70 | 4 | 0 | 66 |
| nhcore-test-service | 1128 | 57 | 0 | 1071 |
| nhintegrations-test | 606 | 3 | 0 | 603 |
| **total** | **3088** | **67** | **0** | **3021** |

Linux and pilot Hurl jobs: 17 successful file executions, 14 unique files (cold-start-readiness runs four times: initial, 1k, 10k, 100k), 0 failed files. Cold-start health latencies: Linux 123/121/119ms (spread 4ms); pilot 141/142/138ms (spread 4ms). Hurl evidence is under `evidence/linux-test-hurl-evidence` and `evidence/pilot-test-hurl-evidence`.

## Fresh-store / build-disabled evidence

Each consumer job shows the exact step title `Fetch exact outputs without compilation` (macOS) or `./dev nix-components fetch` (Linux/pilot), followed by `./dev nix-components run --suite ...`; no consumer log has a `nix-components build` invocation. Build logs show one producer `./dev nix-components build` and export/publish before consumers:
- `/tmp/nh-luna-ci-899/mac-build.log` (producer build 13:30:11Z; publish closure 13:42:46Z)
- `/tmp/nh-luna-ci-899/pilot-build.log` (build 13:50:02Z; export 13:54:47Z)
- `/tmp/nh-luna-ci-899/linux-build.log` (build 13:47:10Z; export 13:53:35Z)

Postgres fixture evidence is in the service logs: Docker fixture starts and `POSTGRES_AVAILABLE: true` before the store-run; Hurl logs show Postgres accepting connections before Hurl/cold-start. Main consumer logs: `/tmp/nh-luna-ci-899/mac-*.log`, `/tmp/nh-luna-ci-899/pilot-*.log`, `/tmp/nh-luna-ci-899/linux-*.log`.

Downloaded report/evidence artifacts only (no multi-GB caches) are under `/tmp/nh-luna-ci-899/evidence/`.

## Controlled failure — run 36144876178 @d8dacd2 — failure (expected disposable probe)

Run: https://github.com/neohaskell/NeoHaskell/actions/runs/36144876178
Created 14:02:50Z; updated 14:12:34Z.
- test-core 108106295352, failed 14:10:41–14:11:41: https://github.com/neohaskell/NeoHaskell/actions/runs/36144876178/job/108106295352
- ci-gate 108106965386, failed 14:12:30–14:12:33: https://github.com/neohaskell/NeoHaskell/actions/runs/36144876178/job/108106965386

`/tmp/nh-luna-ci-899/controlled-test-core.log` and `evidence/controlled-test-core/component-report.log` show `[DISPOSABLE_CI_GATE_FAILURE_PROBE]` at `test-core/Main.hs:55:5`, `1096 examples, 1 failure, 3 pending`, exit 1. Gate log `/tmp/nh-luna-ci-899/controlled-ci-gate.log` records `test-core=failure` and `test-core did not pass (result: failure)`; all other build/test jobs succeeded.
