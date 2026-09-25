# Full CI production evidence — PR 899

Collected read-only from GitHub Actions. Evidence root: `/tmp/nh-full-ci-evidence-899-36155662081-c0d34a8`. No repository, shared Nix store, cache, or workflow state was changed.

## Revision and run status

All three production runs checked out the same immutable revision `c0d34a80f1ae99474231715232f28119a6a297fe` (checkout fetch/rev-parse proof is in `revision.txt`):

| platform/run | conclusion | production jobs | production critical span* | production runner aggregate* |
|---|---:|---:|---:|---:|
| macOS `36155662081` | success | 7/7 | 861 s (15:39:53–15:54:14Z) | 1,817 s |
| Linux `36155654547` candidate/benchmark | success | 11/11 | 583 s (15:39:38–15:49:21Z) | 1,446 s |
| Linux `36155658355` pilot | success | 11/11 | 562 s (15:39:41–15:49:03Z) | 1,352 s |

`*` Critical span is earliest completed production job start through production `ci-gate` completion; runner aggregate is the sum of completed production job wall intervals. Optional `baseline-measurement` jobs are excluded from both. The complete run tails were optional-load scheduling: Linux benchmark gate ended 15:49:21Z but the last optional job ended 15:58:53Z; Linux pilot gate ended 15:49:03Z but the last optional job ended 15:59:14Z. These are contention/scheduling observations, not performance wins.

The timing details and every production/optional job interval are in `production-timing.json`.

## Five suite counts

Each production run has exactly the same five suite results: `auth 189/0/0`, `core 1095/0/3`, `integration 70/0/4`, `service 1128/0/57`, and `nhintegrations 606/0/3` (examples/failures/pending), totaling 3,088 examples, 0 failures, and 67 pending. `suite-counts.tsv` records the exact log and line for every result. All failures are zero; the pending counts are the existing suite pending cases.

The macOS service ran its real PostgreSQL fixture; its other four matrix entries intentionally skip the fixture. Linux service and Hurl both passed PostgreSQL readiness. Small downloaded test evidence artifacts contain the reports, `timings.jsonl`, and Hurl cold-start logs; artifact IDs and names are in `artifacts/downloaded.tsv`.

## Other production gates

- Linux Hurl passed all groups (`Succeeded files` 100%, `Failed files` 0) and printed `All tests completed!`; the cold-start route ran 1k, 10k, and 100k cases with zero failed files and readiness latencies `142 139 138ms; spread 4ms` in run `36155654547`, and `138 140 138ms; spread 2ms` in run `36155658355`. See `production-checks.txt` and the two `test-hurl` logs.
- Linux codemap sync regenerated `300 modules / 3353 exports`, produced both Hoogle databases with `smoke-checked`, and passed `git diff --exit-code codemap/` in both runs. Doctest jobs ran `./dev doctest` and finished successfully. Raw logs are under `logs/linux-*/`.
- The integration criterion step is **not evidence in these workflow-dispatch runs**: `Prove integration criterion selectors and real fixtures` is `completed/skipped` in both Linux service job JSONs (`108142220239` and `108142136838`). The workflow source gates it on `github.event_name == 'pull_request'`; this must remain a conspicuous gap.
- macOS has no Linux Hurl/cold-start/codemap/doctest jobs by workflow design; all five macOS suites and `ci-gate-macos` succeeded.

## Exact fetched paths and no builders

The consuming test jobs run `./dev nix-components fetch`; raw logs show `copying 659 paths...` on Linux and `copying 646 paths...` on macOS from the run-local `file:///.../component-artifact/cache` path. `fetched-paths.txt` records the exact nine component output paths, bundle path, and the first/final copied paths for one production test job per platform. The Linux bundle was `/nix/store/k9haj4aw65cv5h3r9phrh9dn8l30ymn4-neohaskell-ci-components`; macOS was `/nix/store/866avk4nzlyb99vzzavbh36lmgqx1x0z-neohaskell-ci-components`.

The fetch command's source is captured in `cache-route-proof.txt`: it uses `nix copy --from ... --no-check-sigs --option max-jobs 0 --option builders ''`, then `nix path-info --recursive`, so this consuming route disables local and remote builders and performs no flake evaluation/IFD. Production build logs also show the configured extra substituters `https://cache.iog.io https://neohaskell.cachix.org`.

## Queue/scheduling signal

Using `build` completion as the dependency-ready point, Linux benchmark tests started 3–4s later except `test-auth` at 50s and `test-integrations` at 52s while all six optional measurement jobs were active. Linux pilot test fanout started 3–4s after build despite three optional pilot jobs. macOS test fanout started 6–10s after build. These are GitHub runner scheduling gaps observed from job intervals, and should be labeled contended timing rather than interpreted as a cache/build speedup.

## Raw collection commands and limits

`commands.txt` records the exact read-only command shapes (`gh run view`, `gh api` jobs/artifacts, `gh run view --job --log`, `unzip`, and local `jq`/`python3`/`rg` parsing). `logs/mac/` contains all seven macOS production job logs; `logs/linux-36155654547/` contains production logs plus the already-completed optional logs; `logs/linux-36155658355/` contains the completed production logs. The large producer `haskell-components*` archives were intentionally not downloaded; no NAR payloads or caches were mutated.

At collection time the separate corrected macOS Cabal run `36158410770` at `5604e6c` was still in progress, so it is not included here. Optional measurement conclusions are included only as run metadata; another worker owns their detailed measurement interpretation.
