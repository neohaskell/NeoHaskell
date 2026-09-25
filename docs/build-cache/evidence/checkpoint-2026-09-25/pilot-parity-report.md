# PR899 foundation pilot verification

- Source commit: `1c9741f0e13431ea0b658d87d8d16a2772f2680a`
- Detached pilot checkout: `/private/tmp/nh-luna-pilot-899-v2`
- Recorded foundation-pilot commit: `465d4983b61083b229086e46bcff415cfc75e71d`
- Evidence root: `/tmp/nh-luna-pilot-evidence-899-465d498/results`
- Host/system: `aarch64-darwin`
- Command: `python3 docs/build-cache/pilot-mutations.py "$PWD" /tmp/nh-luna-pilot-evidence-899-465d498/results`
- PostgreSQL: own PostgreSQL 17.7 instance, data directory `/tmp/nh-luna-pilot-pg-899-465d498`, port 5432; started because the port was free and stopped by its own `pg_ctl` after the run. No user-owned server was modified or stopped.

## Case results

| Case | Revision | Changed outputs | Build exit | Captured build time (s) |
|---|---|---|---:|---:|
| no-change | `465d4983b61083b229086e46bcff415cfc75e71d` | none | 0 | 2.603 |
| markdown | `cb2e799afb1deca67bc68cace8c0c64da0bcfc1d` | none | 0 | 2.342 |
| test-only | `a229e02833c11a9b894f561d3db2eeeb03c07994` | `foundation-test` | 0 | 10.186 |
| implementation | `47ddbe8b295381c705b73dde537d45eec7a7e801` | all 11 pilot outputs | 0 | 172.519 |
| sibling | `706e2e0e9d73890e955859b3fb2d909ee94c8572` | all expected non-foundation outputs | 0 | 163.756 |
| flag | `49af7298d0a0039eedea121d9a1ca3f020172a06` | all 11 pilot outputs | 0 | 133.593 |
| runtime-fixture | `e8d0c640f13bac3e55f7ecf00d0d6109ced54529` | none | 0 | 2.488 |

The flag mutation was the effective parity change in `nix/hix.nix`, `ghcOptions = [ "-O1" ];` → `ghcOptions = [ "-O0" ];`. The runtime fixture ran `testbed/scripts/run-tests.sh` and returned the expected exit 4 after the appended Hurl request; `fixture_expected_failure_verified=true`, fixture command time 11.414s.

Every expected output set matched the harness assertion, and every Nix build command exited 0. `results.json`, each case's `paths.json`, `evaluation.log`, `build.log`, `build.json`, mutation patch, and runtime fixture records are retained under the evidence root. The checkout returned clean to `465d498`; no shared Nix paths were purged.

These are correctness/invalidation records from one local warm-store `aarch64-darwin` run. They are not performance claims or evidence for hosted cold-cache behavior. Native build logs include existing linker warnings about a missing `pg_config` library directory but no failing command.
