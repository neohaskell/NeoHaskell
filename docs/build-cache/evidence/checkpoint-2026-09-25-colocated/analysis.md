# Co-location run 36160325157

- Commit: `17f9a149339f2bd1a77cb4c8acd1c9ab17bfc031`
- Result: `success`; repetitions valid: **3/3**
- Raw artifacts: `/tmp/nh-colocated-36160325157`
- Scope: five component suites + Hurl + cold-start; codemap/doctest excluded; same worker warm pass only.

## Per-repetition builds and workload

| repetition | first build / realize (s) | warm build / realize (s) | first Hurl / cold (s) | warm Hurl / cold (s) | workload first / warm (s) | root/counts |
|---:|---:|---:|---:|---:|---:|---|
| 1 | 327.523 / 323.754 | 3.802 / 0.044 | 9.261 / 7.445 | 9.277 / 7.375 | 68.309 / 68.192 | root/counts equal; `True` / `True` |
| 2 | 286.373 / 283.552 | 2.980 / 0.040 | 9.306 / 8.573 | 9.129 / 7.866 | 74.516 / 69.360 | root/counts equal; `True` / `True` |
| 3 | 331.352 / 327.533 | 3.774 / 0.043 | 9.286 / 7.393 | 9.268 / 7.291 | 67.620 / 67.423 | root/counts equal; `True` / `True` |

Suite counts in every pass: core 1095 examples/3 pending, auth 189/0, integration 70/4, service 1128/57, nhintegrations 606/3. Hurl inventories were equal between first and warm; both completion markers were present.

The workload column sums the five suite observations plus Hurl and cold-start. It excludes cache-audit time and PostgreSQL fixture setup; those observations remain in `analysis.json`.

## Cache audits

Probed caches: https://cache.iog.io, https://cache.nixos.org, https://install.determinate.systems, https://neohaskell.cachix.org.
All ten evaluated outputs were remote `absent` at every first and warm audit. Each first pass had ten local `absent`; each warm pass had ten local `present`. `comparison_unusable` was false for all six audits.

| pass | median eval (s) | median probe (s) | median audit elapsed (s) |
|---|---:|---:|---:|
| first | 34.466 | 6.612 | 41.026 |
| warm | 4.142 | 5.409 | 9.471 |

The warm build is therefore a same-run local-store reuse measurement. It must not be reported as a remote-cache speedup; the remote cache remained absent and the later publisher run is a separate cache state.

## n=3 medians

| pass | build (s) | realisation (s) | workload (s) | Hurl (s) | cold-start (s) | measured stage sum (s) | pass wall from audit (s) |
|---|---:|---:|---:|---:|---:|---:|---:|
| first | 327.523 | 323.754 | 68.309 | 9.286 | 7.445 | 399.041 | 438.100 |
| warm | 3.774 | 0.043 | 68.192 | 9.268 | 7.375 | 75.127 | 81.859 |

## Wall intervals and fixture/setup costs

| job | interval (UTC) | wall (s) |
|---|---|---:|
| colocated (1) | 2026-09-25T16:22:24Z → 2026-09-25T16:31:30Z | 546.000 |
| colocated (2) | 2026-09-25T16:31:32Z → 2026-09-25T16:40:34Z | 542.000 |
| colocated (3) | 2026-09-25T16:40:37Z → 2026-09-25T16:49:46Z | 549.000 |

Serial matrix interval (first job start → last job end): **1642.000s** (27.37 min), including all three repetitions; it is not the production CI critical path. GitHub workflow interval including setup/queue: 1646.000s.

Fixture/setup observations:

| repetition | image pull (s) | first PG start / ready / stop (s) | warm PG start / ready / stop (s) |
|---:|---:|---:|---:|
| 1 | 6.099 | 0.223 / 2.290 / 0.299 | 0.151 / 2.294 / 0.279 |
| 2 | 13.946 | 0.708 / 2.227 / 0.700 | 0.405 / 2.227 / 0.405 |
| 3 | 5.225 | 0.332 / 2.307 / 0.270 | 0.146 / 2.282 / 0.290 |

Median postgres image pull observation: **6.099s** across n=3. Fixture/setup observations are retained under each repetition: `postgres-image`, per-pass `postgres-start`, `postgres-ready`, and `postgres-stop` observation directories, alongside per-suite/Hurl/cold-start observations. The full per-pass timing and cache-output maps are in `analysis.json`; no closure contents are used in the summary.
