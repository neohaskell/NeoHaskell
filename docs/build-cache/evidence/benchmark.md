# PR899 measurement report

Runs: baseline/candidate `36143016033`, pilot `36143308159`. All downloaded artifacts were `measurement-linux-*`; no component-closure artifact was downloaded.

Each cell is all observations in seconds `[r1, r2, r3] / median`; `—` means that route did not record that stage. These are grouped by exact scenario/stage; repeat/export/compatibility are outside fresh matched totals.

| Stage | Baseline | Candidate | Pilot |
|---|---:|---:|---:|
| setup | [86.374, 77.204, 76.355] / 77.204 | — | — |
| fresh build / compile-link | [337.730, 337.880, 339.373] / 337.880 | — | — |
| fresh build / evaluate-realize | — | [287.568, 350.483, 314.763] / 314.763 | — |
| fresh build / evaluate-realize | — | — | [344.240, 346.186, 662.894] / 346.186 |
| nhcore-test-core | [17.887, 17.918, 17.646] / 17.887 | — | — |
| nhcore-test-core | — | [3.517, 3.692, 3.595] / 3.595 | — |
| nhcore-test-core | — | — | [3.683, 3.666, 3.624] / 3.666 |
| nhcore-test-auth | [13.960, 14.200, 13.796] / 13.960 | — | — |
| nhcore-test-auth | — | [0.713, 0.597, 0.625] / 0.625 | — |
| nhcore-test-auth | — | — | [0.613, 0.609, 0.594] / 0.609 |
| nhcore-test-integration | [21.599, 21.764, 21.463] / 21.599 | — | — |
| nhcore-test-integration | — | [8.465, 8.489, 8.459] / 8.465 | — |
| nhcore-test-integration | — | — | [8.478, 8.480, 8.474] / 8.478 |
| nhcore-test-service | [51.857, 51.948, 51.954] / 51.948 | — | — |
| nhcore-test-service | — | [36.093, 38.159, 37.715] / 37.715 | — |
| nhcore-test-service | — | — | [37.703, 37.206, 36.916] / 37.206 |
| nhintegrations-test | [14.217, 14.512, 13.958] / 14.217 | — | — |
| nhintegrations-test | — | [0.590, 0.691, 0.640] / 0.640 | — |
| nhintegrations-test | — | — | [0.689, 0.633, 0.650] / 0.650 |
| hurl | [17.807, 18.318, 17.890] / 17.890 | — | — |
| hurl | — | [9.167, 9.316, 9.254] / 9.254 | — |
| hurl | — | — | [9.216, 9.336, 9.311] / 9.311 |
| cold-start | [17.990, 18.201, 17.912] / 17.990 | — | — |
| cold-start | — | [6.968, 7.438, 7.460] / 7.438 | — |
| cold-start | — | — | [7.474, 7.535, 7.001] / 7.474 |
| repeat build | [10.071, 9.972, 9.956] / 9.972 | — | — |
| repeat build | — | [2.445, 4.500, 3.586] / 3.586 | — |
| repeat build | — | — | [3.742, 4.431, 4.321] / 4.321 |
| representative edit build | [195.880, 195.659, 198.422] / 195.880 | — | — |
| representative edit build | — | [245.975, 291.892, 255.489] / 255.489 | — |
| representative edit build | — | — | [287.900, 278.125, 270.837] / 278.125 |
| representative edit core test | [17.733, 17.823, 17.618] / 17.733 | — | — |
| representative edit core test | — | [3.562, 3.704, 3.597] / 3.597 | — |
| representative edit core test | — | — | [3.703, 3.648, 3.609] / 3.648 |
| export / transfer | — | [7.026, 9.476, 7.759] / 7.759 | — |
| export / transfer | — | — | [9.333, 10.221, 11.045] / 10.221 |
| pilot compatibility | — | — | [11.465, 10.463, 10.630] / 10.630 |

| Derived stage total (s) | Baseline | Candidate | Pilot |
|---|---:|---:|---:|
| Fresh execution only: five suites + Hurl + cold | 155.491 | 67.733 | 67.393 |
| Recorded fresh route: setup or evaluate/realize + execution | 570.576 | 382.496 | 413.579 |
| Representative edit build + core test | 213.613 | 259.086 | 281.773 |

Compiler flag gate:
- Baseline Cabal logs: `Build profile: -w ghc-9.8.4 -O1`; configure includes `--enable-optimization`, `--disable-debug-info`, `--disable-split-sections`, `--ghc-option=-fwrite-ide-info`; GHC response includes `-O -static -dynamic-too`.
- Candidate/pilot Nix logs: `--enable-static --enable-shared --enable-split-sections`; no explicit `--enable/disable-optimization`, `-O0..3`, or `-fwrite-ide-info` appears. Exact Nix optimization level is unproven, so direct timing comparisons are conditional.

Correctness:
- Every recorded observation: exit code 0; `measure.py summarize` validates all digests, 12 groups baseline, 12 candidate, 13 pilot, each n=3 with a median.
- Each of the five suites: baseline/candidate/pilot counts are 1095/3 pending, 189/0, 70/4, 1128/57, 606/3; all failures 0. Hurl all reps completed with 0 failed files. Linux cold-start all reps passed (health spread 2–11 ms). Pilot compatibility: 3 examples/0 failures plus nhcore consumer on all reps.
- Raw baseline SHA `cab923c...` had the known integration fixture failure; normalized SHA `e94a6c...` changes only the mock refresh token (`mkRefreshToken` removed, `refreshToken = Nothing`), assertions unchanged, and all measured reps pass.

Failures/gaps:
- Pilot fresh build has a large realization outlier: 662.894 s (realization 627.478 s) versus 344.240/346.186 s; realization combines download/build.
- Substituters are configured and logs show dependency copies/local derivation builds; remote availability and hit rate are unproven.
- Text pilot edit invalidates all component outputs; no matched sibling timing exists.
- Representative edit raw route totals: baseline 213.613 s vs candidate 259.086 s (candidate +45.473 s slower (21.288% slower)); not a valid threshold claim while flags/routes are unmatched.
- Workflow job wall sums: baseline 2519 s, candidate 2116 s, pilot 2610 s. No runner billing rate/cost is available; no sequential/parallel speedup claim.

Decision: no expansion. Minimal next measurement: make Nix emit/record the exact optimization/link flags and rerun matched baseline/candidate with those flags, then add the missing matched sibling edit (>=3 reps) and capture billing/runner cost; keep pilot compatibility separate.
