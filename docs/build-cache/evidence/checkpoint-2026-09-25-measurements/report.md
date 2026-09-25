# PR 899 Linux optional measurement analysis

Read-only analysis of completed workflows; route totals are per-repetition sums followed by median. For candidate/pilot, only cache audit `evaluation.eval_s` is added to the matching build/repeat/edit/sibling-edit stage; `probe_s` and remaining audit wall time are excluded.

## Runs and source revisions

- baseline/candidate workflow [36155654547](https://github.com/neohaskell/NeoHaskell/actions/runs/36155654547), head `c0d34a80f1ae99474231715232f28119a6a297fe`; baseline normalized `e94a6c576b4b92c70ececacf615113eb6608e25f`, candidate initial `c0d34a80f1ae99474231715232f28119a6a297fe`.
- pilot workflow [36155658355](https://github.com/neohaskell/NeoHaskell/actions/runs/36155658355), head `c0d34a80f1ae99474231715232f28119a6a297fe`; pilot initial `a760fc8b616c2f5089a09dd4005f7cfff832b0a9`.
- Text edits: baseline `d8f9f95c8df29c468eda5ff26b76486b704a2323`, candidate `224793590ad7566e790f248bebd532b06f634915`, pilot `a9ee823e06ee3ccc64d62fe55810d84a9cee85c2`.
- Int sibling edits: baseline `b71062e6290122ac96efddcbd9a5f27274e5ce2c`, candidate `9074f62df5d95aabbb96cd908bab3b5daabbdbb6`, pilot `a5fe40d0487d4471d9661ac5c0f2bac731646c97`.
- Shared platform/lock: `x86_64-Linux`, lock `1effc4277140756e9669e68a4585ae4694e5dabe6bba3d586cf2128a5adf3621`.

## Corrected route totals (seconds)

| workload | route | rep1 | rep2 | rep3 | median | validity |
|---|---|---:|---:|---:|---:|---|
| baseline | fresh_execution | 118.336 | 154.022 | 157.784 | 154.022 | 5 suites + Hurl + cold; no audit on execution stages |
| baseline | fresh_route | 468.171 | 576.969 | 584.845 | 576.969 | valid |
| baseline | repeat_build | 6.348 | 9.790 | 10.038 | 9.790 | valid |
| baseline | sibling_edit_route | 152.064 | 201.689 | 210.153 | 201.689 | valid |
| baseline | text_edit_route | 139.863 | 191.664 | 199.659 | 191.664 | valid |
| candidate | fresh_execution | 69.069 | 71.899 | 78.286 | 71.899 | 5 suites + Hurl + cold; no audit on execution stages |
| candidate | fresh_route | 440.142 | 379.915 | 419.838 | 419.838 | valid |
| candidate | repeat_build | 8.407 | 5.599 | 8.544 | 8.407 | valid |
| candidate | sibling_edit_route | 311.394 | 237.542 | 258.063 | 258.063 | INVALID audit (1/3) |
| candidate | text_edit_route | 312.037 | 241.812 | 259.269 | 259.269 | valid |
| pilot | fresh_execution | 68.308 | 67.384 | 68.781 | 68.308 | 5 suites + Hurl + cold; no audit on execution stages |
| pilot | fresh_route | 426.227 | 388.829 | 442.127 | 426.227 | valid |
| pilot | repeat_build | 8.033 | 6.030 | 8.385 | 8.033 | valid |
| pilot | sibling_edit_route | 292.817 | 248.994 | 294.474 | 292.817 | valid |
| pilot | text_edit_route | 298.890 | 257.745 | 307.736 | 298.890 | valid |

The pilot `export` and `compatibility` observations are present but excluded from these matched routes. Baseline has no cache audit by design: raw Cabal has no `ci-components` output to probe.

Audit proof: candidate eval/probe medians (fresh, repeat, Text, Int sibling) are `(36.098, 11.482)`, `(4.352, 6.487)`, `(3.413, 12.322)`, `(2.872, 12.680)` seconds; pilot is `(34.920, 9.281)`, `(4.200, 4.054)`, `(3.726, 7.809)`, `(3.709, 8.300)`. The route corrections add only the first number in each pair; the second is instrumentation and remains excluded. Both audit sets share lock `1effc4277140756e9669e68a4585ae4694e5dabe6bba3d586cf2128a5adf3621` and substituter configuration `3dd5520010a9df893c9d2e847fac7347f1617b97e4981e77785e3f667bbb1740`.

## Validity and flags

- `baseline`: n=3; stage counts per repetition=14; all observation exit codes zero=True; five suites + Hurl + cold present in 3/3 reps; repeat + Text edit/core + Int sibling/core present in 3/3 reps.
  Cache audits: 0 expected/found; the baseline route intentionally has no `ci-components` probe.
- `candidate`: n=3; stage counts per repetition=14; all observation exit codes zero=True; five suites + Hurl + cold present in 3/3 reps; repeat + Text edit/core + Int sibling/core present in 3/3 reps.
  Cache audits: 11/12 valid; invalid=1; candidate invalid sample is `measurement-linux-candidate-3/cache-state-sibling-edit.json` (`neohaskell.cachix.org: network error`), so candidate sibling median is diagnostic only.
- `pilot`: n=3; stage counts per repetition=15; all observation exit codes zero=True; five suites + Hurl + cold present in 3/3 reps; repeat + Text edit/core + Int sibling/core present in 3/3 reps.
  Cache audits: 12/12 valid; invalid=0; all pilot route audit flags are usable.
- Existing `docs/build-cache/measure.py summarize` completed successfully for baseline, candidate, and pilot summary files, including observation log digest checks.

## Artifact archive

- Root: `/tmp/pr899-measurement-runs.EkPfRY`
- baseline:
  - `/tmp/pr899-measurement-runs.EkPfRY/baseline/measurement-linux-baseline-1` — id `10874300354`, archive sha256 `02c5a0b3591ebe555383e47132ffc414894b5c295204791b15574f7070049dca`
  - `/tmp/pr899-measurement-runs.EkPfRY/baseline/measurement-linux-baseline-2` — id `10874501166`, archive sha256 `0e1df4d91a3fc88b3d32393bc9dfc2bd7f655445fee3b860e1cfe7ec145a0ba`
  - `/tmp/pr899-measurement-runs.EkPfRY/baseline/measurement-linux-baseline-3` — id `10873953893`, archive sha256 `8fc9787d6b6b16b1d9302ea7233e80c315f288ea57ee34f4d1c5c380a790ab8b`
- candidate:
  - `/tmp/pr899-measurement-runs.EkPfRY/candidate/measurement-linux-candidate-1` — id `10874661244`, archive sha256 `5530b836e017bed05f5055b12befef411ae6f1414d6b9358780a8d8d02bd33bd`
  - `/tmp/pr899-measurement-runs.EkPfRY/candidate/measurement-linux-candidate-2` — id `10874280728`, archive sha256 `d62af0d0faa358032e476554d7b7bb2e97c5be8147bd2fd31b67636f9e854320`
  - `/tmp/pr899-measurement-runs.EkPfRY/candidate/measurement-linux-candidate-3` — id `10874225952`, archive sha256 `4012d37791e76bfc1e89ec75e58b50ce8605656bd1e0b920065c2f9ffe3d4113`
- pilot:
  - `/tmp/pr899-measurement-runs.EkPfRY/pilot/measurement-linux-pilot-1` — id `10874400973`, archive sha256 `daf15f21db23d325ef7a658b32f6dd362a2ae953dd98b87eff208316cfd5ca1b`
  - `/tmp/pr899-measurement-runs.EkPfRY/pilot/measurement-linux-pilot-2` — id `10873838712`, archive sha256 `226c32674ec0398a6f6a89f29796e49dbcc9f0307fba5e08efabedec40f828a7`
  - `/tmp/pr899-measurement-runs.EkPfRY/pilot/measurement-linux-pilot-3` — id `10873943978`, archive sha256 `e52453130745152a3d24755c3daa4633f47c77fad8cb5f763bbc1aa05321d61a`
- Machine-readable analysis: `/tmp/pr899-measurement-runs.EkPfRY/analysis.json` (sha256 `25dff035e85bb72761e9a05b9b792add3cdbf7b2e11b813330013b750a1f26fa`).

No causal speedup claim: baseline is Cabal while candidate/pilot are Nix component routes with different build flags and cache conditions; these are descriptive measurements.
