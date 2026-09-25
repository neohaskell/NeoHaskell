# Independently cached component builds

[PR #899](https://github.com/neohaskell/NeoHaskell/pull/899) owns the current plan,
acceptance ledger, measured revisions and resume instructions. Keep it draft
until verification is complete. No performance improvement is established yet.

## Implementation and existing consumers

Cabal still defines the full workspace. The existing pinned haskell.nix project
builds its components; `ci-components` references three libraries, four split
core test executables, the integration-package suite and testbed. Generated
standalone outputs remain available, including the overlapping aggregate suite.
The independent Rust `neo` output and development shell are preserved.

`./dev nix-components build` resolves and realizes the bundle, recording the
revision, lock digest, output paths and separate evaluation/realization timing.
`export` writes its complete closure to a zstd file cache. `fetch` verifies the
producer revision/output, then substitutes exact paths with local and remote
builders disabled, without flake evaluation. Trust unsigned artifacts only from
the same workflow run. Fork jobs need no cache-write credentials; main's Cachix
workflow publishes actual component outputs for later reuse.

| Consumer | Working route |
|---|---|
| Five test suites | Execute fetched binaries; retain fresh reports/counts |
| PostgreSQL service tests | Real fixture, readiness and SQL preflight |
| Hurl and cold-start | Explicit NHTESTBED_BINARY; existing Cabal default retained |
| Runtime criterion evidence | Current service report, existing spec-check parser |
| Codemap/Hoogle | Dedicated Cabal build/cache route |
| Doctest | Existing matching-GHC shell and source interpretation |
| REPL/watch/hiedb | Full Cabal workspace, O0 dev project and .hie artifacts |

The two suites using hspec-discover now declare their existing build tool.
An Oura mock no longer supplies a refresh token that triggered real network
traffic after mocked Unauthorized responses; existing assertions are unchanged.

## Reproducible comparisons

Baseline: `cab923c0b098fdb82ece539380719c0757ec9057`.
Pinned haskell.nix: `d7e420f9450ffd26b381e2b01a674c45e42420c3`.
GHC 9.8.4 (`ghc98`), Cabal O1, dev O0, `-fwrite-ide-info`; default Haddock off.
Do not reuse or relax the unrelated fixed-task `scripts/pipeline-benchmark`.

`measure.py` records immutable revisions, commands, cache conditions, lock hashes,
exit codes and timestamped raw logs. It validates log digests when summarizing;
medians require at least three successful comparable observations. Unknown cache
state is not a cache miss. Never purge a shared local store for this experiment.

The candidate component routes run `cache-state.py` immediately before each
fresh, repeat, Text-edit and sibling-edit realization. Each audit has its own
`cache-state-*.json` and sibling `.evaluation.log`; `comparison_unusable=true`
is retained and printed conspicuously. Add that audit's `evaluation.eval_s` to
the matching route's evaluation work because it warms Nix before the build;
exclude `probe_s` and the rest of the audit wall time as instrumentation, while
keeping `observation.json.elapsed_s` as the raw command wall time. The probe is
not run against `baseline.sh`: the raw Cabal baseline has no `ci-components`
flake output to evaluate.

`baseline.sh` and `candidate.sh` run matched sequential targets and suites on
fresh hosted runners, then repeat the build and apply a deterministic Text edit.
The baseline retains both its raw SHA and the exact fixture-only normalization
patch matching the candidate's Oura mock. It does not silently count failed raw
baseline runs as successful observations. `pilot.sh` applies the recorded
extraction and adds separately measured compatibility validation.

```sh
gh workflow run test.yml --ref snotty-kiwi -f baseline_measurements=true
gh workflow run test.yml --ref snotty-kiwi -f pilot_measurements=true
gh workflow run test-macos.yml --ref snotty-kiwi
```

Report every observation, median and suite count. Separate evaluation/setup,
downloads, compilation/linking, execution and artifact transfers. Sequential
workload times do not establish parallel CI critical-path latency or aggregate
runner cost; derive those independently from actual job/step intervals. Cached
executables must still execute. Draft-triggered skips are not verification.

Expansion threshold, chosen before results: all seven invalidation cases pass;
>=15% **and** >=60s median representative-edit elapsed improvement; no >10%
median fresh-path regression or aggregate runner-time increase; n>=3. Incomplete
or noisy evidence means no expansion. Cache-hit rate alone is insufficient.

## Extraction pilot

`foundation-pilot.patch` is a disposable experiment, not a production split.
It extracts Text's cohesive 16-module/4004-line dependency closure, explicitly
owns dependencies, reexports 15 public modules through nhcore and has no umbrella
back-edge. Its 194 reverse callers make invalidation important. Three standalone
happy/error/boundary examples and a consumer depending only on nhcore exercise
Core plus package-qualified Array/Text/Result imports.

```sh
git worktree add --detach /tmp/nh-foundation-pilot 59cb5c8
git -C /tmp/nh-foundation-pilot apply --index "$PWD/docs/build-cache/foundation-pilot.patch"
git -C /tmp/nh-foundation-pilot commit -m 'experiment: apply recorded foundation pilot'
python3 docs/build-cache/pilot-mutations.py /tmp/nh-foundation-pilot /path/to/new/evidence
```

The mutation harness requires a clean detached checkout and real disposable
PostgreSQL fixture. It records derivation/output paths and actual builds for:
no change, Markdown, test-only, implementation, sibling, flags and runtime data.
All seven passed locally. The runtime case deliberately gets HTTP 404 where its
new fixture demands 200; Hurl exit 4 proves the changed input executed. Full pilot
hosted suites and replicated performance interpretation remain pending.

Before shipping any extraction, update codemap/Hoogle discovery, doctest paths,
release inventory and CI registration, then pass full required CI. The original
full-workspace development route remains the production default.

## Evidence storage and limitations

Generated logs and import-graph dumps are excluded from the implementation diff.
The permanent [evidence archive at 58df721](https://github.com/neohaskell/NeoHaskell/tree/58df7214733075ab78ff104aca988ede723053b3/docs/build-cache/evidence)
is retained on `evidence/pr-899-build-cache`; do not merge or delete that reference.
It includes native suite results, both pilot runs, exact mutation patches/paths,
raw build logs and failed diagnostics. These committed records do not expire.

CI closures/reports retain 14 days; measurement artifacts retain 30 days. Record
run URLs and compact findings in the PR, and preserve important raw evidence in
the archive before expiry. Reproduce expired runs using their exact revisions,
recorded scripts and commands, not an assumed equivalent current checkout.

Native suite reports contain 3088 examples including 67 pending. Local cold-start
fails its existing first-probe readiness assertion on this fast host; the unchanged
baseline reproduces it. Linux cold-start passes. No assertion was weakened.
The 5.84GB native closure genuinely retains GHC through warp. Initial local timing
samples were contended and are excluded from performance decisions. Consult the
PR for the latest hosted outcomes; neither cache substitution nor a single fast
sample establishes end-to-end improvement.
