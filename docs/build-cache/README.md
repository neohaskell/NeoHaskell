# Component cache experiment

The implementation plan and current acceptance ledger live in
[PR #899](https://github.com/neohaskell/NeoHaskell/pull/899). These notes record
reproducible technical evidence. No speedup or extraction result is claimed yet.

## Revisions and scope

Baseline: `cab923c0b098fdb82ece539380719c0757ec9057` on `main`.
Pinned haskell.nix: `d7e420f9450ffd26b381e2b01a674c45e42420c3`;
nixpkgs: `647e5c14cbd5067f44ac86b74f014962df460840`;
Hackage snapshot: `6628238498c563b52150998747a11ffba9b1d7fe`.
GHC 9.8.4, project `ghc98`, Cabal O1 by default, dev project O0,
`-fwrite-ide-info`, Haddock disabled except explicit documentation generation.
The full compiler invocation is retained in the baseline log.

Pinned implementation inspected: haskell.nix `builder/comp-builder.nix` and
`lib/clean-cabal-component.nix`. Component derivations do not execute tests;
test executables install under `bin/`. Component cleaning uses Cabal source,
data and extra-source declarations; it is not a hand-written extension whitelist.
Generated attributes were evaluated on aarch64-darwin, not inferred from current
web documentation. The original flake already exposes all project components.

The first isolated Nix build found an undeclared `hspec-discover` build tool in
`core/test/Main.hs` and `integrations/test/Main.hs`. The development shell hid
this missing dependency. Declare this already-used tool in those test components.
The combined nhcore test executable remains individually available in generated
outputs; the CI bundle builds the four split suites and integration-package suite
to avoid compiling and executing the overlapping combined suite unnecessarily.

## Artifact consumers

| Consumer | Working route |
|---|---|
| Split core suites and integration package suite | Nix component executable, fresh execution report |
| Hurl and cold-start readiness | Explicit NHTESTBED_BINARY from Nix; existing Cabal default retained |
| PostgreSQL fixtures | Real server plus readiness **and SQL connection** preflight |
| Criterion runtime evidence | Current service execution report, existing spec-check parser |
| Codemap / hoogle generation | Dedicated existing Cabal route and dist-newstyle cache |
| Doctest | Existing matching-GHC development shell and source interpretation |
| test-match / watch / hiedb | Full Cabal workspace and O0 dev project, .hie files unchanged |
| Rust neo | Original independent flake output unchanged |

`./dev nix-components build` creates a revision-bound producer manifest.
`export` writes a file binary cache including the complete bundle closure.
`fetch` reads the producer path, verifies checkout identity, and copies with
`max-jobs=0` and no remote builders. It does not evaluate the Haskell flake.
Unsigned imports trust only the artifact from the **same workflow run**; never
point this command at an arbitrary artifact. No fork job needs cache-write keys.
The main-only Cachix workflow builds the actual bundle/components for later reuse.

## Comparison protocol (set before interpreting results)

Use dedicated runners, immutable Git revisions and identical target sets. Never
purge the user's shared Nix store. Fresh runner/store evidence must include the
pre-build store inventory and queries of the exact project output paths against
both remote caches. Empty local store with populated remote project outputs and
empty local store with absent remote project outputs are distinct scenarios.
Record unavailable remote evidence as unknown, not a cache miss.

For each platform and baseline/candidate pair collect at least three observations
of A (fresh store, documented remote state), B (repeat exact revision), and C
(one representative implementation edit). Run each suite separately and retain
counts/reports; record environment/evaluation (including IFD), dependency/project
downloads, compile/link, execution and upload/download times separately. Keep
all raw commands, resolved derivation/output paths, lock hashes and logs. Derive
critical-path elapsed time from workflow intervals; aggregate runner time is
the sum of job durations, not the workflow span. Overlapping work must not be
added to elapsed time. Report all observations and medians, not tail percentiles.

Baseline CI `cabal build all` does **not** build the test executables: the later
`cabal test` jobs pay their compilation. A valid comparison must include those
builds, not compare that build-only number against a Nix bundle containing tests.
A log that combines setup and compilation is diagnostic only, not a phase timing.
The fixed issue-862 `scripts/pipeline-benchmark` protocol is unchanged and must
not receive these unrelated observations.

Expansion threshold: correct seven-case invalidation, >=15% **and** >=60s median
representative-edit elapsed improvement, no >10% median fresh-path regression,
and no >10% aggregate runner-time increase; n>=3 per compared scenario/platform.
Noise or incomplete samples means no expansion. Increased cache-hit rate alone
is not success.

## Initial diagnostic

`evidence/baseline-existing-store.json` and matching `.log.gz` record one
successful library/testbed build in a detached baseline worktree: 147.329s,
aarch64-darwin, previously populated shared Nix store and fresh dist-newstyle.
No suite execution, isolated-store condition or phase separation was measured.
This is **not** a performance comparison. Reproduce with:

```sh
git worktree add --detach /tmp/nh-cache-baseline cab923c0b098fdb82ece539380719c0757ec9057
cd /tmp/nh-cache-baseline
nix develop --accept-flake-config --command bash -c \
  'ghc --numeric-version; cabal --numeric-version; cabal build all --disable-documentation -v2'
```

CI closure and execution artifacts currently retain evidence for 14 days. The
committed diagnostic has no such expiry. After hosted artifacts expire, check out
the exact recorded revision and dispatch `test.yml` with `--ref` for Linux;
macOS dispatch/verification is still pending. Draft-triggered skip results are
never counted as execution evidence.

## Recording comparable observations

The independent `measure.py` recorder refuses dirty tracked files and never
rewrites an observation directory. Example (run from the measured checkout):

```sh
python3 docs/build-cache/measure.py record /path/to/evidence/repeat-1 \
  --scenario repeat --stage build \
  --local-state 'exact project outputs present' \
  --remote-state 'not consulted: local hits' -- \
  nix build --accept-flake-config -L --no-link .#ci-components
python3 docs/build-cache/measure.py summarize /path/to/evidence/repeat-{1,2,3}
```

The report validates log digests and groups by revision, platform, command,
lock digest, stage and cache conditions. Fewer than three successes, or any
failure, produces no median. The recorder makes no automatic claim that a
reported state was established: retain store inventories/cache queries alongside
it. Timestamped logs distinguish overlap but do not magically identify GHC link
CPU time. Preserve Nix build-phase logs and workflow step intervals too.

The source-graph reconnaissance found 253 library modules. The dependency closure
of Text comprises 16 modules / 4,004 lines with 194 direct callers elsewhere;
Parser requires 29 / 6,344 lines, Schema 23 / 5,255, Config 108 / 18,034.
These are candidate boundaries, not a selection: fanout and compile cost still
need weighing. Config.Builder derives TH names from actual `tyConPackage`; the
pilot must test identity-sensitive derivation and package-qualified imports.

## Foundation execution results

`evidence/local-foundation.json` records the exact component paths and native
macOS suite results (1,095 core / 189 auth / 70 integration-runtime / 606
integration-package / 1,128 service examples, zero failures in corrected runs).
Existing pending examples are enumerated, not counted as newly executed checks.
Raw Nix build logs and test reports are committed as deterministic gzip files.
The first service run correctly rejected a trust-authenticated fixture: invalid
passwords were accepted. Its failure report is retained; the disposable cluster
was changed to SCRAM and the unchanged suite rerun successfully.

Linux run [36137923111](https://github.com/neohaskell/NeoHaskell/actions/runs/36137923111)
has passed doctest and codemap synchronization; component transfer/execution is
still being verified. The initial replicated baseline run
[36138728431](https://github.com/neohaskell/NeoHaskell/actions/runs/36138728431)
failed before measurement because `nix-store --query --all` is unsupported. The
recorder now uses locally verified `nix path-info --all`; these failed runs are
not timing samples.

The 5.84 GB macOS closure includes GHC through warp's references (verified with
`nix why-depends`), so removing a compiler from PATH does not remove it from the
runtime closure. Preserve the full closure. One contended default-xz export took
369s / ~668 MiB; this is diagnostic, not a replicated comparison. The artifact
format now explicitly uses zstd level 3; fresh-consumer round trips and replicated
transfer comparisons remain required. Three local warm-repeat observations per
route are also recorded but **excluded from decisions** because compilation and
export were running concurrently. Do not derive a speedup from these numbers.

Local Hurl execution at f02d642 passed from the supplied Nix binary. The native
cold-start probe failed at its unchanged `1000 events => initial /ready=503`
expectation: replay had already completed. Running the **unmodified immutable
baseline** reproduces the same failure on this fast host. Both raw logs are
retained; do not weaken the expectation or report this check green. Hosted Linux
cold-start validation remains required.

Two corrected-inventory hosted baseline jobs then exposed a fixture startup race:
`docker exec pg_isready` observed PostgreSQL's temporary initialization **Unix
socket** server just before it shut down. Query TCP (`-h 127.0.0.1`) so readiness
only succeeds for the final server. These failures also precede measurements;
rerun the three observations with the corrected fixture.
