# Metadata lineage prototype

Throwaway planning artifacts. No production implementation is included. The accepted direction reuses EventMetadata. Contracts.hs and candidate-a/candidate-b preserve the superseded comparison, not the current recommendation.

## Reproduce the current defect

Run from the repository root.

```sh
python3 .agents-state/prototypes/metadata-lineage/run_haskell.py .agents-state/prototypes/metadata-lineage/Baseline.hs --match 'Baseline characterization'
python3 .agents-state/prototypes/metadata-lineage/run_haskell.py .agents-state/prototypes/metadata-lineage/Baseline.hs --match 'Required behavior' --expect-failure
```

The characterization passed with 1 example and 0 failures. The required actor check produced the expected 1 example and 1 assertion failure. This executes the real command executor, decision effects, insert and event-store readback. The source-bound failure receipt is `.agents-state/regression-evidence/runtime-4g4iax4g/receipt.json`.

## Check direct metadata reuse

```sh
python3 .agents-state/prototypes/metadata-lineage/run_haskell.py .agents-state/prototypes/metadata-lineage/DirectMetadata.hs
```

Result was eight examples and zero failures, plus 100 callback-depth property cases. This uses existing EventMetadata and RequestContext values. It proves the proposed updater and real in-memory persistence, not production executor treatment.

## Check the superseded contract comparison

```sh
python3 .agents-state/prototypes/metadata-lineage/run_haskell.py .agents-state/prototypes/metadata-lineage/Contracts.hs
```

Result was 22 examples and 0 failures. Each carrier representation ran 100 callback-depth property cases. The pinned toolchain is selected by the maintained `with-toolchain` wrapper. No warning policy was disabled. The passing source-bound receipt is `.agents-state/regression-evidence/runtime-6l55qubz/receipt.json`.

The carrier representations share proposed helper semantics. They exercise real framework metadata construction and in-memory persistence. They do not replace the production executor or prove actual callback, conflict-retry, tenancy, PostgreSQL or log-scope treatment.

## Drive the walkthrough

Open `lineage.html` directly. Select a scenario and press its next-step button. Both owner variants preserve the same proposed lineage. The page is illustrative and does not call production Haskell code.

The browser probe uses an existing external Playwright installation because this worktree has no installed IDE dependencies. It adds no repository dependency.

```sh
LINEAGE_PLAYWRIGHT=/Users/nick/Work/egregore-website/node_modules/playwright node .agents-state/prototypes/metadata-lineage/check_walkthrough.cjs
```

Result was five exact guided walkthroughs, both architecture selectors, absent reconstructed claims, mobile width and no page errors. Captures are under `.agents-state/control-ui/metadata-lineage/`.

## Read the proposed decision

See `../../wayfinder-event-metadata/lineage-proposal.md`. The independent candidate designs live in `candidate-a/design.md` and `candidate-b/design.md`. The user rejected the extra Lineage model in the current agent tab. The revised decision uses existing EventMetadata. See direct-a/design.md and direct-b/design.md for the second comparison.
