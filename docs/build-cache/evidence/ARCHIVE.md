# PR 899 raw evidence archive

This branch preserves raw diagnostic logs and generated import-graph data outside
the implementation PR's review diff. It is evidence, not a release branch.
Do not merge this branch. PR: https://github.com/neohaskell/NeoHaskell/pull/899

The implementation checkpoint is 987fe3dbbd520f1ae87f64c2494023b5a1a12b6c.
Both pilot-mutations archives contain exact paths, mutation patches, commands,
build logs and elapsed times. The first has six completed cases plus the intended
runtime HTTP assertion failure (the harness expected exit 1 instead of Hurl's 4).
The second completes all seven cases after correcting that harness expectation.
The first includes actual compilation; the second reuses the first outputs.
These contended local timings prove correctness, not a performance improvement.

Existing reports include failed disposable-fixture and baseline-readiness runs;
consult local-foundation.json and the implementation PR for their interpretation.
No evidence here establishes a complete cross-platform performance comparison.
