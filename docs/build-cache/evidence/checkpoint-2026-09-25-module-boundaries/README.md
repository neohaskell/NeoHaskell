# Module-boundary audit checkpoint

This checkpoint preserves the textual source inventory recorded at [`ea50205`](https://github.com/neohaskell/NeoHaskell/commit/ea50205) in `module-graph.json`. It is a regex and text based inventory, not a Cabal ownership graph, CPP-expanded graph, generated-code inventory, or complete Template Haskell/compiler graph. Line counts are source line counts; they are not compile-time measurements.

The method recursively scans `core/**/*.hs`, excluding top-level source directories beginning with `test` except `testlib`. Module declarations use `^module\s+([A-Z][\w.]*)`; import parsing allows the `SOURCE` pragma and prequalified or package-qualified forms. Traversal follows only discovered local modules, and SCCs are computed with Tarjan's algorithm.

The saved graph reproduces these key counts:

| roots or inventory | modules | lines |
| --- | ---: | ---: |
| `Test.Spec` closure | 125 | 18,751 |
| union closure of `Array`, `Text`, `Task`, `Var`, `Environment` | 20 | 4,555 |
| `testlib` inventory under `core/testlib/` | 51 | 9,980 |

Run `python3 reproduce.py module-graph.json` to recompute the counts and report Tarjan SCCs from the saved JSON. Before using this evidence for extraction, redo the compiler graph; this audit does not establish compiler-complete ownership or dependency boundaries.
