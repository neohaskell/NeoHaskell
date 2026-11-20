# Candidate contract fixtures

Release-tool tests generate a fresh version-1 corpus before they consume it. The corpus contains `fixture-set.json`, `invalid-cases.json`, a copied `source/` snapshot, and the complete `previous-0.17.1/` and `current-0.17.2/` candidate directories.

Every binary, bootstrap, NAR hash, signature, and receipt observation is synthetic. The fixture index marks the corpus as parse-only synthetic metadata. Its generated graph has 599 paths, a maximum of 65 references, and a total NAR size of 5,044,109,824 bytes. These values make no cache, build, network-size, or platform claim.

The source snapshot copies declared inputs from the selected repository. It sets registered package and lockfile versions to the historical fixture baseline of 0.17.1. This keeps the synthetic examples reproducible when a platform release increments its version. The export manifest hashes the resulting snapshot bytes. Actual release builds use the version in their frozen preparation. If its bootstrap generator is absent, generation supplies a synthetic placeholder solely for source-byte binding tests. Tests never execute the placeholder or synthetic bootstraps. Production source binding still requires the real generator.

Run the maintained groups with `python3 -B .agents/skills/neohaskell-release/scripts/semantic-release --self-test candidate-contract candidate-runtime`. Tests own fresh temporary directories under `.agents-state/neohaskell-release` and remove them on completion.

To retain a corpus for inspection, first create its parent directory, then run `python3 -B .agents/skills/neohaskell-release/scripts/releases/candidate_contract_tests/generate_fixtures.py --output-root .agents-state/neohaskell-release/inspection`. The output directory must not already exist. `--source-root` selects copied source bytes. It does not replace the maintained generator or candidate validator. Generation refuses destinations under `.agents` or `neo/lifecycle/contract`, existing output trees, and output paths that traverse symlinks.

Run `python3 -B .agents/skills/neohaskell-release/scripts/releases/runtime_inputs.py --write` to refresh the production package inputs under `neo/lifecycle/contract`. Run the same command with `--check` to verify their bytes. Each invocation creates its own temporary corpus. The manifest attributes copied inputs to their snapshot bytes and the maintained producers to their source bytes. Conflicting attribution to one logical source path fails. The export retains package paths needed by native Rust consumers.
