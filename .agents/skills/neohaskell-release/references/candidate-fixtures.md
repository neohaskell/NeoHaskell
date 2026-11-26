# Candidate contract fixtures

Release-tool tests generate one candidate in a temporary `current/` directory. The generator reads its version from `neo/Cargo.toml`. The fixture directory also contains `fixture-set.json`, a source snapshot, and a copy of the shared parser cases. Keep these outputs out of Git.

Every binary, bootstrap, NAR hash, signature, and receipt observation is synthetic. The fixture index marks the corpus as parse-only synthetic metadata. Its generated graph has 76 paths, a maximum of 65 references, and a total NAR size of 5,044,109,824 bytes. These values make no cache, build, network-size, or platform claim.

The snapshot copies the declared inputs from the selected repository. A release-seeded checkout still generates an engineering fixture. Its temporary starter uses the engineering pin required by the contract. If the bootstrap generator is absent, fixture generation supplies a placeholder for source binding tests. Production source binding requires the real generator. Tests that need two versions derive the next patch version from the current version in a temporary directory.

Run the maintained groups with `python3 -B .agents/skills/neohaskell-release/scripts/semantic-release --self-test candidate-contract candidate-runtime`. Tests own fresh temporary directories under `.agents-state/neohaskell-release` and remove them on completion.

To retain a corpus for inspection, first create its parent directory, then run `python3 -B .agents/skills/neohaskell-release/scripts/releases/candidate_contract_tests/generate_fixtures.py --output-root .agents-state/neohaskell-release/inspection`. The output directory must not already exist. `--source-root` selects copied source bytes. It does not replace the maintained generator or candidate validator. Generation refuses destinations under `.agents` or `neo/lifecycle/contract`, existing output trees, and output paths that traverse symlinks.

The production registry lives at `neo/lifecycle/contract/registry.json`. Release plans pin its digest.
