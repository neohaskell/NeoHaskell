# Candidate contract fixture corpus

The fixture corpus is versioned at `v1`. Its frozen paths are `fixture-set.json`, `invalid-cases.json`, `source/`, `previous-0.17.1/`, and `current-0.17.2/`.

Each candidate directory is a complete materialized input. `candidate.json`, `closures.json`, and `build-receipts.json` use the candidate contract's canonical UTF-8 JSON encoding. The remaining files are the declared native, bootstrap, and auxiliary file identities. The Rust reader can consume these directories and the named mutation descriptors without importing Python implementation code.

Every binary, bootstrap, NAR hash, signature, and receipt observation in this corpus is synthetic. `parse_only` and `synthetic_metadata_only` are true in `fixture-set.json`. The 599-path graph preserves the saved local closure's 65-reference maximum and 5,044,109,824-byte NAR-size sum. It does not reuse those local NAR contents or signatures and makes no cache, build, network-size, or platform claim.

The committed `source/` snapshot is also synthetic. The current checkout has no bootstrap generator file, so this snapshot includes a placeholder generator recipe solely to exercise exact source-byte binding. Production source binding still fails closed when that required file is missing. The placeholder and the bootstrap scripts are never executed by the tests.

Regenerate the fixtures with `python3 -B .agents/skills/neohaskell-release/scripts/releases/candidate_contract_tests/generate_fixtures.py`. The generator reads the saved local closure only to confirm its bounded shape, then writes deterministic synthetic content. The test suite validates the frozen fixture bytes and the source tree commit identity.
