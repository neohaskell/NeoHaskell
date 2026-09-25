Archive of run https://github.com/neohaskell/NeoHaskell/actions/runs/36160325157 at immutable source `17f9a149339f2bd1a77cb4c8acd1c9ab17bfc031`.

The raw archive retains all commands, timestamps, six cache audits, fixtures, suite logs, output paths and run metadata. GitHub artifacts expire after 30 days; this evidence branch is durable. Extract with `tar -xzf raw-evidence.tar.gz`.

Reproduce with `.github/workflows/build-cache-colocated.yml` and `docs/build-cache/colocated.sh` from the recorded source, on three fresh Linux hosted workers. Each job runs its warm pass on the same worker. Dispatch requires the exact 40-character checkout SHA via `source_sha` and this authorized experiment branch. Remote project outputs were absent during this batch. Publication after this batch changes that condition: audit remote availability before a new comparison and never relabel a remote-present run as absent. No codemap/doctest or cross-run persistence claim.
