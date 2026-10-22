# CI verification

Every pull request runs the generated-project consumer contract against its own
framework revision, including drafts and documentation changes. Its aggregate
gate rejects failure, cancellation and skipped execution. See
[NeoHaskell consumer verification](../../neohaskell-consumer-verification/SKILL.md)
for starter and testbed freshness and same-PR migration requirements.

Other jobs select work by changed paths. Apply the optional `run-full-ci` label
to a draft pull request to request the selected framework build, doctest,
test-suite, native Rust, IDE and Nix package jobs while keeping it in draft.
The label requests execution only and grants no merge, release, cache-write or
publication authority. Each aggregate gate requires its selected jobs to succeed.

Each subscribed pull-request event runs those selected jobs when the pull request
is ready or when a draft has `run-full-ci`. Removing `run-full-ci` stops them on
later draft events. The generated-project contract continues to run.
Push behavior and cache, secret, publication, checkout-credential,
immutable-tool-reference and security gates remain owned by their workflows.
