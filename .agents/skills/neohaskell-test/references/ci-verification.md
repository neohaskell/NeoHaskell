# CI verification

The pull-request workflows select work by changed paths. Apply the optional `run-full-ci` label to a draft pull request to request the selected framework build, doctest, test-suite, native Rust, IDE, Nix package, and generated-consumer jobs while keeping the pull request in draft. The label requests execution only and grants no merge, release, cache-write, or publication authority. Each workflow still uses its own path selection, and its aggregate gate requires selected jobs to succeed.

Each subscribed pull-request event runs selected jobs when the pull request is ready or when a draft has `run-full-ci`. An event that adds another label follows the same rule. Removing `run-full-ci` stops selected jobs on later draft events. Push behavior and existing cache, secret, publication, checkout-credential, immutable-tool-reference, and security gates remain owned by their workflow conditions.
