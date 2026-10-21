# CI verification

The pull-request workflows select work by changed paths. Apply the optional `run-full-ci` label to a draft pull request to request the selected framework build, doctest, test-suite, native Rust, IDE, Nix package, generated-consumer, and installer jobs while keeping the pull request in draft. The label requests execution only and grants no merge, release, cache-write, or publication authority. Each workflow still uses its own path selection, and its aggregate gate requires selected jobs to succeed.

Only the event that adds `run-full-ci` requests an immediate run. Later pull-request updates keep the run enabled while the label remains present. Events that add another label do not rerun expensive jobs. Removing the label stops full-CI execution on later events. Push behavior and existing cache, secret, publication, checkout-credential, immutable-tool-reference, and security gates remain owned by their workflow conditions.
