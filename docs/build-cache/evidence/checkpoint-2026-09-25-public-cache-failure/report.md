# Signed public-cache verification failure

Run [36163339478](https://github.com/neohaskell/NeoHaskell/actions/runs/36163339478), source176a5bf: both publishers passed, both fresh Linux/macOS consumers failed before tests. This is correctness evidence, not a benchmark.

Ubuntu closure659 paths:15 pushed,644 reported already present. macOS646:15 pushed,631 already present. All15 logged pushed paths per platform were HTTP200 from Cachix, including signed roots. Upstream dependencies are omitted by Cachix filtering. Required pg_config paths4043yq19p13aabhi8a370cdrh44r2mf9 (Linux) and ha0192b1ax8llz1am0idbda8q3j9fr5q (macOS) return404 from Cachix but200 from NixOS and IOG. Publisher logs show retrieval from NixOS. The single-source `nix copy --from https://neohaskell.cachix.org` consumer incorrectly assumed all closure dependencies were mirrored there. Signatures/configured key/cache alias are healthy.

Smallest proposed correction: realize the exact non-derivation output path through configured trusted public substituters with local and remote builders disabled, without evaluating the flake. `nix-store --realise OUTPUT --option max-jobs 0 --option builders ''` dry-run can resolve the Linux root with the configured caches; Cachix-only cannot. This dry-run does not prove full fresh-runner retrieval or test execution. That hosted acceptance is still owed.

Raw archive includes both publisher and consumer logs, producer metadata/closure manifests, narinfo probes and diagnostic dry-run output. Local dry runs use an already populated Darwin store and are not fresh-runner benchmarks. CI artifacts retain30 days; this archive is durable.

Reproduce failed behavior at176a5bf with `.github/workflows/cachix-push.yml` manual source_sha matching the immutable experiment-branch head, then inspect exact producer root and single-source consumer logs. Narinfo probes use `https://CACHE/HASH.narinfo`. Before repeating, record changing cache availability and do not infer absent-cache conditions from this old batch.

Official sources: https://docs.cachix.org/faq and https://docs.cachix.org/garbage-collection describe upstream filtering; https://nix.dev/manual/nix/latest/command-ref/nix-store/realise documents exact output realization. Preserve signature checks, never re-enable compilation in consuming jobs.
