# Public-cache success checkpoint

This checkpoint archives Populate Cachix run [36170358310](https://github.com/neohaskell/NeoHaskell/actions/runs/36170358310) from source `21f102c5d25c711af2b09e61e65b6d59113efc05` (`21f102c`). It is n1 correctness evidence only; the timing fields copied from job metadata are not a performance comparison.

| runner | producer root | consumer root | closure paths | core Hspec |
| --- | --- | --- | ---: | --- |
| Ubuntu | `/nix/store/k9haj4aw65cv5h3r9phrh9dn8l30ymn4-neohaskell-ci-components` | `/nix/store/k9haj4aw65cv5h3r9phrh9dn8l30ymn4-neohaskell-ci-components` | 659 | 1095 examples, 0 failures, 3 pending |
| macOS | `/nix/store/866avk4nzlyb99vzzavbh36lmgqx1x0z-neohaskell-ci-components` | `/nix/store/866avk4nzlyb99vzzavbh36lmgqx1x0z-neohaskell-ci-components` | 646 | 1095 examples, 0 failures, 3 pending |

The producer and consumer roots match exactly on both runners. The consumer fetch step is logged as running with all builders disabled, and it uses `./dev nix-components fetch --from-public-caches`. The consumer build/evaluation scan has zero matches; `commands.txt` records the expected non-zero pipeline exit for that empty scan and a successful wrapper exit.

`evidence-lines.log` and `fetch-project-lines.log` show project outputs copied from `https://neohaskell.cachix.org`; the `pg_config` and `pg_config.env` paths came from the upstream NixOS cache at `https://cache.nixos.org`. `fact-table.json` and `summary.json` are the compact fact records; `run.json`, `full.log`, `commands.txt`, the producer directories, and the verification directories preserve their supporting raw evidence.
