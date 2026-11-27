---
group: platform
component: CLI
impact: compatible
category: Fixed
---

## Summary

The NeoHaskell installer now downloads and retains the pinned compiler, build and test tools, Haskell Language Server, HLint, Fourmolu, and local documentation tools. Projects with the same toolchain pin reuse those files after Nix garbage collection. Run `neo update` to install the toolchain for an existing managed installation. Project-specific dependencies still have their own build inputs. Repeating `neo generate` also preserves the timestamps of unchanged build files.
