---
group: platform
component: Framework
impact: none
category: Improved
---

## Summary

Framework test jobs no longer upload their local Nix stores through Magic. They read the configured public Cachix and IOG caches and build cache misses normally. Test macOS also runs on pull requests that change Nix, Cabal, or relevant workflow files. Application behavior, public APIs, and test workloads stay the same.
