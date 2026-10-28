---
group: platform
component: Framework
impact: none
category: Fixed
---

## Summary

The core test and doctest CI jobs now use a Nix shell without Haskell Language Server. Release self-test Git fixtures disable automatic maintenance so a detached Git process cannot write inside a fixture during cleanup. This changes repository checks only. Application behavior and public APIs remain unchanged, so applications need no migration.
