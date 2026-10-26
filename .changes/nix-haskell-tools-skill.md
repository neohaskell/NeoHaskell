---
group: platform
component: Framework
impact: none
category: Added
---

## Summary

Add a repository skill that runs Haskell tools through the project's Nix development shell. Agent instructions and test skills now use that shell for compiler and Cabal commands. This changes the development workflow only. Application APIs and runtime behavior are unchanged.
