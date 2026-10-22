---
group: platform
component: IDE
impact: none
category: Fixed
---

## Summary

Refresh the IDE's build and test dependencies and regenerate its committed bundle.
This keeps the packaged IDE reproducible with the current toolchain. It does not
change NeoHaskell application APIs or require a project migration.
