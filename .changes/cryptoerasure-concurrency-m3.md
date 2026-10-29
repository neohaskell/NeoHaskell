---
group: platform
component: Framework
impact: compatible
category: Fixed
---

## Summary

The maintained erasure example now checks that an erased value stays erased during concurrent owner requests, stale writes, and restart within one application instance. Its runner can use private loopback ports, and its crash check pauses only the selected local request. Application authors can run the example to check their erasure flow. The framework API and production erasure behavior are unchanged, so existing applications need no migration.
