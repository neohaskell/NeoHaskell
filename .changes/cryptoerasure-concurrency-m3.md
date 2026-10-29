---
group: platform
component: Framework
impact: none
category: Fixed
---

## Summary

The maintained testbed and PostgreSQL fixture now check protected-value erasure during concurrent requests, stale writes, query updates, and reconnect. This change adds verification and example instructions. It does not change the framework API or application runtime behavior.
