---
group: platform
component: Framework
impact: none
category: Improved
---

## Summary

Integration dispatch preserves atomic worker admission and drain during reaping and shutdown through a dedicated queue control. The constructor record change affects the framework builder and repository tests; application setup continues through `Application.withDispatcherConfig`. Event processing and `integration_result` logs keep their existing behavior.
