---
group: platform
component: Framework
impact: compatible
category: Improved
---

## Summary

Integration dispatch preserves atomic worker admission and drain during reaping and shutdown through a dedicated queue control. Event processing and `integration_result` logs keep their existing behavior.
