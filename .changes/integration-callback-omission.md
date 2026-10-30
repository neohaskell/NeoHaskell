---
group: platform
component: Framework
impact: compatible
category: Fixed
---

## Summary

An outbound integration now reports failure if its command handler returns without sending a result. Your application can keep an erasure request pending and retry it explicitly instead of treating the missing result as completion. No application code change is required.
