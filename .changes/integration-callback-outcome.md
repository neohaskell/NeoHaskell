---
group: platform
component: Framework
impact: compatible
category: Fixed
---

## Summary

An outbound integration now reports a failed result when its callback command is rejected, fails, is unauthorized, or has no handler. Callback failure text stays out of dispatcher errors and logs. An application still decides when and how to retry a pending request.
