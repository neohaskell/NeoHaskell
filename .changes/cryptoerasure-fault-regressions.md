---
group: platform
component: Framework
impact: compatible
category: Fixed
---

## Summary

If a stored event is unreadable, query replay now reports failure through `/ready` instead of marking an incomplete query ready. Applications need no code change. The maintained protected-erasure testbed checks missing keys, malformed envelopes, and an unavailable key table without returning protected values.
