---
group: platform
component: Framework
impact: compatible
category: Fixed
---

## Summary

If a stored event is unreadable, query replay reports failure through `/ready` instead of marking an incomplete query ready. An affected HTTP query returns a generic 500 response while its projection reports rebuilding or failed. Applications need no code change. The maintained testbed checks missing keys, malformed envelopes, and an unavailable key table without returning protected values.
