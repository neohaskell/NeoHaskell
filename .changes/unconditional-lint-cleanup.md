---
group: platform
component: Framework
impact: compatible
category: Improved
---

## Summary

Command requests retain one safe result when decoding or handling fails, and cancellation still reaches blocked requests. Framework contributors can run the unconditional service-code lint check without an allowance for old findings. Public APIs and response formats are unchanged, so application projects need no migration.
