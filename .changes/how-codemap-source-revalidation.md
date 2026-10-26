---
group: platform
component: Framework
impact: none
category: Fixed
---

## Summary

After a slow local HIE indexing attempt, the `how` codemap keeps returning
current-source search results. This changes repository tooling only; it does
not affect app APIs or require a migration.
