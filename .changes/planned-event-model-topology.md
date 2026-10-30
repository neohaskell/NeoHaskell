---
group: platform
component: Framework
impact: compatible
category: Added
---

## Summary

An application can describe a planned state change or query in a source-local Feature before its handlers exist. The model keeps each planned command, event, and query source visible without starting it. Query sources must belong to the query's `EntitiesOf` list, so an invalid source fails compilation.
