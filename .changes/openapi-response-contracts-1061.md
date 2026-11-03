---
group: platform
component: Framework
impact: compatible
category: Fixed
---

## Summary

OpenAPI now describes query results as one page of rows and command responses with fields beside their `tag`. Regenerate API clients to replace types based on the old schema. HTTP response bodies did not change.
