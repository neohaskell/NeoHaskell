---
group: platform
component: CLI
impact: compatible
category: Fixed
---

## Summary

New projects and the testbed use the JSON and deriving instances supplied by
their framework markers. Every pull request verifies the generated starter
against its own framework revision. CI rejects redundant marker-generated
boilerplate so framework changes keep these examples current.
