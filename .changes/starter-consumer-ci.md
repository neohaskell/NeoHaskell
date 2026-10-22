---
group: platform
component: CLI
impact: compatible
category: Fixed
---

## Summary

New projects and the testbed use the JSON and deriving instances supplied by
their framework markers and omit unused derivations. Every pull request verifies
the generated starter against its own framework revision. Framework changes
include starter and testbed updates under the consumer verification skill's
review requirements.
