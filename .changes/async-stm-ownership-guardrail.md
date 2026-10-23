---
group: platform
component: Framework
impact: none
category: Fixed
---

## Summary

Contributor lint checks now reject raw Async and STM imports outside their named
core owners. This changes internal development checks only. Application behavior
and public APIs are unchanged, so application developers need no migration.
