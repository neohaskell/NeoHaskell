---
group: platform
component: Framework
impact: none
category: Fixed
---

## Summary

Contributor checks reject raw concurrency imports outside their named owners and
enforce the reviewed conditional, text parameter and formatting conventions.
Lint reads component language defaults from Cabal, so contributors no longer need
duplicate Strict pragmas solely for lint. These changes affect internal development
checks only. Application behavior and public APIs are unchanged, so application
developers need no migration.
