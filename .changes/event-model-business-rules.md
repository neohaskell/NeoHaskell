---
group: platform
component: Framework
impact: compatible
category: Added
---

## Summary

An Event Model state-change or state-view Slice can now keep direct Given-When-Therefore rules beside its implemented command or query. Rules hold concrete historical events, a typed command or incoming event, and an exact command or query outcome. A rule can request typed values and reuse them in the same block. The runner that generates and executes cases will follow separately; applications should keep their existing behavior tests.
