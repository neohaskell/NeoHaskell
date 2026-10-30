---
group: platform
component: Framework
impact: compatible
category: Added
---

## Summary

An application can now describe its event flow in source-local Features. Export a Feature action beside its commands, queries, and integrations, then compose those actions in a named Event Model. The compiler checks typed event sources and command targets, and model evaluation checks each Slice has one primary shape. The description does not start workers yet. Existing application registration remains in place.
