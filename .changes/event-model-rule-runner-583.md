---
group: platform
component: Framework
impact: compatible
category: Added
---

## Summary

An Event Model can now run its direct rules as generated Hspec examples. Each example checks a command's complete ordered events or rejection. Query examples check the action and all rows before and after the event. Register generators for requested values and call `specFor` in the test suite. Each sample has separate in-memory storage. Commands that accept an existing entity without emitting events now succeed without appending an empty payload.
