---
group: platform
component: Framework
impact: compatible
category: Added
---

## Summary

New applications can list an entity's event payload types in `deriveEntityEvents`.
The marker creates the parent event type and lets commands emit a single payload
type without writing its wrapper constructor. Existing event types and stored
events are unchanged. An application needs no migration to keep using a handwritten
event type.
