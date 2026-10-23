---
group: platform
component: Framework
impact: compatible
category: Added
---

## Summary

Framework code can now update an `AtomicVar` inside a shared transaction and wait for a value without polling. Applications that already use `AtomicVar` need no changes.
