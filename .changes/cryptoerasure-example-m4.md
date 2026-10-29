---
group: platform
component: Framework
impact: compatible
category: Fixed
---

## Summary

The maintained protected-erasure example now shows a case status that remains after erasure and is reconstructed from events after restart. Its automated checks assert the status alongside the erased value, explicit retry, and application completion. Application authors can use the example to test their own retention choices. The framework erasure API and behavior are unchanged, so existing applications need no migration.
