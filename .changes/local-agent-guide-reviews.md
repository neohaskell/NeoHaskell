---
group: platform
component: Framework
impact: none
category: Fixed
---

## Summary

The agent guide check now keeps ignored Haskell fragment reviews in ignored worktree-local state. A fresh checkout must review the fragments before the local corpus gate passes. This changes contributor verification only and does not change generated applications.
