---
group: platform
component: Framework
impact: breaking
category: Breaking changes
---

## Summary

Framework code can compose `AtomicVar` reads and updates with conditional `ConcurrentMap` operations in one transaction. `Transaction` is now a distinct type, so application code that imports `AtomicVar.Transaction` or combines raw STM with `AtomicVar.peekSTM` must migrate.

## Migration

Import `Transaction` from the `Transaction` module instead of `AtomicVar`. Use `ConcurrentMap.getOrInsertIfTransaction` and `ConcurrentMap.removeIfTransaction` for predicates that return `Transaction Bool`. Compose `AtomicVar.peekSTM` and `AtomicVar.modifyWithResultSTM` directly in those predicates. Use `Transaction.run` when a composed operation needs its own atomic commit. Raw `STM` operations cannot be mixed into a `Transaction`; keep those operations behind the owning framework concurrency module.

### Verify

Compile affected application code against the updated `nhcore` package. Run its tests for concurrent map replacement, conditional removal, and `AtomicVar` updates.

## Agent prompt

```text
Find application imports of AtomicVar.Transaction and callbacks that combine AtomicVar.peekSTM or AtomicVar.modifyWithResultSTM with raw STM. Import Transaction from the Transaction module. Pass typed callbacks to ConcurrentMap.getOrInsertIfTransaction or ConcurrentMap.removeIfTransaction. Move unrelated raw STM callbacks into the concurrency module that owns their shared state. Replace standalone raw STM execution with Transaction.run only when the code is an owned Transaction. Preserve each callback's decision and all atomic updates. Compile the application against the updated nhcore package and run its concurrent map and AtomicVar behavior tests. Report changed files and results.
```
