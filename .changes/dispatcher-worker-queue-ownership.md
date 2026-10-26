---
group: platform
component: Framework
impact: breaking
category: Breaking changes
---

## Summary

Integration dispatch preserves atomic worker admission and drain during reaping and shutdown through a dedicated queue control. The constructor record change affects the framework builder and repository tests; application setup continues through `Application.withDispatcherConfig`. Event processing and `integration_result` logs keep their existing behavior. The temporary raw STM predicate operations `ConcurrentMap.getOrInsertIfM` and `ConcurrentMap.removeIfM` are removed in favor of typed transactions.

## Migration

Replace `ConcurrentMap.getOrInsertIfM` with `ConcurrentMap.getOrInsertIfTransaction` and `ConcurrentMap.removeIfM` with `ConcurrentMap.removeIfTransaction`. Predicates that read or update an `AtomicVar` can compose `AtomicVar.peekSTM` and `AtomicVar.modifyWithResultSTM` in `Transaction`. Move predicates using other raw STM operations into the concurrency module that owns their shared state.

### Verify

Compile affected code against the updated `nhcore` package. Run tests that cover map replacement and conditional removal while worker status changes.

## Agent prompt

```text
Find getOrInsertIfM and removeIfM call sites. Replace them with getOrInsertIfTransaction and removeIfTransaction using callbacks that return Transaction Bool. Keep map lookup, queue status changes, and removal in one transaction. For callbacks that use unrelated raw STM operations, move the coordinated operation into its owning concurrency module. Compile against the updated nhcore package and run concurrent replacement and conditional removal tests. Report changed files and results.
```
