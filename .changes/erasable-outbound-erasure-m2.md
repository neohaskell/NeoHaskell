---
group: platform
component: Framework
impact: breaking
category: Breaking changes
---

## Summary

An application can now request deletion of one protected value through `Integration.Erasable.Erase`. The application checks who owns the record, records a pending request, and records completion or failure when the outbound action returns. Repeating a deletion reports `AlreadyErased`, so an application can finish a request after a restart without restoring the key. The remaining protected values stay available.

## Migration

Apps that build `PostgresEventStore` with a record constructor must add `erasableApplicationScope` with a stable, nonempty identifier for that application. For example, add `erasableApplicationScope = "my-application"` to the existing configuration record. Apps using `def` can set the field with a record update before creating protected values they intend to erase. The event store refuses outbound deletion when the scope is empty.

Apps that construct the exported `EventStore` record, including stores returned by a custom `EventStoreConfig` instance, must add its new `eraseErasableKey` field. For a store without protected-value erasure, set `eraseErasableKey = \_ -> Task.throw (StorageFailure "Protected-value erasure is unsupported by this event store")`. Add `import Task qualified` and `import Service.EventStore (Error (StorageFailure))` if those imports are absent. Do not omit the field. The current compiler rejects a custom record constructor that lacks it. A store that supports erasure must implement the one-reference `ErasureTarget -> Task Error ErasureResult` contract. Return `UnknownReference` only for a missing reference, `AlreadyErased` only for a known tombstone, and an error for scope denial or storage failure.

Existing protected keys written before this change remain readable but have no application scope. The new scoped deletion action will refuse those keys. A data owner must assess and migrate legacy keys under a separately reviewed process before using this action on them. Do not assign a scope to a shared key table without first proving which application owns each key.

The application must authorize requests and retain pending work. Register an outbound handler using `Integration.Erasable.Erase`, then record its result in an application command. The framework does not replay a lost result automatically. The maintained `OwnedProtectedEntity` testbed example shows explicit retry after a restart.

### Verify

Build each application and call a custom store's `eraseErasableKey` field in a test. An unsupported store must return `StorageFailure`. In this repository, run `nix develop --command cabal build lib:nhcore lib:nhtestbed` to check application wiring. With PostgreSQL running and ports 8080, 8081, and 8181 free, run `nix develop --command .agents/skills/neohaskell-test/scripts/run-testbed --strict`. The erasure scenario must show one `Erased` value, one `Available` value, a pending request after a rejected result, and a completed request after retry.

## Agent prompt

```text
Find every application-owned PostgresEventStore record constructor and add a stable, nonempty erasableApplicationScope value unique to the application. For constructors based on def, set the field before using outbound erasure. Find every custom EventStore record constructor, including those returned by EventStoreConfig instances, and add eraseErasableKey. If that store cannot erase protected values, use eraseErasableKey = \_ -> Task.throw (StorageFailure "Protected-value erasure is unsupported by this event store"). Add import Task qualified and import Service.EventStore (Error (StorageFailure)) if absent. The current compiler rejects a constructor with this field missing. A supporting store must delete only the named reference in its application, entity, and stream scope; distinguish a known tombstone from an unknown reference; and return an error for scope denial or storage failure. Preserve existing connection, OAuth, and file-upload settings. If the application needs erasure, put owner authorization and reference discovery in its commands and queries, use Integration.Erasable.Erase for one reference per request, and record completion or failure in its own entity. Keep pending requests until a result command commits and add an explicit retry path for a lost result. Do not assign a scope to legacy keys without proving their application ownership. Build the application and execute its custom store's erasure field. Run protected-value request, restart, retry, and authorization tests for applications that support erasure. Report every changed file, test result, and legacy-key ownership that remains unresolved.
```
