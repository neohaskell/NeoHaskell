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

Existing protected keys written before this change remain readable but have no application scope. The new scoped deletion action will refuse those keys. A data owner must assess and migrate legacy keys under a separately reviewed process before using this action on them. Do not assign a scope to a shared key table without first proving which application owns each key.

The application must authorize requests and retain pending work. Register an outbound handler using `Integration.Erasable.Erase`, then record its result in an application command. The framework does not replay a lost result automatically. The maintained `OwnedProtectedEntity` testbed example shows explicit retry after a restart.

### Verify

Run `nix develop --command cabal build lib:nhcore lib:nhtestbed` to check application wiring. With PostgreSQL running and ports 8080, 8081, and 8181 free, run `nix develop --command .agents/skills/neohaskell-test/scripts/run-testbed --strict`. The erasure scenario must show one `Erased` value, one `Available` value, a pending request after a rejected result, and a completed request after retry.

## Agent prompt

```text
Find every application-owned PostgresEventStore record constructor and add a stable, nonempty erasableApplicationScope value unique to the application. For constructors based on def, set the field before using outbound erasure. Preserve existing connection, OAuth, and file-upload settings. If the application needs erasure, put owner authorization and reference discovery in its commands and queries, use Integration.Erasable.Erase for one reference per request, and record completion or failure in its own entity. Keep pending requests until a result command commits and add an explicit retry path for a lost result. Do not assign a scope to legacy keys without proving their application ownership. Build the application and run its protected-value request, restart, retry, and authorization tests. Report every changed file, test result, and legacy-key ownership that remains unresolved.
```
