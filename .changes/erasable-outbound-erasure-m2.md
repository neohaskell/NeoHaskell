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

Apps that construct the exported `EventStore` record, including stores returned by a custom `EventStoreConfig` instance, must add all four fields introduced since 0.13.0. Existing `insert` implementations also need to reject protected values if the store cannot protect them. For a store without protected-value support, follow `Service.EventStore.Simple`, which also backs `Service.EventStore.InMemory`:

- `insertWithErasableKeys` takes new keys and an insertion payload. Reject a nonempty key array with `InsertionError (InsertionFailed "Erasable values require the PostgreSQL event store")`. With an empty key array, call the existing insertion implementation only after checking every event for protected values. Apply the same check to `insert` itself. Never store a plaintext fallback.
- `renderProtectedJson` takes a JSON value. Use the public JSON check below. Return ordinary JSON unchanged with `Task.yield`. Return `StorageFailure` whenever the check finds a protected marker, including a malformed envelope.
- `readErasableKey` takes a reference. Set `readErasableKey = \_ -> Task.throw (StorageFailure "Protected-value storage is unsupported by this event store")`.
- `eraseErasableKey` takes an `ErasureTarget`. Set `eraseErasableKey = \_ -> Task.throw (StorageFailure "Protected-value erasure is unsupported by this event store")`.

Inspect the JSON tree with the public `Json.parts` API. Reject any object with a `$erasable` key, regardless of its value or the other fields. Recurse through all object values and array elements. This also rejects malformed envelopes without decoding private framework types. For example, add this helper to the custom store's module:

```haskell
containsProtectedValue :: Json.Value -> Bool
containsProtectedValue value =
	case Json.parts value of
		Json.ObjectParts pairs ->
			pairs |> Array.any (\(key, nested) ->
				key == "$erasable" || containsProtectedValue nested)
		Json.ArrayParts values -> values |> Array.any containsProtectedValue
		Json.Scalar _ -> False
```

For `InsertionPayload Json.Value`, check every `insertion.event` with this helper. For a custom event type with a `Json.ToJSON` instance, check `Json.encode insertion.event` instead. Use the JSON representation that the store actually persists if it has a custom encoder. Keep the original payload for ordinary insertion. The helper takes JSON values, so adapt its call site to the store's event type and encoding.

Add `import Core` and qualified imports for `Array`, `Json`, and `Task` when using these helpers. Import `Error (StorageFailure, InsertionError)` from `Service.EventStore` and `InsertionFailure (InsertionFailed)` from `Service.Event`. Do not omit any field. Builds with `-Wall -Werror`, including this repository, reject incomplete record constructors. A store that supports protected values must implement all four operations. Its erasure operation has the one-reference `ErasureTarget -> Task Error ErasureResult` contract. Return `UnknownReference` only for a missing reference, `AlreadyErased` only for a known tombstone, and an error for scope denial or storage failure.

Existing protected keys written before this change remain readable but have no application scope. The new scoped deletion action will refuse those keys. A data owner must assess and migrate legacy keys under a separately reviewed process before using this action on them. Do not assign a scope to a shared key table without first proving which application owns each key.

The application must authorize requests and retain pending work. Register an outbound handler using `Integration.Erasable.Erase`, then record its result in an application command. The framework does not replay a lost result automatically. The maintained `OwnedProtectedEntity` testbed example shows explicit retry after a restart.

### Verify

Build each application and exercise all four new fields on every custom store. For an unsupported store, ordinary inserts and unprotected JSON must still work. Protected or malformed values, including markers nested in objects and arrays, must fail through both insertion paths and JSON rendering. For custom event types, exercise the actual event encoder before the check. Nonempty new-key arrays must fail even with an ordinary payload. Key reads and erasure must return `StorageFailure`. In this repository, run `nix develop --command cabal build lib:nhcore lib:nhtestbed` to check application wiring. With PostgreSQL running and ports 8080, 8081, and 8181 free, run `nix develop --command .agents/skills/neohaskell-test/scripts/run-testbed --strict`. The erasure scenario must show one `Erased` value, one `Available` value, a pending request after a rejected result, and a completed request after retry.

## Agent prompt

```text
1. Find every application-owned PostgresEventStore record constructor and add a stable, nonempty erasableApplicationScope value unique to the application. For constructors based on def, set the field before using outbound erasure.

2. Find every custom EventStore record constructor, including those returned by EventStoreConfig instances. Add all four fields introduced since 0.13.0: insertWithErasableKeys, renderProtectedJson, readErasableKey, and eraseErasableKey.

3. For a store without protected-value support, follow Service.EventStore.Simple, which backs InMemory. Implement insertWithErasableKeys so a nonempty new-key array returns InsertionError (InsertionFailed "Erasable values require the PostgreSQL event store"). For an empty array, delegate to the existing insertion implementation only after rejecting protected values in every event. Add the same rejection to insert itself. Use only public APIs. Write a recursive containsProtectedValue :: Json.Value -> Bool helper with Json.parts. In Json.ObjectParts pairs, use Array.any to reject a pair when its key equals "$erasable" or its value recursively contains that key. In Json.ArrayParts values, use Array.any containsProtectedValue. In Json.Scalar _, return False. Reject every "$erasable" object, including malformed envelopes, without decoding private framework types. For InsertionPayload Json.Value, check each insertion.event. For custom event types with Json.ToJSON, check Json.encode insertion.event. If the store has a custom encoder, inspect the JSON representation it actually persists. Keep the original payload for ordinary insertion. Adapt the helper's call site to the store's event type and encoding. Implement renderProtectedJson to return ordinary JSON unchanged with Task.yield and to return StorageFailure for protected values or malformed protected envelopes. Set readErasableKey = \_ -> Task.throw (StorageFailure "Protected-value storage is unsupported by this event store") and eraseErasableKey = \_ -> Task.throw (StorageFailure "Protected-value erasure is unsupported by this event store"). Add import Core and qualified imports for Array, Json, and Task as needed. Import Error (StorageFailure, InsertionError) from Service.EventStore and InsertionFailure (InsertionFailed) from Service.Event. Never store a plaintext fallback or omit a field. Builds with -Wall -Werror reject incomplete record constructors.

4. Stores that support protected values must implement all four operations. A supporting store must delete only the named reference in its application, entity, and stream scope; distinguish a known tombstone from an unknown reference; and return an error for scope denial or storage failure. Preserve existing connection, OAuth, and file-upload settings. If the application needs erasure, put owner authorization and reference discovery in its commands and queries, use Integration.Erasable.Erase for one reference per request, and record completion or failure in its own entity. Keep pending requests until a result command commits and add an explicit retry path for a lost result. Do not assign a scope to legacy keys without proving their application ownership.

5. Build the application and exercise every custom store's four new fields. For unsupported stores, verify ordinary inserts and unchanged unprotected JSON, rejection of protected and malformed values nested in objects and arrays through both insertion paths and JSON rendering, rejection of nonempty key arrays, and StorageFailure from key reads and erasure. For custom event types, exercise the actual event encoder before the check. Run protected-value request, restart, retry, and authorization tests for applications that support erasure. Report every changed file, test result, and legacy-key ownership that remains unresolved.
```
