# Changelog

## 0.15.0 — 2026-10-30

### Breaking changes

- **Framework**: Event Model Slices now require implemented components. A declared command, query, entity, or event payload without its required implementation fails compilation. Applications cannot describe future components with `plannedEntity`, `plannedCommand`, `plannedProduces`, `plannedQuery`, `plannedReadsFrom`, or `plannedOnEvent`. ([change](https://github.com/neohaskell/NeoHaskell/commit/0ca306213afe31b5f23b3733de06fc8c1b76f541))

### Added

- **Framework**: An application can now describe its event flow in source-local Features. Export a Feature action beside its commands, queries, and integrations, then compose those actions in a named Event Model. The compiler checks typed event sources and command targets, and model evaluation checks each Slice has one primary shape. The description does not start workers yet. Existing application registration remains in place. ([change](https://github.com/neohaskell/NeoHaskell/commit/522a94bc2fe8e3187eb03a940231561656d56521))
- **Framework**: An Event Model state-change or state-view Slice can now keep direct Given-When-Therefore rules beside its implemented command or query. Rules hold concrete historical events, a typed command or incoming event, and an exact command or query outcome. A rule can request typed values and reuse them in the same block. The runner that generates and executes cases will follow separately; applications should keep their existing behavior tests. ([change](https://github.com/neohaskell/NeoHaskell/commit/ce5df894f75a80532a89f029dd8f15f72d177909))

### Fixed

- **Framework**: An outbound integration now reports a failed result when its callback command is rejected, fails, is unauthorized, or has no handler. Callback failure text stays out of dispatcher errors and logs. An application still decides when and how to retry a pending request. ([change](https://github.com/neohaskell/NeoHaskell/commit/829928e19db7d5e00ee9d1e98818af420cc73d32))
- **Framework**: An outbound integration now reports failure if its command handler returns without sending a result. Your application can keep an erasure request pending and retry it explicitly instead of treating the missing result as completion. No application code change is required. ([change](https://github.com/neohaskell/NeoHaskell/commit/23231da55d8e95bbe9e3084b2f4f768ebc80149c))

### Migration from 0.14.0

#### Framework

If your application used a `planned*` builder, implement that component before adding it to a Feature. Replace `plannedEntity` with `entity`, `plannedCommand` with `command`, `plannedProduces` with `produces`, `plannedQuery` with `query`, `plannedReadsFrom` with `readsFrom`, and `plannedOnEvent` with `onEvent`. Define the matching `Entity`, `Command`, `Query`, `QueryOf`, and event payload relationships. Generate each entity Event ADT with a separate `deriveEntityEvents` call. Keep the existing `Application` registrations until Event Model runtime wiring is available.

### Verify

Compile the application and run the command, query, and integration tests for every declared Slice. In this repository, run `nix develop --command python3 core/test/Service/EventModel/check_compiler_rejections.py` and `nix develop --command .agents/skills/neohaskell-test/scripts/run-testbed --strict`.

<details>
<summary>Copy this prompt to your coding agent</summary>

```text
Migrate Framework from NeoHaskell 0.14.0 to 0.15.0.
Find every plannedEntity, plannedCommand, plannedProduces, plannedQuery, plannedReadsFrom, plannedOnEvent, and sliceIsPlanned use in my NeoHaskell application. Implement the declared entity, command, query, and payload behavior before replacing each planned builder with entity, command, produces, query, readsFrom, or onEvent. Generate event ADTs separately with deriveEntityEvents, and preserve the existing Application registrations until Event Model runtime wiring is available. Remove any topology-only Slice whose behavior is not being implemented. Compile the app, run its command, query, and integration tests, and report the changed files, results, and any missing implementation that still blocks compilation.
```

</details>

[Compare changes](https://github.com/neohaskell/NeoHaskell/compare/neo-v0.14.0...neo-v0.15.0)

## 0.14.0 — 2026-10-30

### Breaking changes

- **Framework**: An application can now request deletion of one protected value through `Integration.Erasable.Erase`. The application checks who owns the record, records a pending request, and records completion or failure when the outbound action returns. Repeating a deletion reports `AlreadyErased`, so an application can finish a request after a restart without restoring the key. The remaining protected values stay available. ([change](https://github.com/neohaskell/NeoHaskell/commit/e3142bb358ab331e0a0efb539bf1fb8feeca1dbb))

### Added

- **Framework**: Apps can create protected values in commands with `Erasable.new` and carry them through events and queries. The PostgreSQL event store encrypts these values before storing events and keeps their keys separately. Apps can request deletion through `Integration.Erasable.Erase`, with authorization and retry tracking in application code. Follow the protected-value erasure migration when updating custom event stores or configuring PostgreSQL application scopes. ([change](https://github.com/neohaskell/NeoHaskell/commit/c2dc34b2590cffd8a5bc291d614b0491ba5ceb29))
- **Framework**: An inbound worker can now emit several command types, including commands for different entities. List each command and its target in a typed worker, then register the handler with `Application.withInboundHandler`. The compiler rejects undeclared commands and wrong targets. Startup reports a missing receiving service before the worker runs. Existing `Integration.inbound`, `Timer.every`, and `Application.withInbound` calls keep working. ([change](https://github.com/neohaskell/NeoHaskell/commit/9df0280ccd34a21c278892183e388e763d165eaa))

### Fixed

- **Framework**: Records with `DateTime` or `Maybe DateTime` fields can now derive `ToSchema`. Timestamp fields appear as strings in OpenAPI and JSON Schema, matching their JSON encoding. Applications that defined an orphan `ToSchema DateTime` instance as a workaround must remove that instance to avoid a duplicate-instance error after updating NeoHaskell. ([change](https://github.com/neohaskell/NeoHaskell/commit/6f534e067dc61140796e46d3c32e6c144b9f30d4))
- **Framework**: Web commands with acronym names now use the expected kebab-case URL. For example, `SaveIBAN` responds at `POST /commands/save-iban`, and OpenAPI lists the same path. Internal dispatch and MCP command names retain their original spelling.

If two different command names produce the same Web URL, service registration rejects the duplicate instead of choosing one command. Rename one of the commands before starting the service. ([change](https://github.com/neohaskell/NeoHaskell/commit/3c05e3d83cdb6d2579b653694918961154d59391))
- **Framework**: `actual |> shouldContain expected` now checks whether the actual array contains the expected subarray. `actual |> shouldNotContain expected` checks whether the actual array does not contain it. Both matchers previously checked the arrays in the opposite direction.

If you swapped the arguments to work around this bug, put the expected subarray first again. For example, change `expected |> shouldContain actual` to `actual |> shouldContain expected`. Run your tests to confirm that their assertions still check the intended array. ([change](https://github.com/neohaskell/NeoHaskell/commit/e07c3910cdf69baf16543bebf7b9338dc98b98e3))
- **Framework**: The maintained erasure example now checks that an erased value stays erased during concurrent owner requests, stale writes, and restart within one application instance. Its runner can use private loopback ports, and its crash check pauses only the selected local request. Application authors can run the example to check their erasure flow. The framework API and production erasure behavior are unchanged, so existing applications need no migration. ([change](https://github.com/neohaskell/NeoHaskell/commit/1abd28799061797b7dbf0d1e22ebcec7bbddd0bf))
- **Framework**: The maintained protected-erasure example now shows a case status that remains after erasure and is reconstructed from events after restart. Its automated checks assert the status alongside the erased value, explicit retry, and application completion. Application authors can use the example to test their own retention choices. The framework erasure API and behavior are unchanged, so existing applications need no migration. ([change](https://github.com/neohaskell/NeoHaskell/commit/34af414272051e3e19ac69dddf587308dbd90778))
- **Framework**: If a stored event is unreadable, query replay reports failure through `/ready` instead of marking an incomplete query ready. An affected HTTP query returns a generic 500 response while its projection reports rebuilding or failed. A query that reads a partial page during rebuilding also returns 500 if replay becomes ready before the response. Applications need no code change. The maintained testbed checks missing keys, malformed envelopes, and an unavailable key table without returning protected values. ([change](https://github.com/neohaskell/NeoHaskell/commit/b878164011e2177543c02b7d776a88d785134301))
- **Framework**: The PostgreSQL event store now rejects a retry that reuses a protected event's ID with a different payload, even when the retry omits the protected value. It also rejects the rest of that retry batch. Existing protected data and its erasure state stay intact. Applications need no code change for normal event writes. ([change](https://github.com/neohaskell/NeoHaskell/commit/708848a109251d6665ced9324e90c2c6dcf60e33))

### Migration from 0.13.0

#### Framework

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

<details>
<summary>Copy this prompt to your coding agent</summary>

```text
Migrate Framework from NeoHaskell 0.13.0 to 0.14.0.
1. Find every application-owned PostgresEventStore record constructor and add a stable, nonempty erasableApplicationScope value unique to the application. For constructors based on def, set the field before using outbound erasure.

2. Find every custom EventStore record constructor, including those returned by EventStoreConfig instances. Add all four fields introduced since 0.13.0: insertWithErasableKeys, renderProtectedJson, readErasableKey, and eraseErasableKey.

3. For a store without protected-value support, follow Service.EventStore.Simple, which backs InMemory. Implement insertWithErasableKeys so a nonempty new-key array returns InsertionError (InsertionFailed "Erasable values require the PostgreSQL event store"). For an empty array, delegate to the existing insertion implementation only after rejecting protected values in every event. Add the same rejection to insert itself. Use only public APIs. Write a recursive containsProtectedValue :: Json.Value -> Bool helper with Json.parts. In Json.ObjectParts pairs, use Array.any to reject a pair when its key equals "$erasable" or its value recursively contains that key. In Json.ArrayParts values, use Array.any containsProtectedValue. In Json.Scalar _, return False. Reject every "$erasable" object, including malformed envelopes, without decoding private framework types. For InsertionPayload Json.Value, check each insertion.event. For custom event types with Json.ToJSON, check Json.encode insertion.event. If the store has a custom encoder, inspect the JSON representation it actually persists. Keep the original payload for ordinary insertion. Adapt the helper's call site to the store's event type and encoding. Implement renderProtectedJson to return ordinary JSON unchanged with Task.yield and to return StorageFailure for protected values or malformed protected envelopes. Set readErasableKey = \_ -> Task.throw (StorageFailure "Protected-value storage is unsupported by this event store") and eraseErasableKey = \_ -> Task.throw (StorageFailure "Protected-value erasure is unsupported by this event store"). Add import Core and qualified imports for Array, Json, and Task as needed. Import Error (StorageFailure, InsertionError) from Service.EventStore and InsertionFailure (InsertionFailed) from Service.Event. Never store a plaintext fallback or omit a field. Builds with -Wall -Werror reject incomplete record constructors.

4. Stores that support protected values must implement all four operations. A supporting store must delete only the named reference in its application, entity, and stream scope; distinguish a known tombstone from an unknown reference; and return an error for scope denial or storage failure. Preserve existing connection, OAuth, and file-upload settings. If the application needs erasure, put owner authorization and reference discovery in its commands and queries, use Integration.Erasable.Erase for one reference per request, and record completion or failure in its own entity. Keep pending requests until a result command commits and add an explicit retry path for a lost result. Do not assign a scope to legacy keys without proving their application ownership.

5. Build the application and exercise every custom store's four new fields. For unsupported stores, verify ordinary inserts and unchanged unprotected JSON, rejection of protected and malformed values nested in objects and arrays through both insertion paths and JSON rendering, rejection of nonempty key arrays, and StorageFailure from key reads and erasure. For custom event types, exercise the actual event encoder before the check. Run protected-value request, restart, retry, and authorization tests for applications that support erasure. Report every changed file, test result, and legacy-key ownership that remains unresolved.
```

</details>

[Compare changes](https://github.com/neohaskell/NeoHaskell/compare/neo-v0.13.0...neo-v0.14.0)

## 0.13.0 — 2026-10-28

### Breaking changes

- **Framework**: Outbound handlers can now receive one event payload. The runtime skips the handler
for other event types. Existing handlers that take the full event type need the
explicit `outboundIntegrationFullEvent` marker to keep their current behavior. ([change](https://github.com/neohaskell/NeoHaskell/commit/e411f0e0ea2ec504065267c987c98c3c0e5460aa))

### Added

- **Framework**: New applications can list an entity's event payload types in `deriveEntityEvents`.
The marker creates the parent event type and lets commands emit a single payload
type without writing its wrapper constructor. Existing event types and stored
events are unchanged. An application needs no migration to keep using a handwritten
event type. ([change](https://github.com/neohaskell/NeoHaskell/commit/b6bf53e813498ef4c2c53e5500c9c14f4dd3dbb2))

### Fixed

- **Framework**: JSON text decoding now preserves names and messages with accented characters, ñ, Japanese text, and emoji. Existing apps need no code changes. For example, decoding `"niño"` now returns `niño` instead of an error. ([change](https://github.com/neohaskell/NeoHaskell/commit/a8ca484045e9f263c4402746fe560bfdb73ce95b))

### Migration from 0.12.0

#### Framework

Find each `outboundIntegration ''Handler` or `deriveOutboundIntegration ''Handler`
whose `handleEvent` second argument is the entity's full event type. Replace
the marker with `outboundIntegrationFullEvent ''Handler` and import that name
from `Service.OutboundIntegration.TH`. The handler body and stored events stay
the same. To narrow the handler, give it one concrete payload argument, provide
an `EventVariantOf` mapping from the stored event type, and keep
`outboundIntegration ''Handler`.

### Verify

Run `neo build` and `neo test --ci` in the application. Exercise one matching
event and one unrelated event. A payload handler runs only for the matching
event. A full-event handler keeps its previous behavior.

<details>
<summary>Copy this prompt to your coding agent</summary>

```text
Migrate Framework from NeoHaskell 0.12.0 to 0.13.0.
Update my NeoHaskell outbound handlers for payload dispatch. Find each
`outboundIntegration ''Handler` or `deriveOutboundIntegration ''Handler` marker.
If `handleEvent` takes the entity's full event type as its second argument,
import `outboundIntegrationFullEvent` from `Service.OutboundIntegration.TH` and
use `outboundIntegrationFullEvent ''Handler` to preserve its behavior. Keep the
handler body and stored event type unchanged. For a handler that already takes
one concrete payload, keep `outboundIntegration ''Handler` and confirm that
`EventVariantOf` maps the stored event type to that payload. Run `neo build`
and `neo test --ci`. Report the changed files, test results, and any handler
whose payload mapping or intended event selection remains unclear.
```

</details>

[Compare changes](https://github.com/neohaskell/NeoHaskell/compare/neo-v0.12.0...neo-v0.13.0)

## 0.12.0 — 2026-10-26

### Breaking changes

- **Framework**: `AsyncTask.runWithCancellationEnabled` starts a child that can receive cancellation while its parent protects a handoff. It replaces `AsyncTask.runUnmasked`. Apps that call the old name need to update that call. ([change](https://github.com/neohaskell/NeoHaskell/commit/f70bf7f7b1854c0cc26b606d18a274182308225b))
- **Framework**: Framework code can compose `AtomicVar` reads and updates with conditional `ConcurrentMap` operations in one transaction. `Transaction` is now a distinct type, so application code that imports `AtomicVar.Transaction` or combines raw STM with `AtomicVar.peekSTM` must migrate. ([change](https://github.com/neohaskell/NeoHaskell/commit/9bc20f0b06821102dccb8e82c43860f0e7be48fd))
- **Framework**: Integration dispatch preserves atomic worker admission and drain during reaping and shutdown through a dedicated queue control. The constructor record change affects the framework builder and repository tests; application setup continues through `Application.withDispatcherConfig`. Event processing and `integration_result` logs keep their existing behavior. The temporary raw STM predicate operations `ConcurrentMap.getOrInsertIfM` and `ConcurrentMap.removeIfM` are removed in favor of typed transactions. ([change](https://github.com/neohaskell/NeoHaskell/commit/64c28a77047d47d151ffff30b3dd4925583de18d))

### Added

- **Framework**: `Http.Client` can now send PUT, HEAD, DELETE, POST and PATCH requests with a raw byte body and read the status code and headers of every response, including 4xx and 5xx. Use `Http.Client.sendSecure method request body` for HTTPS services and `Http.Client.Internal.sendRaw` for trusted loopback endpoints. `Http.getSecure` and `getRaw` keep working exactly as before.

HTTPS byte requests load the system certificate store while keeping certificate
and hostname checks and TLS 1.2 or later. Byte-level requests ignore environment
proxies and stop retaining response chunks as soon as the configured size limit
is exceeded, closing the response instead of waiting for the entire body.
The limit is not a fixed process-memory cap; network buffers can contain extra
bytes. The raw internal API remains caller-restricted, not a loopback validator.

Existing applications do not need code changes. To verify locally, run `./dev test "Http.Client" nhcore-test-core` and confirm the `sendRaw` and `sendSecure` examples pass. ([change](https://github.com/neohaskell/NeoHaskell/commit/c243b9787e361f54f6436dc07be06694c7198852))
- **Framework**: `Crypto` now offers the raw building blocks behind signatures: `Crypto.sha256` (32-byte digest), `Crypto.hmacSha256 key message` (raw HMAC that can be chained, any key length) and `Crypto.toHex` (lowercase hexadecimal). Use them when a protocol tells you exactly which bytes to hash or sign; keep using `Crypto.signWith` and `Crypto.verifyWith` for webhook secrets, which are unchanged.

Existing applications do not need code changes. To verify locally, run `./dev test "Crypto" nhcore-test-core` and confirm the `sha256`, `hmacSha256` and `toHex` examples pass. ([change](https://github.com/neohaskell/NeoHaskell/commit/c243b9787e361f54f6436dc07be06694c7198852))
- **Framework**: `Directory.withTemporaryFolder` gives a task a private, unique folder under the system temporary directory. It attempts to remove the folder when the task finishes, fails, or is cancelled. Use it for files that should last only as long as one operation. Existing apps need no changes. ([change](https://github.com/neohaskell/NeoHaskell/commit/f70bf7f7b1854c0cc26b606d18a274182308225b))
- **Framework**: `Task.withCancellationBlocked` lets a cleanup task finish an ownership handoff before cancellation can interrupt it, including while it waits. Use it only when the wait and cleanup can finish. A stalled task in this operation cannot be cancelled. `AsyncTask.isWaitingForExceptionDelivery` exposes a momentary status snapshot for cancellation tests. ([change](https://github.com/neohaskell/NeoHaskell/commit/f70bf7f7b1854c0cc26b606d18a274182308225b))
- **CLI**: New projects include shared instructions for Codex and Claude. The CLI keeps their framework guide matched to the exact NeoHaskell revision used by the project's build files, including a local framework source override. If the source resolves but the guide cannot be retrieved, the build continues and the instructions show that guidance is unavailable. Existing instructions and edited skills stay as the developer wrote them. ([change](https://github.com/neohaskell/NeoHaskell/commit/97d33f620444468070a0b470d52b128127c81187))
- **Framework**: Each framework revision includes an agent guide for core application patterns, reusable HTTP side effects, and external OAuth2 account connections. Developers and their agents can follow self-contained examples tied to the same revision as their app, without a framework checkout. ([change](https://github.com/neohaskell/NeoHaskell/commit/97d33f620444468070a0b470d52b128127c81187))
- **Framework**: Framework code can convert a `DateTime` to rounded Unix epoch milliseconds. Existing applications need no changes. ([change](https://github.com/neohaskell/NeoHaskell/commit/64c28a77047d47d151ffff30b3dd4925583de18d))

### Improved

- **Framework**: Command requests retain one safe result when decoding or handling fails, and cancellation still reaches blocked requests. Framework contributors can run the unconditional service-code lint check without an allowance for old findings. Public APIs and response formats are unchanged, so application projects need no migration. ([change](https://github.com/neohaskell/NeoHaskell/commit/f70bf7f7b1854c0cc26b606d18a274182308225b))

### Fixed

- **Framework**: A duplicate file upload no longer rewrites the existing stored blob when the blob store's existence check returns an error. The upload instead fails with "Failed to verify stored file content. Please retry." Backend details are logged at error level via `Log.critical`. A check that reports the blob present still reuses it; a check that reports it missing still restores the uploaded content. This does not change how the Local backend reports existence-check results.

A concurrent upload that loses a state-store conflict may already have written its own provisional blob. It attempts to delete that provisional blob; it does not rewrite the winner's existing blob when the winner's existence check returns an error. This check does not prevent another actor from removing a blob later.

No code or data changes are needed in applications. To verify locally, run `./.agents/skills/tdd/scripts/test-match "ContentDedup" nhcore-test-service` and confirm `dedup fails closed when the existence check errors` passes. ([change](https://github.com/neohaskell/NeoHaskell/commit/b71a18bb7a793189c4aeda5cab48f11b4b8baa29))
- **Framework**: The migration guidance for command event attribution now identifies the applications that need an edit. Applications using `Application.withService` and built-in transports need no source changes. Applications that construct a custom `ServiceRunner` for `Application.withServiceRunner` must update command handlers in the third result of `getEndpointsByTransport` to accept `Maybe EventMetadata` before the request context, request bytes, and response callback. The first two result maps are unchanged, and an empty third map needs no edit. This clarifies the already released 0.11.0 change for these supported application entrypoints.

For a handler that does not start another command, wrap its existing `EndpointHandler` as `\_sourceEvent requestContext body respond -> existingHandler requestContext body respond`. A handler that delegates to another dispatch handler must pass `sourceEvent` through. Keep the verified request context as the source of authorization. Stored event attribution does not grant permission to act as its user.

Code that imports the internal `Service.CommandExecutor.Core.execute` function directly must now group its event store, entity fetcher, and entity name into `ExecutionDependencies`. Pass `FromRequest requestContext` for a root command or `FromEvent requestContext sourceEventMetadata` for an event-triggered command. Handle the typed `CommandExecutionFailure` result instead of a `Text` error.

Build the application and run its command and integration tests. Execute a command and inspect its saved event. Check that `relatedUserSub` is the verified user and `correlationId` is the server request ID. For a command started by an event, check that `causationId` is the immediate source event ID. An absent source should not produce a `causationId`. ([change](https://github.com/neohaskell/NeoHaskell/commit/39d9e93f565aa4d7577e40497fde9b569256fec9))
- **Framework**: Secure URL validation checks both IPv4 and IPv6 DNS records under one timeout. When the timeout expires, it stops both lookups. Existing calls to `validateSecureUrlWithDns` keep the same type and validation errors. ([change](https://github.com/neohaskell/NeoHaskell/commit/64c28a77047d47d151ffff30b3dd4925583de18d))

### Migration from 0.11.0

#### Framework

Replace `AsyncTask.runUnmasked task` with `AsyncTask.runWithCancellationEnabled task`. The child still runs with cancellation enabled even when its parent starts it during a protected handoff. Keep ordinary `AsyncTask.run` calls when the child should inherit the parent's cancellation state.

### Verify

Search the app for `runUnmasked` and update each call. Run `neo build` and `neo test --ci` in the app. Both commands should finish successfully, and the search should find no old calls.

<details>
<summary>Copy this prompt to your coding agent</summary>

```text
Migrate Framework from NeoHaskell 0.11.0 to 0.12.0.
In this NeoHaskell app, replace every AsyncTask.runUnmasked call with AsyncTask.runWithCancellationEnabled. This is an API rename: the child still receives cancellation while its parent protects a handoff. Do not replace ordinary AsyncTask.run calls. Search for remaining runUnmasked uses, run neo build and neo test --ci, and report the changed files and command results. If a call relies on a different cancellation contract, report it instead of changing that call automatically.
```

</details>

#### Framework

Import `Transaction` from the `Transaction` module instead of `AtomicVar`. Use `ConcurrentMap.getOrInsertIfTransaction` and `ConcurrentMap.removeIfTransaction` for predicates that return `Transaction Bool`. Compose `AtomicVar.peekSTM` and `AtomicVar.modifyWithResultSTM` directly in those predicates. Use `Transaction.run` when a composed operation needs its own atomic commit. Raw `STM` operations cannot be mixed into a `Transaction`; keep those operations behind the owning framework concurrency module.

### Verify

Compile affected application code against the updated `nhcore` package. Run its tests for concurrent map replacement, conditional removal, and `AtomicVar` updates.

<details>
<summary>Copy this prompt to your coding agent</summary>

```text
Migrate Framework from NeoHaskell 0.11.0 to 0.12.0.
Find application imports of AtomicVar.Transaction and callbacks that combine AtomicVar.peekSTM or AtomicVar.modifyWithResultSTM with raw STM. Import Transaction from the Transaction module. Pass typed callbacks to ConcurrentMap.getOrInsertIfTransaction or ConcurrentMap.removeIfTransaction. Move unrelated raw STM callbacks into the concurrency module that owns their shared state. Replace standalone raw STM execution with Transaction.run only when the code is an owned Transaction. Preserve each callback's decision and all atomic updates. Compile the application against the updated nhcore package and run its concurrent map and AtomicVar behavior tests. Report changed files and results.
```

</details>

#### Framework

Replace `ConcurrentMap.getOrInsertIfM` with `ConcurrentMap.getOrInsertIfTransaction` and `ConcurrentMap.removeIfM` with `ConcurrentMap.removeIfTransaction`. Predicates that read or update an `AtomicVar` can compose `AtomicVar.peekSTM` and `AtomicVar.modifyWithResultSTM` in `Transaction`. Move predicates using other raw STM operations into the concurrency module that owns their shared state.

### Verify

Compile affected code against the updated `nhcore` package. Run tests that cover map replacement and conditional removal while worker status changes.

<details>
<summary>Copy this prompt to your coding agent</summary>

```text
Migrate Framework from NeoHaskell 0.11.0 to 0.12.0.
Find getOrInsertIfM and removeIfM call sites. Replace them with getOrInsertIfTransaction and removeIfTransaction using callbacks that return Transaction Bool. Keep map lookup, queue status changes, and removal in one transaction. For callbacks that use unrelated raw STM operations, move the coordinated operation into its owning concurrency module. Compile against the updated nhcore package and run concurrent replacement and conditional removal tests. Report changed files and results.
```

</details>

[Compare changes](https://github.com/neohaskell/NeoHaskell/compare/neo-v0.11.0...neo-v0.12.0)

## 0.11.0 — 2026-10-23

### Breaking changes

- **CLI**: NeoHaskell no longer publishes the standalone `neo-install` bootstrap. The
public `bootstrap.sh` URL stops working, and future Neo releases no longer
include installer assets. Future Neo releases also stop producing native CLI
assets for Linux ARM and Intel macOS. Historical releases and their assets
remain unchanged. New Neo CLI assets remain available for Linux x86-64 and
Apple silicon macOS. ([change](https://github.com/neohaskell/NeoHaskell/commit/991f800e99c44a72ca265511b1df004e2d32c69c))
- **Framework**: Events created by a command now record the verified user and a correlation ID. When an integration runs another command, its events retain that attribution and identify the event that caused them. JSON logs carry the same IDs through commands and integrations. Scheduled commands have a correlation ID without an invented user. Existing events stay unchanged. ([change](https://github.com/neohaskell/NeoHaskell/commit/deacffb43babc44cc12855f0897d7259901b3c33))

### Added

- **Integrations**: You can now pull the audio out of an uploaded video with the new
`Integration.Video.ExtractAudio` integration. Give it the video's file
reference and it returns mono 16 kHz WAV bytes (`audio/wav`). Upload those
bytes as a new file, save the returned `FileRef`, and pass that reference to
`Integration.Audio.Transcribe` with `mimeType = "audio/wav"`. By default,
extraction allows 180 seconds and stops at 25 MB; both limits are in the
config. It needs `ffmpeg` on the machine that runs your app (for example
`brew install ffmpeg`). If `ffmpeg` is missing, the action fails with a
message that tells you to install it. If the video itself cannot be
converted, your `onError` command receives the reason.

The dispatcher's `eventProcessingTimeoutMs` must cover video retrieval and
extraction. Raise it above the extraction timeout and expected retrieval time.
For example, `Just 200000` may fit a 180-second extraction, but it is not a
guarantee for every upload. The 30-second default can stop a slow operation
before your `onSuccess` or `onError` command is sent.
To keep the audio in a command, store it as Base64 text
(`result.audio |> Bytes.toBase64 |> Text.fromBytes`). The new
`Bytes.fromBase64` turns it back into the exact audio bytes
(`audioBase64 |> Text.toBytes |> Bytes.fromBase64`) and returns an error
instead of different bytes if the text is not valid Base64.

Existing applications do not need any changes. From a NeoHaskell checkout,
run `./.agents/skills/tdd/scripts/test-match 'Integration.Video' nhintegrations-test`
and confirm the extraction examples pass. They use a stand-in for `ffmpeg`,
so you do not need it installed to run them. ([change](https://github.com/neohaskell/NeoHaskell/commit/7bd77c0fd88a4b56091fc015388df85837e97d96))

### Fixed

- **Framework**: Concurrent commands that update the same entity now use the entity revision they actually read. If another command writes first, the stale command retries with fresh state instead of appending an outdated event. New-entity commands also prevent duplicate stream creation, while unconditional appends keep their existing behavior.

Existing applications do not need code or data migrations. From a NeoHaskell checkout, run `./.agents/skills/tdd/scripts/test-match 'Retry Logic' nhcore-test-service` and confirm the command-handler retry examples pass. PostgreSQL must be available for the PostgreSQL backend examples. ([change](https://github.com/neohaskell/NeoHaskell/commit/cab923c0b098fdb82ece539380719c0757ec9057))
- **Framework**: OpenAPI now preserves enum values exactly as declared in your schema.
For example, a derived enum that sends `"Pending"` in JSON now advertises
`"Pending"` in `/openapi.json`. Generated clients can match these values
without a casing mismatch. Your application's JSON format stays the same.
Regenerate clients from the updated specification to get the corrected values. ([change](https://github.com/neohaskell/NeoHaskell/commit/991f800e99c44a72ca265511b1df004e2d32c69c))
- **CLI**: A newly generated project now uses framework APIs that match the framework
revision pinned by its starter. Generate a project with `neo --ci new my-app`,
then run `neo --ci build` and `neo --ci test` from its directory. Both commands
should pass. Existing applications do not need edits for this fix. ([change](https://github.com/neohaskell/NeoHaskell/commit/36cd89bf60c19cfa89f2a20eca4716572c1e16f2))
- **CLI**: The Neo CLI now uses an updated TLS library that rejects malformed TLS 1.3 handshake messages sent at the wrong encryption level. This hardens HTTPS connections. Existing commands and configuration need no changes. ([change](https://github.com/neohaskell/NeoHaskell/commit/71893b6609b3ad190a6971360d226b85368c6d1f))

### Migration from 0.10.1

#### CLI

Remove scripts that call
`https://raw.githubusercontent.com/neohaskell/NeoHaskell/main/installer/scripts/bootstrap.sh`.
Existing copies of the old installer have no guarantee of working with future
Neo releases.

Download `neo-<target>` and `SHA256SUMS` from the same `neo-vX.Y.Z` release. The
new native targets are `x86_64-unknown-linux-gnu` and `aarch64-apple-darwin`.
New assets are no longer produced for `aarch64-unknown-linux-gnu` or
`x86_64-apple-darwin`; historical release assets remain unchanged. No
replacement installation method is defined here for those platforms. Compare
the downloaded asset's SHA-256 with its line in `SHA256SUMS`. From a NeoHaskell
checkout, the verified release helper checks the selected asset and installs
it atomically:

```sh
mkdir -p "$HOME/.local/bin"
./.agents/skills/neohaskell-release/scripts/neo-release install \
  "$HOME/Downloads/neo-vX.Y.Z" x86_64-unknown-linux-gnu \
  "$HOME/.local/bin/neo"
"$HOME/.local/bin/neo" --version
```

Use `aarch64-apple-darwin` on an Apple silicon Mac. The example invokes the
installed destination directly. Add `$HOME/.local/bin` to `PATH` to run `neo`
by name. Confirm the binary reports the version you downloaded. Nix setup for
building and running generated projects remains a separate prerequisite; use
the existing [Nix installation instructions](https://nixos.org/download).

### Verify

Confirm that the release directory contains the selected `neo-<target>` asset
and `SHA256SUMS`. Install it with the verified helper, then run the installed
destination's `--version` command and confirm it prints the selected release
version. Keep project build and run verification unchanged after installation.

<details>
<summary>Copy this prompt to your coding agent</summary>

```text
Migrate CLI from NeoHaskell 0.10.1 to 0.11.0.
Migrate uses of the retired NeoHaskell installer to direct Neo CLI release
assets. The public bootstrap path
https://raw.githubusercontent.com/neohaskell/NeoHaskell/main/installer/scripts/bootstrap.sh
and the standalone installer release assets are retired. New Neo CLI assets are
no longer produced for `aarch64-unknown-linux-gnu` or `x86_64-apple-darwin`.
Historical release assets remain unchanged. Existing installer binaries have
no guarantee of working with future Neo releases.

Find scripts and setup instructions that call the bootstrap URL. Replace that
step with a download of `neo-<target>` and `SHA256SUMS` from the same
`neo-vX.Y.Z` release. The supported targets are `x86_64-unknown-linux-gnu`
and `aarch64-apple-darwin`. Verify the selected asset against the manifest
with the platform checksum tool before placing it on PATH. The verified
repository helper is `.agents/skills/neohaskell-release/scripts/neo-release`;
its `install <dir> <target> <dest>` command verifies the bytes during atomic
installation. Preserve the existing external Nix setup for generated projects.

Use the following standalone example from a shell with `curl`, `awk`, and
either `sha256sum` or `shasum` installed. It downloads both files from the
same release, checks the selected asset, installs it, and prints its version.

~~~sh
set -eu
version=0.11.0
target=x86_64-unknown-linux-gnu
download_dir="$HOME/Downloads/neo-$version"
destination="$HOME/.local/bin/neo"
release_url="https://github.com/neohaskell/NeoHaskell/releases/download/neo-v$version"
mkdir -p "$download_dir" "$HOME/.local/bin"
curl -fL "$release_url/neo-$target" -o "$download_dir/neo-$target"
curl -fL "$release_url/SHA256SUMS" -o "$download_dir/SHA256SUMS"
expected=$(awk -v name="neo-$target" '$2 == name {print $1}' "$download_dir/SHA256SUMS")
test -n "$expected"
if command -v sha256sum >/dev/null 2>&1; then
  actual=$(sha256sum "$download_dir/neo-$target" | awk '{print $1}')
else
  actual=$(shasum -a 256 "$download_dir/neo-$target" | awk '{print $1}')
fi
test "$actual" = "$expected"
install -m 0755 "$download_dir/neo-$target" "$destination"
export PATH="$HOME/.local/bin:$PATH"
"$destination" --version
~~~

Use `aarch64-apple-darwin` for an Apple silicon Mac. Add `$HOME/.local/bin`
to your shell configuration to keep `neo` on `PATH` in new sessions. If a
consumer uses a retired native target, flag the need for an owner-selected
installation method instead of proposing an unverified replacement.

Run the installed destination's `--version` command and check that it reports
the chosen release. Preserve unrelated setup behavior. Report changed files,
commands and results, and any script whose owner must choose a different
installation method. Do not restore the bootstrap, publish assets, or assume
old installer binaries are compatible with future releases.
```

</details>

#### Framework

Applications built with `Application` and the built-in transports need no code changes. If your code calls `CommandExecutor.execute` directly, pass `Nothing` before the request context for a root command. Pass `Just sourceEvent.metadata` for a command triggered by an event.

If your code calls `ServiceRunner.getEndpointsByTransport` directly, its third result now contains `DispatchHandler` values. Each handler takes `Maybe EventMetadata` before the existing request context, command bytes, and response callback. Update any explicit map type and pass `Nothing` for autonomous work or `Just sourceEvent.metadata` for an event callback. Stored user attribution does not grant authorization.

### Verify

Build the application and run its command and integration tests. Execute a command and read its saved event. Check that `relatedUserSub` is the verified user and `correlationId` is the server request ID. For an event callback, check that `causationId` is the immediate source event ID.

<details>
<summary>Copy this prompt to your coding agent</summary>

```text
Migrate Framework from NeoHaskell 0.10.1 to 0.11.0.
Update this NeoHaskell application for event attribution. Find direct calls to CommandExecutor.execute. Add Nothing before the request context for root commands, or Just sourceEvent.metadata for commands triggered by an event. Find direct calls to ServiceRunner.getEndpointsByTransport. Change explicit types for its third result from Map Text EndpointHandler to Map Text DispatchHandler. Pass Nothing or Just sourceEvent.metadata to each dispatch handler before its existing arguments. Keep verified request claims and stored event attribution separate. Do not change application authorization rules. Build the application, run its command and integration tests, execute a command, and read its saved events. Report the changed files, test results, and any calls whose event source is unclear.
```

</details>

[Compare changes](https://github.com/neohaskell/NeoHaskell/compare/neo-v0.10.1...neo-v0.11.0)

## 0.10.1 — 2026-09-22

### Added

- **Framework**: Declare events, commands, entities, queries, and outbound integrations with
`deriveEvent`, `deriveCommand`, `deriveEntity`, `deriveQuery`, and
`deriveOutboundIntegration`, all available through `import Core`.

The new `deriveEntity ''CartEntity ''CartEvent` helper generates routine entity
instances and connects your `initialState`, `update`, and `getEventEntityId`
functions. Existing custom instances remain supported. The previous marker
names still work, so existing applications need no changes. ([change](https://github.com/neohaskell/NeoHaskell/commit/4ba756552d2b5ba20c734ba9ae29fa31dab3a11e))

### Fixed

- **CLI**: Applications can use the framework's declaration helpers without adding language
pragmas to their source files. Neo now enables the required deriving strategy in
the project settings it generates for application, library, and test components.

After updating Neo, run `neo build` and `neo test` from your application directory.
Neo refreshes the generated settings; no manual Cabal configuration is needed. ([change](https://github.com/neohaskell/NeoHaskell/commit/4ba756552d2b5ba20c734ba9ae29fa31dab3a11e))

[Compare changes](https://github.com/neohaskell/NeoHaskell/compare/neo-v0.10.0...neo-v0.10.1)

## 0.10.0 — 2026-09-15

### Breaking changes

- **CLI**: The Rust Neo CLI joins NeoHaskell, its integrations and starter under one
platform version. It creates a complete starter project, builds and tests it,
and includes the browser IDE. Projects made with the old 0.9 Haskell CLI need
a configuration and launcher update; `neo shell` is replaced by `nix develop`.
The Rust CLI and IDE already shipped separately as `neo-v0.1.x`: users with
that project layout can keep it while applying any framework API migrations. ([change](https://github.com/neohaskell/NeoHaskell/commit/1fcad50a1e945c24cfc20008219642bfeae8e173))
- **Framework**: Apps upgrading from 0.9.0 need to update event-store calls and several core
helpers. Text and array appends now take the value being extended last;
mutable variables run inside `Task`; durable-channel writes accept batches;
and JSON constraint names and subprocess error types have changed. The guide
below preserves existing ordering, error handling and event identities. ([change](https://github.com/neohaskell/NeoHaskell/commit/1fcad50a1e945c24cfc20008219642bfeae8e173))

### Added

- **Framework**: Build an app around its recorded history: define commands such as “add an item,”
  events such as “item added,” and queries that build the lists users see. The new
  service framework connects these parts, checks their types, and supplies the
  following capabilities. Existing 0.9 apps can adopt them after applying the
  core/event-store migration guide.

  - **Commands and saved state.** Declare entities, events, commands and queries
    with the framework's derivation helpers instead of writing their supporting
    instances by hand. Command execution checks access, rejects invalid decisions
    and retries conflicting updates. Entity fetching rebuilds state from events;
    optional snapshots avoid replaying the same old history on every fetch.
    `Uuid.generateV5` and `Decider.generateDeterministicUuid` let an app derive a
    repeatable ID from a namespace and a natural key, such as an external order ID.
    Verify accepted, rejected and repeated commands with the app's domain tests.

  - **Storage that survives restarts.** Use the PostgreSQL event store for shared
    database storage, or SimpleEventStore's optional JSONL files for local
    persistence. PostgreSQL subscriptions catch up after a listener reconnect;
    fixes prevent missing events during that catch-up and release subscription
    connections on unsubscribe. PostgreSQL settings include explicit pool sizes
    and TLS modes. Configure storage and restart a test instance to check that its
    history recovers and PostgreSQL subscriptions resume. Database event listeners
    require a direct,
    session-preserving connection.

  - **Lists, filters and pages.** Queries maintain read models: saved views of
    events used to answer requests. They can use memory or PostgreSQL storage.
    HTTP responses include `items`, `total`, `hasMore` and `effectiveLimit`, so a
    screen can request one page at a time. The default page contains at most 100
    items, with a maximum of 1,000 per request. Results and counts respect query
    authorization. NeoQL supplies field access and equality filtering. Test
    first/next/empty pages and access restrictions in your app before connecting
    the UI.

  - **Sign-in and access rules.** JWT authentication validates signed access
    tokens; OAuth2 support connects external accounts, stores tokens and refreshes
    them. Typed configuration loads fields from flags, environment variables and
    `.env` files, reports missing or invalid values, and supports secret fields.
    Commands can declare public or authenticated access. Multi-tenant commands
    and the `tenantOnly` query filter let apps separate each customer's data when
    explicitly configured. Test both allowed and denied requests, including users
    from different tenants; adding sign-in alone does not select your access rules.

  - **HTTP, command-line and agent access.** Expose service commands and queries
    through HTTP, an app-specific CLI, or MCP (the protocol coding assistants use
    to call tools). Internal commands can serve integrations without becoming
    public endpoints. Generated JSON schemas and OpenAPI descriptions reflect
    command inputs and paginated query responses. WebTransport supports CORS for
    browser clients. Select the transports your app needs and exercise their
    requests, including invalid inputs and denied access.

  - **Uploads and downloads.** Apps can validate uploaded file sizes and types,
    retain an opaque file reference, confirm or delete files, and control download
    access. Local blob storage keeps bytes; the optional PostgreSQL file-state
    store keeps lifecycle information. Deduplication reuses identical content for
    the same owner and restores a missing blob on re-upload. Configure both stores
    for your deployment, then test upload, authorized download, rejection and
    deletion with real files.

  - **Startup and diagnostics.** HTTP can start accepting health checks while
    queries rebuild from history. With readiness enabled, `/ready` stays at 503
    until the read models are usable; route traffic after it reaches 200.
    PostgreSQL query checkpoints support resuming rebuilds. Structured logging
    adds command and event context, and line-buffered output reaches container
    logs promptly. Follow the [deployment guide](https://github.com/neohaskell/NeoHaskell/blob/d2201cc555d55cc558d751ff67d04569a42eda18/website/src/content/docs/guides/deployment.mdx)
    for separate startup, liveness and readiness probes; test a cold restart,
    rather than only an already-warm process.

  - **Everyday data and concurrency helpers.** Fixed-point `Decimal` provides
    money-oriented arithmetic and formatting; `Crypto` signs and verifies
    HMAC-SHA256 messages with an opaque key type. Arrays gain trimming, searching,
    chunking and zipping helpers; Bytes gains binary conversions, slicing and
    Base64 support. Map and Maybe gain convenience operations, and `Text.escapeHtml`
    escapes text for HTML. Concurrent maps, streams and atomic variables support
    shared work; asynchronous tasks gain cancellation and racing. `Parser` and
    `Layout` help parse and format text, with comment/function syntax primitives.
    These primitives do not constitute a complete new language compiler. Adopt
    the helpers where needed and retain boundary tests for your data formats,
    rounding, authentication and cancellation behavior. ([change](https://github.com/neohaskell/NeoHaskell/commit/1fcad50a1e945c24cfc20008219642bfeae8e173))
- **IDE**: View and edit your app's event model in the browser with `neo ide`. The IDE is
  bundled into the Neo CLI, opens locally at `127.0.0.1:2323` by default, and
  saves the diagram in `event-model.json`. It can show commands, events, queries
  and their connections, validate references, and synchronize information from
  source files. This brings the IDE already available in the independent Neo
  CLI series into the coordinated NeoHaskell release.

  From the project root, run `neo ide` and open its printed URL. Review autosaved
  changes before committing the model. You can keep working in your usual source
  editor; adopting the diagram is optional.

  Use `neo validate` to check an existing model without opening the browser;
  expect exit code 0 for a valid file. `neo inspect domains` and
  `neo inspect wiring` show what Neo can discover from your source. When you
  intend to update the model from code, run `neo inspect sync` and review the
  resulting diff: that command writes to `event-model.json`. A missing or invalid
  model makes validation fail; it is not treated as a successful check. ([change](https://github.com/neohaskell/NeoHaskell/commit/1fcad50a1e945c24cfc20008219642bfeae8e173))
- **Integrations**: Connect app events to external services through the new `nhintegrations`
  package. Configure an integration's request and the commands to run on success
  or failure; the framework handles dispatch. These integrations are additions
  since 0.9.0, so there are no old integration imports to rename in a 0.9 app.

  - **HTTP and email:** call external HTTP endpoints with `Integration.Http`,
    send transactional email with `Integration.Brevo`, or use Azure Communication
    Services through `Integration.Acs`.
  - **AI conversations and tools:** use `Integration.OpenRouter` or
    `Integration.AzureAI` for chat completions. `Integration.Agent` lets a model
    call typed application commands as tools. Register only the commands and
    permissions you intend the agent to have.
  - **Documents and recordings:** `Integration.Pdf.ExtractText` extracts text
    from digital PDFs with `pdftotext`; `Integration.Ocr.Ai` extracts text from
    documents/images through a selected multimodal model. `Integration.Audio.Transcribe`
    supports WAV, MP3, M4A and OGG recordings. AI extraction/transcription sends
    content to the selected provider through OpenRouter. Configure the MIME type
    and model for the actual input; large-file chunking and streaming transcription
    are not supplied. For long transcription requests, make the integration
    dispatcher's timeout at least as long as the request timeout.
  - **Oura:** connect an Oura account with OAuth2 and access typed sleep, activity,
    readiness and other Oura API v2 data through `Integration.Oura`.

  Add `nhintegrations` to your app's dependencies, configure the chosen service's
  credentials and required local tools, and register its outbound handler. Use
  an isolated test account or a fake integration to check the success and failure
  commands before sending real emails or changing external data. Also test token
  refresh where applicable. Provider accounts and usage are separate from a
  Codex subscription. The release process itself requires no paid AI API. ([change](https://github.com/neohaskell/NeoHaskell/commit/1fcad50a1e945c24cfc20008219642bfeae8e173))

### Migration from 0.9.0

#### CLI

### Choose the path matching your project

If your app already has `neo.json` with `neo-version`, a `launcher/Launcher.hs`
and the Rust CLI layout, keep that layout. Updating the CLI does not
automatically change an existing app's framework pin. Compare its
`neo-version` with `neohaskell.source_revision` in this release's
`neo-compatibility.json`; updating that pin is a framework upgrade and needs
the relevant API migrations and tests.

For projects created with the **0.9 Haskell CLI**, use the following steps.
They preserve the app's behavior; a simple command-line program need not become
a web service.

### Prepare a reference project

Download the native binary for your OS/CPU from the
[0.10.0 release assets](https://github.com/neohaskell/NeoHaskell/releases/tag/neo-v0.10.0),
along with `SHA256SUMS` and `neo-compatibility.json`:

| Computer | Binary |
| --- | --- |
| Mac, Apple Silicon | `neo-aarch64-apple-darwin` |
| Mac, Intel | `neo-x86_64-apple-darwin` |
| Linux, ARM64 | `neo-aarch64-unknown-linux-gnu` |
| Linux, x86-64 | `neo-x86_64-unknown-linux-gnu` |

Run `shasum -a 256 <downloaded-file>` (or `sha256sum` on Linux) and compare its
hash with that file's entry in `SHA256SUMS`. Make the verified binary executable
with `chmod +x <downloaded-file>`, then install it as `neo` in a directory on
your PATH. Check `neo --version` reports 0.10.0 before migrating. With Nix and Git available, create a
reference in a separate temporary directory, using your app's existing package
name so generated names match:

```sh
neo --ci new my-app
```

Use `neo --ci new my-app --library` for a library; it has no executable launcher.
Replace `my-app` with your own package name. The starter is embedded in the
binary; resolving or building dependencies can still require network access.
Keep your original project and Git history, and bring over the needed scaffold
files on an upgrade branch rather than replacing the app with the example.

### Update configuration and build files

Keep `name`, `version`, `description`, `author` and `license`
from your existing `neo.json`; review each dependency as described below. The application's `version` remains its own
version. Add `neo-version` using the full 40-character revision generated by
the new CLI, and set `type` to `executable` or `library` as appropriate.

Dependency values now have meaning: the 0.9 generator used the package names
and ignored the values. For a Hackage dependency, use a `hackage:` key and a
supported npm-style version range, for example:

```json
"dependencies": {
  "hackage:aeson": ">=2.0.0 <3.0.0"
}
```

Choose the range your app needs; this example is not a required Aeson upgrade.
A bare key uses the NeoPackages registry. Git/local dependencies use values
such as `github:owner/repo#ref`, `git:https://host/repo.git#ref`, or
`file:../local-lib`. Preserve the selected source and intended versions; old
arbitrary values or Cabal-specific constraints are not automatically valid.

The old optional `overrideNeohaskell` setting is no longer read. Record why
it was used before removing it. For a normal upstream dependency, replace it
with the release's `neo-version`; if it pointed to a local checkout or custom
fork, resolve that build arrangement with the owner before running the
regeneration commands below. `neo-version` alone cannot represent a different
repository or local path; an unresolved override means migration is incomplete.

`neo build`, `neo test` and `neo run` regenerate `<name>.cabal`, `cabal.project`
and `flake.nix`. Put supported dependency settings in `neo.json`. Save any
custom flags, source directories, native dependencies or Nix overrides from
the old files and check whether the generated configuration represents them.
Unsupported custom build settings need a deliberate solution before migration
can be considered complete.

### Preserve the executable entrypoint and tests

The old scaffold called `<PascalName>.run :: Task Text ()` through
`.launcher/Main.hs`. The new executable entrypoint is
`launcher/Launcher.hs`, containing `module Main`; the generated Cabal file uses `launcher/` for the
executable and `src/` for library modules. The old `.launcher/` path is no longer
used. For a simple program, the new launcher can
continue to run the same task:

```haskell
module Main where

import Core
import MyApp qualified
import Task qualified

main :: IO ()
main = MyApp.run |> Task.runOrPanic
```

Replace `MyApp` with your existing module name. For an app adopting the service
framework, follow the new starter's `src/App.hs` / `app :: Application` and
`Application.run` setup instead. Preserve the app's actual startup behavior,
arguments and side effects. Do not copy the starter's service `src/App.hs` over
a simple app's existing source.

Keep application source in `src/`, merge existing tests into the new `tests/`
setup, and inspect `.envrc`, `.env.example`, Docker configuration and editor
settings before adopting them. Do not replace real configuration with starter
example values or commit secrets. The scaffold installs a Git lock hook;
`neo lock` helps protect existing event definitions from accidental edits.
Review `.locked-files` and the hook before recording intentional migrations.
Do not lock incompatible code and then skip the check just to make a build pass.

Replace scripts or editor tasks calling `neo shell` with `nix develop` in the
project directory. Optional `neo skills setup --dry-run --tool codex` previews
available agent guidance; installing skills is separate from migrating code.

### Verify

From the upgraded application root, run:

```sh
neo --ci build
neo --ci test
```

Expect a successful build and passing app tests. For an executable, also run
`neo --ci run` with development configuration and check its actual behavior:
for example, its expected terminal output or a real HTTP request. Stop the app
once the smoke check is complete. Libraries need their API tests, not `neo run`.

Confirm `neo.json`'s `neo-version`, the regenerated Nix source revision and
Cabal source tags agree with the intended release. Inspect the diff for lost
source, dependencies, test assertions and custom configuration. If the app has
an event model, `neo validate` should pass; do not treat a missing model as a
reason to create unrelated product features.

<details>
<summary>Copy this prompt to your coding agent</summary>

```text
Migrate CLI from NeoHaskell 0.9.0 to 0.10.0.
Upgrade this application's NeoHaskell tooling from the 0.9 Haskell CLI to the
0.10 Rust Neo CLI, preserving its behavior, Git history, sources, tests and
configuration. Work on a branch and inspect the current layout first.

If neo.json already has neo-version and the app already uses the Rust CLI
launcher/build layout (including users of neo-v0.1.x), keep that layout. A CLI
update does not change the existing framework pin. Compare it with the release's
neo-compatibility.json neohaskell.source_revision; apply framework migrations
and tests before intentionally changing it.

For an old 0.9 layout, create a reference project in a separate temporary
parent directory using `neo --ci new <existing-package-name>`; add --library
only for a library. Keep the original repository. Preserve neo.json name,
application version, description, author and license; add the
reference's immutable 40-character neo-version and the correct type. Review
all dependencies: 0.9 ignored their values. Current Hackage entries use
"hackage:name": "<npm-style-range>"; bare keys select NeoPackages. Git/local
values use git:<url>#ref, github:<owner>/<repo>#ref, or file:<path>. Convert old
values to preserve intended sources/versions, not just their spelling. The old
overrideNeohaskell field is no longer read: document any custom fork/local
checkout and get its build mapping resolved before removing it or running regeneration. Never substitute
main for the release's compatible revision.

Preserve src/ and tests. The executable now uses launcher/Launcher.hs with
module Main. It can retain the old module's `run :: Task Text ()` by calling
`MyApp.run |> Task.runOrPanic` (replace MyApp with the actual module). Do not
convert a simple program to a service unnecessarily. Apps adopting services can
follow the starter's App.app/Application.run pattern. Libraries have no launcher.
The CLI regenerates <name>.cabal, cabal.project and flake.nix; put supported
dependency configuration in neo.json and report any custom build settings that
cannot be represented. Review .envrc, environment examples, Docker/editor files,
.locked-files and Git hooks. Preserve actual environment values securely and
never commit secrets. Replace neo shell in scripts with nix develop.

Run neo --ci build and neo --ci test. For executables, run neo --ci run with
safe development configuration, verify the app's actual output or endpoint and
stop it afterward. For libraries, run API tests without neo run. Verify emitted
Nix/Cabal revisions agree with neo-version and the release compatibility file.
If event-model.json already exists, run neo validate. Report changed files,
commands/results, unresolved build overrides and behavior requiring the owner's
judgment. Do not delete tests, bypass lock checks to hide failures, deploy,
change production data or publish a release.
```

</details>

#### Framework

Apply only the sections matching APIs your app uses. Commit or back up the app
before starting, and test against a copy of any persistent data.

### Preserve the order of appended text and arrays

These calls still compile but produce the opposite order if left unchanged:

| 0.9 code | Replacement with the same result |
| --- | --- |
| `Text.append "butter" "fly"` | `"butter" \|> Text.append "fly"` |
| `Array.append left right` | `left \|> Array.append right` |

The first row must still produce `"butterfly"`. The second must put every
item from `left` before every item from `right`. For direct calls, reverse the
arguments instead: `Text.append suffix subject` or `Array.append right left`.
Generic `Collection.append` on arrays uses the same reversed argument order.
The `Appendable` `(++)` operator is unchanged. Review partial applications and
folds individually: a blanket swap can reverse
an entire accumulated list. Include empty inputs and distinct values in tests.

### Move mutable variables into Task

`Var.new`, `Var.get` and `Var.set` now return `Task error value`, replacing
`IO value`. Their names and argument order stay the same. Inside an existing
Task, remove an outer `Task.fromIO` around a Var operation:

```haskell
-- Before
counter <- Task.fromIO (Var.new 0)
-- After
counter <- Var.new 0
```

If the caller is an `IO` entrypoint, move the variable operations into a Task
and run it once at that boundary. A task that cannot fail can use
`Task.runNoErrors`; a fallible task needs the application's existing error
handling. Do not wrap a Task in `Task.fromIO`, discard its errors, or change
shared-variable synchronization while migrating.

### Write one-item batches to durable channels

For existing code that writes one value, keep that behavior by wrapping it
with `Array.wrap`:

| 0.9 code | Replacement |
| --- | --- |
| `DurableChannel.write value channel` | `DurableChannel.write (Array.wrap value) channel` |
| `DurableChannel.checkAndWrite predicate value channel` | `DurableChannel.checkAndWrite predicate (Array.wrap value) channel` |
| `DurableChannel.writeWithIndex makeValue channel` | `DurableChannel.writeWithIndex (\index -> Array.wrap (makeValue index)) channel` |

Reads still return individual values. `writeWithIndex` returns the batch's
starting index. Test write/read order, a predicate that rejects a write, and
index values; adopting larger batches is a separate behavior change.

### Rename JSON constraints and handle subprocess failures

Replace `Json.Decodable value` with `Json.FromJSON value` and
`Json.Encodable value` with `Json.ToJSON value`, including explicit import
lists. Existing JSON encoding/decoding functions retain their names. Keep
fixtures that check the shape of saved or exchanged JSON.

`Subprocess.open` and `Subprocess.openInherit` now return
`Task Subprocess.Error Completion`. If your caller uses `Task Text`, translate
the error at the call boundary, for example:

```haskell
completion <- Subprocess.open executable arguments directory |> Task.mapError toText
```

`executable` is Text, `arguments` is an Array of Text, and `directory` is a
Path. For an app-specific error type, map to its appropriate constructor. Check the
completion's `exitCode` as before: a process exiting unsuccessfully is different
from being unable to launch it (`ProcessError`). Test both a missing executable and a nonzero exit.

### Update direct EventStore users

The new event store saves typed event content, supports batches and streams
reads without loading the whole history at once. If you called the 0.9
`Service.EventStore` directly, update both writes and readers:

| 0.9 API | Current API / required decision |
| --- | --- |
| `EntityId` wrapping a UUID | `EntityName` wrapping text; choose a stable name for each entity kind |
| `StreamId` wrapping a UUID | Text-backed `StreamId`; `toStreamId oldUuid` preserves a UUID as text |
| `InsertionEvent` and `appendToStream` | `InsertionPayload`, `Insertion` and `store.insert` |
| `EventStore` | `EventStore eventType`; `InMemory.new` returns `EventStore Json.Value` inside its Task |
| Plain `Event` with top-level `id`, positions | `Event eventType`; read `metadata.eventId`, `metadata.localPosition`, `metadata.globalPosition` (positions are optional) |
| `Array Event` from reads | `Stream (ReadStreamMessage eventType)` or `Stream (ReadAllMessage eventType)` |
| Subscription callback `Task Error Unit` | Callback `Task Text Unit`; map app errors explicitly |
| `ConcurrencyConflict ...` | `InsertionError ConsistencyCheckFailed` |
| `StreamNotFound streamId` | `StreamNotFound entityName streamId` |
| `Limit Int`, `StreamPosition Int` | `Limit Int64`, `StreamPosition Int64` |

Keep existing UUIDs and the mapping from entity kinds to streams. `EntityName`
is a kind such as `"Cart"`, not a freshly generated ID per write. Import `Service.Event.StreamId` qualified as `StreamId` for
`StreamId.toStreamId`, or use the `toStreamId` method exported by `Service.Event`.
The old event
record contained identifiers and positions but no domain payload: decide what
`eventType` your app will store instead of inventing missing historical content.
`EventStore.castEventStore` adapts a raw `EventStore Json.Value` to a type with
`Json.FromJSON` and `Json.ToJSON` instances.

The old `localPosition` was the expected **next** position. For the first
insertion use `StreamCreation`; when the expected next position is `n > 0`,
use `InsertAfter (StreamPosition (n - 1))`. Also set the first insertion's
`metadata.localPosition` to `Just (StreamPosition n)`. Keep subsequent batch
positions consecutive. Derive `n` from the stream you actually read. Simple checks the
next position; PostgreSQL rejects conflicting stored positions but does not
validate an arbitrary position ahead of the stream tail. Do not invent or
skip positions. Test the stale-write case on the backend your app uses.

For example, in a Task using qualified `Service.Event` as `Event` and
`Service.Event.EventMetadata` as `EventMetadata`, the first write becomes:

```haskell
metadata <- EventMetadata.new
let insertion = Event.Insertion
      { id = existingEventId
      , event = yourEvent
      , metadata = metadata
          { eventId = existingEventId
          , localPosition = Just (Event.StreamPosition 0)
          }
      }
let payload = Event.InsertionPayload
      { streamId = migratedStreamId
      , entityName = Event.EntityName "Cart"
      , insertionType = Event.StreamCreation
      , insertions = Array.wrap insertion
      }
success <- store.insert payload
```

Here `existingEventId` is the UUID you previously supplied, `yourEvent` is the
app's chosen payload, and `migratedStreamId` preserves the old stream identity.
`insert` returns `InsertionSuccess`, with `localPosition` and
`globalPosition`, rather than an Event. Persistence uses `metadata.eventId`;
setting `Insertion.id` alone does not preserve the stored event UUID. The convenience `payloadFromEvents`
generates new IDs and defaults to `AnyStreamState`, which does **not** preserve
an old expected-position check; do not substitute it blindly.

Preserve handling of the outer read `Task Error` before consuming its stream.
For finite reads, `Stream.toArray` collects messages, then
`EventStore.collectStreamEvents` / `collectAllEvents` extracts events. Inspect
`ToxicStreamEvent` / `ToxicAllEvent` (content that could not decode) and
`StreamTerminated` / `Terminated` before filtering: simply extracting events
can hide read failures. Large histories should use `Stream.consume` with
explicit handling of event, checkpoint and failure messages. Update any
custom EventStore implementation for these protocols and its new `close` and
`truncateStream` operations; call `close` during shutdown.

`InMemory.new` remains available and non-persistent. To retain events across
restarts, configure `SimpleEventStore` with a valid `Path` and
`persistent = True`, or configure the PostgreSQL store. This is a storage
choice, not an automatic conversion: the old in-memory store has no disk data
to recover after shutdown. If your app supplied its own storage, export and
validate its identifiers, positions and content before changing formats; this
release provides no general converter for custom stores.

### Verify

After also applying the CLI guide if needed, run `neo build` and `neo test`
from the app root (or its existing Cabal build/test commands if it does not use
Neo). Expect a successful build and passing app tests. Add focused checks for
append order, Var read/write, channel ordering and rejected writes, JSON
round-trips, and subprocess failures where those APIs are used.

If your app uses the `Command` options-parser facade, also test its flags,
environment inputs, help output and invalid-input errors: its parser backend
changed, even though the public facade names remain.

For EventStore users, test first insertion, the next insertion, two competing
writers, a rejected stale write, forward/backward reads, subscription catch-up
and shutdown. Confirm IDs and ordering remain unchanged, and that errors cannot
silently drop events. For persistent stores, restart against a **copy** of data
and verify the same history can be read. Report unresolved storage mappings
before deploying the upgraded app.

<details>
<summary>Copy this prompt to your coding agent</summary>

```text
Migrate Framework from NeoHaskell 0.9.0 to 0.10.0.
Upgrade this application's NeoHaskell 0.9 core/EventStore usage to 0.10. Preserve
behavior, tests, user data, UUIDs and event ordering. Start on a branch; identify
which changes apply before editing.

1. Rewrite old Text.append first second and Array.append first second to
   first |> Text.append second and first |> Array.append second. Review partial
   applications, Collection.append on Array, and folds for order. Appendable
   (++) is unchanged. Test distinct
   values and empty inputs; "butter" followed by "fly" must remain "butterfly".
2. Var.new/get/set now return Task instead of IO. Remove Task.fromIO wrappers
   around them in Task code. At an IO boundary, run a composed Task using the
   app's error policy (Task.runNoErrors only when the error type is Never).
3. DurableChannel.write/checkAndWrite accept Array values. Preserve single
   writes with Array.wrap. writeWithIndex callbacks must return a singleton
   Array; reads remain individual values. Test order, indexes and rejection.
4. Rename Json.Decodable/Encodable constraints and imports to FromJSON/ToJSON.
   Preserve JSON round-trip fixtures. Subprocess.open/openInherit now return
   Task Subprocess.Error Completion: map errors to the caller's type and retain
   exitCode checks. Test missing executables and nonzero exits.
5. For direct EventStore users, replace EntityId with a stable EntityName per
   entity kind and convert old UUID StreamIds with StreamId.toStreamId. Use typed
   Event eventType, InsertionPayload/Insertion and store.insert. Preserve the
   old insertion UUID in Insertion.id and metadata.eventId. Choose actual domain
   content with the owner when old records lack it; never invent historical data.
   Old expected-next position 0 uses StreamCreation; n > 0 uses InsertAfter
   (StreamPosition (n - 1)). Set metadata.localPosition = Just (StreamPosition n)
   and consecutive positions for batches. Derive n from actual stream state,
   never an invented future position; PostgreSQL rejects position collisions
   but does not validate gaps ahead of the tail. Positions and Limit use Int64.
   payloadFromEvents generates new IDs and defaults to AnyStreamState; using it
   unchanged would drop the old concurrency check. insert returns positions.
   Persistence uses metadata.eventId, not Insertion.id alone.
   Read Event IDs/positions through metadata; positions are Maybe values.
   Reads now return Stream messages. Preserve outer Task error handling,
   then handle toxic/terminated messages before
   collectStreamEvents/collectAllEvents; Stream.toArray is only for bounded
   reads. Subscription callbacks return Task Text Unit. Replace old concurrency
   errors with InsertionError ConsistencyCheckFailed and add EntityName to
   StreamNotFound matches. Implement close/truncateStream for custom stores.
6. Run the application's build/tests (neo build and neo test after any CLI
   migration). Exercise stale/conflicting writes, ordering, subscription catch-up
   and shutdown. Use a copy of persistent data for restart verification. The old
   InMemory backend has no persistent data; custom storage has no automatic
   migration. Do not delete data, weaken tests or silently skip decoding errors.

Report changed files, tests and exact outcomes, unresolved API or data mappings,
and any assumptions needing the app owner's decision. Do not deploy or publish.
```

</details>

[Compare changes](https://github.com/neohaskell/NeoHaskell/compare/0.9.0...neo-v0.10.0)

## Legacy history

# Changelog

Entries are **generated from contract-delta specs** (`docs/changes/*.md`) by
`./dev changelog` — do not hand-write them; regenerate instead. A change is
**breaking** iff its spec's `diff signatures` delta removes or changes a
signature line; a breaking entry carries a mandatory migration note (from the
spec's `## User impact`). CI gate: `changelog --check` in `.github/workflows/checks.yml`.

**Release promotion:** at release time, rename the `## [Unreleased]` heading to
`## [X.Y.Z] — YYYY-MM-DD` and add a fresh empty `## [Unreleased]` above it; a
breaking entry in the section forces a major/minor bump per semver. (No release
has been cut yet — everything accrues under Unreleased until the first tag.)

## Legacy development notes

### 008-harden-pipeline-evidence-and-regression-smoke — Change 008: Harden pipeline evidence and regression smoke

No public application API breaks. Contributors get earlier, deterministic failures instead of false-green specs, red tests, stale localization data, or incomparable timing claims. The new benchmark is local/nightly rather than PR-blocking because shared-runner wall-clock noise is not deterministic.

API delta:

- `+ Test.Service.Command.Core: AddItemToCartAfterItemCount :: Uuid -> Uuid -> Int -> AddItemToCartAfterItemCount`
- `+ Test.Service.Command.Core: data AddItemToCartAfterItemCount`
- `+ Test.Service.EventStore.Regression: assertBehavior :: Text -> Bool -> Task Text Unit`
- `+ Test.Service.EventStore.Regression: awaitInsertions :: Int -> InsertBarrier -> Task error Unit`
- `+ Test.Service.EventStore.Regression: barrierBeforeInsert :: InsertBarrier -> EventStore event -> EventStore event`
- `+ Test.Service.EventStore.Regression: data InsertBarrier`
- `+ Test.Service.EventStore.Regression: failFirstWithConsistencyConflict :: EventStore event -> Task error (EventStore event)`
- `+ Test.Service.EventStore.Regression: knownBad :: Text -> Text -> Task error Bool`
- `+ Test.Service.EventStore.Regression: newInsertBarrier :: Task error InsertBarrier`
- `+ Test.Service.EventStore.Regression: recordFetchedRevisions :: forall k state (event :: k) error. EntityFetcher state event -> Task error (EntityFetcher state event, Task error (Array (Maybe StreamPosition)))`
- `+ Test.Service.EventStore.Regression: recordFetches :: forall k state (event :: k) error. EntityFetcher state event -> Task error (EntityFetcher state event, Task error (Array (EntityFetchResult state)))`
- `+ Test.Service.EventStore.Regression: recordInsertions :: EventStore event -> Task error (EventStore event, Task error (Array (InsertionPayload event)))`
- `+ Test.Service.EventStore.Regression: releaseInsertions :: Int -> InsertBarrier -> Task error Unit`
- `+ Test.Service.EventStore.Regression: requireInsertionType :: InsertionType -> EventStore event -> EventStore event`
- `+ Test.Service.EventStore.Regression: seedStream :: EventStore event -> EntityName -> StreamId -> Array event -> Task Text Unit`

### 007-make-cold-start-health-constant-time — Change 007: Make cold-start health constant-time without losing replayed events

**Runtime:** container liveness no longer scales with event-store size. Operators
can probe `/health` for process liveness and `/ready` for traffic readiness
without widening a grace period as the store grows. Replay remains ordered and
complete when live events overlap startup; fixing the bind delay must not trade
a loud crash-loop for silent projection loss.

**Performance:** rebuild changes from one full-log pass per query plus
per-operation pool construction to one paged, entity-filtered pass over a
reused Postgres pool. Entity snapshots prevent repeated full-stream fetches.
Progress is visible through the ADR-0059 field names rather than a silent
multi-minute gap.

**Public Haskell surface:** additive only. `Service.Query.Registry` exposes
`registeredEntityNames :: QueryRegistry -> Array EntityName`; existing callers
need no migration.

**CI:** `.github/workflows/test.yml` exports `POSTGRES_AVAILABLE=true` for the
Postgres-backed suites so the concurrency and pool regressions execute on every
substantive PR.

**Deployment documentation:** restore the deployment guide and lead with a
`startupProbe`/readiness configuration, including explicit
`periodSeconds × failureThreshold` arithmetic and the distinction between
`/health` and `/ready`.

**Deliberately deferred:** production checkpoint-store wiring and query-state
migration remain tracked by #854/#855/#666. SIGTERM cancellation remains #662;
outbound-integration recovery is #856; the missing `X-Query-Status` contract is
#664; Neon scale-to-zero support is #857. None is required to make port binding
constant-time and in-process replay/live overlap gap-free.

API delta:

- `+ Service.Query.Registry: registeredEntityNames :: QueryRegistry -> Array EntityName`

### 006-deterministic-uuid-v5 — Change 006: Add deterministic UUID v5 generation to `Uuid` and the `Decision` monad

**Not breaking.** Three added signatures, no removals, no behavior change to any
existing function. `Decision`'s constructor set is unchanged, so `runDecision`
and the command executor are untouched — existing commands cannot regress.

**New capability for users.** Natural-key entity identity becomes expressible in
the framework:

```haskell
getEntityId :: RegisterProject -> Maybe Uuid
getEntityId command =
  Uuid.generateV5 projectNamespace command.repoPath |> Just
```

so the same normalized path always routes to the same stream, and `decide` sees
`Just entity` on a repeat and can `Decider.reject "already registered"`. Inside
`decide`, other derived ids come from
`Decider.generateDeterministicUuid namespace key`, which reads alongside
`Decider.generateUuid` without a `pure`/`Task.yield` lift (which the dialect
hook bans at the call site anyway).

**Security constraint, carried in the haddock of both functions.** A v5 UUID is
**not** a secret — anyone who knows the namespace and the name reproduces it
exactly, and names drawn from a small space can be brute-forced from a known
namespace. It must never be used for capability tokens, session ids,
password-reset components, or anything else that is unguessable by design;
`Uuid.generate` (random v4) stays the correct choice there. The division of
labour is now explicit: *random for secrets and fresh identity, deterministic for
derived identity.*

**No default namespace.** The issue floated a `uuidFromText :: Text -> Decision Uuid`
convenience with a built-in namespace; it is deliberately not shipped. A
framework-wide default namespace is a global collision domain, and since the
whole premise of the primitive is that the same input *is* the same stream, such
a collision is a data-integrity bug across unrelated aggregates. Callers build a
namespace with `Uuid.fromText`. RFC 4122's predefined namespaces (DNS/URL/OID/X500)
are likewise not re-exported yet — rule of three, and no consumer has asked.

**Testbed effect.** A new demo command `Testbed.Cart.Commands.RegisterCartByKey`
(registered in `Testbed.Cart.Service`) exercises the feature end-to-end, per the
`new-command-machinery` extension point's rule that framework write-side features
get a demo command plus hurl coverage. It is additive: no existing testbed
command, query, or hurl file changes, and no existing expectation is touched.

**Relationship to the abandoned `feat/uuid-v5-decision` branch.** The issue asks
that the pre-existing branch be merged. It is not merged as-is: alongside the
~130 relevant lines it carries ~5000 lines of unrelated tooling
(`.atomicorch/**`, `docs/designs/**`), duplicate ADR trees (`docs/adr/` and
`docs/decisions/`), and an ADR numbered **0055**, which `main` has since assigned
to *declarative integrations with fakes*. Its `Uuid.generateV5` also reaches for
`Data.Text.Encoding` and `Data.ByteString` directly rather than the dialect's
`Text.toBytes`. This change re-lands the wanted API on a clean branch through the
spec gate, with the ADR renumbered to 0073 and the criteria above; the old branch
should be closed rather than merged.

API delta:

- `+ Uuid: generateV5 :: Uuid -> Text -> Uuid`
- `+ Bytes: unpack :: Bytes -> [Word8]`
- `+ Decider: generateDeterministicUuid :: Uuid -> Text -> Decision Uuid`

### 005-thread-query-name-through-query-object-store — Change 005: Thread the real query name through the QueryObjectStore so multiple queries per entity stop colliding  **[BREAKING]**

**Breaking (source-level, in-repo callers updated in this PR).** Three exported
signatures gain a query-name input:

- `createDefinitionWithStore` — its store-factory argument becomes
  `Text -> Task Text (QueryObjectStore query)`. Migration: a factory `f` that
  ignored the name becomes `\_ -> f`; a factory that needs it receives the
  query's `NameOf` as `Text`.
- `QueryObjectStore.Core.createQueryObjectStore` / `Postgres.newFromConfig` —
  each takes the query name as a trailing `Text`. Migration: pass the query
  name (`newFromConfig cfg "my-query"`).

The two in-repo callers — `Application.withQuery` (config-backed path) and
`Definition.createDefinition` (in-memory convenience) — are updated here.
External services that call `createDefinitionWithStore` with a **custom** store
factory must adapt the factory shape; the changelog carries the migration note
(generated from the removed signature lines).

**Runtime behavior.** Before: any app with ≥2 queries over one entity, running
on the Postgres QueryObjectStore, corrupts state across queries and fails their
rebuilds — `withQueryObjectStore` with a Postgres config is unusable for
real multi-query services. After: each query's rows are keyed by its own name,
so distinct queries over the same entity persist independently. In-memory stores
are unaffected — each `InMemory.new` already allocates an independent map, so its
`createQueryObjectStore` ignores the name.

**Testbed:** no acceptance-test change — the collision needs a Postgres backend
with ≥2 queries per entity, which the default testbed app does not wire; covered
at the integration level. The existing `PostgresSpec` single-store tests keep
running (their `mkStore` helper supplies a fixed default query name), so the
refactor's regression surface stays green.

**Wiring coverage (review follow-up):** the store-level isolation criteria
(C1/C2) prove that *manually named* Postgres stores do not collide, but not that
the application actually threads the right name into the store. C4 closes that
gap: it drives `createDefinitionWithStore` with a spy store factory and asserts
the factory is handed `NameOf query`, not the `"__trait__"` sentinel — a fast
`unit` test (no Postgres) that exercises the exact wiring hop the fix adds.

**Checkpoint coexistence (implementation follow-up, C5):** before the fix the
trait's per-instance rows lived under `"__trait__"` while the checkpoint marker
(`Subscriber.rebuildFrom` via `CheckpointStore`, keyed by the reserved nil UUID)
lived under the real `query_name` — two different partitions. Threading the real
name unifies them under one `query_name`, isolated only by `instance_uuid` (real
vs nil). `get`/`atomicUpdate` are unaffected (they use real instance UUIDs), and
`resumeFromCheckpoint`/`deleteStaleHash` stay correct (they filter by
`query_hash`, which trait rows leave empty; `deleteStaleHash` only fires before a
full replay-from-0 that rebuilds those rows). The one place that needed a guard
is `getAll`, which now excludes the nil-UUID marker so a checkpointed query's
`GET /queries/{name}` never surfaces the marker's placeholder state — proven by
C5.

API delta:

- `- Service.QueryObjectStore.Core: createQueryObjectStore :: (QueryObjectStoreConfig config, FromJSON query, ToJSON query) => config -> Task Text (QueryObjectStore query)`
- `+ Service.QueryObjectStore.Core: createQueryObjectStore :: (QueryObjectStoreConfig config, FromJSON query, ToJSON query) => config -> Text -> Task Text (QueryObjectStore query)`
- `- Service.QueryObjectStore.Postgres: newFromConfig :: (FromJSON query, ToJSON query) => PostgresQueryObjectStoreConfig -> Task QueryObjectStoreError (QueryObjectStore query)`
- `+ Service.QueryObjectStore.Postgres: newFromConfig :: (FromJSON query, ToJSON query) => PostgresQueryObjectStoreConfig -> Text -> Task QueryObjectStoreError (QueryObjectStore query)`
- `- Service.Query.Definition: createDefinitionWithStore :: forall query (queryName :: Symbol) (entities :: [Type]). (Query query, ToSchema query, ToJSON query, FromJSON query, queryName ~ NameOf query, entities ~ EntitiesOf query, KnownSymbol queryName, WireEntities entities query) => Task Text (QueryObjectStore query) -> QueryDefinition`
- `+ Service.Query.Definition: createDefinitionWithStore :: forall query (queryName :: Symbol) (entities :: [Type]). (Query query, ToSchema query, ToJSON query, FromJSON query, queryName ~ NameOf query, entities ~ EntitiesOf query, KnownSymbol queryName, WireEntities entities query) => (Text -> Task Text (QueryObjectStore query)) -> QueryDefinition`

### 004-crypto-hmac-sign-verify — Change 004: Add Crypto module with HMAC-SHA256 signWith/verifyWith

None breaking. New public module `Crypto` and new `Bytes.getRandom`
primitive (secure random bytes, mirroring `Int.getRandom`); no existing
signatures change. The `Bytes` newtype now lives in the hidden internal
module `Bytes.Internal` purely to break an import cycle; it is not
importable by applications. The public `Bytes` API is unchanged (`Bytes
(..)` is re-exported as before). `Auth.OAuth2.StateToken` keeps its private
`HmacKey` for now — migrating it onto `Crypto.HmacKey` is a possible
follow-up refactor, deliberately out of scope here. Signature wire format
is lowercase hex (the common webhook header convention, e.g. GitHub/Stripe
style); `verifyWith` is case-insensitive on input.

API delta:

- `+ Crypto: data HmacKey`
- `+ Crypto: hmacKeyFromText :: Text -> Result Text HmacKey`
- `+ Crypto: hmacKeyFromBytes :: Bytes -> Result Text HmacKey`
- `+ Crypto: generateHmacKey :: Task err HmacKey`
- `+ Crypto: signWith :: HmacKey -> Bytes -> Text`
- `+ Crypto: verifyWith :: HmacKey -> Text -> Bytes -> Bool`
- `+ Bytes: getRandom :: Int -> Task w Bytes`

### 003-maintainer-codemap-regeneration — Change 003: Maintainer-triggered codemap regeneration onto a contributor PR

Not breaking. No public signature or wire-format change. New capability for
maintainers only: a manually-dispatched workflow on `main`. Contributors see
their PR branch receive one `chore: regenerate codemap` fast-forward commit after
Nick approves the protected environment; a no-op (codemap already current) leaves
the branch untouched and the run succeeds. Every unsupported or unsafe condition
(maintainer edits disabled, org-owned fork, metadata race, symlink under
`codemap/`, out-of-allowlist manifest/diff, non-fast-forward) fails with an
actionable Actions summary and mutates nothing — **no fallback PR is ever
created**. Testbed: no acceptance-test change — this is CI/tooling with no
HTTP-observable behavior. One-time maintainer setup is **mandatory and
load-bearing**: the `codemap-publish` Environment with **required reviewer Nick**
AND **deployment branches = `main` only**, plus the `CODEMAP_PUBLISH_TOKEN` secret
— a maintainer classic `public_repo` PAT (broad public-repo blast radius
documented; dedicated bot identity recommended; expiry ≤90d; revoke-on-exposure)
— documented in ADR-0070 and the workflow header. Without any of these the
workflow fails closed at `publish`.

### 002-task-control-flow-dialect-rules — Change 002: Enforce Task control-flow dialect — `|> discard`, `Task.when`, `Task.unless`

Not breaking. No public signature or wire-format change — the migrated `if …
pass` blocks and their `Task.when`/`Task.unless` replacements are behaviourally
identical (`Task.when c a` runs `a` iff `c`, `Task.unless c a` runs `a` iff not
`c`, each otherwise doing nothing — exactly like the `if`/`pass` forms). New Task
code is nudged toward the dialect idioms at edit
time (rule 1) and at `./dev lint`/CI (rules 2–3). Existing non-dialect parser
and `Q`-monad code is deliberately preserved via added-lines grandfathering and
a scoped ignore. Testbed: no acceptance-test change — this is a source-dialect
and tooling change with no HTTP-observable behaviour.

### 001-fileupload-dedup-blob-existence-check — Change 001: Verify the blob still exists before returning a dedup match on file upload

Not breaking. No signature or wire-format change; `UploadResponse` still omits
`blobKey` from JSON. Behavior only changes on the failure path: an upload that
previously returned a reference to a missing blob (poisoning the content hash
forever) now re-stores the content and returns a valid reference. The re-stored
bytes are the caller's own uploaded content, matched by the same owner-scoped
content hash, so there is no cross-owner exposure. Testbed: no acceptance-test
change — blob loss cannot be induced over HTTP; covered at the integration
level.

Side effect of making the reproduction executable: `ContentDedupSpec` is listed
in the cabal `other-modules` but was never registered in
`core/test-service/Main.hs`, so its dedup coverage compiled but never ran. This
change registers it, so the regression tests **and** the existing dedup suite
now execute.
