---
group: platform
component: Framework
impact: breaking
category: Breaking changes
---

## Summary

Apps upgrading from 0.9.0 need to update event-store calls and several core
helpers. Text and array appends now take the value being extended last;
mutable variables run inside `Task`; durable-channel writes accept batches;
and JSON constraint names and subprocess error types have changed. The guide
below preserves existing ordering, error handling and event identities.

## Migration

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

## Agent prompt

```text
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
