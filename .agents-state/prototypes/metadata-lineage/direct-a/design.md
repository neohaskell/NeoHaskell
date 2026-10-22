# Carry source EventMetadata through RequestContext

Design sketch only. It uses the existing persisted metadata shape and adds no lineage record.

## Usage

```haskell
context <- Auth.authenticatedContext claims
execute store fetcher entityName context command
dispatchCommand endpoints callbackPayload (Just sourceEvent.metadata)
dispatchCommand endpoints timerPayload Nothing
```

Callbacks pass the triggering event's metadata. Autonomous commands pass no source. Root commands keep the context built by the transport.

## Shape

`RequestContext` gains one field, `sourceEventMetadata :: Maybe EventMetadata`. Its existing constructors set `Nothing`. The dispatcher adds `Just sourceEventMetadata` to the trusted context for event callbacks. `EventMetadata` already owns persisted identity, actor attribution, correlation, cause, timestamp, and positions.

The dispatcher boundary becomes:

```haskell
dispatchCommand
  :: Map Text EndpointHandler
  -> Integration.CommandPayload
  -> Maybe EventMetadata
  -> Task Text Unit
```

Keep `Event.payloadFromEvents` unchanged. It creates fresh metadata for each insertion. The executor then maps one private pure updater over those insertions:

```haskell
eventMetadataFor :: RequestContext -> EventMetadata -> EventMetadata
```

For a root context, the updater sets `relatedUserSub` from verified `ctx.user.sub`, `correlationId` from the normalized `ctx.requestId`, and leaves `causationId` empty. For a source context, it copies `relatedUserSub`, preserves any present `correlationId` as opaque text, falls back to the source `eventId` text only when correlation is absent, and sets `causationId` to that event ID. It changes no other metadata fields.

Normalize a nil request UUID once at executor entry, before the retry loop. Each retry then reuses that correlation. Each emitted event still gets its own new ID and timestamp. Callbacks capture source metadata once per dispatch, outside runner retries.

## Boundaries and tradeoffs

The new context field makes direct `RequestContext` record construction source-incompatible and requires updating those constructors. The existing endpoint and `decide` signatures stay intact. The extra metadata pass costs O(n) in emitted events and one transient array allocation. It keeps the public event builder independent of authentication. If fan-out makes that pass material under the 50,000 requests-per-second design workload, the same updater can move into the insertion builder without changing the lineage rules.

Historical actor metadata only affects persisted attribution. It never populates verified user claims, tenant claims, or trusted bypass. Inbound headers and command JSON do not set the source field. `EventMetadata` does not import `Auth`, so the proposed `RequestContext` dependency does not create an import cycle.

I considered passing `RequestContext` into `Event.payloadFromEvents`. That avoids the extra pass but couples the event builder to authentication and exposes the full authorization context at a lower-level boundary. The source field keeps provenance explicit while preserving that boundary.
