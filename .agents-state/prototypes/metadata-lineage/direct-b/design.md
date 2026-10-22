# Explicit source metadata at internal dispatch

## Usage

Data shape is the existing `EventMetadata`. A command has an optional source event's metadata. Authorization remains the existing `RequestContext`.

```haskell
-- Signature sketches only. Bodies remain unimplemented.
buildCommandResponseHandler store cache Nothing requestContext command
Dispatcher.dispatchCommand endpoints (Just event.metadata) payload
Dispatcher.dispatchCommand endpoints Nothing inboundPayload
```

Framework consumers receive attributed events without changing `decide`, command JSON, or `RequestContext`. Maintainers distinguish historical attribution from current execution authority in function arguments.

## Problem and evidence

`ServiceDefinition/Core.hs:238-246,379-385,521` already builds public handlers and integration dispatch handlers separately. They currently share `EndpointHandler`. This existing separation permits a narrower change than adding source metadata to every public transport handler. `CommandExecutor/Core.hs:179,357` is the authorization and event-construction boundary.

## Shape

These are incomplete interface sketches. Existing command constraints remain unchanged.

```haskell
type DispatchHandler = Maybe EventMetadata -> EndpointHandler

dispatchCommand ::
  Map Text DispatchHandler -> Maybe EventMetadata ->
  CommandPayload -> Task Text Unit

buildCommandResponseHandler ::
  EventStore event -> Maybe (SnapshotCache entity) ->
  Maybe EventMetadata -> RequestContext -> cmd ->
  Task Text CommandResponse

execute ::
  EventStore event -> EntityFetcher entity event -> EntityName ->
  Maybe EventMetadata -> RequestContext -> command ->
  Task Text ExecutionResult

payloadFromEvents ::
  Task err EventMetadata -> EntityName -> StreamId ->
  Array event -> Task err (InsertionPayload event)
```

`createHandlers` binds `Nothing`. `createDispatchHandler` leaves the source argument available. Only the existing dispatch registry uses `DispatchHandler`; `EndpointHandler` and the `Transport` class stay unchanged. Change that registry's types through `ServiceDefinition`, `Application/Integrations`, and `Integration/Dispatcher`.

The executor normalizes a nil request ID once before retries. With no source, actor comes from verified `requestContext.user.sub`, correlation uses that request ID, and cause is absent. With a source, actor and present correlation text pass through unchanged. Missing correlation falls back to the canonical source `metadata.eventId`. Cause always uses that immediate source event ID.

The executor captures these three derived values in a local metadata task. Each execution calls `EventMetadata.new`, then fills its attribution fields. `payloadFromEvents` executes this task separately for every event. New identity, timestamp, and initially empty positions therefore come from the existing constructor. Existing position assignment and store ownership remain unchanged. Siblings and conflict retries retain the same tracing values. No source or historical event is modified.

## Tradeoffs and principles

Minimal change keeps public transport contracts intact. Model the Domain reuses `EventMetadata` instead of adding another lineage record. Security by Construction keeps stored actor data outside authorization context; trusted dispatch and tenant checks retain their current behavior. Source metadata cannot grant claims or enter command JSON.

We accept source arguments through internal dispatch and execution, plus an event-construction task argument. Source inspection found 31 executor test calls and 12 payload-helper test calls requiring migration. The callback exposes construction policy and cannot statically prevent metadata identity reuse. Its sole production caller must remain the executor.

Design for Throughput predicts constant extra work per command plus existing per-event allocation. Assume small metadata and unchanged event counts at the hypothetical 50,000 requests per second. No ancestry reads, encoding round trips, locks, or global state are added. Storage and conflict retries remain the dominant uncertain costs. This is unmeasured.

## Synthesis decision and alternatives

Recommend this candidate when separating attribution from `RequestContext` justifies explicit internal plumbing. It costs more migration than a context field. A metadata-construction callback through every handler loses because it exports allocation policy across more boundaries. A new lineage record loses because it duplicates existing metadata.

## Open risk and next step

Can the maintained dispatch and executor fixtures prove sibling identity uniqueness, legacy correlation preservation, and unchanged tenant rejection? Extend those fixtures before implementation.
