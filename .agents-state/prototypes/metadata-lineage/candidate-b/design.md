# Candidate B. Execution envelope outside authorization context

## Problem

This candidate gives framework consumers automatic event attribution while preserving their existing `decide` signatures. Maintainers get one owner for lineage rules. Historical attribution remains separate from `RequestContext.user`, permissions, tenant claims, files, and bypass authority. This is a proposed contract, not an implemented guarantee.

## Usage

Web transport wraps its already authenticated context before invoking the endpoint. It ignores inbound correlation headers.

```haskell
execution <- Execution.root requestContext
handler execution bodyBytes respond
```

An event processor captures its origin once before processing and dispatches commands outside runner retries. Each dispatch gets fresh existing framework authority.

```haskell
origin <- Execution.triggered sourceEvent.metadata
payloads <- processEvent sourceEvent
payloads |> Task.forEach (\payload -> do
  requestContext <- Auth.trustedContext
  execution <- Execution.attach requestContext origin
  dispatchCommand endpoints execution payload)
```

An autonomous timer emission starts one origin per emission, shared by that emission's commands.

```haskell
origin <- Execution.autonomous
requestContext <- Auth.trustedContext
execution <- Execution.attach requestContext origin
CommandExecutor.execute store fetcher entityName execution command
```

These are signature sketches. Existing framework constraints and error types remain on the affected operations.

## Shape

`Service.Execution` owns opaque `Execution` and `Origin`. Their constructors and record updates remain private. Conceptually, `Execution` pairs the existing `RequestContext` with `Origin`. `Origin` contains actor attribution, nonempty correlation, and optional immediate cause. The actor is optional independently of the cause. Neither type has JSON decoding or conversion into claims.

```haskell
root :: RequestContext -> Task err Execution
triggered :: EventMetadata -> Task err Origin
autonomous :: Task err Origin
attach :: RequestContext -> Origin -> Task err Execution
request :: Execution -> RequestContext
withFiles :: Map FileRef ResolvedFile -> Execution -> Execution
stamp :: Execution -> EventMetadata -> EventMetadata
withScope :: Execution -> Task err value -> Task err value
```

Model the Domain shapes this pairing and the opaque origin. Security by Construction keeps historical actor data out of `RequestContext`. `root` reads verified `requestContext.user.sub` only to create attribution. `attach` never reads the supplied context's subject into an existing origin. Authorization still reads only `Execution.request`.

The owning signatures become the following.

```haskell
type EndpointHandler =
  Execution -> Bytes ->
  ((CommandResponse, Bytes) -> Task Text Unit) -> Task Text Unit

buildHandler :: transport -> Record.Proxy command ->
  (Execution -> command -> Task Text CommandResponse) -> EndpointHandler

payloadFromEvents :: Execution -> EntityName -> StreamId ->
  Array event -> Task err (InsertionPayload event)
```

`CommandExecutor.execute` takes `Execution` in the existing context position. It passes only `Execution.request execution` to `decideImpl` and the unchanged authorization and tenancy branches. `Event.payloadFromEvents` generates metadata through existing `EventMetadata.new`, then calls `Execution.stamp`. Thus `eventId`, `createdAt`, insertion identity, positions, and stored historical records retain their meanings.

## Semantic contract

1. `root` uses the server-generated request UUID as correlation. A nil request UUID becomes a fresh UUID in the wrapped request before correlation creation. Each invocation keeps one execution outside conflict retries. Anonymous and trusted roots have no invented subject. HTTP request and correlation headers cannot select persisted IDs.
2. `triggered` copies the source actor and usable source correlation. It replaces cause with canonical source `metadata.eventId`, never `Insertion.id` or the source's own cause. Missing, blank, or UUID-nil correlation falls back to canonical source `eventId`. Valid source IDs make that fallback deterministic across redelivery and independent subscribers. Other nonblank historical correlation text stays opaque and unchanged.
3. A nil source event UUID means an absent cause and a fresh fallback correlation when source correlation is unusable. Capture this origin once per delivered source before retries and fan-out. Separate deliveries of this malformed legacy source cannot promise stable grouping. No event identity is invented or rewritten in history.
4. All events from one accepted decision are siblings. They share actor, root correlation, and immediate cause while receiving distinct event IDs and creation timestamps. Callback commands share the captured origin. Each command dispatch has a fresh request ID. Conflict retries keep lineage but retain existing regeneration of unpersisted event metadata. Lineage supplies neither deduplication nor exactly-once delivery.
5. Autonomous emissions get fresh correlation and absent actor and cause. Existing `Auth.trustedContext` remains the authority choice at Dispatcher. Historical attribution never creates claims or tenant IDs. Multi-tenant work with no verified tenant claims remains rejected under the existing executor branches. Exact lineage labels enter `Log.withScope` at each execution boundary, including worker threads, without tokens or other claims.

## Synthesis decision

Candidate-only provisional choice. Use the separate envelope if preserving the distinction between historical attribution and current authority outweighs transport migration. Arena has not selected a base.

## Tradeoffs accepted

- We accept changes to `EndpointHandler`, `Transport.buildHandler`, Web transport, command registration callbacks, executor callers, and payload construction in exchange for keeping `RequestContext` unchanged.
- We accept two framework types in exchange for source-scoped origins that fan out without retaining verified claims in workers.
- We accept no lineage fields inside `decide` in exchange for preserving its uniform signature and keeping attribution framework-owned.

ADR0011 explicitly favors `RequestContext` extension for request-scoped data. This candidate preserves its uniform `decide` and file-access guarantees, but departs from its preferred container because lineage spans requests and survives loss of user authority. That departure needs an explicit architecture decision if selected.

## Alternatives considered

Extending `RequestContext` offers a smaller public migration and follows ADR0011. It loses this candidate's explicit boundary between authorization context and historical origin. Passing a separate lineage argument through every handler exposes more coordination to callers than one envelope.

## Costs and risks

Design for Throughput chooses immutable, invocation-scoped values. At the hypothetical aggregate 50,000 requests per second, added work scales with commands, emitted events, and log labels. There are no ancestry queries, registries, extra queues, or synchronization. Existing storage contention and bounded dispatcher queues remain limiting resources. This is design reasoning, not measured capacity.

The red-flag screen finds one owner and no transport representation in command payloads. The envelope hides normalization, stamping, and log policy. Its public transport migration is its main interface-depth cost. Required evidence includes custom-transport compilation, unchanged multi-tenant rejection, runtime fan-out and retries, and legacy-source behavior.

## Next implementation step

Build throwaway acceptance and rejection fixtures for the envelope and metadata contract before any production change.
