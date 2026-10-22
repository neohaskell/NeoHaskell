# Reuse EventMetadata and populate it at command execution

## Decision

Reuse the existing EventMetadata shape. Do not introduce a Lineage, Origin or Execution domain record. The user explicitly rejected duplicating the existing metadata model in this tab and asked to fill EventMetadata properly. The selected technical wiring is an explicit optional source EventMetadata argument through internal dispatch and command execution. RequestContext and public transport handler signatures remain unchanged.

This is one Wayfinder planning decision. No production implementation is included. The previous proposal in lineage-proposal-v1-superseded.md and its two candidate sketches are superseded.

## Usage and owner

Root command context comes from the existing Auth constructors. Event callbacks supply the triggering event's existing metadata. Autonomous dispatch has no source event.

```haskell
Executor.execute store fetcher entityName Nothing requestContext command
Dispatcher.dispatchCommand endpoints (Just sourceEvent.metadata) callbackPayload
Dispatcher.dispatchCommand endpoints Nothing autonomousPayload
```

These are proposed signatures and call sites. They are not implemented APIs.

The existing separate internal command dispatch map supplies a narrow source-aware handler. It may use a type alias for `Maybe EventMetadata -> EndpointHandler`. This is a function signature, not another metadata model. Public EndpointHandler, Transport.buildHandler, RequestContext and decide retain their existing types.

The executor receives the optional source metadata alongside the current authorization context. It normalizes a nil request ID once before conflict retries. Event.payloadFromEvents continues constructing fresh EventMetadata through its existing constructor. One private pure updater in the executor sets relatedUserSub, correlationId and causationId on the new insertions. Apply it before the existing position assignment and append.

The updater leaves eventId, createdAt and both positions unchanged. It never copies a whole historical record over a fresh event. CommandPayload remains command type plus command data. HTTP headers and command JSON cannot select framework source metadata.

## Lineage contract

1. Root commands copy the verified subject from RequestContext.user when present. Their server-generated request UUID starts the correlation chain. Normalize nil request IDs to a fresh UUID once per invocation. Anonymous and trusted roots have no invented actor. Root events have no triggering-event cause.
2. Event callbacks copy source actor attribution and any present source correlation unchanged. If correlation is absent, use source metadata.eventId text as deterministic correlation. Immediate cause is always that same source metadata.eventId, never Insertion.id, the source's previous cause or a sibling event. Present historical text stays opaque. This decision adds no nil-source-ID reinterpretation policy.
3. All events from one accepted decision are siblings. They share the three tracing values and retain existing fresh per-event identity and creation time. Historical stored events and their positions are unchanged.
4. Capture source metadata once per event delivery before runner retry and fan-out. Each emitted command still has its own request ID. Conflict retries reuse the normalized context and source metadata while retaining current regeneration of unpersisted event identities and timestamps. Command dispatch stays outside runner retries. No deduplication or exactly-once guarantee is added.
5. Each autonomous command emission starts a fresh correlation from its request ID. It has no actor or event cause. Do not reuse a worker-lifetime ID across timer ticks. Historical attribution never becomes verified claims, tenant authority, permissions or trusted bypass. Existing authorization and tenancy outcomes remain unchanged.

## Alternatives and synthesis

The first comparison considered an additional typed Lineage field and a separate execution envelope. Both were technically viable in a shared contract prototype, but neither demonstrated that duplicating the existing metadata model was necessary. The user's response exposed that premise. Those models are rejected.

A fresh pair of independent designs compared carrying source EventMetadata on RequestContext with passing it explicitly through internal dispatch. The independent judge selected explicit source metadata plus the existing builder and a private updater. The coordinator agrees. The separate internal dispatch map already exists in ServiceDefinition.Core, so this avoids changing every public transport handler.

A generic metadata factory parameter to payloadFromEvents is also rejected. It exposes identity and timestamp construction policy and permits callers to return historical metadata unchanged. Keeping EventMetadata.new in the existing builder avoids that additional policy and preserves its current signature.

## Tradeoffs and principles

Minimal Change removes the duplicate model and preserves RequestContext, public transport handlers and payloadFromEvents. Model the Domain uses the existing EventMetadata record and one updater that owns the three tracing-field rules. Security by Construction keeps historical metadata outside the authorization context passed to decisions and never reconstructs claims from attribution.

Design for Throughput accepts one bounded array pass proportional to emitted events. The hypothetical aggregate workload is 50,000 requests per second with variable fan-out and existing persistence. This is design reasoning. There are no ancestry reads, additional registries, encoding round trips, queues or locks. The extra pass allocates a transient insertion array. Its cost has not been benchmarked.

The explicit executor source argument requires migrating its direct callers and internal dispatch plumbing. Candidate source inspection found 31 direct executor test calls. Treat that count as an inventory lead for the compatibility decision, not a frozen acceptance target. The unchanged builder avoids the proposed metadata-factory migration.

## Evidence

The real baseline uses CommandExecutor.execute with AuthenticatedAddItem and an authenticated context, then reads back the accepted event from the actual in-memory store. All three tracing fields are null. The required actor assertion fails as expected. Source-bound receipt is `.agents-state/regression-evidence/runtime-4g4iax4g/receipt.json`.

DirectMetadata.hs uses existing framework RequestContext and EventMetadata values without an additional lineage record. It passed eight Hspec examples and 100 property cases. It verifies root normalization, callback fields, fresh identity and timestamp preservation, historical fallback, opaque historical correlation, autonomous commands, siblings and rebuilt payloads, and actual in-memory persistence.

The direct fixture represents the proposed updater and parameter flow. It does not replace the production executor, wire actual callback dispatch, run real conflicts or demonstrate tenant rejection. Full treatment, PostgreSQL, subscriptions, replay, cancellation and log isolation remain execution obligations. Prove It Works requires preserving that evidence distinction.

The HTML walkthrough illustrates the same contracts. It is a throwaway model, not production execution. Its architecture selector now compares the two direct metadata approaches.

## Remaining map work

Specify correlated log fields and execution coverage next. Then choose compatibility boundaries and implementation proof. Only after all decisions and fog are resolved may the final ordinary implementation task be created. This session authorizes no production implementation, deployment or merge.
