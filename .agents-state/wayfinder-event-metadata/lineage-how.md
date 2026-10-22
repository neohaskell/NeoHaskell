# Current lineage flow

How explainer output. Source-checked before design comparison. Its evidence-file caveat is preserved below. A fresh characterization run now uses a distinct transcript name to prevent overwrites.

## Overview

Source inspection locates two missing lineage connections. Root commands reach event construction with verified request context, but event construction receives no context. Event callbacks retain their source event until command dispatch, where dispatch creates a fresh trusted context. Framework consumers therefore cannot trace persisted events back to the initiating user through these paths. Maintainers have identifiable boundaries to change.

The runtime evidence supports the root-command symptom. The recorded execution at `.agents-state/prototypes/metadata-lineage/Baseline-pass.log` shows an accepted command's persisted event with all three lineage fields null. That file currently records a failing required-behavior assertion despite its filename. A passing characterization is not independently established by that artifact.

## Key concepts

`RequestContext` carries verified user claims, request identity, files, timestamp and trusted bypass authority. `EventMetadata` carries persisted event identity and optional attribution, correlation and cause. These are separate contracts in `core/service/Service/Auth.hs:66` and `core/service/Service/Event/EventMetadata.hs:19`.

`CommandPayload` contains serialized command type and data. It has no execution context, as defined in `core/service/Integration.hs:198`.

## How it works

```text
Verified web claims
        |
   RequestContext
        |
 Executor authorization and decision
        |
 Event.payloadFromEvents receives no context
        |
 Persisted Event with empty lineage
        |
 Subscription and dispatcher retain full Event
        |
 Callback command payload
        |
 dispatchCommand creates fresh trusted context
```

The web transport constructs authenticated or anonymous contexts after resolving authentication. The executor checks access and passes the context into command decisions. Accepted decisions call `Event.payloadFromEvents` without it. That function generates insertions through `EventMetadata.new`, which initializes every lineage field to `Nothing`. This connection explains the observed nulls through `core/service/Service/Transport/Web.hs:624`, executor decisions at `core/service/Service/CommandExecutor/Core.hs:306` and event construction at `core/service/Service/Event.hs:68`.

Subscriptions deliver complete events to the dispatcher. Stateless and lifecycle processors still possess the triggering event when dispatching callback commands. Their dispatch helper accepts only endpoints and payload, then constructs `Auth.trustedContext`. This is a second lineage boundary, established by subscription wiring at `core/service/Service/Application/Integrations.hs:240` and callback processing at `core/service/Service/Integration/Dispatcher.hs:781`.

Autonomous inbound workers use the same helper without a triggering event. A design must distinguish these origins at the existing dispatch boundary. This is a source-based design implication of inbound dispatch at `core/service/Service/Application/Integrations.hs:279`.

## Where things live

The implementation boundaries are command execution at `core/service/Service/CommandExecutor/Core.hs:356`, metadata construction at `core/service/Service/Event/EventMetadata.hs:37` and integration dispatch at `core/service/Service/Integration/Dispatcher.hs:878`.

## Gotchas

Persisted identity is `metadata.eventId`. `Insertion.id` is generated separately. PostgreSQL deduplicates and writes using the metadata identity, so callback cause must reference that identity. See insertion construction at `core/service/Service/Event.hs:68` and PostgreSQL insertion at `core/service/Service/EventStore/Postgres/Internal.hs:325`.

Conflict retries reuse the captured request context but rerun event construction. Callback dispatch already occurs outside runner retries. Preserve both distinctions, as shown in executor retries at `core/service/Service/CommandExecutor/Core.hs:292` and runner dispatch at `core/service/Service/Integration/Dispatcher.hs:788`.

Security by Construction shaped the separation of historical attribution from verified identity. Stored actor metadata must never reconstruct authorization claims. Trusted bypass also leaves tenant-claim requirements intact in the executor at `core/service/Service/CommandExecutor/Core.hs:206`.
