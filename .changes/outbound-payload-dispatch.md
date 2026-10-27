---
group: platform
component: Framework
impact: breaking
category: Breaking changes
---

## Summary

Outbound handlers can now receive one event payload. The runtime skips the handler
for other event types. Existing handlers that take the full event type need the
explicit `outboundIntegrationFullEvent` marker to keep their current behavior.

## Migration

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

## Agent prompt

```text
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
