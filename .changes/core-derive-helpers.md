---
group: platform
component: Framework
impact: compatible
category: Added
---

## Summary

Declare events, commands, entities, queries, and outbound integrations with
`deriveEvent`, `deriveCommand`, `deriveEntity`, `deriveQuery`, and
`deriveOutboundIntegration`, all available through `import Core`.

The new `deriveEntity ''CartEntity ''CartEvent` helper generates routine entity
instances and connects your `initialState`, `update`, and `getEventEntityId`
functions. Existing custom instances remain supported. The previous marker
names still work, so existing applications need no changes.
