---
group: platform
component: Framework
impact: compatible
category: Added
---

## Summary

Apps can create protected values in commands with `Erasable.new` and carry them through events and queries. The PostgreSQL event store encrypts these values before storing events and keeps their keys separately. Apps can request deletion through `Integration.Erasable.Erase`, with authorization and retry tracking in application code. Follow the protected-value erasure migration when updating custom event stores or configuring PostgreSQL application scopes.
