---
group: platform
component: Framework
impact: compatible
category: Added
---

## Summary

Apps can create protected values in commands with `Erasable.new` and carry them through events and queries. The PostgreSQL event store encrypts these values before storing events and keeps their keys separately. This first step does not provide an app-facing erasure operation or a complete erasure workflow. Existing apps need no changes.
