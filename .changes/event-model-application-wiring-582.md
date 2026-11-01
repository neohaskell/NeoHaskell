---
group: platform
component: Framework
impact: compatible
category: Added
---

## Summary

An application can now start from one named Event Model. Its Features register the declared commands, queries, outbound handlers, and typed inbound workers, while the same model supplies configuration and transport. Existing applications that assemble `Application` directly can keep doing so. For a model-backed application, `EventModel.run appEventModel` replaces a second authored registration list, and each query receives events only from its declared sources and payloads.
