---
group: platform
component: Framework
impact: compatible
category: Fixed
---

## Summary

OpenAPI now describes an available protected value with its fields inside `contents`, matching the response returned by an authorized query. Generated clients can read a protected payload such as `firstSecret.contents.name` without a schema mismatch. Applications need no code change.
