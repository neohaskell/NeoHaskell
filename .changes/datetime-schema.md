---
group: platform
component: Framework
impact: compatible
category: Fixed
---

## Summary

Records with `DateTime` or `Maybe DateTime` fields can now derive `ToSchema`. Timestamp fields appear as strings in OpenAPI and JSON Schema, matching their JSON encoding. Applications that defined an orphan `ToSchema DateTime` instance as a workaround must remove that instance to avoid a duplicate-instance error after updating NeoHaskell.
