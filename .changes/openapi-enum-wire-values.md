---
group: platform
component: Framework
impact: compatible
category: Fixed
---

## Summary

OpenAPI now preserves enum values exactly as declared in your schema.
For example, a derived enum that sends `"Pending"` in JSON now advertises
`"Pending"` in `/openapi.json`. Generated clients can match these values
without a casing mismatch. Your application's JSON format stays the same.
Regenerate clients from the updated specification to get the corrected values.
