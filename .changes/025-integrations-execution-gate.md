---
group: platform
component: Integrations
impact: none
category: Improved
---

## Summary

The complete integration test suite now runs in the Linux and macOS test gates and the local all-tests command. Oura refresh-failure tests use a local fixture instead of an external token endpoint; the resulting failures remain visible until their underlying error handling is repaired. This changes contributor checks, not application behavior; no application update is required.
