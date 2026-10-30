---
group: platform
component: Framework
impact: none
category: Fixed
---

## Summary

The protected-value release notes now describe the erasure operation included in the same release. Custom event-store authors upgrading from 0.13.0 get instructions for all four new record fields, including how stores without protected-value support preserve ordinary events and reject protected values through public JSON APIs. The migration covers JSON values and encoded custom event types. This documentation repair changes no application API or runtime behavior.
