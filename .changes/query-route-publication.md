---
group: platform
component: Framework
impact: compatible
category: Added
---

## Summary

To publish a mug shop order detail page without exposing an order list, add `only [Lookup]` inside the query declaration. Reading a visible row by UUID returns that row. Requesting the collection returns HTTP 404. Existing queries continue to publish both routes by default.
